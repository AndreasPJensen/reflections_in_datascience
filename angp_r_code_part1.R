install.packages("jsonlite")   # only if not already installed
library(jsonlite)


raw <- fromJSON("C:/Users/andre/Downloads/casualties_daily.json")
nrow(raw)                          # should be ~1000+
range(as.Date(raw$report_date))    # date range
sum(is.na(raw$killed_cum))         # any missing totals?

install.packages("tidyverse")


library(tidyverse)
library(zoo)

df <- as_tibble(raw) %>%
  mutate(date = as.Date(report_date)) %>%
  select(date,
         total_cum    = killed_cum,
         children_cum = killed_children_cum,
         women_cum    = killed_women_cum) %>%
  arrange(date) %>%
  fill(total_cum, children_cum, women_cum, .direction = "down") %>%
  mutate(
    daily_total    = c(NA, diff(total_cum)),
    daily_children = c(NA, diff(children_cum)),
    daily_women    = c(NA, diff(women_cum)),
    daily_men      = daily_total - daily_children - daily_women
  ) %>%
  filter(!is.na(daily_total))

cat("Series:", as.character(range(df$date)), "| N =", nrow(df), "days\n\n")

# Rolling 15-day CV (SD / mean of daily total)
df <- df %>%
  mutate(cv15 = rollapply(daily_total, width = 15,
                          FUN = function(x) sd(x) / mean(x),
                          align = "right", fill = NA))

wyner_end <- as.Date("2023-11-10")
wyner_cv  <- df %>% filter(date == wyner_end) %>% pull(cv15)
all_cvs   <- df$cv15[!is.na(df$cv15)]

cat("CHERRY-PICK CHECK\n")
cat("  Wyner's window CV:        ", round(wyner_cv, 3), "\n")
cat("  Median CV (all windows):  ", round(median(all_cvs), 3), "\n")
cat("  Mean CV (all windows):    ", round(mean(all_cvs), 3), "\n")
cat("  Min / Max CV:             ",
    round(min(all_cvs), 3), "/", round(max(all_cvs), 3), "\n")
cat("  Percentile of Wyner's CV: ",
    round(100 * mean(all_cvs <= wyner_cv), 1), "%\n")
cat("  # windows with lower CV:  ",
    sum(all_cvs < wyner_cv), "/", length(all_cvs), "\n\n")

w <- df %>% filter(date >= as.Date("2023-10-27"), date <= wyner_end)
cat("WYNER'S WINDOW (Oct 27 - Nov 10, 2023):\n")
cat("  Mean daily total:   ", round(mean(w$daily_total), 1), "\n")
cat("  SD daily total:     ", round(sd(w$daily_total), 1), "\n")
cat("  CV:                 ",
    round(sd(w$daily_total) / mean(w$daily_total), 3), "\n\n")

# Plot 1: rolling CV
p1 <- ggplot(df, aes(date, cv15)) +
  geom_line(colour = "grey40") +
  geom_hline(yintercept = wyner_cv, linetype = "dashed", colour = "red") +
  geom_point(data = filter(df, date == wyner_end),
             colour = "red", size = 3) +
  annotate("text", x = wyner_end, y = wyner_cv,
           label = "Wyner's window", colour = "red",
           hjust = -0.1, vjust = -0.5) +
  labs(title = "Rolling 15-day coefficient of variation",
       subtitle = "Daily Gaza MoH death counts (Oct 2023 - May 2026)",
       x = NULL, y = "CV (SD / mean)") +
  theme_minimal()

# Plot 2: daily counts with Wyner's window flagged
p2 <- ggplot(df, aes(date, daily_total)) +
  geom_line(colour = "grey50") +
  annotate("rect", xmin = as.Date("2023-10-27"), xmax = wyner_end,
           ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.2) +
  labs(title = "Daily reported deaths, full series",
       subtitle = "Red band = Wyner's 15-day window",
       x = NULL, y = "Daily deaths") +
  theme_minimal()

print(p1); print(p2)

# Bonus: cumulative-plot R^2 across all 15-day windows (Pachter's point)
r2_cum <- rollapply(df$daily_total, width = 15,
                    FUN = function(x) {
                      y <- cumsum(x); t <- seq_along(y)
                      summary(lm(y ~ t))$r.squared
                    }, align = "right", fill = NA)

cat("Fraction of 15-day windows with cumulative R^2 > 0.99:",
    round(mean(r2_cum > 0.99, na.rm = TRUE), 3), "\n")
cat("Fraction with R^2 > 0.999:",
    round(mean(r2_cum > 0.999, na.rm = TRUE), 3), "\n")


# --- Inspect the 14 windows with lower CV than Wyner's --------
low_cv_windows <- df %>%
  filter(!is.na(cv15), cv15 < wyner_cv) %>%
  arrange(cv15) %>%
  select(end_date = date, cv15)

# For each, pull the underlying 15 days of data
inspect_window <- function(end_date) {
  df %>%
    filter(date >= end_date - 14, date <= end_date) %>%
    select(date, daily_total, daily_children, daily_women, daily_men) %>%
    mutate(end_date = end_date, .before = 1)
}

cat("WINDOWS WITH CV LOWER THAN WYNER'S (n =", nrow(low_cv_windows), "):\n\n")
print(low_cv_windows)

cat("\nSummary of daily_total in those windows:\n")
low_cv_details <- low_cv_windows$end_date %>%
  map_dfr(inspect_window) %>%
  group_by(end_date) %>%
  summarise(
    mean_daily = round(mean(daily_total), 1),
    sd_daily   = round(sd(daily_total), 1),
    cv         = round(sd(daily_total) / mean(daily_total), 3),
    min_daily  = min(daily_total),
    max_daily  = max(daily_total),
    n_zero     = sum(daily_total == 0),
    n_below_10 = sum(daily_total < 10)
  )
print(low_cv_details)





# Compare Wyner's window only to windows that existed 
# by his publication date (March 2024)
publication_date <- as.Date("2024-03-06")  # Tablet article date

available_by_publication <- df %>%
  filter(!is.na(cv15), date <= publication_date)

wyner_cv_rank_contemporary <- available_by_publication %>%
  summarise(
    n_windows        = n(),
    n_below_wyner    = sum(cv15 < wyner_cv),
    percentile       = round(100 * mean(cv15 <= wyner_cv), 1),
    median_cv        = round(median(cv15), 3),
    mean_cv          = round(mean(cv15), 3),
    min_cv           = round(min(cv15), 3),
    max_cv           = round(max(cv15), 3)
  )

cat("CHERRY-PICK CHECK (windows available by publication, March 6 2024):\n")
print(wyner_cv_rank_contemporary)

# Also: what does the rolling CV look like just for Oct 2023 - Mar 2024?
p3 <- df %>%
  filter(date <= publication_date, !is.na(cv15)) %>%
  ggplot(aes(date, cv15)) +
  geom_line(colour = "grey40") +
  geom_hline(yintercept = wyner_cv, linetype = "dashed", colour = "red") +
  geom_point(data = filter(df, date == wyner_end),
             colour = "red", size = 3) +
  annotate("text", x = wyner_end, y = wyner_cv,
           label = "Wyner's window", colour = "red",
           hjust = 1.1, vjust = -0.5) +
  labs(title = "Rolling 15-day CV (Oct 2023 – Mar 2024 only)",
       subtitle = "Windows available to Wyner at time of publication",
       x = NULL, y = "CV (SD / mean)") +
  theme_minimal()

print(p3)



# Compare Wyner's table to what the JSON gives for the same dates
wyner_check <- df %>%
  filter(date >= as.Date("2023-10-26"), 
         date <= as.Date("2023-11-10")) %>%
  select(date, total_cum, children_cum, women_cum)

print(wyner_check)



# Wyner's exact daily totals from his published table
wyner_exact <- c(334, 341, 302, 304, 216, 280, 256, 196, 228, 285, 252, 306, 241, 249, 260)

cat("Wyner's exact figures:\n")
cat("  Mean:", round(mean(wyner_exact), 1), "\n")
cat("  SD:  ", round(sd(wyner_exact), 1), "\n")
cat("  CV:  ", round(sd(wyner_exact)/mean(wyner_exact), 3), "\n\n")

# Compare to what the JSON gives for the same window
json_daily <- diff(c(7028,7326,7703,8005,8309,8525,8805,9061,
                     9257,9485,9770,10022,10328,10569,10818,11078))

cat("JSON figures for same window:\n")
cat("  Mean:", round(mean(json_daily), 1), "\n")
cat("  SD:  ", round(sd(json_daily), 1), "\n")
cat("  CV:  ", round(sd(json_daily)/mean(json_daily), 3), "\n")


# How does Wyner's exact CV (0.156) rank among contemporaneous windows?
wyner_cv_exact <- sd(wyner_exact) / mean(wyner_exact)

cat("Wyner's exact CV:", round(wyner_cv_exact, 3), "\n")
cat("Percentile (contemporaneous windows):",
    round(100 * mean(available_by_publication$cv15 <= wyner_cv_exact), 1), "%\n")
cat("# windows below:", 
    sum(available_by_publication$cv15 < wyner_cv_exact), 
    "/", nrow(available_by_publication), "\n")

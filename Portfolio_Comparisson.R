library(tidyr)
library(dplyr)

# first we load the two portfolios we want to compare
load("portfolio_burn_in_25_linear_shrinkage_TRUE_robust_linear_shrinkage_FALSE_window_size_Inf.RData")
linear_shrinkage <- portfolio

load("portfolio_burn_in_25_linear_shrinkage_FALSE_robust_linear_shrinkage_TRUE_window_size_Inf.RData")
robust_shrinkage <- portfolio

rm(portfolio)

# now we scrap all irrelevant data
linear_shrinkage <- linear_shrinkage %>% select(date, return, cum_return)
robust_shrinkage <- robust_shrinkage %>% select(date, return, cum_return)

# and now me merge the data
colnames(linear_shrinkage) <- c("date", "return_linear_shrinkage", "cum_return_linear_shrinkage")
colnames(robust_shrinkage) <- c("date", "return_robust_shrinkage", "cum_return_robust_shrinkage")

data <- left_join(x = linear_shrinkage, y = robust_shrinkage, by = "date")

# firstly, thing we do is plotting the two time series against each other
library(ggplot2)

data_long <- data %>%
  pivot_longer(
    cols = starts_with("cum_return_"),
    names_to = "strategy",
    values_to = "cum_return"
  )

ggplot(data_long, aes(x = date, y = cum_return, color = strategy)) +
  geom_line() +
  labs(
    x = "Date",
    y = "Return",
    color = "Strategy"
  ) +
  theme(legend.position = "none")

data <- data %>%
  mutate(diff_cum_return = cum_return_robust_shrinkage - cum_return_linear_shrinkage)

ggplot(data, aes(x = date, y = diff_cum_return)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Date",
    y = "Cumulative Return Difference"
  ) +
  theme_minimal()

# secondly, we perform the kolmogorov smirnov test
library(stats)
ks.test(x = data$return_linear_shrinkage, y = data$return_robust_shrinkage)


# thirdly, we compare compute portfolio metrics
periods_per_year <- 12

calc_annualized <- function(r) {
  m <- mean(r, na.rm = TRUE)
  s <- sd(r, na.rm = TRUE)
  list(
    mean = m * periods_per_year,
    sd = s * sqrt(periods_per_year),
    sharpe = if (s != 0) (m * periods_per_year) / (s * sqrt(periods_per_year)) else NA
  )
}

calc_annualized(data$return_linear_shrinkage)
calc_annualized(data$return_robust_shrinkage)


# fourthly, we examine the drop further
data <- data %>%
  mutate(change_in_diff = diff_cum_return - lag(diff_cum_return))

cut_off <- data %>%
  filter(change_in_diff == min(change_in_diff, na.rm = TRUE)) %>%
  pull(date)

data_cut_off <- data %>% filter(date < cut_off)

calc_annualized(data_cut_off$return_linear_shrinkage)
calc_annualized(data_cut_off$return_robust_shrinkage)



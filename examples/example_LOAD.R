library("tidyverse")
library("pfselect")

price_relative_matrix <- nyse_n[1:100, ]
decay_factor <- 0.5
price_window_size <- 5
momentum_window_size <- 5
historic_price_mean_window_size <- 1

aggregated_momentum_data <- aggregate_price_and_mean_windows(
  price_relative_matrix,
  decay_factor,
  price_window_size,
  historic_price_mean_window_size)
aggregated_momentum_data$momentum <- aggregated_momentum_data %>%
  evaluate_unsigned_momentum()

LOAD_reg_factor <- c(0.1)
LOAD_mom_threshold <- c(0, 0.1, 1.0)

tb <- aggregated_momentum_data %>%
  predict_momentum_LOAD(LOAD_reg_factor, LOAD_mom_threshold,
                        max_cv_window = 10)

ggplot(tb, aes(trading_period, LOAD_regularization_factor)) +
  geom_line(color = "black") +
  geom_line(aes(y = LOAD_momentum_threshold), color = "blue")

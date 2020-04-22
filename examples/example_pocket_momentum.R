library("tidyverse")
library("pfselect")

price_relative_matrix <- nyse_n[200:300, ]
decay_factor <- 0.01
price_window_size <- 10
momentum_window_size <- 5
historic_price_mean_window_size <- 10

pr_eigen <- compute_price_relatives_eigen(price_relative_matrix)
pr_white <- compute_price_relatives_whitener(price_relative_matrix)

# Get windows
aggregated_momentum_data <- aggregate_price_and_mean_windows(
    price_relative_matrix,
    decay_factor,
    price_window_size,
    historic_price_mean_window_size)

# rebase
asset_rownames <- 1:ncol(price_relative_matrix)

aggregated_momentum_data %>%
  rebase_agg_windows(new_assets = tail(pr_eigen$vectors, -9),
                     asset_rownames = asset_rownames,
                     scalar_columns_to_rebase = c("price",
                                                  "historic_price_mean"))

# Compute momentum
aggregated_momentum_data$momentum <- aggregated_momentum_data %>%
  select(price, next_price, historic_price_mean) %>%
  pmap_int(evaluate_signed_momentum_at_window)
# Rescale to have positive trends
aggregated_momentum_data <- aggregated_momentum_data %>%
  mutate(feature = map(price_window, ~(. / lag(.))[-1]))

tb <- aggregated_momentum_data %>%
  predict_momentum_pocket(feature_colname = "feature",
                          weight_elimination = c(0, 0.1, 0.5, 1.0),
                          maxit = 250)

tb %>% count(momentum)
tb %>% summarise(mean_err = mean(pocket_error, na.rm = TRUE),
                 mean_prediction = mean(pocket_prediction, na.rm = TRUE),
                 mean_weight_elim = mean(pocket_weight_elimination, na.rm = TRUE))

tb %>%
  sample_n(10) %>%
  mutate(x = list(1:9),
         price_window = map(price_window, ~.[-1] / .[1])) %>%
  unnest(c(price_window, x)) %>%
  ggplot(aes(x = x,
             y = price_window,
             group = interaction(trading_period, asset))) +
    geom_point() +
    geom_line() +
    facet_wrap(~momentum)

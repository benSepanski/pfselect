library("tidyverse")
library('pfselect')
# Plot
load("price_matrix.rds")

price_relative_matrix <- price_matrix %>%
    purrr::array_branch(1L) %>%
    {list(head(., -1), tail(., -1))} %>%
    purrr::pmap(~.x / .y) %>%
    purrr::flatten_dbl() %>%
    matrix(byrow = TRUE, ncol = ncol(price_matrix)) %>%
    `colnames<-`(colnames(price_matrix)) %>%
    `rownames<-`(rownames(price_matrix) %>% head(-1))

windows <- aggregate_price_and_mean_windows(price_relative_matrix, 0.5, 2, 1)

windows$momentum <- windows %>%
  select(price, historic_price_mean, next_price) %>%
  pmap_dbl(evaluate_signed_momentum_at_window)
windows <- windows %>%
  mutate(window_id = map(price_window, seq_along)) %>%
  unnest(c(price_window, window_id)) %>%
  pivot_wider(names_from = window_id,
              values_from = price_window,
              names_prefix = "price_window"
              )

windows %>%
  sample_n(500) %>%
  ggplot(aes(price_window1, price_window2, color = factor(momentum))) +
    geom_point(na.rm = TRUE) +
    lims(y = c(NA, 3), x = c(NA, 3)) +
    labs(x = "Previous Price",
         y = "Current Price",
         color = "Momentum",
         title = "500 Samples of Price Windows")



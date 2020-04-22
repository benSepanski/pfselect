library(tidyverse)
library(pfselect)

cum_wealth = list()
pr <- nyse_n
tr <- 0.00  # transaction rate

bah <- backtest_market(pr, tr)
cum_wealth$bah <- evaluate_cumulative_wealth(head(bah, -1),
                                             price_relative_matrix = pr,
                                             transaction_rate = tr)
best <- backtest_best_stock(pr, tr)
cum_wealth$best <- evaluate_cumulative_wealth(head(best, -1),
                                              price_relative_matrix = pr,
                                              transaction_rate = tr)

eg <- backtest_exponential_gradient(pr, tr, learning_rate = 0.1)
cum_wealth$eg <- evaluate_cumulative_wealth(head(eg, -1),
                                            price_relative_matrix = pr,
                                            transaction_rate = tr)

ns <- backtest_online_newton_step(pr, tr)
cum_wealth$`Newton Step` <- evaluate_cumulative_wealth(head(ns, -1),
                                                       price_relative_matrix = pr,
                                                       transaction_rate = tr)
up <- backtest_universal_portfolio(pr, tr, 1000)
cum_wealth$up <- evaluate_cumulative_wealth(head(up, -1),
                                            price_relative_matrix = pr,
                                            transaction_rate = tr)

onlLOAD <- backtest_LOAD(pr, tr,
                         decay_factor = 0.5,              # value from paper
                         regularization_factor = 1.0,     # guess, but right picture
                         time_window = 5,                 # value from paper
                         momentum_threshold = 0.1,        # value from paper
                         wealth_factor_threshold = 1.10)   # guess, but right picture
cum_wealth$LOAD <- evaluate_cumulative_wealth(head(onlLOAD, -1),
                                              price_relative_matrix = pr[4:nrow(pr),],
                                              transaction_rate = tr)

cum_wealth %>%
  map(data.frame) %>%
  map(`colnames<-`, "cumulative_wealth") %>%
  map(mutate, trading_period = 1:length(cumulative_wealth)) %>%
  bind_rows(.id = "method") %>%
  ggplot(aes(trading_period, cumulative_wealth, color = method)) +
    geom_line() +
    scale_y_log10()
    #scale_y_log10(limits = c(0.98,1e10))

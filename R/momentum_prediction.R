#' Evaluate momentum of the price
#'
#' We propose three estimators for the next price, and define
#' its momentum by whichever is closest:
#' the max of previous prices is momentum +1, the min of previous
#' prices is momentum -1, and the historical mean is momentum 0
#'
#' Note that this definition of momentum is scale-invariant, i.e.
#' if all the prices are scaled (and the historic price means
#' computed on these scaled prices) the momentum results are identical.
#'
#' @param previous_price prices of asset in period \eqn{t-\tau+1,...t}
#' @param price price of asset in period \eqn{t+1}
#' @param historic_price_mean historic price mean at period \eqn{t}.
#'     For some \eqn{\alpha\in[0,1]}, the historic price mean
#'     is \eqn{MA_t = \alpha p_t + (1-\alpha) MA_{t-1}}, with
#'     \eqn{MA_1 = p_1}.
#'
#' @return the momentum defined in the description
#'
#' @export
#'
evaluate_momentum <- function(previous_prices, price, historic_price_mean) {
  ests <- c(min(previous_prices), historic_price_mean, max(previous_prices))
  # index mapped from [1 2 3] -> [-1 0 1]
  which.min(abs(ests - price)) - 2
}


# LOAD --------------------------------------------------------------------


#' predicts momentum for asset according to LOAD strategy
#'
#' predicts next momentum for one asset
#'  given previous prices according to LOAD
#' strategy (see paper mentioned in description of
#' \code{\link{backtest_LOAD}}), momentu is defined in
#' \code{\link{evaluate_momentum}}.
#'
#' @note NO TYPE CHECKING IS PERFORMED... be careful
#'
#' @param prev_prices a numeric vector of previous prices
#' @param historic_mean The historic mean of the prices
#' @param min_var (OPTIONAL) minimum variance to be determined non-constant
#' @inheritParams backtest_LOAD
#'
#' @return the predicted next momentum (1 or 0)
#'
#' @importFrom glmnet glmnet
#'
predict_momentum_LOAD <- function(prev_prices,
                                  historic_mean,
                                  regularization_factor,
                                  momentum_threshold,
                                  min_var = .Machine$double.eps ^ 0.25) {
  regularized_slope <- 0
  if(var(prev_prices) > min_var) {  # avoid constant case
    # Note glmnet requires we have at least two variables but we only have
    # one, so we just duplicate the variable and sum them together
    regularized_slope <- Matrix::colSums(
      glmnet(x = matrix(rep(1:length(prev_prices), 2), ncol = 2),
             y = prev_prices,
             alpha = 0,  # alpha = 0 -> ridge regression
             lambda = regularization_factor)$beta)
  }
  # mom is 1 if growing faster than momentum threshold
  as.numeric(regularized_slope > momentum_threshold)
}


#' Test momentum prediction using LOAD strategy
#'
#' Test LOAD's strategy of predicting momentum. Returns
#' each momentum prediction.
#' See \code{\link{evaluate_momentum}} for a description
#' of momentum
#'
#' @return A ntime_periods x nassets matrix, with each entry
#'     LOAD's predicted momentum for that time period (i.e.
#'     before seeing that time period) or NA
#'
#' @inheritParams backtest_LOAD
#' @export
evaluate_momentum_predictions_LOAD <- function(price_relatives,
                                               decay_factor,
                                               regularization_factor,
                                               time_window,
                                               momentum_threshold,
                                               wealth_factor_threshold,
                                               prices,
                                               price_means) {
  # compute prices and price means if missing
  if(missing(prices)) {
    prices <- prices_from_relatives(price_relatives,
                                    rep(1, ncol(price_relatives)))
  }
  if(missing(price_means)) {
    price_means <- historical_price_means(prices, decay_factor = decay_factor)
  }

  # now get momentum for each asset at each time
  price_relatives %>%
    purrr::array_branch(2L) %>%
    purrr::map(rollify_dbl(predict_momentum_LOAD,
                           window_sizes = c(time_window, 1)),
               regularization_factor = regularization_factor,
               momentum_threshold = momentum_threshold) %>%
    purrr::flatten_dbl() %>%
    matrix(ncol = ncol(price_relatives))
}













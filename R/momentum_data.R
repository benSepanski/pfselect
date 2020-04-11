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
#' @param price_window of asset in period \eqn{t-\tau+1,...t}
#' @param next_price price of asset in period \eqn{t+1}
#' @param historic_price_mean historic price mean at period \eqn{t}.
#'     For some \eqn{\alpha\in[0,1]}, the historic price mean
#'     is \eqn{MA_t = \alpha p_t + (1-\alpha) MA_{t-1}}, with
#'     \eqn{MA_1 = p_1}.
#' @param consider_negative_momentum If \code{FALSE}, then
#'     does not consider -1 as an option for momentum, i.e.
#'     only checks historic mean and max of previous prices
#'
#' @return the momentum defined in the description
#'
evaluate_momentum <- function(price_window, next_price, historic_price_mean,
                              consider_negative_momentum = TRUE) {
  if(consider_negative_momentum) {
    ests <- c(min(previous_prices), historic_price_mean, max(previous_prices))
    # index mapped from [1 2 3] -> [-1 0 1]
    return(which.min(abs(ests - price)) - 2)
  }
  ests <- c(historic_price_mean, max(previous_prices))
  # index mapped from [1 2] -> [0 1]
  which.min(abs(ests - price)) - 1
}

#' Get momentum data from price relatives
#'
#' Given price relatives for a price and an alpha to use for
#' the historic price means, return a tibble with columns
#'
#' @param price_relatives A numeric vector of the price
#' @param decay_factor The decay factor for the historic means
#'      (as in \link{compute_historical_price_means})
#' @inheritParams evaluate_momentum
#'
#' @return A tibble holding momentum and the trading period,
#'     as in the description
evaluate_momentum_data <- function(prices, price_means, time_window,
                                   consider_negative_momentum = TRUE) {
  prices = compute_pric
  price_means = compute_historical_price_means()
}














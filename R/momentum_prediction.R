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
evaluate_momentum <- function(previous_prices, price, historic_price_mean) {
  ests <- c(min(previous_prices), historic_price_mean, max(previous_prices))
  # index mapped from [1 2 3] -> [-1 0 1]
  which.min(abs(ests - price)) - 2
}

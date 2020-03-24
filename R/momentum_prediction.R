
# Analysis ----------------------------------------------------------------


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
#' @param consider_negative_momentum If \code{FALSE}, then only
#'     does not consider -1 as an option for momentum, i.e.
#'     only checks historic mean and max of previous prices
#'
#' @return the momentum defined in the description
#'
#' @export
#'
evaluate_momentum <- function(previous_prices, price, historic_price_mean,
                              consider_negative_momentum = TRUE) {
  if(consider_negative_momentum) {
    ests <- c(min(previous_prices), historic_price_mean, max(previous_prices))
    # index mapped from [1 2 3] -> [-1 0 1]
    return(which.min(abs(ests - price)) - 2)
  }
  ests <- c(historic_price_mean, max(previous_prices))
  which.min(abs(ests - price)) - 1
}


#' Compute a confusion matrix for price momentum
#'
#' Given prices and historical price means, as well as predicted momenta,
#' produce a confusion matrix for the momentum
#'
#' @inheritParams predict_momentum_LOAD
#' @inheritParams evaluate_momentum
#' @param predicted_momentum A matrix the same dimension as prices
#'     holding the predicted momentum or NA
#'
#' @return A tibble holding the entries of
#'     a confusion matrix for the momentum
#'
#' @importFrom magrittr %>%
#'
#' @export
momentum_confusion_table <- function(prices,
                                      historic_price_means,
                                      time_window,
                                      predicted_momentum,
                                      consider_negative_momentum = TRUE) {

  x <- 1
  y <- 2
  prices %>%
    get_all_previous_price_windows(time_window) %>%
    dplyr::mutate(hpm = historic_price_means[matrix(c(trading_period, asset),
                                                    ncol = 2)
                                             ]) %>%
    dplyr::transmute(
      predicted = predicted_momentum[trading_period],
      actual = purrr::pmap_dbl(
        list(previous_price_window, price, hpm),
        evaluate_momentum,
        consider_negative_momentum = consider_negative_momentum)
      ) %>%
    dplyr::group_by(predicted, actual) %>%
    dplyr::count()
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
#' @param min_var (OPTIONAL) minimum variance to be determined non-constant
#' @inheritParams backtest_LOAD
#'
#' @return the predicted next momentum (1 or 0)
#'
#' @importFrom glmnet glmnet
#'
predict_price_momentum_LOAD <- function(prev_prices,
                                        regularization_factor,
                                        momentum_threshold,
                                        min_var = .Machine$double.eps ^ 0.5) {
  regularized_slope <- 0
  # avoid constant case
  if(var(prev_prices) > min_var) {
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
#' @param prices The prices during the trading periods (a matrix
#'     with one more row than \code{price_relatives}).
#' @param price_means
#'     The mean price \eqn{MA_t} is \code{decay_factor} * \eqn{p_t}
#'     + \code{(1-decay_factor)} * \eqn{MA_{t-1}}, a matrix the
#'     same size as \code{prices}.
#'
#' @return A ntime_periods x nassets matrix, with each entry
#'     LOAD's predicted momentum for that time period (i.e.
#'     before seeing that time period) or NA
#'
#' @inheritParams backtest_LOAD
#'
#' @importFrom magrittr %>%
#'
#' @export
predict_momentum_LOAD <- function(decay_factor,
                                  regularization_factor,
                                  time_window,
                                  momentum_threshold,
                                  prices) {
  # now get momentum for each asset at each time
  prices %>%
    purrr::array_branch(2L) %>%
    purrr::map(rollify_dbl(predict_price_momentum_LOAD,
                           window_sizes = time_window),
               regularization_factor = regularization_factor,
               momentum_threshold = momentum_threshold) %>%
    purrr::flatten_dbl() %>%
    matrix(ncol = ncol(prices))
}


# Regularized Pocket ------------------------------------------------------

#' Predict the momentum using a cross-validated regularized pocket algorithm
#'
#' Cross validates over the first
#' num_train parameters from the given weight elimination parameters
#' and weight decay parameters over time. Then predicts
#' the remaining momenta.
#'
#' @inheritParams predict_momentum_LOAD
#' @param price_means the historic mean of the prices (exponentially
#'      weighted average of past prices)
#' @inheritParams nested_cv_regularized_pocket
#' @export
predict_momentum_reg_pocket <- function(prices,
                                        time_window,
                                        price_means,
                                        weight_elimination,
                                        weight_decay,
                                        nfolds,
                                        maxit_per_fold,
                                        num_train) {
  x <- matrix(nrow = ncol(prices) * (nrow(prices) - time_window+1))
  for(i in time_window:nrow(prices)) {
    for(j in 1:ncol(prices)) {
      prev_prices <- prices[i, (j-time_window+1):j]
      x[ncol(prices) * j + (i - time_window + 1), ] <- prev_prices
      y[ncol(prices) * j + (i-time_window + 1)] <- evaluate_momentum(prev_prices,
                                                                     prices[i][j],
                                                                     price_means[i-1][j])
    }
  }
  cv_errs <- nested_cv_regularized_pocket(x[1:num_train, ],
                                          y[1:num_train],
                                          weight_elimination,
                                          weight_decay,
                                          nfolds,
                                          maxit_per_fold)
  row <- which.min(cv_errs$cv_err)
  momentum <- rep(NA_real_, nrow(prices))
  for(i in (num_train+1):(prices - time_window)) {

  }
}














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
    tidyr::drop_na() %>%
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
#' @inheritParams nested_cv
#' @param price_means the historic mean of the prices (exponentially
#'      weighted average of past prices)
#' @param weight_elimination A list of weight elimination constants
#'      to use during the nested cross-validation.
#' @param nfolds the number of folds to cross-validate on at each
#'      time-step
#' @param trading_period_probs The relative probability of each trading period,
#'      used for sampling and error evaluation
#' @param maxit_per_fold maximum number of iterations in the pocket
#'      algorithm per fold
#' @param num_train The number of trading periods to train on
#'      before predicting anything.
#'
#' @return the predicted momentum as a vector
#'
#' @importFrom magrittr %>%
#' @export
#'
predict_momentum_reg_pocket_cv <- function(prices,
                                           time_window,
                                           price_means,
                                           weight_elimination,
                                           trading_period_probs,
                                           nfolds,
                                           maxit_per_fold,
                                           num_train) {
  stop("In development")
  assert_that(is_whole_number(num_train))
  assert_that(num_train > time_window)

  # As required by nested_cv
  # windows_and_probs should have column $row_probs, and other columns
  # cast-able to a matrix after row_probs is dropped
  # output just the y
  pocket_tuned_by_elim <- function(elim, windows_and_probs, output) {
    row_probs <- windows_and_probs$row_probs
    x <- as.matrix(mutate(windows_and_probs, row_probs = NULL))

    initial_weights <- NULL
    prev_weights <- get("previous_data", envir = parent.frame())
    if(length(prev_weights) > 0) {
      initial_weights <- prev_weights[length(prev_weights)]
    }
    weights <- regularized_pocket(x = x,
                                  y = output,
                                  weight_elimination = elim,
                                  maxit = maxit_per_fold,
                                  row_probs = row_probs,
                                  initial_weights = initial_weights
                                  )
    # record the weights in parent frame
    assign("previous_data",
           append(prev_weights, weights),
           envir = parent.frame())
    # return an error function
    function(windows_and_probs, output) {
      row_probs <- windows_and_probs$row_probs
      x <- as.matrix(mutate(windows_and_probs, row_probs = NULL))
      weighted.mean(sign(drop(weights[1] + x %*% weights[-1])) != output,
                    w = row_probs)
    }
  }

  # this will hold our predicted momentum
  momentum <- rep(NA_real_, nrow(prices))
  # This houses momentum and windows, with extra info
  data <- prices %>%
    get_all_previous_price_windows(time_window) %>%
    dplyr::mutate(hpm = price_means[matrix(c(trading_period, asset), ncol = 2)
                                    ]) %>%
    dplyr::mutate(
      momentum = purrr::pmap_dbl(
        list(previous_price_window, price, hpm),
        evaluate_momentum,
        consider_negative_momentum = FALSE)
      )
  windows_and_probs <- data$previous_price_window %>%
    purrr::flatten_dbl() %>%
    matrix(ncol = time_window, byrow = TRUE) %>%
    tibble::as_tibble()
  windows_and_probs$row_probs <- trading_period_probs[data$trading_period]

  # CV on training data
  cv_res <- nested_cv(f = pocket_tuned_by_elim,
                      data = dplyr::slice(windows_and_probs, 1:num_train),
                      output = data$momentum[1:num_train],
                      tuning_parameters = weight_elimination,
                      maxit_per_fold = maxit_per_fold,
                      nfolds = nfolds)
  elim <- weight_elimination[[which.min(cv_res$err)]]

  # now predict all the momentum
  weights <- NULL
  windows <- mutate(windows_and_probs, row_probs = NULL)
  for(i in (num_train+1):nrow(prices)) {
    weights <- regularized_pocket(x = dplyr::slice(windows, 1:(i-1)),
                                  y = data$momentum[1:(i-1)],
                                  weight_elimination = elim,
                                  maxit = maxit_per_fold,
                                  row_probs = row_probs[1:(i-1)],
                                  initial_weights = weights
                                  )
    momentum[i] <- sign(drop(weights[1] + windows[i, ] %*% weights[-1]))
    # CV with new training data to pick next elim
  cv_res <- nested_cv(f = pocket_tuned_by_elim,
                      data = dplyr::slice(windows_and_probs, 1:i),
                      output = data$momentum[1:i],
                      tuning_parameters = weight_elimination,
                      maxit_per_fold = maxit_per_fold,
                      nfolds = nfolds)
    elim <- weight_elimination[[which.min(cv_res$err)]]
  }
  momentum
}

#' Predict the momentum using a regularized pocket algorithm
#'
#' Trains on the first
#' num_train parameters from the given weight elimination parameters.
#' Then, predicts the remaining momenta.
#'
#' @inheritParams predict_momentum_reg_pocket_cv
#' @param weight_elimination the weight elimination factor to use
#' @param maxit the max number of iterations when training
#' @param num_train the number of initial trading periods to train on
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
predict_momentum_reg_pocket <- function(prices,
                                        time_window,
                                        price_means,
                                        weight_elimination,
                                        trading_period_probs,
                                        maxit,
                                        num_train) {
  assert_that(is_whole_number(num_train))
  assert_that(num_train > time_window)

  # This houses momentum and windows, with extra info
  data <- prices %>%
    get_all_previous_price_windows(time_window) %>%
    dplyr::mutate(hpm = price_means[matrix(c(trading_period, asset), ncol = 2)
                                    ]) %>%
    dplyr::mutate(
      momentum = purrr::pmap_dbl(
        list(previous_price_window, price, hpm),
        evaluate_momentum,
        consider_negative_momentum = FALSE)
      )

  y <- 2 * data$momentum - 1  # [0 1] -> [-1 1]
  x <- data$previous_price_window %>%
    purrr::flatten_dbl() %>%
    matrix(ncol = time_window, byrow = TRUE) %>%
    tibble::as_tibble()
  x$price_means = lag(price_means)[matrix(c(data$trading_period, data$asset),
                                          ncol = 2)]
  x <- as.matrix(x)
  row_probs <- trading_period_probs[data$trading_period]

  nassets <- ncol(prices)
  weights <- regularized_pocket(x = x[1:(num_train*nassets),],
                                y = y[1:(num_train*nassets)],
                                weight_elimination = weight_elimination,
                                maxit = maxit,
                                row_probs = row_probs[1:(num_train*nassets)],
                                )
  # this will hold our predicted momentum
  predicted_momentum <- rep(NA_real_, length(data$momentum))
  remainder <- (num_train*nassets+1):length(predicted_momentum)
  predicted_momentum[remainder] <-
    sign(drop(weights[1] + x[remainder, ] %*% weights[-1]))
  predicted_momentum <- (predicted_momentum + 1) / 2 # [-1 1] -> [0 1]

  reshaped_mom <- matrix(nrow = nrow(prices), ncol = ncol(prices))
  reshaped_mom[matrix(c(data$trading_period, data$asset), ncol = 2)] <-
    predicted_momentum
  reshaped_mom
}












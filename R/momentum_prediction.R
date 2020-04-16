
# LOAD --------------------------------------------------------------------


#' predicts momentum for asset according to LOAD strategy
#'
#' predicts next momentum for one asset
#'  given previous prices according to LOAD
#' strategy (see paper mentioned in description of
#' \code{\link{backtest_LOAD}}), momentu is defined in
#' \code{\link{evaluate_momentum_at_window}}.
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
predict_window_momentum_LOAD <- function(prev_prices,
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
  as.integer(regularized_slope > momentum_threshold)
}


#' Test momentum prediction using LOAD strategy
#'
#' Test LOAD's strategy of predicting momentum. Returns
#' each momentum prediction.
#' See \code{\link{evaluate_momentum_at_window}} for a description
#' of momentum
#'
#' @param regularization_factor A regularization factor used in the
#'     LOAD prediction, if a vector then cross-validates
#'     (at each step choosing the factor that had the
#'     least error over previous time, random first time)
#' @param momentum_threshold The threshold to be classified as
#'     with momentum. If a vector then cross-validates.
#'     (at each step choosing the factor that had the
#'     least error over previous time, random first time)
#' @param aggregated_momentum_data A tibble
#'     of class \code{@eval{AGG_WINDOW_CLASS_NAME}} as returned from
#'     \code{\link{aggregate_price_and_mean_windows}}, with an
#'     added column of column name \code{momentum_colname}
#'     holding the momentum.
#' @param max_cv_window If \code{NULL} then checks over all
#'     past error, otherwise only checks the past \code{max_cv_window}
#'     time steps to determine error.
#' @param momentum_colname The name of the column holding the
#'     true momentum values (as a string),
#'     defaults to \code{"nonnegative_momentum"}.
#'
#' @return \code{aggregated_momentum_data}
#'     with added columns:
#'     \code{LOAD_prediction} holding LOAD's predicted
#'     momentum for that time, \code{regularization_factor}
#'     holding the regularization factor used in that prediction,
#'     \code{momentum_threshold} holding the
#'     momentum threshold used in that prediction.
#'
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#'
#' @export
predict_momentum_LOAD <- function(aggregated_momentum_data,
                                  regularization_factor,
                                  momentum_threshold,
                                  max_cv_window = NULL,
                                  momentum_colname = "nonnegative_momentum") {
  # type checks
  assert_that(inherits(aggregated_momentum_data, AGG_WINDOW_CLASS_NAME))
  assert_that(rlang::has_name(aggregated_momentum_data, momentum_colname))
  assert_that(rlang::is_double(regularization_factor))
  assert_that(all(regularization_factor >= 0))
  assert_that(rlang::is_double(momentum_threshold))
  assert_that(all(momentum_threshold >= 0))
  assert_that(is.null(max_cv_window) || is_whole_number(max_cv_window))
  assert_that(rlang::is_scalar_character(momentum_colname))

  if(is.null(max_cv_window)) {
    max_cv_window <- nrow(aggregated_momentum_data)
  }

  # First we will just compute predictions for all, then we'll
  # go back and "pick the one we would have chosen"
  tb <- aggregated_momentum_data
  # get all predictions possible
  all_predictions <- list(reg = regularization_factor,
                          mom = momentum_threshold) %>%
    purrr::cross_df() %>%
    dplyr::group_by(reg, mom) %>%
    dplyr::group_modify(
      ~tibble::tibble(
        predictions = purrr::map_int(pull(tb, price_window),
                                     predict_window_momentum_LOAD,
                                     regularization_factor = pull(.y, reg),
                                     momentum_threshold = pull(.y, mom)),
        trading_period = pull(tb, trading_period),
        asset = pull(tb, asset),
        error = pull(tb, !! enquo(nonnegative_momentum)) != predictions)
    ) %>%
    dplyr::ungroup()
  # Determine which ones would have been selected at each stage
  selected_predictions <- all_predictions %>%
    dplyr::group_by(reg, mom, trading_period) %>%
    dplyr::summarise(trading_period_err = mean(error)) %>%
    dplyr::ungroup(trading_period) %>%
    dplyr::arrange(trading_period) %>%
    dplyr::mutate(
      cum_err = cumsum(trading_period_err) - trading_period_err,
      cv_err = cum_err - dplyr::lag(cum_err, n = max_cv_window, default = 0),
      trading_period_err = NULL,
      cum_err = NULL) %>%
    dplyr::ungroup(reg, mom) %>%
    dplyr::group_by(trading_period) %>%
    dplyr::top_n(1, desc(cv_err)) %>%
    dplyr::sample_n(1) %>%
    dplyr::ungroup(trading_period) %>%
    dplyr::select(-cv_err) %>%
    dplyr::left_join(all_predictions, by = c("reg", "mom", "trading_period"))%>%
    dplyr::left_join(aggregated_momentum_data, by = c("trading_period", "asset")) %>%
    dplyr::rename(LOAD_regularization_factor = reg,
                  LOAD_momentum_threshold = mom,
                  LOAD_prediction = predictions,
                  LOAD_error = error)
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












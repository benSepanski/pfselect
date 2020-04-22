
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
#'     defaults to \code{"momentum"}. Should be nonnegative momentum
#'     values (see \code{\link{evaluate_momentum_at_window}})
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
                                  momentum_colname = "momentum") {
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
        error = pull(tb, !! enquo(momentum_colname)) != predictions)
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

#' Perform regularized pocket algorithm
#'
#' Performs the Perceptron Learning Algorithm with weight elimination
#'
#' \deqn{
#'     [[sign(w^Tx_n) \neq y_n]]
#'     + \frac{\lambda}{2n}\sum_{i=1}^d\frac{w_i^2}{1+w_i^2}
#' }
#' We treat the PLA update as a ``derivative" of the first component.
#' So, our update in the \eqn{i}th component will be
#' \deqn{
#'     PLA_update
#'     - \frac{\lambda}{n}\frac{w_i}{(1+w_i^2)^2}
#' }
#'
#' @param x A numeric matrix with \eqn{n} rows. Should NOT include
#'     a column of all 1s for bias weight.
#' @param y a numeric vector with \eqn{n} columns
#' @param weight_elimination \eqn{\lambda} in the description
#' @param maxit the maximum number of iterations
#' @param initial_weights the initial weights. If missing or NULL,
#'    uses linear regression
#'
#' @return perceptron weights (bias, weights) where
#'     \eqn{y ~ bias + x * weights}. Attribute \code{"niter"}
#'     holds the iteration count
#'
#' @importFrom assertthat assert_that are_equal
#' @importFrom stats lsfit
#' @export
regularized_pocket <- function(x, y,
                               weight_elimination,
                               maxit,
                               initial_weights = NULL) {
  # Some type checks
  assert_that(is_numeric_matrix(x))
  assert_that(nrow(x) > 0)
  assert_that(ncol(x) > 0)
  assert_that(is_numeric_vector(y))
  assert_that(are_equal(nrow(x), length(y)))
  assert_that(rlang::is_scalar_double(weight_elimination))
  assert_that(is_whole_number(maxit))
  assert_that(maxit > 0)
  if(all( !is.null(initial_weights) )) {
    assert_that(rlang::is_double(initial_weights, n = ncol(x)+1, finite = TRUE))
    assert_that(all( !is.na(initial_weights)))
  }
  else {
    assert_that(all(is.null(initial_weights)))
  }

  ## NOTE: In the documentation, for initial_weights, in the returne weights,
  #        and in standard notation the first component of the weights is
  #        the bias. However, for ease of computation, we split
  #        the bias and weights in the INTERNALS ONLY of this function.

  # initialize weights with linear regression if NULL
  if(is.null(initial_weights)) {
    if(nrow(x) > ncol(x)) {
      least_squares_sol <- lsfit(x = x, y = y)
      bias <- least_squares_sol$coef[1]
      weights <- least_squares_sol$coef[-1]
    }
    else {
      bias <- rnorm(1, sd = 0.05)
      weights <- rnorm(ncol(x), sd = 0.05)
    }
  }
  else {
    bias <- initial_weights[1]
    weights <- initial_weights[-1]
  }
  # Begin PLA
  lambda <- weight_elimination / nrow(x)
  misclassified <- sign(bias + x %*% weights) != y
  error <- mean(misclassified)
  iter <- 1
  while(error > 0 && iter <= maxit) {
    iter <- iter + 1
    # Get PLA update
    star <- sample(which(misclassified), 1)
    pla_update <- c(y[star], y[star] * x[star,])
    # Get shrinking update
    shrink_update <- c(0, -lambda * weights / (1 + weights^2)^2)
    for(update in list(pla_update, shrink_update)) {
      new_weights <- weights + update[-1]
      new_bias <- bias + update[1]
      # if new weights are better, use them!
      new_misclassified <- sign(new_bias + x %*% new_weights) != y
      new_error <- mean(new_misclassified)
      if(new_error <= error) {
        weights <- new_weights
        bias <- new_bias
        misclassified <- new_misclassified
        error <- new_error
      }
    }
  }
  weights <- c(bias, weights)
  attr(weights, "niter") <- iter
  weights
}


#' Predicts momentum using a regularized pocket algorithm
#'
#' See \code{\link{regularized_pocket}()} for a description of the
#' algorithm used.
#' Either uses the \code{weight_elimination} constant given
#' or cross-validates at each trading period to choose one of
#' the selected.
#' If there is at least one cv asset,
#' then each stage runs once with previous weights and once from linear
#' regression weights, then picks whichever does best on cv error
#'
#' @inheritParams regularized_pocket
#' @inheritParams predict_momentum_LOAD
#' @param data A tibble
#'     of class \code{@eval{AGG_WINDOW_CLASS_NAME}} as returned from
#'     \code{\link{aggregate_price_and_mean_windows}}, with an
#'     added column of column name \code{momentum_colname}
#'     holding the momentum.
#' @param feature_colname The column name of the features to use.
#'     Should be a list column, each list entry holding a double vector.
#' @param momentum_colname The name of the column holding the
#'     true momentum values (as a string),
#'     defaults to \code{"momentum"}. Should be (possibly negative) momentum
#'     values (see \code{\link{evaluate_momentum_at_window}})
#' @param weight_elimination A double vector, at each stage picks the
#'     one which has performed best in the past cv window (or all of the
#'     past, whichever is selected)
#'
#' @return A tibble like \code{data} but with a \code{pocket_prediction}
#'     \code{pocket_error}, and \code{pocket_weight_elimination},
#'     columns holding the absolute value momentum prediction,
#'     whether it was an error, and the elimination weight used.
#'
#' @importFrom magrittr %>%
#' @export
predict_momentum_pocket <- function(data,
                                    feature_colname,
                                    weight_elimination,
                                    maxit,
                                    momentum_colname = "momentum",
                                    max_cv_window = NULL) {
  # type checks
  # TODO Check feature columns don't already exist
  # TODO Check to make sure we drop any na's
  assert_that(inherits(data, AGG_WINDOW_CLASS_NAME))
  assert_that(rlang::has_name(data, momentum_colname))
  assert_that(rlang::has_name(data, feature_colname))
  assert_that(!rlang::has_name(data, "pocket_prediction"))
  assert_that(!rlang::has_name(data, "pocket_error"))
  assert_that(!rlang::has_name(data, "pocket_weight_elimination"))
  assert_that(!rlang::has_name(data, "pocket_feature_index"))
  assert_that(rlang::is_double(weight_elimination))
  assert_that(is.null(max_cv_window) || is_whole_number(max_cv_window))
  assert_that(rlang::is_scalar_character(feature_colname))
  assert_that(rlang::is_scalar_character(momentum_colname))
  assert_that(is_whole_number(maxit))

  if(is.null(max_cv_window)) {
    max_cv_window <- nrow(data)
  }
  # Spread out features into columns
  momentum <- enquo(momentum_colname)
  spread_data <- data %>%
    tidyr::unnest(!!feature_colname) %>%
    dplyr::group_by(trading_period, asset) %>%
    dplyr::mutate(pocket_feature_index = as.character(
      glue::glue("pocket_feature_{1:n()}"))) %>%
    tidyr::pivot_wider(names_from = pocket_feature_index,
                       values_from = !!feature_colname) %>%
    dplyr::ungroup(trading_period, asset) %>%
    dplyr::mutate(pm_one_momentum = sign(.data[[momentum_colname]] * 2 - 1)) %>%
    dplyr::select(trading_period,
                  asset,
                  pm_one_momentum,
                  tidyselect::matches("pocket_feature_\\d+"))

  # Get unique trading periods and assets
  trading_periods <- data %>%
    dplyr::distinct(trading_period) %>%
    dplyr::pull(trading_period)
  # Make big x and y
  y <- spread_data %>%
    dplyr::pull(pm_one_momentum)
  x <- spread_data %>%
    dplyr::select(tidyselect::matches("pocket_feature_\\d+")) %>%
    as.matrix()
  # TODO Make sure more than 1 trading period
  # Store cumulative errors
  we_errors <- tibble::tibble(trading_period = trading_periods,
                              errors = list(rep(0, length(weight_elimination)))
                              )
  # To store all the results
  pocket_results <- spread_data %>%
    dplyr::select(trading_period, asset) %>%
    dplyr::mutate(pocket_prediction = NA,
                  pocket_error = NA,
                  pocket_weight_elimination = NA)
  iter <- 1
  for(tp in trading_periods[-1]) {
    # pick out rows, and which weight elimination to use
    prev_rows <- which(spread_data$trading_period < tp)
    tp_rows <- which(spread_data$trading_period == tp)
    we_index <- nnet::which.is.max(-we_errors$errors[[iter]])
    # predict momentum
    predictions <- weight_elimination %>%
      purrr::map(~regularized_pocket(x = x[prev_rows, ],
                                     y = y[prev_rows],
                                     weight_elimination = .,
                                     maxit = maxit)) %>%
      purrr::map(~sign(.[1] + x[tp_rows,] %*% .[-1]))
    # get correct values
    true_momentum <- spread_data %>%
      dplyr::slice(tp_rows) %>%
      dplyr::pull(pm_one_momentum)
    # evaluate errors for this trading period
    tp_errors <- predictions %>%
      purrr::map(~. != true_momentum)
    # Store results
    pocket_results$pocket_prediction[tp_rows] <- 0.5 * (
      predictions[[we_index]] + 1)
    pocket_results$pocket_weight_elimination[tp_rows] <-
      weight_elimination[we_index]
    pocket_results$pocket_error[tp_rows] <- tp_errors[[we_index]]

    # update errors to possibly choose new coefficient next time
    we_errors$errors[[iter+1]] <- we_errors$errors[[iter]] +
      purrr::map_dbl(tp_errors, mean)
    if(iter - max_cv_window >= 1) {
      we_errors$errors[[iter+1]] <- we_errors$errors[[iter+1]] -
        we_errors$errors[[iter - max_cv_window]]
    }
    # mark next iteration
    iter <- iter + 1
  }
  data %>%
    dplyr::full_join(pocket_results, by = c("trading_period", "asset"))
}


# Pocket Features --------------------------------------------------------


# Price Rebasing =========================================================


#' Rebase to new linear combination of assets
#'
#' Change the price windows and historic price mean windows
#' in trading period \eqn{i} to represent a
#' linear combination of prices, the \eqn{j}th combination being the \eqn{j}th
#' column of the matrix which is the \eqn{i}th entry of \code{new_assets}.
#'
#' @param agg_windows A tibble
#'     of class \code{@eval{AGG_WINDOW_CLASS_NAME}} as returned from
#'     \code{\link{aggregate_price_and_mean_windows}}
#' @param new_assets A list, whose \eqn{i}th entry is a square matrix.
#'     Each column represents a new
#'     linear combination of the assets to consider replacing
#'     one of the old assets.
#' @param asset_rownames These are the names of the assets of
#'     \code{agg_windows} in the order which they correspond to
#'     the rows of the matrices in \code{new_assets}
#' @param scalar_columns_to_rebase A character vector of
#'     column names of \code{agg_windows} to be rebased. These
#'     must be double columns.
#'
#' @return the new tibble with rebased values
#'
#' @importFrom assertthat assert_that
#' @importFrom utils head tail
#' @export
rebase_agg_windows <- function(agg_windows,
                               new_assets,
                               asset_rownames,
                               scalar_columns_to_rebase = NULL) {
  # type checks
  # TODO MORE type checking
  # TODO check columns
  assert_that(inherits(agg_windows, AGG_WINDOW_CLASS_NAME))
  assert_that(is.list(new_assets))
  assert_that(is.null(scalar_columns_to_rebase) ||
              rlang::is_character(scalar_columns_to_rebase))
  if(is.null(scalar_columns_to_rebase)) {
    scalar_columns_to_rebase = list()
  }
  #
  rebase_scalar <- function(tb, scalar_colname) {
    # Assume tbl already ordered by (trading_period, asset)
    new_col <- tb %>%
      dplyr::pull(scalar_colname) %>%
      matrix(byrow = TRUE, ncol = length(asset_rownames)) %>%
      purrr::array_branch(1L) %>%
      purrr::map2(new_assets, ~t(.x[asset_rownames]) %*% .y) %>%
      purrr::flatten_dbl()
    tb[[scalar_colname]] <- new_col
    tb
  }
  # now rebase any requested scalars
  agg_windows <- agg_windows %>%
    dplyr::arrange(trading_period, asset)
  for(colname in scalar_columns_to_rebase) {
    agg_windows <- agg_windows %>% rebase_scalar(colname)
  }
  # Now rebase all the windows
  for(window in c("price_window", "historic_price_mean_window")) {
    window_col <- agg_windows %>%
      dplyr::pull(window)
    window_lengths <- map_dbl(window_col, length)
    # Windows must all be same size for this to work
    assert_that(min(window_lengths) == max(window_lengths))
    # Repeatedly operate on ith entry inwindow
    for(i in 1:min(window_lengths)) {
      0
      ith_scalar <- agg_windows %>%
        dplyr::mutate(window_scalar_col = purrr::map_dbl(window_col, ~.[[i]])) %>%
        rebase_scalar("window_scalar_col") %>%
        dplyr::pull("window_scalar_col")
      for(j in seq_along(ith_scalar)) {
        window_col[[i]][j] <- ith_scalar[j]
      }
    }
    agg_windows[[window]] <- window_col
  }
  # Return rebased tibble
  agg_windows
}

#' Get eigenvalue decompositions for each time step
#'
#' Returns a list with two sublists: values and vectors.
#' For \eqn{i >= 3}, the \eqn{i}th values and vectors are the
#' eigenvalue decomposition of the covariance matrix where we
#' treat the first \eqn{i-1} rows as observations and the columns as variables.
#'
#' For \eqn{i=1,2} The values are all one and the vectors form the identity
#' matrix
#'
#' @param price_relative_matrix A matrix of price relatives
#'     with at least three rows.
#' @return A list with two sublists, see description for details
#'
#' @importFrom stats cov
#' @importFrom assertthat assert_that
compute_price_relatives_eigen <- function(price_relative_matrix) {
  # some type checks
  assert_that(is_numeric_matrix(price_relative_matrix))
  assert_that(nrow(price_relative_matrix) >= 3)
  # Now get all the eigen vectors, with identity thrown in for first one
  nassets <- ncol(price_relative_matrix)
  id_decomp <- list(values = rep(1, nassets), vectors = diag(nassets))
  3:nrow(price_relative_matrix) %>%
    purrr::map(~cov(price_relative_matrix[1:(.-1), ])) %>%
    purrr::map(eigen, symmetric = TRUE) %>%
    purrr::prepend(list(id_decomp, id_decomp)) %>%
    purrr::transpose()
}

#' Get whitener for each time step
#'
#' Returns a list of matrices.
#' For \eqn{i >= 3}, the \eqn{i}th values and vectors are the
#' negative square root of the covariance matrix where we
#' treat the first \eqn{i-1} rows as observations and the columns as variables.
#'
#' For \eqn{i=1,2} it's the identity matrix.
#'
#' @inheritParams compute_price_relatives_eigen
#' @return A list of whitening matrices, see description for details
#'
#' @importFrom stats cov
#' @importFrom assertthat assert_that
compute_price_relatives_whitener <- function(price_relative_matrix) {
  # some type checks
  assert_that(is_numeric_matrix(price_relative_matrix))
  # Get all the eigen vectors and take negative square root of each one
  neg_sqrt <- function(values, vectors) {
    # if near zero may be negative
    values[values > 0] <- sqrt(values[values > 0])
    values[values > 1e-7] <- 1 / values[values > 1e-7]
    vectors %*% diag(values) %*% t(vectors)
  }
  price_relative_matrix %>%
    compute_price_relatives_eigen() %>%
    purrr::pmap(neg_sqrt)
}


# Feature Extension -------------------------------------------------------

#' Rescales windows to positive slope
#'
#' Rescales windows (\code{price_window} and \code{historic_price_mean_window})
#' by \eqn{\pm 1} so that each the most recent price is greater than
#' or equal to the most recen thistoric price mean.
#'
#' @param agg_windows a \code{@eval{AGG_WINDOW_CLASS_NAME} } instance
#'     as returned from \code{\link{aggregate_price_and_mean_windows}}.
#' @return \code{agg_windows} but modified to have an extra column
#'     \code{flip_sign} which is -1 if the sign was flipped and 1 otherwise
#'     as well as having rescaled
#'     the \code{price_window} and \code{historic_price mean_window}
#'     columns by \code{flip_sign}
#'
#' @export
rescale_to_price_over_mean <- function(agg_windows) {
  # type check
  assert_that(inherits(agg_windows, AGG_WINDOW_CLASS_NAME))
  assert_that(!has_name(agg_windows, "cor_sign"))
  # Now make the cor sign
  get_flip_sign <- function(price, historic_price_mean) {
    s <- sign(price - historic_price_mean)
    if(s == 0) {
      s <- 1
    }
    s
  }
  agg_windows %>%
    dplyr::mutate(
      flip_sign = purrr::map2_dbl(price, historic_price_mean, get_flip_sign),
      price_window = purrr::map2(price_window, flip_sign, `*`),
      historic_price_mean_window = purrr::map2(historic_price_mean_window,
                                               flip_sign,
                                               `*`)
    )
}
















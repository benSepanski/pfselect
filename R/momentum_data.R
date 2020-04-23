

# Price and mean windows --------------------------------------------------

AGG_WINDOW_CLASS_NAME <- "pfselect_aggregated_windows"
#TODO Make this a class with helper functions instead of
#     how it is...

#' Get Price and historic price mean windows from price relatives
#'
#' Given price relatives and window sizes, returns a tibble
#' with price windows, historic mean windows, and momentum
#' values if desired.
#'
#' @inheritParams compute_price_matrix_from_relatives
#' @inheritParams compute_historical_price_mean_matrix
#' @param momentum_window_size determines how momentum is computed.
#' @param historic_mean_window_size The size of the historic price mean
#'     window to include
#' @param include_missing If \code{TRUE}, includes rows for trading
#'     periods with either missing windows or missing next values/momenta
#'
#' @return A tibble (arranged by \code{c(trading_period, asset)})
#'   subclassed to be of class \code{@eval{AGG_WINDOW_CLASS_NAME}}
#'   with columns
#'   \describe{
#'   \item{trading_period}{The trading period during which prices change}
#'   \item{asset}{The name (or column index if no names are present)
#'                of the asset, corresponding with the column names
#'                of the argument \code{price_relative_matrix}}
#'   \item{price}{The price before the prices change at the end of
#'                the trading period}
#'   \item{next_price}{The price after the prices change at the end of
#'                     the trading period}
#'   \item{price_window}{The \code{price_window_size} previous prices
#'                       (including the price during the trading period)
#'                       in chronological order (i.e. oldest first,
#'                       newest last)}
#'   \item{historic_price_mean}{The historic price mean
#'                              with decay factor \code{decay_factor}
#'                              (see \link{compute_historic_price_means})
#'                              during trading (before prices changes at the end
#'                              of the current trading period)}
#'   \item{next_historic_price_mean}{The historic price mean
#'                                   with decay factor \code{decay_factor}
#'                                  (see \link{compute_historic_price_means})
#'                                  after trading (after prices change
#'                                   at the end of the current trading period)}
#'   \item{historic_price_mean_window}{The previous
#'                                     \code{historic_mean_window_size}
#'                                     many historic price means. These
#'                                     are not used to compute anything,
#'                                     but may be desired as features.
#'                                     They are also in chronological order,
#'                                     oldest -> newest.}
#'  }
#'  You should not modify the values of these columns except using
#'  methods defined in this package (unless you know what you're doing...
#'  you know who you are)
#'
#' @importFrom assertthat assert_that are_equal
#' @export
aggregate_price_and_mean_windows <- function(price_relative_matrix,
                                             decay_factor,
                                             price_window_size,
                                             historic_mean_window_size,
                                             initial_prices = NULL,
                                             include_missing = FALSE,
                                             .check_input = TRUE) {
  assert_that(rlang::is_scalar_logical(.check_input))
  if(.check_input) {
    assert_that(is_numeric_matrix(price_relative_matrix))
    if(any(!is.null(initial_prices))) {
      assert_that(is_numeric_vector(initial_prices))
      assert_that(are_equal(length(initial_prices),
                            ncol(price_relative_matrix)))
    }
    assert_that(is_whole_number(price_window_size))
    assert_that(price_window_size > 0)
    assert_that(is_whole_number(historic_mean_window_size))
    assert_that(historic_mean_window_size > 0)
    assert_that(rlang::is_scalar_logical(include_missing))
  }
  if(any(is.null(initial_prices))) {
    initial_prices = rep(1, ncol(price_relative_matrix))
  }
  # get prices and historical means
  price_matrix <- compute_price_matrix_from_relatives(
    price_relative_matrix = price_relative_matrix,
    initial_prices = initial_prices,
    .check_input = .check_input
  )
  mean_price_mat <- compute_historical_price_mean_matrix(
    price_matrix = price_matrix,
    decay_factor = decay_factor,
    .check_input = .check_input
  )
  # Now compute price and historic mean windows of the desired size
  price_windows <- price_matrix %>%
    purrr::array_branch(2L) %>%
    purrr::map_dfr(get_windows,
                   window_size = price_window_size,
                   include_missing = include_missing,
                   .id = "asset") %>%
    dplyr::rename(trading_period = next_index,
                  price = current_value,
                  next_price = next_value,
                  price_window = window)
  mean_windows <- mean_price_mat %>%
    purrr::array_branch(2L) %>%
    purrr::map_dfr(get_windows,
                   window_size = historic_mean_window_size,
                   include_missing = include_missing,
                   .id = "asset") %>%
    dplyr::rename(trading_period = next_index,
                  historic_price_mean = current_value,
                  next_historic_price_mean = next_value,
                  historic_price_mean_window = window)
  # join price and price means
  by <- c("trading_period", "asset")
  if(include_missing) {
    windows <- dplyr::full_join(price_windows, mean_windows, by = by)
  }
  else {
    windows <- dplyr::inner_join(price_windows, mean_windows, by = by)
  }

  class(windows) <- c(AGG_WINDOW_CLASS_NAME, class(windows))
  windows %>%
    dplyr::arrange(trading_period, asset)
}


#' Puts variable in trading_period x asset matrix
#'
#' @param agg_windows a \code{@eval{AGG_WINDOW_CLASS_NAME} } instance
#'     as returned from \code{\link{aggregate_price_and_mean_windows}}.
#' @param variable_name A string which is a variable name of a double
#'     vector column of \code{agg_windows}.
#'
#' @return A matrix with entries from variable \code{variable_name},
#'     the column names being the \code{asset} variable from \code{agg_windows}
#'     and the row names being the \code{trading_period} variable from
#'     \code{agg_windows}.
#'
#' @importFrom assertthat assert_that has_name
#' @export
aggregated_windows_var_to_matrix <- function(agg_windows, variable_name) {
  # type checks
  assert_that(inherits(agg_windows, AGG_WINDOW_CLASS_NAME))
  assert_that(has_name(agg_windows, variable_names))
  assert_that(rlang::is_double(dplyr::pull(agg_windows, variable_name)))
  # get asset names
  asset_names <- agg_windows %>%
    dplyr::distinct(asset) %>%
    dplyr::pull(asset)
  # get trading periods
  trading_period_names <- agg_windows %>%
    dplyr::distinct(trading_period) %>%
    dplyr::pull(trading_period)
  # arrange in column-major order then return as a matrix
  agg_windows %>%
    dplyr::arrange(asset, trading_period) %>%
    dplyr::pull(!! enquo(variable_name)) %>%
    matrix(ncol = length(asset_names)) %>%
    `colnames<-`(asset_names) %>%
    `rownames<-`(trading_period)
}


# Momentum Evaluation -----------------------------------------------------


#' Evaluate momentum of the price
#'
#' We propose two estimators for the next price, and define
#' its momentum by whichever is closest:
#' the max of previous prices is momentum +1,
#' and the historical mean is momentum 0
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
#'
#' @return the momentum defined in the description if all arguments
#'     are present. If any of the first
#'     three arguments are \code{NA}, returns \code{NA}.
#'
#' @export
evaluate_momentum_at_window <- function(price_window,
                                        next_price,
                                        historic_price_mean) {
  if(any(is.na(c(price_window, next_price, historic_price_mean)))) {
    return(NA)
  }
  ests <- c(historic_price_mean, max(price_window))
  # index mapped from [1 2] -> [0 1]
  which.min(abs(ests - next_price)) - 1L
}

#' Return the signed momentum
#'
#' Returns \code{1L} if \code{next_price} is further from
#' \code{historic_price_mean} than \code{price}, and \code{0L} otherwise.
#'
#' @param price the current price (a scalar double)
#' @param next_price the next price (a scalar double)
#' @param historic_price_mean The current historic price mean (a scalar double)
#' @return \code{0L} or \code{1L}, or \code{NA} if any of the arguments
#'     is missing
#'
#' @export
evaluate_signed_momentum_at_window <- function(price,
                                               next_price,
                                               historic_price_mean) {
  if(any(is.na(c(price, next_price, historic_price_mean)))) {
    return(NA)
  }
  old_sign <- sign(price - historic_price_mean)
  new_sign <- sign(next_price - historic_price_mean)
  if(old_sign == new_sign) {
    return(1L)
  }
  0L
}


#' Evaluates momentum on aggregated windows
#'
#' Given a \code{@eval{AGG_WINDOW_CLASS_NAME}} object as returned
#' from \code{\link{aggregate_price_and_mean_windows}}, computes
#' the momentum at each window (as in \code{evaluate_momentum_at_window})
#'
#' @param agg_windows The \code{@eval{AGG_WINDOW_CLASS_NAME}} object
#'     as returned from \code{\link{aggregate_price_and_mean_windows}}
#' @param momentum_window_size The size of the window used to determine
#'     momentum. Must be at least 2 and no longer than
#'     the price windows in \code{agg_windows}. Defaults to the
#'     length of the price windows in \code{agg_windows}.
#'
#' @return An integer array whose \eqn{i}th entry holds the momentum
#'     corresponding to the \eqn{i}th row of \code{agg_windows}.
#'
#' @importFrom assertthat assert_that
#' @export
evaluate_unsigned_momentum <- function(agg_windows, momentum_window_size) {
  assert_that(inherits(agg_windows, AGG_WINDOW_CLASS_NAME))
  # get size of price windows
  price_window_size <- agg_windows %>%
    select(price_window) %>%
    drop_na() %>%
    pull(price_window) %>%
    pluck(1L) %>%
    length()
  if(missing(momentum_window_size)) {
    momentum_window_size <- price_window_size
  }
  assert_that(is_whole_number(momentum_window_size))
  assert_that(2 <= momentum_window_size &&
                momentum_window_size <= price_window_size)
  # now compute momentum
  agg_windows %>%
    mutate(price_window = purrr::map(price_window,
                                     tail,
                                     momentum_window_size)) %>%
    dplyr::select(price_window, next_price, historic_price_mean) %>%
    purrr::pmap_int(evaluate_momentum_at_window)
}

#' Evaluate momentum price prediction
#'
#' Give the price predicted by the momentum.
#'
#' @param aggregated_momentum_data A tibble
#'     of class \code{@eval{AGG_WINDOW_CLASS_NAME}} as returned from
#'     \code{\link{aggregate_price_and_mean_windows}}, with an
#'     added column of column name \code{momentum_colname}
#'     holding the momentum.
#' @param momentum_window_size the size determining momentum,
#'     which must be at least 2 and is assumed
#'     to be at most the size of the price windows in \code{agg_windows}
#' @param momentum_colname The name of the column holding the
#'     true momentum values (as a string),
#'     defaults to \code{"momentum"}.
#' @param consider_negative_momentum \code{TRUE} to treat momentum as
#'     signed (see \code{\link{evaluate_signed_momentum_at_window}()}),
#'     otherwise assumed to be regular momentum
#'     (see \code{\link{evaluate_momentum_at_window}()})
#'
#' @return A column vector whose \eqn{i}th entry is the predicted
#'     price for the \eqn{i}th row, using the predicted momentum
#'     as a guide.
#'
#' @importFrom assertthat assert_that has_name
#' @export
evaluate_momentum_predict_price <- function(agg_windows,
                                            momentum_window_size,
                                            momentum_colname = "momentum",
                                            use_signed_momentum = TRUE) {
  # type checks
  assert_that(inherits(agg_windows, AGG_WINDOW_CLASS_NAME))
  assert_that(is_whole_number(momentum_window_size))
  assert_that(momentum_window_size > 1)
  assert_that(rlang::is_scalar_character(momentum_colname))
  assert_that(has_name(agg_windows, momentum_colname))
  assert_that(rlang::is_scalar_logical(use_signed_momentum))
  assert_that(use_signed_momentum %in% c(TRUE, FALSE))
  # now do the predictions
  if(use_signed_momentum) {

    price_predictor <- function(price_window, historic_price_mean, momentum) {
      if(any(is.na(c(price_window, historic_price_mean, momentum)))) {
        return(NA)
      }
      # If momentum is 1, take extremal value in window on right side
      # of the mean
      if(momentum == 1L) {
        if(tail(price_window, 1) > historic_price_mean) {
          return(max(tail(price_window, momentum_window_size)))
        }
        return(min(tail(price_window, momentum_window_size)))
      }
      # Otherwise return the historic price mean
      historic_price_mean
    }
  }
  else {
    price_predictor <- function(price_window, historic_price_mean, momentum) {
      if(any(is.na(c(price_window, historic_price_mean, momentum)))) {
        return(NA)
      }
      if(momentum == 1L) {
        return(max(tail(price_window, momentum_window_size)))
      }
      historic_price_mean
    }
  }

  agg_windows %>%
    dplyr::select(price_window,
                  historic_price_mean,
                  !!enquo(momentum_colname)) %>%
    dplyr::rename(momentum = !!enquote(momentum_colname)) %>%
    purrr::pmap_dbl(price_predictor)
}











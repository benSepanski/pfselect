
# pfselectstrat -----------------------------------------------------------


#' makes a new \code{"pfselectstrat"} class
#'
#' Creates a \code{"pfselectstrat"} class, which has a number
#' of assets and a number of time periods. We think of each
#' time period as corresponding to a change of prices. At the
#' beginning of the time period the prices have not yet changed,
#' and at the end of the time period they have changed to the next prices.
#'
#' For validation of \code{pfselectstrat} objects, see
#' \code{\link{validate_pfselectstrat}()}
#'
#' @note This is a class that should be built on, not instantiated
#'     and used directly by end users
#'
#' @param nassets A positive whole number, the number of assets
#' @param ntime_periods A positive whole number, the number of time periods.
#'     The prices before the first trading period we think of as p_0,
#'     and the first trading period occurs right before the prices change
#'     to p_1
#' @param ... Arguments for subclassing, these are NOT set as attributes
#'     but instead are passed as arguments into the list which
#'     underlies this data structure
#' @param class A character vector of the child classes this instance
#'     has (or \code{character()} if no child classes)
#'
#'  @return A new pfselectstrat object with the given
#'      \code{nassets} and \code{ntime_periods}
#'
#' @keywords internal
#' @family pfselectstrat
#'
#' @importFrom assertthat assert_that
new_pfselectstrat <- function(nassets, ntime_periods, ...,
                              class = character()) {
  # make sure nassets and ntime_periods are scalar
  assert_that(is.numeric(nassets))
  assert_that(is.numeric(ntime_periods))

  structure(
    list(nassets = nassets,
         ntime_periods = ntime_periods,
         ...),
    class = c(class, "pfselectstrat")
  )
}

#' validates a \code{"pf_selectstrat"} class
#'
#' Asserts that the given object has values \code{nassets}
#' and \code{ntime_periods} which are both positive whole numbers
#'
#' @param x the \code{"pf_selectstrat"} instance to validate
#'
#' @family pfselectstrat
#' @importFrom assertthat assert_that has_name
validate_pfselectstrat <- function(x) {
  values <- unclass(x)
  # make sure we have nassets and ntime_periods
  assert_that(has_name(values), c("nassets", "ntime_periods"))
  # make sure nassets and ntime_periods are positive whole numbers
  assert_that(is_whole_number(values$nassets))
  assert_that(is_whole_number(values$ntime_periods))
  assert_that(values$nassets > 0)
  assert_that(values$ntime_periods > 0)
}

#' Get first portfolio from strategy
#'
#' Returns the first portfolio for the given strategy.
#' We assume trading is about to begin at the given trading period
#'
#' @param strategy the strategy to use when determining the first portfolio.
#'     Should be of class \code{pfselectstrat}
#' @param trading_period the "first" trading period, i.e. we might
#'     be starting backtesting in the middle of our data.
#'
#'  @return A portfolio: a numeric vector with length equal to
#'      \code{strategy$nassets}, each entry representing the
#'      proportion of total wealth in the corresponding asset.
#'
#' @family pfselectstrat
#' @export
first_portfolio <- function(strategy, trading_period) {
  UseMethod("first_portfolio")
}

#' @export
first_portfolio.default <- function(strategy, trading_period) {
  assert_that("pfselectstrat" %in% class(strategy))
  stop(
    glue::glue("No first_portfolio method found for class {class(strategy)}"))
}

#' Get next portfolio from strategy
#'
#' Return the next portfolio for the given strategy.
#' We assume trading is about to begin at the given trading period.
#'
#' @param strategy the strategy to use when determining the first portfolio.
#'     Should be of class \code{pfselectstrat}
#' @param trading_period the "first" trading period, i.e. we might
#'     be starting backtesting in the middle of our data.
#' @param portfolio The portfolio at the beginning of this trading
#'     period (w.r.t. to the prices before the
#'     prices change according to the price relatives of this trading
#'     period)
#'
#'  @return A portfolio: a numeric vector with length equal to
#'      \code{strategy$nassets}, each entry representing the
#'      proportion of total wealth in the corresponding asset.
#'      This should be the portfolio w.r.t. the current prices
#'      (before the prices change according to the price relatives
#'      of this trading periot)
#'
#' @family pfselectstrat
#' @export
next_portfolio <- function(strategy, trading_period, portfolio) {
  UseMethod("next_portfolio")
}

#' @export
next_portfolio.default <- function(strategy, trading_period, portfolio) {
  assert_that("pfselectstrat" %in% class(strategy))
  stop(
    glue::glue("No next_portfolio method found for class {class(strategy)}"))
}


# buyandhold --------------------------------------------------------------

#' makes a \code{buyandhold} strategy
#'
#' Makes a \code{buyandhold} strategy from the given price
#' relatives, which is a subclass of \code{pfselectstrat}.
#'
#' To validate the object use \code{\link{validate_buyandhold}}.
#'
#' @note End users should not call \link{new_buyandhold} directly,
#'     but instead use one of its subclasses with a defined
#'     initialization method.
#'
#' @keywords internal
#' @family buyandhold
#'
#' @inheritParams new_pfselectstrat
#' @param price_relatives A T x n matrix of price relatives,
#'     where each row represents a trading period and each column
#'     represents an asset. A price relative is the current
#'     price divided by the previous price.
#'
#' @seealso new_pfselectstrat
#'
#' @return a new \code{buyandhold} object with the given price relatives
#'
new_buyandhold <- function(price_relatives, ..., class = character()) {
  new_pfselectstrat(nassets = ncol(price_relatives),
                    ntime_periods = nrow(price_relatives),
                    price_relatives = price_relatives,
                    ...,
                    class = c(class, "buyandhold"))
}

#' validates a buyandhold object
#'
#' Asserts that the dimensions of the price_relatives matches
#' the number of trading period and number of assets,
#' that all the price relatives are non-negative,
#' as well as type checking. Calls \code{\link{validate_pfselectstrat}}.
#'
#' @param x the \code{"buyandhold"} instance to validate
#'
#' @family buyandhold
#' @importFrom assertthat assert_that has_name are_equal
validate_buyandhold <- function(x) {
  validate_pfselectstrat(x)
  values <- unclass(x)
  assert_that(has_name(x, "price_relatives"))
  assert_that(is.matrix(values$price_relatives))
  assert_that(is.numeric(values$price_relatives))
  assert_that(are_equal(values$nassets, ncol(values$price_relatives)))
  assert_that(are_equal(values$ntime_periods, nrow(values$price_relatives)))
  assert_that(all(values$price_relatives >= 0))
}

#' @export
next_portfolio.buyandhold <- function(strategy, trading_period, portfolio) {
  portfolio
}


## uniform_bah =============================================================

#' @describeIn new_buyandhold
#'
#' Starts with a uniform amount of wealth in each stock.
new_uniform_bah <- function(price_relatives, ..., class = character()) {
  new_buyandhold(price_relatives = price_relatives,
                 ...,
                 class = c(class, "uniform_bah"))
}

#' Makes a new \code{uniform_bah} strategy
#'
#' @inherit new_uniform_bah title description return
#' @inheritParams new_uniform_bah
#' @family buyandhold
#'
#' @seealso new_uniform_bah
#'
#' @export
uniform_bah <- function(price_relatives) new_uniform_bah(price_relatives)

#' @export
first_portfolio.uniform_bah <- function(strategy, trading_period) {
  rep(1/strategy$nassets, strategy$nassets)
}

## best_stock =============================================================

#' @describeIn new_buyandhold
#'
#' Makes a buyandhold strategy which invests all its wealth
#' in the best stock from the start of the trading period
#' through the given last_trading_period
#'
#' @param last_trading_period the last trading period after
#'      which we "sell" our stocks (we need this to determine
#'      which stock is the best for the \code{best_stock} strategy).
#'
new_best_stock <- function(price_relatives,
                           last_trading_period,
                           ..., class = character()) {
  new_buyandhold(price_relatives = price_relatives,
                 last_trading_period = last_trading_period,
                 ...,
                 class = c(class, "best_stock"))
}

#' Makes a new \code{best_stock} strategy
#'
#' @inherit new_best_stock title description return
#' @inheritParams new_best_stock
#' @family buyandhold
#'
#' @note if last_trading_period is missing, it defaults
#'      to the last available trading period
#'
#' @seealso new_best_stock
#'
#' @export
best_stock <- function(price_relatives, last_trading_period) {
  if(missing(last_trading_period)) {
    last_trading_period <- nrow(price_relatives)
  }
  new_best_stock(price_relatives, last_trading_period)
}

#' @export
first_portfolio.best_stock <- function(strategy, trading_period) {
  if(trading_period > strategy$last_trading_period) {
    stop("start trading period is after last trading period")
  }
  # compute the best stock
  best_stock <- price_relatives %>%
    `[`(trading_period:last_trading_period, ) %>%
    purrr::array_branch(2L) %>%
    purrr::map_dbl(~reduce(.x, `*`)) %>%  # get wealth of each stock
    which.max()                           # pick the best one

  portfolio <- rep(0, nassets)
  portfolio[best_stock] <- 1.0
  portfolio
}


# pfselectstrat -----------------------------------------------------------


#' makes a new \code{"pfselectstrat"} class
#'
#' Creates a \code{"pfselectstrat"} class, which has a number
#' of assets and a number of trading periods. We think of each
#' trading period as corresponding to a change of prices. At the
#' beginning of the trading period the prices have not yet changed,
#' and at the end of the trading period they have changed to the next prices.
#'
#' The class is functionally a list with named entries
#' \describe{
#'    \item{nassets}{The number of assets}
#'    \item{ntrading_periods}{The number of trading periods}
#'    \item{price_relatives}{A \code{ntrading_periods} x \code{nassets}
#'    matrix of price relatives (price after trading period / price
#'    at beginning of trading period)}
#'    \item{transaction_rate}{A rate in \eqn{[0,1]} which is
#'    lost during each buy/sell}
#' }
#'
#' For validation of \code{pfselectstrat} objects, see
#' \code{\link{validate_pfselectstrat}()}
#'
#' @note This is a class with no next_portfolio and no first_portfolio
#'     method
#'
#' @param price_relatives A T x n matrix of price relatives,
#'     where each row represents a trading period and each column
#'     represents an asset. A price relative is the
#'     price after the trading period divided by the price
#'     during the trading period.
#' @param transaction_rate a transaction rate in \eqn{[0,1]},
#'     the percentage of each buy/sell lost to fees
#' @param ... Arguments for subclassing, these are NOT set as attributes
#'     but instead are passed as arguments into the list which
#'     underlies this data structure
#' @param class A character vector of the child classes this instance
#'     has (or \code{character()} if no child classes)
#'
#'  @return A new pfselectstrat object with the given \code{price_relatives}
#'      \code{nassets} and \code{ntrading_periods}
#'
#' @family pfselectstrat
#'
#' @importFrom assertthat assert_that
new_pfselectstrat <- function(price_relatives,
                              transaction_rate,
                              ...,
                              class = character()) {
  # make sure price_relatives and transaction_rate are numeric
  assert_that(is.numeric(price_relatives))
  assert_that(is.numeric(transaction_rate))

  structure(
    list(nassets = ncol(price_relatives),
         ntrading_periods = nrow(price_relatives),
         price_relatives = price_relatives,
         transaction_rate = transaction_rate,
         ...),
    class = c(class, "pfselectstrat")
  )
}

#' validates a \code{"pf_selectstrat"} class
#'
#' Asserts that the given object has values \code{nassets}
#' and \code{ntrading_periods} which are both positive whole numbers.
#' Makes sure they match the dimensions of price_relatives as for a
#' \link[=new_pfselectstrat]{pfselectstrat} object. Makes sure
#' transaction_rate is in \eqn{[0,1]} and scalar
#'
#' @param x the \link[=new_pfselectstrat]{pfselectstrat} instance to validate
#'
#' @family pfselectstrat
#' @importFrom assertthat assert_that has_name are_equal
#' @importFrom rlang is_scalar_double
validate_pfselectstrat <- function(x) {
  values <- unclass(x)
  # make sure we have all required member attributes
  assert_that(has_name(values),
              c("nassets", "ntrading_periods",
                "price_relatives", "transaction_rate"))
  # make sure nassets and ntrading_periods are positive whole numbers
  assert_that(is_whole_number(values$nassets))
  assert_that(is_whole_number(values$ntrading_periods))
  assert_that(values$nassets > 0)
  assert_that(values$ntrading_periods > 0)
  # make sure price relatives is a numeric matrix of non-negative
  # entries which is ntrading_periods x nassets
  assert_that(is.matrix(values$price_relatives))
  assert_that(is.numeric(values$price_relatives))
  assert_that(are_equal(values$nassets, ncol(values$price_relatives)))
  assert_that(are_equal(values$ntrading_periods, nrow(values$price_relatives)))
  assert_that(all(values$price_relatives >= 0))
  # transaction rate checks
  assert_that(is_scalar_double(values$transaction_rate))
  assert_that(values$transaction_rate > 0 && values$transaction_rate < 1)
}

#' Get first portfolio from strategy
#'
#' Returns the first portfolio for the given strategy.
#' We assume trading is about to begin at the given trading period
#'
#' @param strategy the strategy to use when determining the first portfolio.
#'     Should be of class \link[=new_pfselectstrat]{pfselectstrat}
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
#'     Should be of class \link[=new_pfselectstrat]{pfselectstrat}
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
#' relatives, which is a subclass of \link[=new_pfselectstrat]{pfselectstrat}.
#'
#' To validate the object use \code{\link{validate_buyandhold}}.
#'
#' @note An instance which is just of class \code{buyandhold} and on subclass
#'     has no first_portfolio method
#'
#' @family buyandhold
#'
#' @inheritParams new_pfselectstrat
#'
#' @seealso new_pfselectstrat
#'
#' @return a new \code{buyandhold} object with the given price relatives
#'
new_buyandhold <- function(price_relatives, transaction_rate,
                           ..., class = character()) {
  new_pfselectstrat(price_relatives, transaction_rate,
                    ..., class = c(class, "buyandhold"))
}

#' @describeIn validate_pfselectstrat validates a buyandhold object
validate_buyandhold <- function(x) { validate_pfselectstrat(x) }

#' @export
next_portfolio.buyandhold <- function(strategy, trading_period, portfolio) {
  portfolio
}


## uniform_bah =============================================================

#' @describeIn new_buyandhold
#'
#' Starts with a uniform amount of wealth in each stock.
new_uniform_bah <- function(price_relatives, transaction_rate,
                            ..., class = character()) {
  new_buyandhold(price_relatives = price_relatives,
                 transaction_rate = transaction_rate,
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
uniform_bah <- function(price_relatives, transaction_rate) {
   new_uniform_bah(price_relatives, transaction_rate)
}

#' @export
first_portfolio.uniform_bah <- function(strategy, trading_period) {
  rep(1/strategy$nassets, strategy$nassets)
}

## best_stock =============================================================

#' @describeIn new_buyandhold
#'
#' Makes a \link[=new_buyandhold]{buyandhold} strategy which invests all its
#' wealth in the best stock from the start of the trading period
#' through the given last_trading_period
#'
#' @param last_trading_period the last trading period after
#'      which we "sell" our stocks (we need this to determine
#'      which stock is the best for the \code{best_stock} strategy).
#'
new_best_stock <- function(price_relatives,
                           transaction_rate,
                           last_trading_period,
                           ..., class = character()) {
  new_buyandhold(price_relatives = price_relatives,
                 transaction_rate = transaction_rate,
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
best_stock <- function(price_relatives, transaction_rate, last_trading_period) {
  if(missing(last_trading_period)) {
    last_trading_period <- nrow(price_relatives)
  }
  new_best_stock(price_relatives, transaction_rate, last_trading_period)
}

#' @export
first_portfolio.best_stock <- function(strategy, trading_period) {
  if(trading_period > strategy$last_trading_period) {
    stop("start trading period is after last trading period")
  }
  # compute the best stock
  best_stock <- strategy$price_relatives %>%
    `[`(trading_period:strategy$last_trading_period, ) %>%
    purrr::array_branch(2L) %>%
    purrr::map_dbl(~reduce(.x, `*`)) %>%  # get wealth of each stock
    which.max()                           # pick the best one

  portfolio <- rep(0, strategy$nassets)
  portfolio[best_stock] <- 1.0
  portfolio
}

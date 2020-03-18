
# pfselectstrat -----------------------------------------------------------


#' makes a new \code{\link[=new_pfselectstrat]{pfselectstrat}} class
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
#' @return the object x
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

  x
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

#' makes a \code{\link[=new_buyandhold]{buyandhold}} strategy
#'
#' Makes a \code{\link[=new_buyandhold]{buyandhold}} strategy from the given price
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
   ubah <- new_uniform_bah(price_relatives, transaction_rate)
   validate_buyandhold(ubah)
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
#' @param last_trading_period (OPTIONAL)
#'      the last trading period after
#'      which we "sell" our stocks (we need this to determine
#'      which stock is the best for the \code{best_stock} strategy.
#'      Defaults to the last available trading period
#'      in \code{price_relatives}
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
  bs <- new_best_stock(price_relatives, transaction_rate, last_trading_period)
  validate_best_stock(bs)
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


# loadonline --------------------------------------------------------------------


#' Implements the LOAD Online PS System
#'
#' A class to run the LOAD online PS System: For each stock
#' regresses the prices in the last time_window many trading
#' periods and thresholds the slope to classify the stock as
#' with momentum or without momentum. It uses this classification
#' to predict the price vector, then chooses the closest portfolio
#' which is predicted to return at least wealth_factor_threshold
#' over the next period
#'
#' This method was developed in the following paper:
#' \url{https://www.sciencedirect.com/science/article/abs/pii/S0950705119303922#b19}
#'
#' Returns a \code{loadonline} class object,
#' a subclass of \code{\link[=new_pfselectstrat]{pfselectstrat}}
#'
#' Initial portfolio is uniform.
#'
#' For validation see \code{\link{validate_loadonline}()}.
#'
#' @family loadonline
#' @keywords internal
#' @note Users should call the \code{\link{loadonline}} function instead.
#/'
#' @inheritParams new_pfselectstrat
#' @param decay_factor \eqn{\alpha} in the referenced paper, LOAD predicts
#'     that stocks regressing to the mean have approximate price
#'     \eqn{MA_t = \alpha p_t + (1-\alpha)MA_{t-1}}, \eqn{MA_1 = p_1}
#' @param regularization_factor \eqn{\lambda} in the referenced paper,
#'     the regularization coeffecient for weight decay when
#'     regressing the prices in the recent time window.
#'     Must be non-negative
#' @param time_window \eqn{w} in the referenced paper.
#'     The number of trading periods (including
#'     the most recent) to regress on (must be at least 2)
#' @param momentum_threshold \eqn{\eta} in the referenced paper.
#'     If the regressed slope is greater than \eqn{\eta} we say the
#'     stock has momentum. This must be greater than 0.
#' @param wealth_factor_threshold \eqn{\epsilon} in the referenced
#'     paper. Once a price relative for the next
#'     trading period is predicted, this is the minimum return
#'     our traded portfolio should satisfy. This must be greater
#'     than 0.
#' @param prices The prices during the trading periods (a matrix
#'     with one more row than \code{price_relatives})
#' @param price_means
#'     The mean price \eqn{MA_t} is \code{decay_factor} * \eqn{p_t}
#'     + \code{(1-decay_factor)} * \eqn{MA_{t-1}}, a matrix the
#'     same size as \code{prices}
#'
#' @return A loadonline class object with the given values
#'
#' @importFrom assertthat assert_that
#'
new_loadonline <- function(price_relatives,
                     transaction_rate,
                     decay_factor,
                     regularization_factor,
                     time_window,
                     momentum_threshold,
                     wealth_factor_threshold,
                     prices,
                     price_means, ..., class = character()) {
  # some quick type checks
  assert_that(is.numeric(decay_factor))
  assert_that(is.numeric(regularization_factor))
  assert_that(is.numeric(time_window))
  assert_that(is.numeric(momentum_threshold))
  assert_that(is.numeric(wealth_factor_threshold))
  assert_that(is.numeric(prices))
  assert_that(is.numeric(price_means))

  new_pfselectstrat(price_relatives = price_relatives,
                    transaction_rate = transaction_rate,
                    decay_factor = decay_factor,
                    regularization_factor = regularization_factor,
                    time_window = time_window,
                    momentum_threshold = momentum_threshold,
                    wealth_factor_threshold = wealth_factor_threshold,
                    prices = prices,
                    price_means = price_means,
                    ...,
                    class = c(class, "loadonline"))
}

#' validate a loadonline class
#'
#' Given a loadonline class (see \code{\link{new_loadonline}})
#' validates that any numeric constants are scalars,
#' if they should be non-negative or in \eqn{[0,1]} then they are,
#' that the price means are correct, and the prices match the price
#' relatives
#'
#' @param x the loadonline object to validate
#' @family loadonline
#'
#' @return the object x
#'
#' @importFrom assertthat assert_that are_equal has_name is.scalar
#' @importFrom rlang is_scalar_double
#'
validate_loadonline <- function(x) {
  validate_pfselect(x)
  values <- unclass(x)
  assert_that(has_name(values,
                       c("decay_factor", "regularization_factor",
                         "time_window", "momentum_threshold",
                         "wealth_factor_threshold", "prices",
                         "price_means")
  ))
  # type and scalar checks
  assert_that(is_scalar_double(values$decay_factor))
  assert_that(is_scalar_double(values$regularization_factor))
  assert_that(is_whole_number(values$time_window))
  assert_that(is_scalar_double(values$momentum_threshold))
  assert_that(is_scalar_double(values$wealth_factor_threshold))
  assert_that(is.matrix(values$prices))
  assert_that(is.numeric(values$prices))
  assert_that(is.matrix(values$price_means))
  assert_that(is.numeric(values$price_means))
  # Domain checks
  assert_that(0 <= values$decay_factor && values$decay_factor <= 1)
  assert_that(values$regularization_factor >= 0)
  assert_that(values$time_window >= 2)
  assert_that(values$momentum_threshold >= 0)
  assert_that(values$wealth_factor_threshold >= 1)
  assert_that(all(values$prices >= 0))
  assert_that(all(values$price_means >= 0))
  # dimension checks
  assert_that(are_equal(ncol(values$prices), values$nassets))
  assert_that(are_equal(nrow(values$prices), values$ntime_periods + 1))
  assert_that(are_equal(ncol(values$price_means), values$nassets))
  assert_that(are_equal(nrow(values$price_means), values$ntime_periods + 1))
  # Now check prices match price relatives and price_means match
  # decay_factor
  assert_that(are_equal(
    prices_from_relatives(values$price_relatives, values$prices[1,]),
    prices[-1,]
  ))
  assert_that(are_equal(
    historical_price_means(values$prices, decay_factor = values$decay_factor),
    values$price_means
  ))

  x
}

#' creates a loadonline strategy
#'
#' @family loadonline
#'
#' @inherit new_loadonline description return
#' @inheritParams new_loadonline
#' @param prices (OPTIONAL), initial_prices default to 1 if missing.
#'     See \code{\link{new_loadonline}} for a description of how to pass them in.
#'     Otherwise, they are computed on construction.
#'  @param price_means (OPTIONAL)
#'     See \code{\link{new_loadonline}} for a description of how to pass them in.
#'     Otherwise, they are computed on construction.
#'
#' @export
#'
loadonline <- function(price_relatives,
                       transaction_rate,
                       decay_factor,
                       regularization_factor,
                       time_window,
                       momentum_threshold,
                       wealth_factor_threshold,
                       prices,
                       price_means) {
  # if prices is missing, assume initial prices are 1
  if(missing(prices)) {
    prices <- prices_from_relatives(price_relatives,
                                    rep(1, ncol(price_relatives)))
  }
  # if historical price means are missing, compute them
  if(missing(price_means)) {
    price_means <- historical_price_means(prices, decay_factor)
  }
  ld <- new_loadonline(price_relatives = price_relatives,
                       transaction_rate = transaction_rate,
                       decay_factor = decay_factor,
                       regularization_factor = regularization_factor,
                       time_window = time_window,
                       momentum_threshold = momentum_threshold,
                       wealth_factor_threshold = wealth_factor_threshold,
                       prices = prices,
                       price_means = price_means)
 validate_loadonline(ld)
}

#' @export
first_portfolio.load_online <- function(strategy, trading_period) {
  first_portfolio.uniform_bah(strategy, trading_period)
}

#' @importFrom magrittr %>%
#' @importFrom purrr map map_dbl imap_dbl
#' @importFrom glmnet glmnet
#'
#' @export
next_portfolio.load_online <- function(strategy, trading_period, portfolio) {
  # make sure we have the requisite number of trading periods
  if(trading_period < strategy$trading_window) {
    stop(glue::glue("LOAD requires at least trading_window-1 trading periods \\
before the current trading_period."))
  }
  tp <- trading_period
  window <- (tp - pf_selector$trading_window + 1):tp
  # get predicted price based on regularized slope
  predicted_price <- function(regularized_slope, asset) {
    if(regularized_slope > pf_selector$momentum_threshold) {
      return( max(pf_selector$prices[window, ]) )
    }
    pf_selector$price_means[tp, asset]
  }
  # run ridge regression on each price
  # Note glmnet requires we have at least two variables but we only have
  # one, so we just duplicate the variable and sum them together
  predicted_price <- pf_selector$prices[window, ] %>%
    array_branch(2L) %>%
    map(glmnet,
        x = matrix(rep(1:pf_selector$trading_window, 2), ncol = 2),
        alpha = 0,  # alpha = 0 -> ridge regression
        lambda = pf_selector$regularization_factor) %>%
    map_dbl(`$`, beta) %>%  # get weights
    map_dbl(sum) %>%
    imap_dbl(predicted_price)

  # use predicted price to get price relatives and compute
  # b_{t+1} according to the reference paper
  pred_price_relatives <- pred_price / pf_selector$prices[tp, ]
  mean_zero_pred_pr <- pred_price_relatives - mean(pred_price_relatives)
  gamma <- as.numeric(
    (pf_selector$wealth_factor_threshold - portfolio %*% pred_price_relatives
     ) / (mean_zero_pred_pr %*% mean_zero_pred_pr)
  )
  # if gamma <= 0 don't rebalance, otherwise do according to the
  # gamma * mean zero predicted price relatives
  if(gamma <= 0) {
    return(portfolio)
  }
  portfolio + gamma * mean_zero_pred_pr
}

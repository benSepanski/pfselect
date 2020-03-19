
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
#' The default first portfolio method for this class is a uniform
#' distribution of wealth across all assets
#'
#' For validation of \code{pfselectstrat} objects, see
#' \code{\link{validate_pfselectstrat}()}
#'
#' @note This is a class with no next_portfolio method
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
  assert_that(has_name(values,
              c("nassets", "ntrading_periods",
                "price_relatives", "transaction_rate")))
  # make sure nassets and ntrading_periods are positive whole numbers
  assert_that(is_whole_number(values$nassets))
  assert_that(is_whole_number(values$ntrading_periods))
  assert_that(values$nassets > 0)
  assert_that(values$ntrading_periods > 0)
  # make sure price relatives is a numeric matrix of non-negative
  # entries which is ntrading_periods x nassets
  validate_nonnegative_mat(values$price_relatives)
  assert_that(are_equal(values$nassets, ncol(values$price_relatives)))
  assert_that(are_equal(values$ntrading_periods, nrow(values$price_relatives)))
  # transaction rate checks
  assert_that(is_scalar_double(values$transaction_rate))
  assert_that(values$transaction_rate >= 0 && values$transaction_rate <= 1)

  x
}

#' Get first portfolio from strategy
#'
#' Returns the first portfolio for the given strategy.
#' We assume trading is about to begin at the given trading period
#' The default asserts that the object is of class
#' \link[=new_pfselectstrat]{pfselectstrat}.
#' The default for class \link[=new_pfselectstrat]{pfselectstrat} is
#' a unifrom distribution of wealth across all assets.
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

#' @export
first_portfolio.pfselectstrat <- function(strategy, trading_period) {
  rep(1/strategy$nassets, strategy$nassets)
}

#' Get next portfolio from strategy
#'
#' Return the next portfolio for the given strategy.
#' We assume trading is about to begin at the given trading period.
#' The default asserts that the object is of class
#' \link[=new_pfselectstrat]{pfselectstrat}.
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
#' This is a uniform buy and hold strategy if not subclassing
#' (e.g. \link{new_best_stock})
#'
#' To validate the object use \code{\link{validate_buyandhold}}.
#'
#' @keywords internal
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
  new_pfselectstrat(price_relatives,
                    transaction_rate,
                    ...,
                    class = c(class, "buyandhold"))
}

#' @describeIn validate_pfselectstrat validates a buyandhold object
validate_buyandhold <- function(x) { validate_pfselectstrat(x) }

#' Makes a new \code{buyandhold} strategy
#'
#' @inherit new_buyandhold title description return
#' @inheritParams new_buyandhold
#' @family buyandhold
#'
#' @seealso new_buyandhold
#'
#' @export
buyandhold <- function(price_relatives, transaction_rate) {
   bah <- new_buyandhold(price_relatives, transaction_rate)
   validate_buyandhold(bah)
}

#' @export
next_portfolio.buyandhold <- function(strategy, trading_period, portfolio) {
  portfolio
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
  validate_buyandhold(bs)
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


# onlineLOAD --------------------------------------------------------------------


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
#' Returns a \code{onlineLOAD} class object,
#' a subclass of \code{\link[=new_pfselectstrat]{pfselectstrat}}
#'
#' Initial portfolio is uniform.
#'
#' For validation see \code{\link{validate_onlineLOAD}()}.
#'
#' @family onlineLOAD
#' @keywords internal
#' @note Users should call the \code{\link{onlineLOAD}} function instead.
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
#' @return A onlineLOAD class object with the given values
#'
#' @importFrom assertthat assert_that
#'
new_onlineLOAD <- function(price_relatives,
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
                    class = c(class, "onlineLOAD"))
}

#' validate a onlineLOAD class
#'
#' Given a onlineLOAD class (see \code{\link{new_onlineLOAD}})
#' validates that any numeric constants are scalars,
#' if they should be non-negative or in \eqn{[0,1]} then they are,
#' that the price means are correct, and the prices match the price
#' relatives
#'
#' @param x the onlineLOAD object to validate
#' @param check_prices checks prices match price relatives iff this is
#'     \code{TRUE}. This can be expensive.
#' @param check_price_means  checks price means match prices iff this is
#'     \code{TRUE}. This can be expensive.
#' @family onlineLOAD
#'
#' @return the object x
#'
#' @importFrom assertthat assert_that are_equal has_name is.scalar
#' @importFrom rlang is_scalar_double
#'
validate_onlineLOAD <- function(x, check_prices = TRUE,
                                check_price_means = TRUE) {
  validate_pfselectstrat(x)
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
  assert_that(are_equal(nrow(values$prices), values$ntrading_periods + 1))
  assert_that(are_equal(ncol(values$price_means), values$nassets))
  assert_that(are_equal(nrow(values$price_means), values$ntrading_periods + 1))
  # Now check prices match price relatives and price_means match
  # decay_factor
  if(check_prices) {
    expected_p <- prices_from_relatives(values$price_relatives,
                                        values$prices[1,])
    assert_that(are_equal(expected_p, values$prices))
  }
  if(check_price_means) {
    expected_means <- historical_price_means(values$prices,
                                             decay_factor = values$decay_factor)
    assert_that(are_equal(expected_means, values$price_means))
  }

  x
}

#' creates a onlineLOAD strategy
#'
#' @family onlineLOAD
#'
#' @inherit new_onlineLOAD description return
#' @inheritParams new_onlineLOAD
#' @inheritParams validate_onlineLOAD
#' @param prices (OPTIONAL), initial_prices default to 1 if missing.
#'     See \code{\link{new_onlineLOAD}} for a description of how to pass them in.
#'     Otherwise, they are computed on construction.
#' @param price_means (OPTIONAL)
#'    See \code{\link{new_onlineLOAD}} for a description of how to pass them in.
#'    Otherwise, they are computed on construction.
#'
#' @export
#'
onlineLOAD <- function(price_relatives,
                       transaction_rate,
                       decay_factor,
                       regularization_factor,
                       time_window,
                       momentum_threshold,
                       wealth_factor_threshold,
                       prices,
                       price_means,
                       check_prices = TRUE,
                       check_price_means = TRUE) {
  # if prices is missing, assume initial prices are 1
  if(missing(prices)) {
    prices <- prices_from_relatives(price_relatives,
                                    rep(1, ncol(price_relatives)))
  }
  # if historical price means are missing, compute them
  if(missing(price_means)) {
    price_means <- historical_price_means(prices, decay_factor)
  }
  ld <- new_onlineLOAD(price_relatives = price_relatives,
                       transaction_rate = transaction_rate,
                       decay_factor = decay_factor,
                       regularization_factor = regularization_factor,
                       time_window = time_window,
                       momentum_threshold = momentum_threshold,
                       wealth_factor_threshold = wealth_factor_threshold,
                       prices = prices,
                       price_means = price_means)
 validate_onlineLOAD(ld)
}

#' predicts price according to LOAD strategy
#'
#' predicts next prices given previous prices according to LOAD
#' strategy (see paper mentioned in description of
#' \code{\link{new_onlineLOAD}})
#'
#' @note NO TYPE CHECKING IS PERFORMED... be careful
#'
#' @param prev_prices a numeric vector of previous prices
#' @param historic_mean The historic mean of the prices
#' @param min_var (OPTIONAL) minimum variance to be determined non-constant
#' @inheritParams new_onlineLOAD
#'
#' @return the predicted next price
#'
predict_priceLOAD <- function(prev_prices,
                              historic_mean,
                              regularization_factor,
                              momentum_threshold,
                              min_var = .Machine$double.eps ^ 0.25) {
  regularized_slope <- 0
  if(var(prev_prices) > min_var) {  # avoid constant case
    # Note glmnet requires we have at least two variables but we only have
    # one, so we just duplicate the variable and sum them together
    regularized_slope <- Matrix::colSums(
      glmnet(x = matrix(rep(1:length(prev_prices), 2), ncol = 2),
             y = prev_prices,
             alpha = 0,  # alpha = 0 -> ridge regression
             lambda = regularization_factor)$beta)
  }
  # return max if has momentum
  if(regularized_slope > momentum_threshold) {
    return( max(prev_prices) )
  }
  # otherwise mean
  historic_mean
}

#' @importFrom magrittr %>%
#' @importFrom purrr map map_dbl imap_dbl
#' @importFrom glmnet glmnet
#'
#' @export
next_portfolio.onlineLOAD <- function(strategy, trading_period, portfolio) {
  # make sure we have the requisite number of trading periods
  if(trading_period < strategy$time_window) {
    stop(glue::glue("LOAD requires at least time_window-1 trading periods \\
before the current trading_period."))
  }
  tp <- trading_period
  window <- (tp - strategy$time_window + 1):tp
  # run ridge regression on each price
  predicted_price <- strategy$prices[window, ] %>%
    array_branch(2L) %>%
    map2_dbl(strategy$price_means[tp, ], predict_priceLOAD,
             regularization_factor = strategy$regularization_factor,
             momentum_threshold = strategy$momentum_threshold)

  # use predicted price to get price relatives and compute
  # b_{t+1} according to the reference paper
  pred_price_relatives <- predicted_price / strategy$prices[tp, ]
  mean_zero_pred_pr <- pred_price_relatives - mean(pred_price_relatives)
  gamma <- as.numeric(
    (strategy$wealth_factor_threshold - portfolio %*% pred_price_relatives
     ) / (mean_zero_pred_pr %*% mean_zero_pred_pr)
  )
  # if gamma <= 0 don't rebalance, otherwise do according to the
  # gamma * mean zero predicted price relatives
  if(gamma <= 0) {
    return(portfolio)
  }
  project_to_simplex(portfolio + gamma * mean_zero_pred_pr)
}


# exponential_gradient ----------------------------------------------------

#' Makes a \code{exponential_gradient} class
#'
#' Returns an instance of a \code{exponential_gradient} class,
#' which is a subclass of \code{\link[=new_pfselectstrat]{pfselectstrat}}.
#' See this paper
#' \url{http://rob.schapire.net/papers/HelmboldScSiWa98.pdf}
#' ("Online Portfolio Selection Using Multiplicative Updates"
#' by David P Helmholt, Robert E. Schapire and Yoram Singer,
#' and Manfred K. Warmuth)
#'
#' has a multiplicative weight update rule, and initializes to
#' uniform portfolio
#'
#' Validate using \code{\link{validate_expontial_gradient}()}.
#'
#' @note As a user, instantiate using the \code{\link{exponential_gradient}()}
#'     function.
#'
#' @keywords internal
#' @family exponential_gradient
#'
#' @inheritParams new_pfselectstrat
#' @param \eqn{\eta} in the referenced paper, the learning rate
#'    should be a positive scalar, the higher the learning rate
#'    the more volatile the algorithm
#' @return a \code{exponential_gradient} class
#'
new_exponential_gradient <- function(price_relatives,
                                     transaction_rate,
                                     learning_rate,
                                     ...,
                                     class = character()) {
  new_pfselectstrat(price_relatives = price_relatives,
                    transaction_rate = transaction_rate,
                    learning_rate = learning_rate,
                    ...,
                    class = c(class, "exponential_gradient"))
}

#' validates a \code{\link[=new_exponential_gradient]{exponential_gradient}}
#' class
#'
#' validates the class, making sure learning rate is positive
#' and following checks in \code{\link{validate_pfselectstrat}}
#'
#' @family exponential_gradient
#' @param x the \code{\link[=new_exponential_gradient]{exponential_gradient}}
#'     object to validate
#' @return the object x
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom rlang is_scalar_double
#'
validate_exponential_gradient <- function(x) {
  validate_pfselectstrat(x)
  values <- unclass(x)
  assert_that(has_name(values, "learning_rate"))
  assert_that(is_scalar_double(values$learning_rate))
  assert_that(values$learning_rate > 0)
  x
}

#' Create a \code{exponential_gradient} class
#'
#' @family exponential_gradient
#' @inherit new_exponential_gradient description return
#' @inheritParams new_exponential_gradient
#'
#' @export
#'
exponential_gradient <- function(price_relatives,
                                 transaction_rate,
                                 learning_rate) {
  eg <- new_exponential_gradient(price_relatives = price_relatives,
                                 transaction_rate = transaction_rate,
                                 learning_rate = learning_rate)
  validate_exponential_gradient(eg)
}

#' @export
next_portfolio.exponential_gradient <- function(strategy,
                                                trading_period,
                                                portfolio) {
  # next portfolio comes from multiplicative update rule
  next_pf <- portfolio * exp(
    strategy$learning_rate *
      strategy$price_relatives[trading_period, ] /
      as.numeric(portfolio %*% strategy$price_relatives[trading_period, ])
  )
  next_pf / sum(next_pf)  # return normalized portfolio
}

# universal_portfolio -----------------------------------------------------

#' Makes a \code{universal_portfolio} class
#'
#' Returns an instance of a \code{universal_portfolio} class,
#' which is a subclass of \code{\link[=new_pfselectstrat]{pfselectstrat}}.
#' See Cover's paper "Universal Portfolios" (1991,
#' \url{https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.1467-9965.1991.tb00002.x}
#' ) for a description.
#' We estimate the universal portfolio using
#' a sampling estimation method mentioned in Blum & Kalai's
#' 1999 paper "Universal Portfolios With and Without Transaction Costs"
#' (\url{https://link.springer.com/article/10.1023/A:1007530728748}),
#' but is basically just random sampling of portfolios, then the
#' average weighted by the produced cumulative wealth up to this point.
#' However, we use Dirichlet(1/2,1/2,...,1/2) priors for the portfolios
#' because they may have better performance according to
#' Cover & Ordentlich (1996)
#'
#' initializes to uniform portfolio
#'
#' Validate using \code{\link{validate_universal_portfolio}()}.
#'
#' @note As a user, instantiate using the \code{\link{universal_portfolio}()}
#'     function.
#'
#' @keywords internal
#' @family universal_portfolio
#'
#' @inheritParams new_pfselectstrat
#' @param nsamples Number of samples to perform at each day.
#'     In general, this will be lower than theoretically necessary,
#'     so expect a sub-optimal estimate
#' @param consider_transaction_rate If \code{FALSE}, uses the original
#'     1991 algorithm proposed by Cover, where the cumulative wealth
#'     produced by a constantly rebalanced portfolio (CRP) is
#'     computed without consider transaction costs. If \code{TRUE},
#'     follows Blum & Kalai's 1991 paper and considers the transaction
#'     costs.
#' @return a \code{universal_portfolio} class
#'
new_universal_portfolio <- function(price_relatives,
                                    transaction_rate,
                                    nsamples,
                                    consider_transaction_rate,
                                    ...,
                                    class = character()) {
  new_pfselectstrat(price_relatives = price_relatives,
                    transaction_rate = transaction_rate,
                    nsamples = nsamples,
                    consider_transaction_rate = consider_transaction_rate,
                    ...,
                    class = c(class, "universal_portfolio"))
}

#' validates a \code{\link[=new_universal_portfolio]{universal_portfolio}}
#' class
#'
#' validates the class, making sure \code{nsamples} is a positive
#' whole number and that consider_transaction_rate is a single boolean.
#' Follows checks in \code{\link{validate_pfselectstrat}}
#'
#' @family universal_portfolio
#' @param x the \code{\link[=new_universal_portfolio]{universal_portfolio}}
#'     object to validate
#' @return the object x
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom rlang is_scalar_logical
#'
validate_universal_portfolio <- function(x) {
  validate_pfselectstrat(x)
  values <- unclass(x)
  assert_that(has_name(values, c("nsamples", "consider_transaction_rate")))
  assert_that(is_whole_number(values$nsamples))
  assert_that(values$nsamples > 0)
  assert_that(is_scalar_logical(values$consider_transaction_rate))
  x
}

#' Create a \code{universal_portfolio} class
#'
#' @family universal_portfolio
#' @inherit new_universal_portfolio description return
#' @inheritParams new_universal_portfolio
#'
#' @export
#'
universal_portfolio <- function(price_relatives,
                                transaction_rate,
                                nsamples,
                                consider_transaction_rate) {
  up <- new_universal_portfolio(price_relatives = price_relatives,
                               transaction_rate = transaction_rate,
                               nsamples = nsamples,
                               consider_transaction_rate = consider_transaction_rate)
  validate_universal_portfolio(up)
}

#' @importFrom assertthat are_equal
#' @importFrom magrittr %>%
#' @importFrom purrr map array_branch cross map2_dbl
#' @export
next_portfolio.universal_portfolio <- function(strategy, trading_period,
                                               portfolio) {
    if(trading_period < 1) {
      stop("trading_period must be at least 1")
    }
    if(trading_period > strategy$ntrading_periods) {
      stop("trading_period must be <= strategy$ntrading_periods")
    }

    # If in first trading period, just return uniform portfolio
    if(trading_period == 1) {
      return(first_portfolio.pfselectstrat(strategy, trading_period))
    }

    # otherwise we have some price relatives to work with.
    rportfolios <- rdirichlet_onehalf(strategy$nsamples, strategy$nassets)
    # compute wealth obtained up to this period by each sample
    # CRP
    if(!strategy$consider_transaction_rate ||
       are_equal(strategy$transaction_rate, 0)) {
      # nsamples x (trading_period - 1)
      wealth_factors <- (
        rportfolios %*% t(strategy$price_relatives[1:(trading_period-1),,
                                                   drop=FALSE])
      )
      wealth <- apply(wealth_factors, 1L, prod)
    }
    else {
      wealth <- list(rportfolios,
                     strategy$price_relatives[1:(trading_period-1)]) %>%
        map(array_branch, 1L) %>%
        cross() %>%
        map_dbl(~ wealth_increase_factor(price_relatives = .[[2]],
                                         prev_portfolio = .[[1]],
                                         portfolio = .[[1]],
                                         tr = strategy$transaction_rate)) %>%
        flatten_dbl() %>%
        matrix(nrow = strategy$nsamples) %>%
        apply(1L, function(row) reduce(row, `*`))
    }
    # return weighted average of CRP's by wealth return
    apply(rportfolios, 2L, weighted.mean, w = wealth)
}












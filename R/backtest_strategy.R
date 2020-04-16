
# Roxygen help ------------------------------------------------------------


backtest_strategy_return <- function() {
  c("@return a matrix with the same number of columns and one more",
    "row than \\code{price_relative_matrix}, row \\eqn{i}",
    "is the portfolio after a trade during period \\eqn{i}, i.e.",
    "right before the \\eqn{i}th price relatives change the prices."
  )
}
backtest_strategy_args <- function() {
  c("@param price_relative_matrix a matrix of price relatives, each row",
    "representing a trading period and each column an asset.",
    "A price relative is \\eqn{p_{t+1} / p_t}, i.e.",
    "the ratio of trading price to next price. Prices change",
    "according to the price relatives after the trade,",
    "i.e. the price relatives for the trading period are not known",
    "at trading time",
    "@param transaction_rate The percentage of each transaction (buy and sell)",
    "spent on broker fees"
  )
}


# Input Checking ----------------------------------------------------------



#' checks input args to a strategy
#'
#' asserts that price relatives is a matrix with non-negative entries,
#' and if a column hits zero it stays zero.
#' asserts that transaction rate is a scalar double in \eqn{[0,1]}
#'
#' @eval backtest_strategy_args()
#' @return Invisibly \code{TRUE}
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang is_scalar_double
#'
#' @noRd
#'
check_strategy_args <- function(price_relative_matrix, transaction_rate) {
  assert_that(is_numeric_matrix(price_relative_matrix))
  assert_that(all(price_relative_matrix >= 0))
  assert_that(length(price_relative_matrix) > 0)
  for(col in 1:ncol(price_relative_matrix)) {
    # if ever zeros out, must be zero rest of the time
    near_zero <- price_relative_matrix[, col] < .Machine$double.eps ^ 0.5
    if(any(near_zero)) {
      first_zero_index <- min(which(price_relative_matrix[, col]))
      assert_that(all(near_zero[first_zero:length(near_zero)]))
    }
  }
  # now check transaction_rate
  assert_that(is_scalar_double(transaction_rate))
  assert_that(0 <= transaction_rate && transaction_rate <= 1)
  invisible(TRUE)
}

# Buy and Hold ---------------------------------------------------------------

#' backtest a buy and hold strategy
#'
#' @eval backtest_strategy_args()
#' @eval backtest_strategy_return()
#'
#' @param initial_portfolio a vector whose \eqn{i}th entry is the
#'     amount of wealth in the \eqn{i}th entry. Should
#'     be a portfolio for the assets in \code{price_relative_matrix}.
#'     This is the portfolio before the first trading period.
#'
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' @export
backtest_buyandhold <- function(price_relative_matrix,
                                transaction_rate,
                                initial_portfolio) {
  # validate input
  check_strategy_args(price_relative_matrix, transaction_rate)
  validate_portfolio(initial_portfolio, ncol(price_relative_matrix))

  price_relative_matrix %>%
    purrr::array_branch(1L) %>%
    purrr::accumulate(price_adjusted_portfolio, .init = initial_portfolio) %>%
    purrr::flatten_dbl() %>%
    matrix(ncol = ncol(price_relative_matrix), byrow = TRUE) %>%
    validate_portfolio_matrix(nrow(price_relative_matrix),
                              ncol(price_relative_matrix))
}

#' backtests a market (uniform buy and hold) strategy
#'
#' Backtests a strategy which buys an equal amount of all stocks
#' and then holds them.
#'
#' @inherit backtest_buyandhold return
#' @inheritParams backtest_buyandhold
#' @export
backtest_market <- function(price_relative_matrix, transaction_rate) {
  backtest_buyandhold(price_relative_matrix,
                      transaction_rate,
                      uniform_portfolio(ncol(price_relative_matrix)))
}

#' backtests a best stock strategy
#'
#' Backtests a stragey which buys the best stock and sits on it
#' for the duration. This is a hindsight strategy.
#'
#' @inherit backtest_buyandhold return
#' @inheritParams backtest_buyandhold
#' @importFrom magrittr %>%
#'
#' @export
backtest_best_stock <- function(price_relative_matrix, transaction_rate) {
  check_strategy_args(price_relative_matrix, transaction_rate)

  best_stock_index <- price_relative_matrix %>%
    apply(2L, prod) %>%
    which.max()
  pfs <- matrix(data = 0,
                nrow = nrow(price_relative_matrix)+1,
                ncol = ncol(price_relative_matrix))
  pfs[, best_stock_index] <- 1
  validate_portfolio_matrix(pfs,
                            nrow(price_relative_matrix),
                            ncol(price_relative_matrix))
}


# LOAD --------------------------------------------------------------------


#' checks args to \code{\link{backtest_LOAD}}
#'
#' checks that the args to \code{\link{backtest_LOAD}}
#' satisfy their requirements. Does not check
#' price_relative_matrix or transaction_rate.
#'
#' @inheritParams backtest_LOAD
#'
#' @importFrom assertthat assert_that are_equal
#' @importFrom rlang is_scalar_double
#' @importFrom magrittr %>%
#'
#' @noRd
check_LOAD_args <- function(price_relative_matrix,
                            transaction_rate,
                            decay_factor,
                            regularization_factor,
                            time_window,
                            momentum_threshold,
                            wealth_factor_threshold,
                            price_matrix,
                            price_mean_matrix) {
  # type and scalar checks
  assert_that(is_scalar_double(decay_factor))
  assert_that(is_scalar_double(regularization_factor))
  assert_that(is_whole_number(time_window))
  assert_that(is_scalar_double(momentum_threshold))
  assert_that(is_scalar_double(wealth_factor_threshold))
  assert_that(is_numeric_matrix(price_matrix))
  assert_that(is_numeric_matrix(price_mean_matrix))
  # Domain checks
  assert_that(0 <= decay_factor && decay_factor <= 1)
  assert_that(regularization_factor >= 0)
  assert_that(time_window >= 2)
  assert_that(momentum_threshold >= 0)
  assert_that(wealth_factor_threshold >= 1)
  assert_that(all(price_matrix >= 0))
  assert_that(all(price_mean_matrix >= 0))
  # dimension checks
  ntrading_periods <- nrow(price_relative_matrix)
  nassets <- ncol(price_relative_matrix)
  assert_that(are_equal(ncol(price_matrix), nassets))
  assert_that(are_equal(nrow(price_matrix), ntrading_periods + 1))
  assert_that(are_equal(ncol(price_mean_matrix), nassets))
  assert_that(are_equal(nrow(price_mean_matrix), ntrading_periods + 1))
  # Now check prices match price relatives and price_means match
  # decay_factor
  expected_p <- compute_price_matrix_from_relatives(price_relative_matrix,
                                                    price_matrix[1,])
  assert_that(are_equal(expected_p, price_matrix))
  expected_means <- compute_historical_price_mean_matrix(price_matrix,
                                                         decay_factor)
  assert_that(are_equal(expected_means, price_mean_matrix))
}


#' predicts price for asset according to LOAD strategy
#'
#' predicts next price for one asset
#'  given previous prices according to LOAD
#' strategy (see paper mentioned in description of
#' \code{\link{backtest_LOAD}})
#'
#' @note NO TYPE CHECKING IS PERFORMED... be careful
#'
#' @inheritParams predict_window_momentum_LOAD
#' @param historic_mean The historic mean of the prices
#' @return the predicted next price
#'
predict_price_LOAD <- function(prev_prices,
                               historic_mean,
                               regularization_factor,
                               momentum_threshold) {
  predicted_momentum <- predict_window_momentum_LOAD(prev_prices,
                                                     regularization_factor,
                                                     momentum_threshold)
  if(predicted_momentum == 1) {
    return( max(prev_prices) )
  }
  historic_mean
}


#' Backtests the LOAD Online PS System
#'
#' Runs the LOAD online PS System: For each stock
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
#' Initial portfolio is uniform.
#'
#' @eval backtest_strategy_return()
#' @eval backtest_strategy_args()
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
#' @param price_matrix (OPTIONAL) The prices during the trading periods
#'     (a matrix with one more row than \code{price_relative_matrix}).
#'     If not included, initial prices are assumed to be one.
#' @param price_mean_matrix (OPTIONAL)
#'     The mean price \eqn{MA_t} is \code{decay_factor} * \eqn{p_t}
#'     + \code{(1-decay_factor)} * \eqn{MA_{t-1}}, a matrix the
#'     same size as \code{prices}. If not used, these are computed
#'     from the prices
#'
#' @importFrom assertthat assert_that
#' @export
#'
backtest_LOAD <- function(price_relative_matrix,
                          transaction_rate,
                          decay_factor,
                          regularization_factor,
                          time_window,
                          momentum_threshold,
                          wealth_factor_threshold,
                          price_matrix,
                          price_mean_matrix) {
  check_strategy_args(price_relative_matrix, transaction_rate)
  # if prices is missing, assume initial prices are 1
  if(missing(price_matrix)) {
    price_matrix <-
      compute_price_matrix_from_relatives(price_relative_matrix,
                                          rep(1, ncol(price_relative_matrix)))
  }
  # if historical price means are missing, compute them
  if(missing(price_mean_matrix)) {
    price_mean_matrix <- compute_historical_price_mean_matrix(price_matrix,
                                                              decay_factor)
  }
  check_LOAD_args(price_relative_matrix,
                  transaction_rate,
                  decay_factor,
                  regularization_factor,
                  time_window,
                  momentum_threshold,
                  wealth_factor_threshold,
                  price_matrix,
                  price_mean_matrix)

  ntrading_periods <- nrow(price_relative_matrix)
  nassets <- ncol(price_relative_matrix)

  pred_prices <- list(price_matrix, price_mean_matrix) %>%
    purrr::map(purrr::array_branch, 2L) %>%
    purrr::pmap(rollify_dbl(predict_price_LOAD,
                            window_sizes = c(time_window, 1)),
                regularization_factor = regularization_factor,
                momentum_threshold = momentum_threshold) %>%
    purrr::flatten_dbl() %>%
    matrix(ncol = nassets)
  # strip first (time_window-1) many rows (NA)
  pred_prices <- tail(pred_prices, -(time_window-1))

  # predicted price relatives
  pred_pr <- pred_prices / tail(price_matrix, -(time_window-1))

  # now get each portfolio
  #                   portfolio, price rel, price rel
  next_pf <- function(prev_pf, prev_pr, pred_pr) {
    # adjust portfolio to prices of the period
    prev_pf <- price_adjusted_portfolio(prev_pf, prev_pr)
    mean_zero_pred_pr <- pred_pr - mean(pred_pr)
    gamma <- wealth_factor_threshold - drop(prev_pf %*% pred_pr)
    gamma <- gamma / drop(mean_zero_pred_pr %*% mean_zero_pred_pr)
    if(gamma <= 0) {
      return(prev_pf)
    }
    project_to_simplex(prev_pf + gamma * mean_zero_pred_pr)
  }
  # nb: need curly brace so magrittr can parse .[[1]] and .[[2]]
  list(tail(price_relative_matrix, -(time_window-2)), pred_pr) %>%
    purrr::map(purrr::array_branch, 1L) %>%
    {purrr::accumulate2(.[[1]], .[[2]],
                        next_pf, .init = uniform_portfolio(nassets))} %>%
    purrr::flatten_dbl() %>%
    matrix(ncol = nassets, byrow = TRUE) %>%
    validate_portfolio_matrix(ntrading_periods - time_window + 2, nassets)
}


# Exponential Gradient ----------------------------------------------------


#' Backtests using a exponential gradient rule
#'
#' backtests using an exponential gradient rule.
#' See this paper
#' \url{http://rob.schapire.net/papers/HelmboldScSiWa98.pdf}
#' ("Online Portfolio Selection Using Multiplicative Updates"
#' by David P Helmholt, Robert E. Schapire and Yoram Singer,
#' and Manfred K. Warmuth)
#'
#' has a multiplicative weight update rule, and initializes to
#' uniform portfolio
#'
#' @param learning_rate The learning rate (\eqn{\eta} in the referenced
#'     paper), a higher learning rate corresponds to a higher
#'     sensitivitiy as one would expect.
#' @eval backtest_strategy_args()
#' @eval backtest_strategy_return()
#'
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#'
#' @export
#'
backtest_exponential_gradient <- function(price_relative_matrix,
                                          transaction_rate,
                                          learning_rate) {
  # input checks
  check_strategy_args(price_relative_matrix, transaction_rate)
  assert_that(rlang::is_scalar_double(learning_rate))
  assert_that(0 <= learning_rate)

  # given the price relatives and portfolio (before trade)
  # from last period,
  # get the portfolio recommended for this period
  next_pf <- function(prev_pf, prev_price_relative) {
    unnormalized_pf <- prev_pf * exp(learning_rate * prev_price_relative /
                                     drop(prev_pf %*% prev_price_relative))
    unnormalized_pf / sum(unnormalized_pf)
  }
  ntrading_periods <- nrow(price_relative_matrix)
  nassets <- ncol(price_relative_matrix)
  # now accumulate the exponential gradient solution
  price_relative_matrix %>%
    purrr::array_branch(1L) %>%
    purrr::accumulate(next_pf, .init = uniform_portfolio(nassets)) %>%
    purrr::flatten_dbl() %>%
    matrix(ncol = nassets, byrow = TRUE) %>%
    validate_portfolio_matrix(ntrading_periods, nassets)
}


# Online Newton Step ------------------------------------------------------


#' Backtests using an online newton step method
#'
#' Backtests using an online newton step method for portfolio
#' selection.
#'
#' This is implements the Online Newton Step
#' (\url{https://www.researchgate.net/publication/221346006_Algorithms_for_portfolio_management_based_on_the_Newton_method})
#' from Agarwal, Hazan, Kale, & Schapire's 2006 paper
#' "Algorithms for portfolio management based on the Newton method".
#' Each step solves a small linear set of equation then projects to
#' the unit simplex in a matrix norm (estimated using projected
#' gradient descent).
#'
#' According to the paper, we set \eqn{\eta=0}, \eqn{\beta=1}, and
#' \eqn{\delta=1/8}
#'
#' initializes to uniform portfolio
#'
#' @eval backtest_strategy_args()
#' @eval backtest_strategy_return()
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
backtest_online_newton_step <- function(price_relative_matrix,
                                        transaction_rate) {
  # input checks
  check_strategy_args(price_relative_matrix, transaction_rate)

  ntrading_periods <- nrow(price_relative_matrix)
  nassets <- ncol(price_relative_matrix)

  eta <- 0
  beta <- 1
  delta <- 1/8

  bt <- rep(0, nassets)
  At <- diag(nassets)  ## Identity
  unif_p <- uniform_portfolio(nassets)

  # previous portfolio and price relatives (from last trading period)
  next_pf <- function(prev_pf, prev_pr) {
    bt_update <- prev_pr / drop(prev_pf %*% prev_pr)
    assign("bt", bt + (1 + 1/beta) * bt_update, envir = parent.frame())
    assign("At", At + outer(bt_update, bt_update), envir = parent.frame())
    p <- project_to_simplex_A_norm( delta * drop(solve(At, bt)) , At)
    p * (1-eta) + eta * unif_p
  }

  price_relative_matrix %>%
    purrr::array_branch(1L) %>%
    purrr::accumulate(next_pf, .init = uniform_portfolio(nassets)) %>%
    purrr::flatten_dbl() %>%
    matrix(ncol = nassets, byrow = TRUE) %>%
    validate_portfolio_matrix(ntrading_periods, nassets)
}


# Universal Portfolio -----------------------------------------------------


#' Backtests the universal portfolio algorithm
#'
#' Backtests using Cover's 1991 universal portfolio algorithm,
#' estimated with samples of portfolios.
#'
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
#' @param nsamples The number of portfolios to sample when estimating
#'     the universal portfolio
#' @param consider_transaction_rate If \code{FALSE}, uses the original
#'     1991 algorithm proposed by Cover, where the cumulative wealth
#'     produced by a constantly rebalanced portfolio (CRP) is
#'     computed without consider transaction costs. If \code{TRUE},
#'     follows Blum & Kalai's 1991 paper and considers the transaction
#'     costs.
#' @eval backtest_strategy_args()
#' @eval backtest_strategy_return()
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
backtest_universal_portfolio <- function(price_relative_matrix,
                                         transaction_rate,
                                         nsamples,
                                         consider_transaction_rate = TRUE) {
  # input checks
  check_strategy_args(price_relative_matrix, transaction_rate)
  assert_that(is_whole_number(nsamples))
  assert_that(nsamples > 1)
  assert_that(rlang::is_scalar_logical(consider_transaction_rate))

  ntrading_periods <- nrow(price_relative_matrix)
  nassets <- ncol(price_relative_matrix)

  # nsamples x nassets
  rportfolios <- rdirichlet_onehalf(nsamples, nassets)
  if(!consider_transaction_rate || are_equal(transaction_rate, 0)) {
    # nsamples x (ntrading periods)
    daily_return <- rportfolios %*% t(price_relative_matrix)
  }
  else {
    wealth <- list(rportfolios, price_relative_matrix) %>%
      map(array_branch, 1L) %>%
      cross() %>%
      purrr::map_dbl(~ wealth_increase_factor(price_relatives = .[[2]],
                                              prev_portfolio = .[[1]],
                                              portfolio = .[[1]],
                                              tr = transaction_rate)) %>%
      purrr::flatten_dbl() %>%
      matrix(nrow = nsamples)
  }
  # nsamples x ntrading_period
  wealth <- daily_return %>%
    purrr::array_branch(1L) %>%
    purrr::map(cumprod) %>%
    purrr::flatten_dbl() %>%
    matrix(nrow = nsamples)
  # get the wealth of each sample by time period, then take the
  # weighted mean at each time period and recombine
  wealth %>%
    array_branch(2L) %>%
    purrr::map(~apply(rportfolios, 2L, weighted.mean, w = .)) %>%
    purrr::flatten_dbl() %>%
    c(uniform_portfolio(nassets), .) %>%  # prepend first portfolio
    matrix(ncol = nassets, byrow = TRUE) %>%
    validate_portfolio_matrix(ntrading_periods = ntrading_periods,
                              nassets = nassets)
}














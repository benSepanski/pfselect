#' backtest to get cumulative wealth and portfolios over time
#'
#' Run a portfolio selection strategy from the given trading
#' period until the end of its data, reporting the cumulative
#' wealth at each stage and the portfolio before each
#' trading period
#'
#' @param strategy the \link[=new_pfselectstrat]{pfselectstrat} instance to use
#' @param first_trading_period the trading period to start backtesting
#'     on (right before prices change)
#' @param last_trading_period (OPTIONAL) if supplied, only backtests
#'     up through \code{max_trading_period} (instead of through all
#'     available trading periods). This is the last price change we experience
#' @param check_portfolio (OPTIONAL) defaults to \code{TRUE},
#'     if \code{TRUE} then asserts that each portfolio sums to 1 and is
#'     non-negative. Never checks
#'     the first portfolio, so be careful.
#'     If \code{FALSE}, performs no checks.
#' @return A matrix which has the same number of columns as
#'     \code{strategy$price_relatives} and one more row.
#'     Row \eqn{i} represents the portfolio before trading during
#'     period \eqn{i} (Recall that trading during period \eqn{i}
#'     happens before the price relatives for that period are known,
#'     i.e. the trade occurs, then the price changes according to the
#'     relatives). The column names of this matrix
#'     match the column names of \code{strategy$price_relatives}.
#'
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that are_equal
#' @export
#'
backtest_portfolio_selector <- function(strategy,
                                        first_trading_period,
                                        last_trading_period,
                                        check_portfolio = TRUE){
  if(missing(last_trading_period)) {
    last_trading_period <- strategy$ntrading_periods
  }
  assert_that(is_whole_number(first_trading_period))
  assert_that(is_whole_number(last_trading_period))
  assert_that(first_trading_period >= 1)
  assert_that(last_trading_period <= strategy$ntrading_periods)
  assert_that(first_trading_period <= last_trading_period)
  assert_that(inherits(strategy, "pfselectstrat"))
  # If checking portfolios, pre-compose next_portfolio with a
  # portfolio validator
  if(check_portfolio) {
    next_pf <- validate_portfolio %>%
      partial(nassets = strategy$nassets) %>%
      purrr::compose(next_portfolio)
  } else {
    next_pf <- next_portfolio
  }
  # Use this function for accumulation with purrr:
  #    previous_portfolio -> move_to_next_period(trading_period)
  move_to_next_period <- function(portfolio_before_trade, trading_period) {
    # trade in current prices then return portfolio price-adjusted to
    # next period
    price_adjusted_portfolio(strategy$price_relatives[trading_period, ],
                             next_pf(strategy, trading_period,
                                     portfolio_before_trade))
  }
  # now backtest
  portfolios_before_trade <- first_trading_period:last_trading_period %>%
    purrr::accumulate(move_to_next_period,
                      .init = first_portfolio(strategy,
                                              first_trading_period)) %>%
    purrr::flatten_dbl() %>%
    matrix(ncol = strategy$nassets, byrow = TRUE) %>%
    `colnames<-`(colnames(strategy$price_relatives))
}


#' Computes the cumulative increase in wealth
#'
#' Given price relatives and portfolios at each period,
#' returns a vector containing the factor by
#' which cumulative wealth has increased at that trading period
#'
#' @param price_relatives A T x n matrix of price relatives
#' @param portfolios_before_trade a (T+1) x n matrix of the portfolios
#'     before the trade (an entry in a portfolio is the portion
#'     of total wealth in that asset)
#' @param transaction_rate The proportion of each buy/sell
#'     lost to transaction fees, must be in \eqn{[0,1]}
#'
#' @return a vector of length T reporting the factor of wealth
#'      increase at each period
#'
#' @importFrom assertthat assert_that are_equal
#' @importFrom magrittr %>%
#' @export
cumulative_wealth <- function(price_relatives,
                              portfolios_before_trade,
                              transaction_rate) {
  # Input validation
  validate_nonnegative_mat(price_relatives)
  portfolios_before_trade %>%
    purrr::array_branch(1L) %>%
    purrr::walk(validate_portfolio, nassets = ncol(price_relatives))
  assert_that(are_equal(nrow(price_relatives)+1,
                        nrow(portfolios_before_trade)))
  assert_that(is_scalar_double(transaction_rate))
  assert_that(transaction_rate >= 0 && transaction_rate <= 1)

  # pf (at period t), next_pf (at period t+1), price_relatives t->(t+1)
  get_wealth_increase <- function(pf, next_pf, price_relatives) {
    (1 / price_relatives) %>%
      price_adjusted_portfolio(next_pf) %>% # adjust back to period t
      wealth_increase_factor(price_relatives,
                             transaction_rate,
                             pf,
                             portfolio = .)
  }
  list(head(portfolios_before_trade, -1),
       portfolios_before_trade[-1, ],
       price_relatives) %>%
    purrr::map(purrr::array_branch, 1L) %>%
    purrr::pmap_dbl(get_wealth_increase) %>%
    accumulate(`*`)
}

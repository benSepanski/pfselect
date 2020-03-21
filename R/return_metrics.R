#' Computes the daily increase in wealth
#'
#' Given price relatives and portfolios at each period,
#' returns a vector containing the factor by
#' which cumulative wealth has increased at that trading period
#'
#' @param price_relatives A T x n matrix of price relatives
#' @param portfolios_after_trade a T x n matrix of the portfolios
#'     after the trade (an entry in a portfolio is the portion
#'     of total wealth in that asset), the \eqn{i}th row
#'     is after a trade in period \eqn{i} but before the \eqn{i}th
#'     price relatives occur.
#' @param transaction_rate The proportion of each buy/sell
#'     lost to transaction fees, must be in \eqn{[0,1]}
#'
#' @return a vector of length T whose
#'      \eqn{i}th entry reports the factor of wealth
#'      after the prices change according to the \eqn{i}th price relatives
#'
#' @importFrom assertthat assert_that are_equal
#' @importFrom magrittr %>%
#' @export
evaluate_daily_return <- function(price_relatives,
                                  portfolios_after_trade,
                                  transaction_rate) {
  # Input validation
  validate_nonnegative_mat(price_relatives)
  validate_portfolio_matrix(portfolios_after_trade,
                            nrow(price_relatives),
                            ncol(price_relatives))
  daily_return <- vector(mode = "numeric",
                         length = nrow(portfolios_after_trade))

  # prev_pf (at period t-1), pf (at period t), price_relatives t->(t+1)
  get_wealth_increase <- function(prev_pf, pf, price_relatives) {
    price_relatives %>%
      price_adjusted_portfolio(prev_pf) %>% # adjust up to period t
      get_return_from_trade(price_relatives = price_relatives,
                            tr = transaction_rate,
                            prev_portfolio = .,
                            portfolio = pf)
  }
  daily_return[1] <- (1-transaction_rate) * drop(
    portfolios_after_trade[1,] %*% price_relatives[1,]
    )
  daily_return[-1] <-  list(head(portfolios_after_trade, -1),
                              portfolios_after_trade[-1, ],
                              price_relatives[-1, ]) %>%
    purrr::map(purrr::array_branch, 1L) %>%
    purrr::pmap_dbl(get_wealth_increase)

  daily_return
}

#' @describeIn evaluate_daily_return
#'
#' Gives the factor by which initial wealth has increased at each trading
#' period.
#'
#' @importFrom magrittr %>%
#' @export
evaluate_cumulative_wealth <- function(price_relatives,
                                       portfolios_before_trade,
                                       transaction_rate) {
  evaluate_daily_return(price_relatives,
                       portfolios_before_trade,
                       transaction_rate) %>%
    purrr::accumulate(`*`)
}

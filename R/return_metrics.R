#' Computes the daily increase in wealth
#'
#' Given price relatives and portfolios at each period,
#' returns a vector containing the factor by
#' which cumulative wealth has increased at that trading period
#'
#' @param price_relative_matrix A T x n matrix of price relatives
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
evaluate_daily_return <- function(price_relative_matrix,
                                  portfolios_after_trade,
                                  transaction_rate) {
  # Input validation
  assert_that(is_numeric_matrix(price_relative_matrix))
  validate_portfolio_matrix(portfolios_after_trade,
                            nrow(price_relative_matrix) - 1,
                            ncol(price_relative_matrix))
  daily_return <- vector(mode = "numeric",
                         length = nrow(portfolios_after_trade))

  # prev_pf (at period t-1), pf (at period t), price_relative_matrix t->(t+1)
  get_wealth_increase <- function(prev_pf, pf, tp_price_relatives) {
    prev_pf %>%
      price_adjusted_portfolio(tp_price_relatives) %>% # adjust up to period t
      get_return_from_trade(tp_price_relatives = tp_price_relatives,
                            tr = transaction_rate,
                            prev_portfolio = .,
                            portfolio = pf)
  }
    daily_return[1] <- (1-transaction_rate) * drop(
    portfolios_after_trade[1,] %*% price_relative_matrix[1,]
    )
  daily_return[-1] <- list(head(portfolios_after_trade, -1),
                           portfolios_after_trade[-1,, drop = FALSE],
                           price_relative_matrix[-1,, drop = FALSE ]) %>%
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
evaluate_cumulative_wealth <- function(price_relative_matrix,
                                       portfolios_before_trade,
                                       transaction_rate) {
  evaluate_daily_return(price_relative_matrix,
                       portfolios_before_trade,
                       transaction_rate) %>%
    purrr::accumulate(`*`)
}

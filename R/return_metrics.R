#' Computes the daily increase in wealth
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
daily_return <- function(price_relatives,
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
    purrr::pmap_dbl(get_wealth_increase)
}

#' @describeIn daily_return
#'
#' Gives the factor by which initial wealth has increased at each trading
#' period.
#'
#' @importFrom magrittr %>%
#' @export
cumulative_wealth <- function(price_relatives,
                              portfolios_before_trade,
                              transaction_rate) {
  daily_return(price_relatives, portfolios_before_trade, transaction_rate) %>%
    purrr::accumulate(`*`)
}

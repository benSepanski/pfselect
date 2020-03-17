#' get prices from price relatives
#'
#' Given price_relatives and the initial prices, get the
#' actual prices
#'
#' @param price_relatives a matrix of price relatives
#'      where each row represents
#'      a trading period and each column represents an asset
#' @param initial_prices a vector whose \eqn{i}th entry is the price
#'      of the asset represented by column \eqn{i}
#'      of \code{price_relatives} immediately before trading period 1
#'
#' @return A matrix with one more row and the same
#'      number of columns as \code{price_relatives}
#'      representing the true prices of the assets.
#'
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that are_equal
#'
prices_from_relatives <- function(price_relatives, initial_prices) {
  # type and dimension checks
  assert_that(is_numeric_vector(initial_prices))
  assert_that(is.numeric(price_relatives))
  assert_that(is.matrix(price_relatives))
  assert_that(are_equal(ncol(price_relatives), length(initial_prices)))

  price_relatives %>%
    purrr::array_branch(2L) %>%
    purrr::map2(initial_prices, ~c(.y, .x)) %>%
    purrr::map(purrr::accumulate, `*`) %>%
    purrr::flatten_dbl() %>%
    matrix(ncol = ncol(price_relatives))
}


#' renormalizes portfolio to show total portion of wealth in each asset
#'
#' Starting with a portfolio whose \eqn{i}th entry was the portion
#' of ones total wealth in asset \eqn{i} for the previous prices,
#' return a new vector reflecting the portion of total wealth
#' in each asset under the new prices.
#'
#' @param price_relatives A vector whose \eqn{i}th entry
#'     is the ratio of the \eqn{i}th asset's new price
#'     to its old price. More precisely, if asset \eqn{i}
#'     had price \eqn{p_{t-1}} when wealth was distributed
#'     according to \code{portfolio}, but now has price \eqn{p_t},
#'     the \eqn{i}th entry of \code{price_relatives} is
#'     \eqn{\frac{p_t}{p_{t-1}}}.
#' @param portfolio A vector of non-negative numbers which sum to 1,
#'     the \eqn{i}th entry representing the portion of total wealth
#'     in asset \eqn{i}.
#'
#' @returns A numeric vector whose \eqn{i}th entry is the portion of total
 #'      wealth in asset \eqn{i} (according to the new prices).
 #'
 #' @importFrom assertthat assert_that are_equal
 #'
price_adjusted_portfolio <- function(price_relatives, portfolio) {
  # some type checks
  assert_that(is_numeric_vector(price_relatives))
  assert_that(is_numeric_vector(portfolio))
  assert_that(are_equal(length(portfolio), length(price_relatives)))
  # return the rebalanced portfolio
  portfolio * price_relatives / (portfolio %*% price_relatives)
}

#' Return the factor by which wealth increases
#'
#' For given price relatives from trading period \eqn{(t-1)}->\eqn{t},
#' two portfolios (before and after a trade) at times \eqn{t-1}
#' and a transaction rate, return the factor by which total
#' wealth increases from time \eqn{t-1} to time \eqn{t}
#'
#' @note both portfolios are
#'      numeric vectors whose \eqn{i}th entry represents
#'      the proportion of total wealth in asset \eqn{i}
#'      during the trading period \eqn{t-1}
#'
#' @param price_relatives A numeric vector whose \eqn{i}th entry
#'      is the price relative for asset \eqn{i}, i.e. its price
#'      at the new trading period divided by its price at the
#'      previous trading period
#' @param tr The transaction rate: a scalar in \eqn{[0,1]} which represents
#'      the percentage of any stock transaction (buying and selling) which
#'      goes towards transaction costs.
#' @param prev_portfolio The portfolio during the previous trading period
#'      before the trade
#' @param portfolio The portfolio during the previous trading period
#'      after the trade
#' @param tol We guarantee the trade is chosen so that
#'      the portfolio matches the argument \code{portfolio}
#'      within this tolerance (we have to iterate to
#'      reach a solution, since the trade is defined implicitly,
#'      see the comments inside the function)
#'  @param maxit Stops after \code{maxit} iterations in
#'      used in computing a trade
#'
#' @return A numeric scalar representing the factor by which wealth
#'      increases, i.e. if \eqn{S_t} is wealth
#'      at trading period \eqn{t}, returns \eqn{\frac{S_t}{S_{t-1}}},
#'
#' @importFrom assertthat assert_that are_equal
#'
wealth_increase_factor <- function(price_relatives, tr,
                                   prev_portfolio, portfolio,
                                   tol = 1e-10, maxit = 15) {
  # make sure we have vectors
  assert_that(is_numeric_vector(price_relatives))
  assert_that(is_numeric_vector(portfolio))
  assert_that(is_numeric_vector(prev_portfolio))
  assert_that(are_equal(length(price_relatives), length(portfolio)))
  assert_that(are_equal(length(prev_portfolio), length(portfolio)))

  # To move from portfolio b -> (b+a) with prices p and transaction
  # rate r and total wealth S, we have
  # Sb_i amount of currency of asset i.
  # If we trade +-Sa_i currency of asset i,
  # we lose rSa_i currency of the trade to transaction fees,
  # so wind up with S * (b_i +- (1-r)a_i) total wealth in asset
  # i. Therefore, our new total wealth is S(1- r\sum a_i).
  # The new proportion of our wealth in asset i is
  # (b_i +- (1-r)a_i) / (1 - r\sum a_i)
  # solving for a_i explicity is not easy, so we use a fixed
  # point iteration
  pf_from_trade <- function(trade) {
   (prev_portfolio + (1-tr) * trade) / (1-tr * sum(abs(trade)))
  }
  iter <- 1
  trade <- rep(0, length(portfolio))
  # iterate to compute a \approx (c - b)
  while(max(abs(pf_from_trade(trade) - portfolio)) >= tol) {
    if(iter > maxit) {
      stop(glue::glue("Max number of iterations reached {maxit}."))
    }
    trade <- (
      portfolio * (1 - tr * sum(abs(trade))) - prev_portfolio
    ) / (1 - tr)
    iter <- iter + 1
  }
  # Now the wealth factor is (1-trading_cost) * factor from price relatives
  (1 - tr * sum(abs(trade))) * as.double(portfolio %*% price_relatives)
}

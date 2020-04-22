
# Validation --------------------------------------------------------------


#' Validates and returns a portfolio
#'
#' Asserts that \code{portfolio} is a numeric vector
#' of length \code{nassets} with non-negative entries
#' that sum to one. Returns \code{portfolio}
#'
#' @param portfolio the portfolio to validate
#' @param nassets the number of assets the portfolio should have
#' @return portfolio
#'
#' @importFrom assertthat assert_that are_equal
#'
#' @noRd
validate_portfolio <- function(portfolio, nassets) {
  assert_that(is_numeric_vector(portfolio))
  assert_that(are_equal(length(portfolio), nassets))
  assert_that(are_equal(sum(portfolio), 1))
  assert_that(all(portfolio >= 0))
  portfolio
}

#' validate and return a matrix of portfolios
#'
#' @param portfolio_matrix a matrix where each row is a portfolio
#' @param ntrading_periods the number of trading periods, \code{portfolio_matrix}
#'     should have this many rows plus one (because have initial portfolio,
#'     then get a new one during each trading period).
#' @param nassets the number of assets, should be the number of columns
#'     in \code{portfolio_matrix}
#' @return portfolio_matrix
#'
#' @importFrom assertthat assert_that are_equal
#'
#' @noRd
validate_portfolio_matrix <- function(portfolio_matrix,
                                      ntrading_periods,
                                      nassets) {
  assert_that(are_equal(nrow(portfolio_matrix), ntrading_periods+1))
  assert_that(are_equal(ncol(portfolio_matrix), nassets))
  for(row in 1:nrow(portfolio_matrix)) {
    validate_portfolio(portfolio_matrix[row, ], nassets)
  }
  portfolio_matrix
}


# Uniform Portfolio -------------------------------------------------------


#' Returns a uniform portfolio
#'
#' Returns a uniform portfolio over the given number of assets
#'
#' @param nassets the number of assets
#' @return a numeric vector of length \code{nassets}, each
#'     with value \code{1/nassets}.
#'
#' @noRd
uniform_portfolio <- function(nassets) {
  rep(1/nassets, nassets)
}


# Common Price Computations -----------------------------------------------


#' get prices from price relatives
#'
#' Given price_relatives and the initial prices, get the
#' actual prices
#'
#' @param price_relatives a vector of price relatives
#'      where each entry represents the price relative for
#'      a trading period, i.e. price relative \eqn{x_t = p_{t+1} / p_t}.
#' @param initial_price \eqn{p_1}, the initial price.
#' @param .check_input If \code{TRUE}, checks that input meets the
#'      given assumptions.
#'
#' @return A vector with one more entry than \code{price_relatives}
#'      representing the true prices of the assets,
#'      the \eqn{i}th entry being the price during trading in period
#'      \eqn{i}, immediately before the price changes represent by price
#'      relatives \eqn{i} take place.
#'
#' @importFrom assertthat assert_that
#'
#' @export
#'
compute_prices_from_relatives <- function(price_relatives, initial_price,
                                          .check_input = TRUE) {
  # type and dimension checks
  if(.check_input) {
    assert_that(is_numeric_vector(price_relatives))
    assert_that(rlang::is_scalar_double(initial_price))
  }
  cumprod(c(initial_price, price_relatives))
}

#' @describeIn compute_prices_from_relatives
#'
#' Computes a matrix of prices from a matrix
#' of price relatives, where each column represents an asset.
#' Output has same column names as \code{price_relative_matrix}
#'
#' @eval backtest_strategy_args()
#' @param initial_prices A vector holding the initial prices of
#'     each asset \eqn{i}
#'
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#'
#' @export
compute_price_matrix_from_relatives <- function(price_relative_matrix,
                                                initial_prices,
                                                .check_input = TRUE) {
  if(.check_input) {
    assert_that(is_numeric_matrix(price_relative_matrix))
    assert_that(is_numeric_vector(initial_prices))
  }
  price_relative_matrix %>%
    purrr::array_branch(2L) %>%
    purrr::map2(initial_prices,
                compute_prices_from_relatives,
                .check_input = .check_input) %>%
    purrr::flatten_dbl() %>%
    matrix(ncol = ncol(price_relative_matrix)) %>%
    `colnames<-`(colnames(price_relative_matrix))
}

#' Return historical price means computed with decay factor
#'
#' We define the historical price mean MA_t at time t
#' to be
#' \deqn{MA_t = decay_factor \cdot p_t + (1-decay_factor) \cdot MA_{t-1}}
#' And \eqn{MA_1 = p_1}.
#'
#' @inheritParams compute_prices_from_relatives
#' @param prices A vector of prices
#' @param decay_factor the decay factor for the mean (see description)
#' @return A vector with the same dimension as \code{prices}
#'      with historical price means as the entries
#'
#' @importFrom assertthat assert_that
#'
#' @export
#'
compute_historical_price_means <- function(prices, decay_factor = 0.5,
                                           .check_input = TRUE) {
  if(.check_input) {
    assert_that(is_numeric_vector(prices))
    assert_that(rlang::is_scalar_double(decay_factor))
    assert_that(0 <= decay_factor && decay_factor <= 1)
  }
  purrr::accumulate(prices, ~decay_factor*.y + (1-decay_factor)*.x)
}


#' @describeIn compute_historical_price_means
#'
#' Computes a matrix of historical price means from a matrix
#' of prices, where each column represents an asset.
#' Ouptut has same column names as \code{price_matrix}
#'
#' @param price_matrix A matrix of prices, where each row represents
#'     a trading period and each column represents an asset
#'
#' @importFrom magrittr %>%
#'
#' @export
compute_historical_price_mean_matrix <- function(price_matrix,
                                                 decay_factor = 0.5,
                                                 .check_input = TRUE) {
  if(.check_input) {
    assert_that(is_numeric_matrix(price_matrix))
  }
  price_matrix %>%
    purrr::array_branch(2L) %>%
    purrr::map(compute_historical_price_means,
               decay_factor = decay_factor,
               .check_input = .check_input) %>%
    purrr::flatten_dbl() %>%
    matrix(ncol = ncol(price_matrix)) %>%
    `colnames<-`(colnames(price_matrix))
}



# Common Portfolio Computations -------------------------------------------


#' renormalizes portfolio to show total portion of wealth in each asset
#'
#' Starting with a portfolio whose \eqn{i}th entry was the portion
#' of ones total wealth in asset \eqn{i} for the previous prices,
#' return a new vector reflecting the portion of total wealth
#' in each asset under the new prices.
#'
#' @param portfolio A vector of non-negative numbers which sum to 1,
#'     the \eqn{i}th entry representing the portion of total wealth
#'     in asset \eqn{i}.
#' @param tp_price_relatives A vector whose \eqn{i}th entry
#'     is the ratio of the \eqn{i}th asset's new price
#'     to its old price at the end of this
#'     trading period. More precisely, if asset \eqn{i}
#'     had price \eqn{p_{t-1}} when wealth was distributed
#'     according to \code{portfolio}, but now has price \eqn{p_t},
#'     the \eqn{i}th entry of \code{price_relatives} is
#'     \eqn{\frac{p_t}{p_{t-1}}}.
#' @inheritParams compute_prices_from_relatives
#'
#' @return A numeric vector whose \eqn{i}th entry is the portion of total
 #'      wealth in asset \eqn{i} (according to the new prices).
 #'
 #' @importFrom assertthat assert_that are_equal
 #'
price_adjusted_portfolio <- function(portfolio, tp_price_relatives,
                                     .check_input = TRUE) {
  # some type checks
  if(.check_input) {
    assert_that(is_numeric_vector(tp_price_relatives))
    validate_portfolio(portfolio, length(tp_price_relatives))
  }
  # return the adjusted portfolio
  portfolio * tp_price_relatives / drop(portfolio %*% tp_price_relatives)
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
#' @param tp_price_relatives A numeric vector whose \eqn{i}th entry
#'      is the price relative for asset \eqn{i}, i.e. its price
#'      at the new trading period divided by its price at the
#'      previous trading period
#' @param transaction_rate The transaction rate: a scalar in \eqn{[0,1]}
#'      which represents
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
#'  @inheritParams compute_prices_from_relatives
#'
#' @return A numeric scalar representing the factor by which wealth
#'      increases, i.e. if \eqn{S_t} is wealth
#'      at trading period \eqn{t}, returns \eqn{\frac{S_t}{S_{t-1}}},
#'
#' @importFrom assertthat assert_that are_equal
#'
get_return_from_trade <- function(tp_price_relatives, transaction_rate,
                                  prev_portfolio, portfolio,
                                  tol = 1e-10, maxit = 15,
                                  .check_input = TRUE) {
  if(.check_input) {
    # make sure we have portfolios
    assert_that(is_numeric_vector(tp_price_relatives))
    validate_portfolio(prev_portfolio, length(tp_price_relatives))
    validate_portfolio(portfolio, length(tp_price_relatives))
    assert_that(rlang::is_scalar_double(transaction_rate))
    assert_that(0 <= transaction_rate && transaction_rate <= 1)
    assert_that(is_whole_number(maxit))
    assert_that(maxit > 0)
    assert_that(is_scalar_double(tol))
  }

  # abbreviated names
  tr <- transaction_rate
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
  (1 - tr * sum(abs(trade))) * as.double(portfolio %*% tp_price_relatives)
}

#' backtest to get cumulative wealth and portfolios over time
#'
#' Run a portfolio selection strategy from the given trading
#' period until the end of its data, reporting the cumulative
#' wealth at each stage and the portfolio before each
#' trading period
#'
#' @param pf_select the \link[=new_pfselectstrat]{pfselectstrat} instance to use
#' @param first_trading_period the trading period to start backtesting
#'     on (right before prices change)
#' @param last_trading_period (OPTIONAL) if supplied, only backtests
#'     up through \code{max_trading_period} (instead of through all
#'     available trading periods). This is the last price change we experience
#' @return A data frame which has
#'     @format{
#'       @describe{
#'          \item{trading_period}{The row index into \code{pf_select}'s
#'              price_relatives, indicating the current trading period}
#'          \item{cumulative_wealth}{The factor of wealth increase accumulated
#'              up to this trading period}
#'          \item{<Asset Names>}{Names matching the column names of
#'              \code{pf_select@@price_relatives} indicating the
#'              portion of total wealth in that asset at the given trading
#'              period}
#'       }
#'     }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr slice
#' @export
#'
backtest_portfolio_selector <- function(pf_select,
                                        first_trading_period,
                                        last_trading_period) {
  wealth_factor <- 1.0
  # Run some basic sanity checks on the arguments
  if(missing(last_trading_period)) {
    last_trading_period <- pfselect$ntrading_periods
  }
  else if(last_trading_period > pfselect$ntrading_periods) {
    warn(glue::glue("max_trading_period of {last_trading_period} is greater \\
than last trading period of {pfselect$ntrading_periods}."))
  }
  if(first_trading_period > last_trading_period) {
    stop("first trading period is after last trading period.")
  }

  # initialize space for recording portfolios and cumulative wealth
  nt_periods <- last_trading_period - first_trading_period + 1
  portfolios_before_trade <- matrix(nrow = nt_periods,
                                    ncol = pf_select$nassets)
  colnames(portfolios_before_trade) <- colnames(pf_select$price_relatives)
  cumulative_wealth <- vector(mode = "numeric", length = nt_periods)

  # now backtest
  portfolio <- first_portfolio(pf_select, first_trading_period)
  for(index in 1:nt_periods) {
    # record portfolio and get price relatives
    portfolios_before_trade[index, ] <- portfolio

    per <- first_trading_period + index - 1
    # get the next portfolio, record it and the cumulative wealth increase
    next_portfolio <- next_portfolio(pf_select, per, portfolio)
    increase_factor <- wealth_increase_factor(pf_select$price_relatives[per, ],
                                              pf_select$transaction_rate,
                                              portfolio,
                                              next_portfolio)
    wealth_factor <- increase_factor * wealth_factor
    cumulative_wealth[index] <- wealth_factor

    # adjust portfolio to next trading periods prices
    portfolio <- pf_select$price_relatives[per, ] %>%
      price_adjusted_portfolio(next_portfolio)
  }

  # Return a data frame with each of the portfolios, the cumulative
  # wealth, and the trading period
  portfolios_before_trade %>%
    data.frame() %>%
    dplyr::as.tbl()  %>%
    dplyr::mutate(trading_period = first_trading_period:last_trading_period,
                  cumulative_wealth = cumulative_wealth)
}

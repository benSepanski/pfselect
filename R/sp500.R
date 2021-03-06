#' S&P 500
#'
#' Selected S&P 500 prices from 1998 to 2003
#' Downloaded by Ben Sepanski
#'
#' \describe{
#'   \item{Market:}{Standard & Poor's 500}
#'   \item{Number of stocks}{25}
#'   \item{Number of days}{1276}
#'   \item{Number of years}{5}
#'   \item{Period}{Jan 2, 1998 to Jan 31, 2003}
#'   \item{Other Info}{25 largest market cap stocks in the SP500
#'                     index (as of Apr 2003). Source: MSN Money}
#' }
#'
#' @format A matrix with 1276 rows and 25 variables of daily closing prices.
#'    Each entry is a price relative: if a stock has price
#'    \eqn{p_t} at trading period \eqn{t}, it has price relative
#'    \eqn{\frac{p_t}{p_{t-1}}} at trading period \eqn{t}.
#'    The column names are the stock names.
#'
#' @source \url{http://www.cs.technion.ac.il/~rani/portfolios/portfolios.htm}
"sp500"

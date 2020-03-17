#' NYSE(O)
#'
#' New York Stock Exchange prices from 1962-1984.
#' Downloaded by Ben Sepanski
#'
#' \describe{
#'   \item{Market:}{New York Stock Exchange}
#'   \item{Number of stocks}{36}
#'   \item{Number of days}{5651}
#'   \item{Number of years}{22}
#'   \item{Period}{Jul 3, 1962 to Dec 31, 1984}
#'   \item{Other Info}{Contains very large market cap stocks.
#'                     Dataset was collected by Hal Stern}
#' }
#'
#' @format A matrix with 5651 rows and 36 columns of daily
#'    closing prices.
#'    Each row represents a trading day and each column
#'    represents an asset.
#'    Each entry is a price relative: if a stock has price
#'    \eqn{p_t} at trading period \eqn{t}, it has price relative
#'    \eqn{\frac{p_t}{p_{t-1}}} at trading period \eqn{t}.
#'    The column names are the stock names.
#'
#' @source \url{http://www.cs.technion.ac.il/~rani/portfolios/portfolios.htm}
"nyse_o"

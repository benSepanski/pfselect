#' TSE
#'
#' Toronto Stock Exchange prices from 1994 to 1998.
#' Downloaded by Ben Sepanski
#'
#' \describe{
#'   \item{Market:}{Toronto Stock Exchange}
#'   \item{Number of stocks}{88}
#'   \item{Number of days}{1258}
#'   \item{Number of years}{5}
#'   \item{Period}{Jan 4, 1994 to Dec 31, 1998}
#'   \item{Other Info}{contains all TSE stocks that were traded on
#'                     each of the 1258 trading days in this period.}
#' }
#'
#' @format A matrix with 1258 rows and 88 columns of daily closing prices.
#'    Each row represents a trading day and each column
#'    represents an asset.
#'    Each entry is a price relative: if a stock has price
#'    \eqn{p_t} at trading period \eqn{t}, it has price relative
#'    \eqn{\frac{p_t}{p_{t-1}}} at trading period \eqn{t}.
#'    Each column name is the stock name.
#'
#' @source \url{http://www.cs.technion.ac.il/~rani/portfolios/portfolios.htm}
"tse"

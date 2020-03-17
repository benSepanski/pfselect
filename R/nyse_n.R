#' NYSE(N)
#'
#' New York Stock Exchange prices from 1985-2010
#' Downloaded by Ben Sepanski
#'
#' \describe{
#'   \item{Market:}{New York Stock Exchange}
#'   \item{Number of stocks}{23}
#'   \item{Number of days}{6431}
#'   \item{Number of years}{25}
#'   \item{Period}{Jan 1, 1985 - Jun 30, 2010}
#'   \item{Other Info}{Contains very large market cap stocks}
#' }
#'
#' @format A matrix with 6431 rows and 23 columns of daily closing prices.
#'    Each row represents a trading day and each column
#'    represents an asset.
#'    Each entry is a price relative: if a stock has price
#'    \eqn{p_t} at trading period \eqn{t}, it has price relative
#'    \eqn{\frac{p_t}{p_{t-1}}} at trading period \eqn{t}.
#'
#' @source \url{https://github.com/OLPS/OLPS/tree/master/Data}
"nyse_n"

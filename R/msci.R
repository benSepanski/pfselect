#' MSCI
#'
#' MSCI country data from 2006-2010
#' Downloaded by Ben Sepanski
#'
#' \describe{
#'   \item{Market:}{New York Stock Exchange}
#'   \item{Number of stocks}{24}
#'   \item{Number of days}{1043}
#'   \item{Number of years}{4}
#'   \item{Period}{Jan 4, 2006 - March 31, 2010}
#' }
#'
#' @format A matrix with 1043 rows and 24 columns of daily closing prices.
#'    Each row represents a trading day and each column
#'    represents an asset.
#'    Each entry is a price relative: if a stock has price
#'    \eqn{p_t} at trading period \eqn{t}, it has price relative
#'    \eqn{\frac{p_t}{p_{t-1}}} at trading period \eqn{t}.
#'
#' @source \url{https://github.com/OLPS/OLPS/tree/master/Data}
"msci"

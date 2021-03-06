% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/backtest_strategy.R
\name{backtest_buyandhold}
\alias{backtest_buyandhold}
\title{backtest a buy and hold strategy}
\usage{
backtest_buyandhold(price_relative_matrix, transaction_rate, initial_portfolio)
}
\arguments{
\item{price_relative_matrix}{a matrix of price relatives, each row
representing a trading period and each column an asset.
A price relative is \eqn{p_{t+1} / p_t}, i.e.
the ratio of trading price to next price. Prices change
according to the price relatives after the trade,
i.e. the price relatives for the trading period are not known
at trading time}

\item{transaction_rate}{The percentage of each transaction (buy and sell)
spent on broker fees}

\item{initial_portfolio}{a vector whose \eqn{i}th entry is the
amount of wealth in the \eqn{i}th entry. Should
be a portfolio for the assets in \code{price_relative_matrix}.
This is the portfolio before the first trading period.}
}
\value{
a matrix with the same number of columns and one more
row than \code{price_relative_matrix}, row \eqn{i}
is the portfolio after a trade during period \eqn{i}, i.e.
right before the \eqn{i}th price relatives change the prices.
}
\description{
backtest a buy and hold strategy
}

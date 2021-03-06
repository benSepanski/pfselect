% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/momentum_prediction.R
\name{predict_window_momentum_LOAD}
\alias{predict_window_momentum_LOAD}
\title{predicts momentum for asset according to LOAD strategy}
\usage{
predict_window_momentum_LOAD(
  prev_prices,
  regularization_factor,
  momentum_threshold,
  min_var = .Machine$double.eps^0.5
)
}
\arguments{
\item{prev_prices}{a numeric vector of previous prices}

\item{regularization_factor}{\eqn{\lambda} in the referenced paper,
the regularization coeffecient for weight decay when
regressing the prices in the recent time window.
Must be non-negative}

\item{momentum_threshold}{\eqn{\eta} in the referenced paper.
If the regressed slope is greater than \eqn{\eta} we say the
stock has momentum. This must be greater than 0.}

\item{min_var}{(OPTIONAL) minimum variance to be determined non-constant}
}
\value{
the predicted next momentum (1 or 0)
}
\description{
predicts next momentum for one asset
 given previous prices according to LOAD
strategy (see paper mentioned in description of
\code{\link{backtest_LOAD}}), momentu is defined in
\code{\link{evaluate_momentum_at_window}}.
}
\note{
NO TYPE CHECKING IS PERFORMED... be careful
}

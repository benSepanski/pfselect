% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/momentum_prediction.R
\name{compute_price_relatives_eigen}
\alias{compute_price_relatives_eigen}
\title{Get eigenvalue decompositions for each time step}
\usage{
compute_price_relatives_eigen(price_relative_matrix)
}
\arguments{
\item{price_relative_matrix}{A matrix of price relatives
with at least three rows.}
}
\value{
A list with two sublists, see description for details
}
\description{
Returns a list with two sublists: values and vectors.
For \eqn{i >= 3}, the \eqn{i}th values and vectors are the
eigenvalue decomposition of the covariance matrix where we
treat the first \eqn{i-1} rows as observations and the columns as variables.
}
\details{
For \eqn{i=1,2} The values are all one and the vectors form the identity
matrix
}

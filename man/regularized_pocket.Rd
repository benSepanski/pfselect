% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/momentum_prediction.R
\name{regularized_pocket}
\alias{regularized_pocket}
\title{Perform regularized pocket algorithm}
\usage{
regularized_pocket(x, y, weight_elimination, maxit, initial_weights = NULL)
}
\arguments{
\item{x}{A numeric matrix with \eqn{n} rows. Should NOT include
a column of all 1s for bias weight.}

\item{y}{a numeric vector with \eqn{n} columns}

\item{weight_elimination}{\eqn{\lambda} in the description}

\item{maxit}{the maximum number of iterations}

\item{initial_weights}{the initial weights. If missing or NULL,
uses linear regression}
}
\value{
perceptron weights (bias, weights) where
    \eqn{y ~ bias + x * weights}. Attribute \code{"niter"}
    holds the iteration count
}
\description{
Performs the Perceptron Learning Algorithm with weight elimination
}
\details{
\deqn{
    [[sign(w^Tx_n) \neq y_n]]
    + \frac{\lambda}{2n}\sum_{i=1}^d\frac{w_i^2}{1+w_i^2}
}
We treat the PLA update as a ``derivative" of the first component.
So, our update in the \eqn{i}th component will be
\deqn{
    PLA_update
    - \frac{\lambda}{n}\frac{w_i}{(1+w_i^2)^2}
}
}

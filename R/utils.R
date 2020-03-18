#' tests whether x is close to an integer
#'
#' Test whether x is close to an integer,
#' asserts that x is a numeric scalar
#'
#' @param x the numeric scalar vector to test
#' @return TRUE if x is close to an integer and
#'      false elsewhere
#'
#' @importFrom assertthat assert_that is.scalar
#' @importFrom rlang is_double
#'
is_whole_number <- function(x) {
  assert_that(is.scalar(x))
  assert_that(is.numeric(x))
  abs(x - round(x)) < .Machine$double.eps ^ 0.5
}

#' checks if x is a numeric vector
#'
#' @param x the object to check
#' @return TRUE iff x is a numeric vector
#'
#' @importFrom rlang is_vector
is_numeric_vector <- function(x) {
  is.numeric(x) && is_vector(x)
}

#' Computes Euclidean Projection onto simplex
#'
#' Returns the closest vector on the simplex of radius
#' r
#'
#' Returns minimizer of
#' \deqn{\frac{1}{2}||w-v||^2,  v^T1 = r, v >= 0}
#' using method from the following paper:
#' \url{https://stanford.edu/~jduchi/projects/DuchiShSiCh08.pdf}.
#'
#' @param v The vector to project
#' @param r (DEFAULT 1) radius of simplex
#' @return the projection of \code{v} onto the \code{r}-simplex
#'
project_to_simplex <- function(v, r = 1) {
  u <- sort(v, decreasing = TRUE)
  test_vec <- u - (cumsum(u) - r) / 1:length(u)
  rho <- max(which(test_vec > 0))
  theta <- u[rho] - test_vec[rho]
  pmax(v - theta, 0)
}

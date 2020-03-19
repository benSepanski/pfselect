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

#' Validates and returns a numeric matrix
#'
#' Asserts that \code{mat} is a numeric matrix
#' with non-negative entries.
#' Returns \code{portfolio}
#'
#' @param mat the object to validate
#' @return Returns \code{mat}
#'
#' @importFrom assertthat assert_that
#'
validate_nonnegative_mat <- function(mat) {
  assert_that(is.matrix(mat))
  assert_that(is.numeric(mat))
  assert_that(all(mat >= 0))
  mat
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

#' Samples from Dirichlet(1/2,1/2,...,1/2)
#'
#' Returns independent samples from the Dirichlet(1/2,1/2,...,1/2)
#' distribution
#'
#' @param n the number of samples
#' @param d the dimension of each sample
#' @return An n x d matrix
#'      of \code{n} samples from the \code{d}-dimensional
#'      Dirichlet(1/2,1/2,...,1/2) distribution
#'      (each sample is a row)
#'
#' @seealso \url{https://www.wikiwand.com/en/Dirichlet_distribution#/Random_number_generation}
#'
rdirichlet_onehalf <- function(n, d) {
  samples <- matrix(rgamma(n * d, 0.5 , 1), nrow = n)
  sweep(samples,
        MARGIN = 1L,
        STATS = rowSums(samples),
        FUN = "/")
}

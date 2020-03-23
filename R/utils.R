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

#' checks if x is a numeric matrix
#'
#' @param x the object to check
#' @return TRUE iff x is a numeric matrix
#'
is_numeric_matrix <- function(x) {
  is.numeric(x) && is.matrix(x)
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

#' project to simplex in the A norm
#'
#' Find the closest vector on the simplex to v,
#' where closest is in terms of the A norm
#' \eqn{x^TAx}
#' Runs projected gradient descent for maxit
#' or until tolernace is reached
#'
#' @inheritParams project_to_simplex
#' @param A The positive-definite matrix to use for a norm
#'     (NOTE: no checks performed to verify)
#' @param step_sizes (OPTIONAL) Projected gradient descent step size.
#'     If a vector, tries each step size and picks closest.
#'     Should be sorted greatest -> smallest (descending)
#' @param tol (OPTIONAL) Min update before termination
#' @param maxit (OPTIONAL) max number of iterations
project_to_simplex_A_norm <- function(v, A, r = 1,
                                      step_sizes = c(1,.5,.1,.05,.01,.005,.001),
                                      maxit = 5,
                                      tol = 1e-7) {
  ata <- t(A) %*% A
  atv <- t(A) %*% v
  w <- project_to_simplex(v)
  prev_w <- vector(mode = "numeric", length = length(w))
  for(step_size in step_sizes) {
    for(iter in 1:maxit){
      prev_w <- w
      w <- project_to_simplex(drop(w - step_size * (ata %*% w - atv)))
      if(max(abs(w - prev_w)) < tol) {
        break
      }
    }
  }
  w
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


#' rollify f to operate over windows
#'
#' Returns a function which rolls \code{f} over windows.
#' \code{window_sizes} is a vector,
#' \code{window_sizes[i]} should be the window size for the \eqn{i}th
#' positional argument of \code{f}. This function can only
#' roll positional arguments.
#'
#' For example, if we have f(x,y,z) where x, y, and z are some windows,
#' and we want to roll f across vectors (xvec, yvec, zvec), we
#' would call rollify(f)(xvec, yvec, zvec). These vecs
#' must all have the same length, and there must be at least one.
#'
#' @param f the function to roll. \code{f} must return
#'     some double value.
#' @param window_sizes the size of the windows in each parameter.
#'     Must be at least one.
#' @param fill the value to fill in entries when there isn't
#'     enough data for a given window size
#'
#' @return a function which rolls \code{f} over windows of the
#'     given sizes.
#'
#' @importFrom purrr map
#' @importFrom magrittr %>%
#'
rollify_dbl <- function(f, window_sizes, fill = NA) {

  get_windowed_l <- function(offset, l) {
    window <- map(window_sizes,
                  ~seq.int(from = offset, by = -1, length.out = .x))
    l[1:length(window)] <- map2(l[1:length(window)], window, `[`)
    l
  }

  function(...) {
    l <- list(...)
    # assume all same length
    roll_length <- length(l[[1]])
    rolled_result <- rep(NA_real_, roll_length)
    # now roll results over windows
    roll_range <- max(window_sizes):roll_length
    rolled_result[roll_range] <- roll_range %>%
      map(get_windowed_l, l) %>%
      purrr::map_dbl(do.call, what = f)
    rolled_result
  }
}


#' Perform regularized pocket algorithm
#'
#' Performs the Perceptron Learning Algorithm with two regularization
#' constants.
#'
#' \deqn{
#'     [[sign(w^Tx_n) \neq y_n]]
#'     + \frac{\eta}{2n}\sum_{i=1}^d\frac{w_i^2}{1+w_i^2}
#'     + \frac{\lambda}{2n} \sum_{i=1}^d w_i^2
#' }
#' We treat the PLA update as a ``derivative" of the first component.
#' So, our update in the \eqn{i}th component will be
#' \deqn{
#'     PLA_update
#'     - \frac{\eta}{n}\frac{w_i}{(1+w_i^2)^2}
#'     - \frac{\lambda}{n} w_i
#' }
#'
#' @param x A numeric matrix with \eqn{n} rows. Should NOT include
#'     a column of all 1s for bias weight.
#' @param y a numeric vector with \eqn{n} columns
#' @param weight_elimination \eqn{\eta} in the description
#' @param weight_decay \eqn{\lambda} in the description
#' @param maxit the maximum number of iteratoins
#'
#' @return perceptron weights
#'
#' @export
#'
regularized_pocket <- function(x, y,
                               weight_elimination,
                               weight_decay,
                               maxit) {
  n <- length(y)
  weight_elimnation <- weight_elimination/n
  weight_decay <- weight_decay/n
  update_weights <- function(prev_weights, misclassified_indices) {
    reg_update <- -weight_elimination * weights[-1] / (1+weights[-1]^2)^2
    reg_update <- update - weight_decay * weights[-1]
    weights[-1] <- weights[-1] + reg_update

    row <- misclassified_indices[sample.int(length(misclassified_indices), 1)]
    weights + y[row] * x[row, ]
  }

  weights <- lm(y~x)
  best_err <- 1
  for(i in 1:maxit) {
    misclassified_indices <- which(sign(drop(x %*% weights)) != y)
    if(length(misclassified_indices) == 0) {
      break
    }
    next_weights <- update_weights(weights)
    err <- mean(sign(drop(x %*% next_weights)) != y)
    if(err < best_err) {
      weights <- next_weights
    }
  }
  weights
}














# Type checks -------------------------------------------------------------


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

# Simplex Utils -----------------------------------------------------------


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



# Rolling -----------------------------------------------------------------


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
#' @param f the function to roll.
#' @param window_sizes the size of the windows in each parameter.
#'     Must be at least one.
#' @param fill the value to fill in entries when there isn't
#'     enough data for a given window size
#'
#' @return a function which rolls \code{f} over windows of the
#'     given sizes.
#'
#' @importFrom purrr map map2
#' @importFrom magrittr %>%
#'
rollify <- function(f, window_sizes, fill = NA) {

  get_windowed_l <- function(offset, l) {
    window <- map(window_sizes,
                  ~seq.int(from = offset + 1 - .x, length.out = .x))
    l[1:length(window)] <- map2(l[1:length(window)], window, `[`)
    l
  }

  function(...) {
    l <- list(...)
    # assume all same length
    roll_length <- length(l[[1]])
    rolled_result <- rep(list(NA), roll_length)
    # now roll results over windows
    roll_range <- max(window_sizes):roll_length
    rolled_result[roll_range] <- roll_range %>%
      map(get_windowed_l, l) %>%
      map(do.call, what = f)
    rolled_result
  }
}

#' @describeIn rollify
#'
#' \code{rollify} is to \code{rollify_dbl} as \code{purrr::map}
#' is to \code{purrr::map_dbl}. i.e. for \code{rollify_dbl}, \code{f}
#' \emph{MUST} return a single numeric value.
#' Instead of returning a list, the function returned
#' by \code{rollify_dbl} returns a numeric vector.
#'
#' @importFrom purrr map map2 map_dbl
#' @importFrom magrittr %>%
#'
rollify_dbl <- function(f, window_sizes, fill = NA) {

  get_windowed_l <- function(offset, l) {
    window <- map(window_sizes,
                  ~seq.int(from = offset + 1 - .x, length.out = .x))
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
      map_dbl(do.call, what = f)
    rolled_result
  }
}


# Regularized Pocket ------------------------------------------------------


#' Perform regularized pocket algorithm
#'
#' Performs the Perceptron Learning Algorithm with weight elimination
#'
#' \deqn{
#'     [[sign(w^Tx_n) \neq y_n]]
#'     + \frac{\lambda}{2n}\sum_{i=1}^d\frac{w_i^2}{1+w_i^2}
#' }
#' We treat the PLA update as a ``derivative" of the first component.
#' So, our update in the \eqn{i}th component will be
#' \deqn{
#'     PLA_update
#'     - \frac{\lambda}{n}\frac{w_i}{(1+w_i^2)^2}
#' }
#'
#' @param x A numeric matrix with \eqn{n} rows. Should NOT include
#'     a column of all 1s for bias weight.
#' @param y a numeric vector with \eqn{n} columns
#' @param weight_elimination \eqn{\lambda} in the description
#' @param maxit the maximum number of iterations
#' @param row_probs A vector of length \code{nrow(x)}.
#'     Each entry is the relative probability of choosing
#'     the corresponding row from x. It is also the weight
#'     which this entry receives when computing the average error
#'     for a set of weights.
#'     Defaults to uniform.
#' @param initial_weights the initial weights. If missing or NULL,
#'    uses linear regression
#'
#' @return perceptron weights
#'
#' @importFrom assertthat assert_that are_equal
#'
#' @export
#'
regularized_pocket <- function(x, y,
                               weight_elimination,
                               maxit,
                               row_probs,
                               initial_weights) {
  assert_that(is_numeric_matrix(x))
  assert_that(is_numeric_vector(y))
  assert_that(is_whole_number(maxit))
  assert_that(maxit > 0)
  assert_that(rlang::is_scalar_double(weight_elimination))
  assert_that(weight_elimination >= 0)
  assert_that(are_equal(nrow(x), length(y)))
  if(missing(row_probs)) {
    row_probs <- rep(1, nrow(x))
  }
  assert_that(is_numeric_vector(row_probs))
  assert_that(are_equal(nrow(x), length(row_probs)))
  if(missing(initial_weights) || is.null(initial_weights)) {
    initial_weights <- lm(y~x)$coefficients
  }
  assert_that(is_numeric_vector(initial_weights))
  assert_that(are_equal(length(initial_weights), ncol(x) + 1))


  weight_elimnation <- weight_elimination/length(y)

  weights <- initial_weights
  best_err <- 1
  for(i in 1:maxit) {
    misclassified_indices <- which(
      sign(drop(weights[1] + x %*% weights[-1])) != y
    )
    # if none misclassified, done
    if(length(misclassified_indices) == 0) {
      break
    }
    # OW choose a row and update to new weights if they're better
    probs <- row_probs[misclassified_indices]
    ind <- sample.int(length(misclassified_indices), 1, prob = probs)
    row <- misclassified_indices[ind]

    next_weights <- weights
    next_weights[1] <- next_weights[1] + y[row]
    next_weights[-1] <- (
      y[row] * x[row,] - weight_elimination * weights[-1] / (1+weights[-1]^2)^2
    )
    err <- weighted.mean(
      x = sign(drop(next_weights[1] + x %*% next_weights[-1])) != y,
      w = row_probs
    )
    if(err < best_err) {
      weights <- next_weights
      best_err <- err
    }
  }
  weights
}


#' Use nested cross-validation to estimate error f w.r.t. tuning_parameter
#'
#' Tests each given option to tuning_parameter
#' using nested cross validation (i.e. the nth row of data depends
#' on the (n-1)th) to estimate the out-of-sample error of f
#' w.r.t. the tuning_parameter
#'
#' @param f Must be evaluated by calling on any consecutive subset
#'     of the rows of data, output, and an entry from tuning_parameter,
#'     e.g. \code{f(tuning_parameter[[1]], data, output)}. Should return
#'     a function \code{g} which may be evaluated
#'     on any consecutive subset of rows of data and output
#'     as \code{g(data, output)} and return the error.
#'     For both \code{f} and \code{g}, arguments are passed positionally.
#'     To have \code{f} re-use data, have it access
#'     the list \code{previous_data} in its \code{parent.frame()},
#'     i.e. the execution environment of this function.
#' @param data A data frame. Treats row index like time, first
#'     rows are revealed first.
#' @param output A vector of the desired output, each entry corresponds
#'     to a row in the data frame
#' @param tuning_parameters a list, each entry of which is a potential
#'     tuning parameter to f
#' @param nfolds Number of folds, if missing defaults to folds of size 1
#'
#' @return a list with each weight elimination parameter
#'     and the average cv error
#'
#' @importFrom assertthat assert_that are_equal
#'
nested_cv <- function(f,
                      data,
                      output,
                      tuning_parameters,
                      maxit_per_fold,
                      nfolds) {
  # some input validation
  assert_that(is.data.frame(data))
  assert_that(all(output >= 0))
  assert_that(are_equal(nrow(data), length(output)))
  if(missing(nfolds)) {
    nfolds = length(output) - 1
  }
  assert_that(is_whole_number(nfolds))
  assert_that(nfolds > 1)
  # compute each fold
  n <- length(output)
  index_folds <- 1:n %>%
    split(cut(1:n, nfolds, labels = FALSE)) %>%
    purrr::map_int(max)
  # for functions to re-use data
  previous_data <- list()

  # Now estimate with nested cv
  err_df <- tibble::tibble(tuning_parameters = tuning_parameters,
                           error = rep(0, length(tuning_parameters)))
  for(i in 1:(n-1)) {
    # get train/test data
    train_data <- dplyr::slice(data, 1:index_folds[i])
    train_out <- output[1:index_folds[i]]
    test_data <- dplyr::slice(data, (index_folds[i]+1):index_folds[i+1])
    test_out <- output[(index_folds[i]+1):index_folds[i+1]]
    # compute error for each tuning parameter
    err <- tuning_parameters %>%
      purrr::map(f, train_data, train_out) %>%
      purrr::map_dbl(~.(test_data, test_out))
    err_df$error <- err_df$error + err
  }

  err_df$error <- err_df$error / (n-1)
  err_df
}

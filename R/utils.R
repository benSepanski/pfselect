
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
#' @noRd
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
#'
#' @noRd
#'
is_numeric_vector <- function(x) {
  is.numeric(x) && is_vector(x)
}

#' checks if x is a numeric matrix
#'
#' @param x the object to check
#' @return TRUE iff x is a numeric matrix
#'
#' @noRd
#'
is_numeric_matrix <- function(x) {
  is.numeric(x) && is.matrix(x)
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
#' @noRd
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
#'
#' @noRd
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
#' @noRd
rdirichlet_onehalf <- function(n, d) {
  samples <- matrix(rgamma(n * d, 0.5 , 1), nrow = n)
  sweep(samples,
        MARGIN = 1L,
        STATS = rowSums(samples),
        FUN = "/")
}

# Windows -------------------------------------------------------------

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
#' @noRd
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
#' @noRd
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

#' Gets all windows
#'
#' A window of size w is the
#' w entries in a vector before the current entry.
#'
#' This function
#' returns a tibble with columns
#' \describe{
#'     \item{window}{The window of entries before the current entry}
#'     \item{current_value}{The current entry (last value in the window)}
#'     \item{next_value}{The entry after the current window}
#'     \item{next_index}{The index of next_value}
#' }
#' Any missing entries are filled in as \code{fill}, i.e. the last
#' window has no next value, and the first few next_values
#' have incomplete (and hence missing) windows.
#'
#' @param values A vector of values to get windows from
#' @param window_size the size of a previous price window.
#'     Must be a postivie whole number
#' @param include_missing If \code{TRUE}, includes the first few rows
#'     and last row which have either a missing window or missing
#'     next value. If \code{FALSE}, does not include these rows.
#' @return A tibble with the windows next values, and indices
#'     (see description for details)
#'
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#'
#' @noRd
get_windows <- function(values, window_size, include_missing = FALSE) {
  # some input validation
  assert_that(rlang::is_vector(values))
  assert_that(is_whole_number(window_size))
  assert_that(window_size > 0)
  assert_that(window_size < length(values))
  assert_that(rlang::is_scalar_logical(include_missing))

  get_window <- function(next_index) {
    if(next_index - window_size <= 0) {
      return(NA)
    }
    values[(next_index-window_size):(next_index-1)]
  }

  if(include_missing){
    first_index = 1
    last_index = length(values)+1
  }
  else {
    first_index = window_size+1
    last_index = length(values)
  }

  tibble::tibble(next_index = first_index:last_index,
                 window = purrr::map(next_index, get_window),
                 current_value = values[next_index-1],
                 next_value = values[next_index])
}








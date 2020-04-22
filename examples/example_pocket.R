## Examples of using regularized pocket
library("tidyverse")
library("pfselect")

# Let's generate some (probably) non-separable data
test_pocket <- function(n, d, maxit) {
  x <- 10 * rnorm(d * n) %>%
    matrix(ncol = d)
  test_x <- 10 * rnorm(1000 * d) %>%
    matrix(ncol = d)

  stopifnot(d > 5)
  w <- c(rnorm(5), rnorm(d-5, sd =0.001))
  b <- rnorm(1)
  y <- sign(drop(x %*% w + b) + rnorm(n, sd=0.05))
  test_y <- sign(drop(test_x %*% w + b))

  weight_elimination <- c(0, 0.1, 1.0, 10.0)
  weights <- weight_elimination %>%
    map(~pfselect::regularized_pocket(x,
                                      y,
                                      .,
                                      maxit,
                                      initial_weights = rnorm(d+1, sd = 0.05)))
  list(weight_elimination = weight_elimination,
       in_error = map_dbl(weights, ~mean(sign(.[1] + x %*% .[-1]) != y)),
       test_error = map_dbl(weights, ~mean(sign(.[1] + test_x %*% .[-1]) != test_y)))
}

n <- 100
d <- 20
tb <- replicate(200, test_pocket(n, d, maxit=200), simplify = FALSE) %>%
  transpose() %>%
  as_tibble() %>%
  unnest(c(weight_elimination, in_error, test_error))
tb %>%
  ggplot(aes(test_error,
             fill = factor(weight_elimination),
             color = factor(weight_elimination))) +
    geom_density(alpha = 0.4)
  group_by(weight_elimination) %>%
  summarise(test_err_mean = mean(test_error), test_err_sd = sd(test_error)) %>%
  ungroup(weight_elimination)


#####################################################
# Now let's do it in 2d so we can make some pictures#
#####################################################

# Let's generate some (probably) non-separable data
n <- 100
x <- rnorm(2 * n) %>%
  matrix(ncol = 2)
test_x <- rnorm(1e4) %>%
  matrix(ncol = 2)

w <- rnorm(2)
b <- rnorm(1)
y <- sign(drop(x %*% w + b) + rnorm(n, sd=0.05))
test_y <- sign(drop(x %*% w + b))

# Compute lines with various weight elimination constants
weight_elimination <- c(0, 0.1, 0.5, 1.0)
maxit <- 4 * n
weights <- weight_elimination %>%
  map(~pfselect::regularized_pocket(x, y, ., maxit, initial_weights = c(0,0,0)))
# Plot the lines
tibble(weight_elimination = weight_elimination, weights = weights) %>%
  mutate(intercept = map_dbl(weights, ~-.[1] / .[3]),
         slope = map_dbl(weights, ~-.[2] / .[3]),
         error = map_dbl(weights, ~mean(sign(.[1] + test_x %*% .[-1]) != y)),
         avg_dist_to_line = map_dbl(weights,
                                    ~mean(abs(
                                      .[1] + test_x %*% .[-1]) /
                                        drop(.[-1] %*% .[-1])
                                      )),
         niter = map_dbl(weights, ~attr(., "niter")),
         weights = NULL) %>%
  ggplot() +
    facet_wrap(~weight_elimination, labeller = "label_both") +
    geom_point(aes(x1, x2, color = y), tibble(x1 = x[,1],
                                              x2 = x[,2],
                                              y = factor(y))) +
    geom_abline(aes(slope = slope, intercept = intercept)) +
    geom_text(aes(label = glue::glue("Error = {round(error,2)}")), x =-1.5, y= 2) +
    geom_text(aes(label = glue::glue("Avg Dist = {round(avg_dist_to_line,2)}")), x =-1.5, y= -1.5) +
    geom_text(aes(label = glue::glue("Iter = {niter}")), x =1.5, y= 1.5)



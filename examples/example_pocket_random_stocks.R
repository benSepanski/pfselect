library("tidyverse")
library("quantmod")
library("BatchGetSymbols")

# How many stocks from NYSE, when, and how frequent?
num_stocks <- 5
first_date <- Sys.Date() - 150
last_date <- Sys.Date()
freq_data <- 'daily'

# Decay Factor candidates
decay_factor <- 0.5

# LOAD Parameters
momentum_window_size <- 5
regularization_factors <- c(0, 0.01, 0.1, 1.0, 5.0)
momentum_thresholds <- c(0, 0.01, 0.1, 1.0)

# Pocket parameters
price_window_size <- 10
historic_mean_window_size <- 5
weight_elimination <- c(0, 0.01, 0.1, 1.0)
rebasing <- "eigen"  # or "eigen" or "whiten"
interactions <- TRUE  # Have interaction variables?

# quick check
assert_that(price_window_size >= momentum_window_size)

# Get relative matrix from prices
price_relative_matrix_from_prices <- function(price_matrix) {
  price_matrix %>%
    purrr::array_branch(1L) %>%
    {list(head(., -1), tail(., -1))} %>%
    purrr::pmap(~.x / .y) %>%
    purrr::flatten_dbl() %>%
    matrix(byrow = TRUE, ncol = ncol(price_matrix)) %>%
    `colnames<-`(colnames(price_matrix)) %>%
    `rownames<-`(rownames(price_matrix) %>% head(-1))
}

# Used to create the data to pass to the LOAD predictor
LOAD_data_creator <- function(price_matrix, decay_factor) {
  price_matrix %>%
    price_relative_matrix_from_prices() %>%
    aggregate_price_and_mean_windows(decay_factor = decay_factor,
                                     price_window_size = momentum_window_size,
                                     historic_mean_window_size = 1,
                                     initial_prices = price_matrix[1,]) %>%
    dplyr::mutate(momentum = evaluate_unsigned_momentum(.))
}

# Create the data to pass to the pocket momentum predictor
pocket_data_creator <- function(price_matrix, decay_factor) {
  price_relative_matrix <- price_matrix %>%
    price_relative_matrix_from_prices()

  # Make the data
  data <-  aggregate_price_and_mean_windows(
      price_relative_matrix,
      decay_factor = decay_factor,
      price_window_size = price_window_size,
      historic_mean_window_size = historic_mean_window_size,
      initial_prices = price_matrix[1,])
  # Rebase if necessary
  if(stringr::str_to_lower(rebasing) == "eigen") {
    eigen_decomps <- price_relative_matrix %>%
      compute_price_relatives_eigen() %>%
      map(~tail(., 1-max(price_window_size, historic_mean_window_size)))
    data <- data %>%
      rebase_agg_windows(new_assets = eigen_decomps$vectors,
                         asset_rownames = 1:num_stocks,
                         scalar_columns_to_rebase = c("price",
                                                      "historic_price_mean")
                         )
  }
  else if(stringr::str_to_lower(rebasing) == "whiten") {
    whiteners <- price_relative_matrix %>%
      compute_price_relatives_whitener() %>%
      tail(1-max(price_window_size, historic_mean_window_size))
    data <- data %>%
      rebase_agg_windows(new_assets = whiteners,
                         asset_rownames = 1:num_stocks,
                         scalar_columns_to_rebase = c("price",
                                                      "historic_price_mean")
                         )
  }
  # Flip sign so that momentum always on top
  data <- data %>%
    rescale_to_price_over_mean()

  # Compute momentum
  data$momentum <- data %>%
    dplyr::select(price, historic_price_mean, next_price) %>%
    purrr::pmap_int(evaluate_signed_momentum_at_window)

  # Make features: combine price window and historic price mean window,
  # throwing out oldest price after scaling it to 1
  data <- data %>%
    dplyr::mutate(feature = purrr::map2(price_window,
                                        historic_price_mean_window,
                                        ~c(.x[-1], .y) / .x[1]
                                        ))
  feature_length <- price_window_size + historic_mean_window_size - 1

  # add eigenvalues to feature
  if(rebasing == "eigen") {
    eigen_vals <- purrr::flatten_dbl(eigen_decomps$values)
    data <- data %>%
      dplyr::mutate(feature = purrr::map2(feature, eigen_vals, c))
    feature_length <- feature_length + 1
  }
  # Add interactions if requested, i.e. keep regular terms
  # and add xy for each variable x \neq y
  if(interactions) {
    distinct_pairs <- tibble::tibble(first = 1:(feature_length-1)) %>%
      dplyr::mutate(second = purrr::map(first, ~(.+1):feature_length)) %>%
      tidyr::unnest(second) %>%
      as.matrix(ncol = 2)
    get_interactions <- function(v) {
      c(v, v[distinct_pairs[, 1]] * v[distinct_pairs[,2]])
    }
    data <- data %>%
      dplyr::mutate(feature = purrr::map(feature, get_interactions))
  }
  # Return the data
  data
}

# Get NYSE stock symbols, but nb that some may not have data
nyse_symbols <- stockSymbols(exchange = "NYSE")
tickers <- nyse_symbols$Symbol %>%
  sample(num_stocks)
symbols <- BatchGetSymbols(tickers = tickers,
                           first.date = first_date,
                           last.date = last_date,
                           freq.data = freq_data,
                           )
# Check symbols
assert_that(are_equal(length(symbols$df.control$download.status), num_stocks))
assert_that(all(symbols$df.control$download.status == rep("OK", num_stocks)),
            msg = "Download Failure")

prices <- symbols$df.tickers %>%
  dplyr::select(ticker, price.close, ref.date) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = price.close) %>%
  dplyr::rename(trading_period = ref.date)

price_matrix <- prices %>%
  dplyr::select(-trading_period) %>%
  as.matrix() %>%
  `rownames<-`(prices %>% dplyr::pull(trading_period))

# Try LOAD
load_data <- LOAD_data_creator(price_matrix, decay_factor)

# Try Pocket
pocket_data <- pocket_data_creator(price_matrix, decay_factor)

pocket_data <- pocket_data %>%
  predict_momentum_pocket(feature_colname = "feature",
                          weight_elimination = weight_elimination,
                          maxit = 200)

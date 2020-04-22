library("quantmod")
library("BatchGetSymbols")

# How many stocks from NYSE, when, and how frequent?
num_stocks <- 5
first_date <- Sys.Date() - 60
last_date <- Sys.date()
freq_data <- 'daily'

# Decay Factor candidates
h <- 0.05
decay_factor <- seq(h, 1-h, by = h)
# make sure there are at least this many periods
num_periods_to_train_decay <- 10

# LOAD Parameters
momentum_window_size <- 5
regularization_factors <- c(0, 0.01, 0.1, 1.0, 5.0)
momentum_thresholds <- c(0, 0.01, 0.1, 1.0)

# Pocket parameters
assert_that(price_window_size >= momentum_window_size)
price_window_size <- 10
historic_mean_window_size <- 5
rebasing <- "identity"  # or "eigen" or "whiten"
interactions <- TRUE  # Have interaction variables?

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
    aggregate_price_and_mean_windows(price_relative_matrix,
                                     decay_factor = decay_factor,
                                     price_window_size = momentum_window_size,
                                     historic_mean_window_size = 1,
                                     initial_prices = price_matrix[1,]) %>%
    dplyr::mutate(momentum = evalute_unsigned_momentum(.))
}

# Create the data to pass to the pocket momentum predictor
pocket_data_creator <- function(price_matrix) {
  price_relative_matrix <- price_matrix %>%
    price_relative_matrix_from_prices()

  # Rebase if necessary
  if(stringr::str_to_lower(rebasing) == "eigen") {
    eigen_decomps <- compute_price_relatives_eigen(price_relative_matrix)
    price_relative_matrix <- price_relative_matrix %>%
      rebarebase_price_relative_matrix(eigen_decomps$vectors)
  }
  else if(stringr::str_to_lower(rebasing) == "whiten") {
    whiteners <- compute_price_relatives_whitener(price_relative_matrix)
    price_relative_matrix <- price_relative_matrix %>%
      rebarebase_price_relative_matrix(whiteners)
  }

  # Make the data
  data <-  aggregate_price_and_mean_windows(
      price_relative_matrix,
      decay_factor = decay_factor,
      price_window_size = price_window_size,
      historic_mean_window_size = historic_price_window_size,
      initial_prices = price_matrix[1,])
  # Compute momentum
  data$momentum <- data %>%
    dplyr::select(price, historic_price_mean, next_price) %>%
    purrr::pmap(evaluate_signed_momentum_at_window)

  # Make features
  data <- data %>%
    dplyr::mutate(feature = purrr::map2(price_window,
                                        historic_price_mean_window,
                                        c))
  feature_length <- price_window_size + historic_price_mean_window_size

  # add eigenvalues to feature
  if(rebasing == "eigen") {
    data <- data %>%
      dplyr::mutate(feature = purrr::pmap(list(feature,
                                               eigen_decomps$values,
                                               asset)
                                          ~c(..1, ..2[..3])))
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
prices <- symbols$df.tickers %>%
  dplyr::select(ticker, price.close, ref.date) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = price.close) %>%
  dplyr::rename(trading_period = ref.date)

price_matrix <- prices %>%
  dplyr::select(-trading_period) %>%
  as.matrix() %>%
  `rownames<-`(prices %>% dplyr::pull(trading_period))




library("tidyverse")
library("assertthat")
library("quantmod")
library("BatchGetSymbols")
library("pfselect")

get_new_stocks <- FALSE # Only gets stock if TRUE, otherwise loads price matrix
# How many stocks from NYSE, when, and how frequent?
num_stocks <- 50
first_date <- lubridate::mdy('01/01/2010')
last_date <- lubridate::mdy('01/01/2020')
freq_data <- 'weekly'

# LOAD parameters
recompute_LOAD_data <- FALSE # recompute LOAD_data?
LOAD_momentum_window_size <- 5
LOAD_decay_factor <- 0.5
LOAD_regularization_factor <- c(0, 0.01, 0.1, 1.0)
LOAD_momentum_threshold <- c(0, 0.01, 0.1, 1.0)

# pocket parameters
rebasing <- "eigen"  # "identity" "eigen" "whiten"
alpha <- 0.5
lambda <- c(0, 0.01, 0.05, 0.1, 1.0)
price_window_size <- c(3, 5, 10)
interactions <- FALSE  # Use interaction terms?
cv_maxit <- 500
training_maxit <- 5000

# Get NYSE stock symbols and data
if(get_new_stocks) {
  nyse_symbols <- stockSymbols(exchange = "NYSE")
  tickers <- nyse_symbols$Symbol %>%
    sample(2 * num_stocks)  # ad hoc + 10 bc some rejected
  batch_symbols <- BatchGetSymbols(tickers = tickers,
                             first.date = first_date,
                             last.date = last_date,
                             freq.data = freq_data,
                             thresh.bad.data = 0.9,
                             do.complete.data = TRUE,
                             do.fill.missing.prices = TRUE
                             )
  # Compute a price matrix and price relatives matrix
  prices <- batch_symbols$df.tickers %>%
    dplyr::ungroup() %>%
    dplyr::select(ticker, price.close, ref.date) %>%
    tidyr::drop_na() %>%
    tidyr::pivot_wider(names_from = ticker, values_from = price.close) %>%
    dplyr::rename(trading_period = ref.date)

  price_matrix <- prices %>%
    dplyr::select(-trading_period) %>%
    as.matrix() %>%
    `rownames<-`(prices %>% dplyr::pull(trading_period))
  # only want num_stocks many assets
  price_matrix <- price_matrix[, sample.int(ncol(price_matrix), num_stocks)]
} else {
  load("price_matrix.rds")
}


price_relative_matrix <- price_matrix %>%
    purrr::array_branch(1L) %>%
    {list(head(., -1), tail(., -1))} %>%
    purrr::pmap(~.x / .y) %>%
    purrr::flatten_dbl() %>%
    matrix(byrow = TRUE, ncol = ncol(price_matrix)) %>%
    `colnames<-`(colnames(price_matrix)) %>%
    `rownames<-`(rownames(price_matrix) %>% head(-1))


# LOAD prediction ---------------------------------------------------------


# Go ahead and test LOAD on the data
if(recompute_LOAD_data) {
  LOAD_data <- price_relative_matrix %>%
    aggregate_price_and_mean_windows(decay_factor = LOAD_decay_factor,
                                     price_window_size = LOAD_momentum_window_size,
                                     historic_mean_window_size = 1,
                                     initial_prices = price_matrix[1,]) %>%
    dplyr::mutate(momentum = evaluate_unsigned_momentum(.)) %>%
    predict_momentum_LOAD(regularization_factor = LOAD_regularization_factor,
                          momentum_threshold = LOAD_momentum_threshold)
  # Go ahead and get signed momentum too for comparison
  LOAD_data$signed_momentum <- LOAD_data %>%
    dplyr::select(price, historic_price_mean, next_price) %>%
    purrr::pmap_int(evaluate_signed_momentum_at_window)
  LOAD_data$price_prediction <- LOAD_data %>%
    evaluate_momentum_predict_price(LOAD_momentum_window_size,
                                    momentum_colname = "LOAD_prediction",
                                    use_signed_momentum = FALSE)
}


test_pocket_method <- function(rebasing, interaction) {
# CV Pocket ---------------------------------------------------------------

  # Now choose the pocket parameters
  best_err <- Inf
  cv_price_window_size <- NA
  cv_lambda <- NA
  cv_range <- 1:round(nrow(price_relative_matrix) / 5)
  for(w in price_window_size) {
    pocket_data <-  aggregate_price_and_mean_windows(
        price_relative_matrix[cv_range, ],
        decay_factor = alpha,
        price_window_size = w,
        historic_mean_window_size = 1,
        initial_prices = price_matrix[1,])
    # Rebase if necessary
    if(stringr::str_to_lower(rebasing) == "eigen") {
      eigen_decomps <- price_matrix[cv_range, ] %>%
        compute_price_relatives_eigen() %>%
        map(~tail(., 1 - w))
      pocket_data <- pocket_data %>%
        rebase_agg_windows(new_assets = eigen_decomps$vectors,
                           asset_rownames = 1:num_stocks,
                           scalar_columns_to_rebase = c("price",
                                                        "historic_price_mean")
                           )
    }
    else if(stringr::str_to_lower(rebasing) == "whiten") {
      whiteners <- price_matrix[cv_range, ] %>%
        compute_price_relatives_whitener() %>%
        tail(1 - w)
      pocket_data <- pocket_data %>%
        rebase_agg_windows(new_assets = whiteners,
                           asset_rownames = 1:num_stocks,
                           scalar_columns_to_rebase = c("price",
                                                        "historic_price_mean")
                           )
    }
    # Flip sign so that momentum always on top
    pocket_data <- pocket_data %>%
      rescale_to_price_over_mean()

    # Compute momentum
    pocket_data$momentum <- pocket_data %>%
      dplyr::select(price, historic_price_mean, next_price) %>%
      purrr::pmap_int(evaluate_signed_momentum_at_window)
    # Make features: combine price window and historic price mean window,
    # throwing out oldest price after scaling it to 1
    pocket_data <- pocket_data %>%
      dplyr::mutate(feature = purrr::map2(price_window,
                                          historic_price_mean_window,
                                          ~c(.x[-1], .y) / .x[1]
                                          ))
    feature_length <- price_window_size
    # add eigenvalues to feature if doing that
    if(rebasing == "eigen") {
      eigen_vals <- purrr::flatten_dbl(eigen_decomps$values)
      pocket_data <- pocket_data %>%
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
      pocket_data <- pocket_data %>%
        dplyr::mutate(feature = purrr::map(feature, get_interactions))
    }

    # Now predict
    pocket_data <- pocket_data %>%
      predict_momentum_pocket(feature_colname = "feature",
                              weight_elimination = lambda,
                              maxit = cv_maxit)
    cv_err <- pocket_data %>% pull(pocket_error) %>% mean(na.rm = TRUE)
    if(!is.na(cv_err) && cv_err < best_err) {
      best_err <- cv_err
      cv_price_window_size <- w
      cv_lambda <- pocket_data %>%
        count(pocket_weight_elimination) %>%
        top_n(n, 1) %>%
        pull(pocket_weight_elimination) %>%
        sample(1)
    }
  }


  # Train Pocket ------------------------------------------------------------



  # Now get first half of data for training
  train_range <- 1:round(nrow(price_relative_matrix) / 2)
  pocket_data <-  aggregate_price_and_mean_windows(
      price_relative_matrix[train_range, ],
      decay_factor = alpha,
      price_window_size = cv_price_window_size,
      historic_mean_window_size = 1,
      initial_prices = price_matrix[1,])
  # Rebase if necessary
  if(stringr::str_to_lower(rebasing) == "eigen") {
    all_eigen_decomps <- price_matrix %>%
      compute_price_relatives_eigen()
    eigen_decomps <- all_eigen_decomps %>%
      map(~.[train_range]) %>%
      map(~tail(., 1 - cv_price_window_size))
    pocket_data <- pocket_data %>%
      rebase_agg_windows(new_assets = eigen_decomps$vectors,
                         asset_rownames = 1:num_stocks,
                         scalar_columns_to_rebase = c("price",
                                                      "historic_price_mean")
                         )
  } else if(stringr::str_to_lower(rebasing) == "whiten") {
    all_whiteners <- price_matrix %>%
      compute_price_relatives_whitener()
    whiteners <- all_whiteners[train_range] %>%
      tail(1 - cv_price_window_size)
    pocket_data <- pocket_data %>%
      rebase_agg_windows(new_assets = whiteners,
                         asset_rownames = 1:num_stocks,
                         scalar_columns_to_rebase = c("price",
                                                      "historic_price_mean")
                         )
  }
  # Flip sign so that momentum always on top
  pocket_data <- pocket_data %>%
    rescale_to_price_over_mean()

  # Compute momentum
  pocket_data$momentum <- pocket_data %>%
    dplyr::select(price, historic_price_mean, next_price) %>%
    purrr::pmap_int(evaluate_signed_momentum_at_window)
  # Make features: combine price window and historic price mean window,
  # throwing out oldest price after scaling it to 1
  pocket_data <- pocket_data %>%
    dplyr::mutate(feature = purrr::map2(price_window,
                                        historic_price_mean_window,
                                        ~c(.x[-1], .y) / .x[1]
                                        ))
  feature_length <- price_window_size
  # add eigenvalues to feature if doing that
  if(rebasing == "eigen") {
    eigen_vals <- purrr::flatten_dbl(eigen_decomps$values)
    pocket_data <- pocket_data %>%
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
    pocket_data <- pocket_data %>%
      dplyr::mutate(feature = purrr::map(feature, get_interactions))
  }

  # Now train
  # spread out features
  spread_data <- pocket_data %>%
    dplyr::select(trading_period, asset, feature, momentum) %>%
    tidyr::unnest(feature) %>%
    dplyr::group_by(trading_period, asset) %>%
    dplyr::mutate(pocket_feature_index = as.character(glue::glue("pocket_feature_{1:n()}"))) %>%
    tidyr::pivot_wider(names_from = pocket_feature_index,
                       values_from = feature) %>%
    dplyr::ungroup(trading_period, asset) %>%
    dplyr::mutate(pm_one_momentum = sign(momentum * 2 - 1))
  y <- spread_data %>%
    dplyr::pull(pm_one_momentum)
  x <- spread_data %>%
    dplyr::select(tidyselect::matches("pocket_feature_\\d+")) %>%
    as.matrix()
  weights <- regularized_pocket(x, y, cv_lambda, training_maxit)


  # Pocket Data for Testing -------------------------------------------------


  # Next, get data for testing
  test_range <- (round(nrow(price_relative_matrix) / 2)+1):nrow(price_relative_matrix)

  pocket_data <-  aggregate_price_and_mean_windows(
      price_relative_matrix[test_range, ],
      decay_factor = alpha,
      price_window_size = cv_price_window_size,
      historic_mean_window_size = 1,
      initial_prices = price_matrix[test_range[1],])
  # Rebase if necessary
  if(stringr::str_to_lower(rebasing) == "eigen") {
    eigen_decomps <- all_eigen_decomps %>%
      map(~.[test_range]) %>%
      map(~tail(., 1 - cv_price_window_size))
    pocket_data <- pocket_data %>%
      rebase_agg_windows(new_assets = eigen_decomps$vectors,
                         asset_rownames = 1:num_stocks,
                         scalar_columns_to_rebase = c("price",
                                                      "historic_price_mean")
                         )
  } else if(stringr::str_to_lower(rebasing) == "whiten") {
    whiteners <- all_whiteners[test_range] %>%
      tail(1 - cv_price_window_size)
    pocket_data <- pocket_data %>%
      rebase_agg_windows(new_assets = whiteners,
                         asset_rownames = 1:num_stocks,
                         scalar_columns_to_rebase = c("price",
                                                      "historic_price_mean")
                         )
  }
  # Flip sign so that momentum always on top
  pocket_data <- pocket_data %>%
    rescale_to_price_over_mean()

  # Compute momentum
  pocket_data$momentum <- pocket_data %>%
    dplyr::select(price, historic_price_mean, next_price) %>%
    purrr::pmap_int(evaluate_signed_momentum_at_window)
  # Make features: combine price window and historic price mean window,
  # throwing out oldest price after scaling it to 1
  pocket_data <- pocket_data %>%
    dplyr::mutate(feature = purrr::map2(price_window,
                                        historic_price_mean_window,
                                        ~c(.x[-1], .y) / .x[1]
                                        ))
  feature_length <- price_window_size
  # add eigenvalues to feature if doing that
  if(rebasing == "eigen") {
    eigen_vals <- purrr::flatten_dbl(eigen_decomps$values)
    pocket_data <- pocket_data %>%
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
    pocket_data <- pocket_data %>%
      dplyr::mutate(feature = purrr::map(feature, get_interactions))
  }

  # Predict momentum then unflip windows
  pocket_data <- pocket_data %>%
    dplyr::mutate(
      pocket_prediction = map_dbl(feature,
                                  ~sign(weights[1] + .x %*% weights[-1])),
      pocket_prediction = 0.5 * (pocket_prediction + 1)
    )
  pocket_data <- pocket_data %>%
    dplyr::mutate(price_window = map2(price_window, flip_sign, `*`),
                  historic_price_mean_window = map2(historic_price_mean_window, flip_sign, `*`))

  # Now look at predicted prices
  pocket_data$price_prediction <- pocket_data %>%
    evaluate_momentum_predict_price(cv_price_window_size,
                                    momentum_colname = "pocket_prediction",
                                    use_signed_momentum = TRUE)

  if(rebasing == "eigen") {
    new_assets <- map(eigen_decomps$vectors, t)
  } else if(rebasing == "whiten") {
    # May have some un-invertible ones for the way we did whitening
    rebase_solver <- function(mat) {
      if(Matrix::rankMatrix(mat) < nrow(mat)) {
        return(matrix(NA, nrow = nrow(mat), ncol = ncol(mat)))
      }
      solve(mat)
    }
    new_assets <- map(whiteners, rebase_solver)
  }
  if(rebasing != 'identity') {
    unbased_pocket_data <- pocket_data %>%
      rebase_agg_windows(new_assets,
                         1:num_stocks,
                         c("price_prediction",
                           "price",
                           "historic_price_mean")
      )
    price_mean_rel_err <- mean(abs(unbased_pocket_data$price_prediction
                                   - unbased_pocket_data$price)
                               / unbased_pocket_data$price,
                               na.rm = TRUE)
  } else {
    price_mean_rel_err <- mean(abs(pocket_data$price_prediction
                                   - pocket_data$price)
                               / pocket_data$price,
                               na.rm = TRUE)

  }
  list(lambda = cv_lambda,
       alpha = alpha,
       price_window_size = cv_price_window_size,
       confusion = pocket_data %>% count(momentum, pocket_prediction),
       price_mean_relative_error = price_mean_rel_err
       )
}

tb <- tibble(rebasing = c("identity", "eigen", "whiten",
                          "identity", "eigen", "whiten"),
             interaction = c(TRUE, TRUE, TRUE,
                             FALSE, FALSE, FALSE)
             ) %>%
  mutate(output = pmap(., test_pocket_method))

save(tb, file = "results.rds")

#' Link to investing.com's historical data for a given MSCI country
#'
#' Given a country name, returns a string which we expect to
#' be the url at investing.com for the historical data
#' of that msci index.
#'
#' @param country_name The country name, e.g. \code{"New Zealand"}
#' @return A string with the appropriate url
#'
#' @examples
#' msci_link("Australia")
#' msci_link("New Zealand")
#' msci_link("Austria")
#'
#' @importFrom magrittr %>%
#'
msci_link <- function(country_name) {
  country_name %>%
    stringr::str_to_lower() %>%
    stringr::str_trim() %>%  # just in case
    stringr::str_replace_all(" ", "-") %>%  # not we pipe to 2nd arg
    stringr::str_c("https://www.investing.com/indices/msci-",
                   ., "-historical-data")
}

#' Get investing data at the url from the given dates
#'
#' Given a url for historical data of a stock at investing.com,
#' extract a tibble with the stock data from \code{start_date}
#' to \code{end_date}
#'
#' @param url The url to the historical data at investing.com
#' @param st_date the start date as a character in MM/DD/YYYY format
#' @param end_date the end date as a character in MM/DD/YYYY format
#' @param message If \code{TRUE}, messages the user with the results of the http query for the data. Defaults to \code{FALSE}.
#'
#' @examples
#' "New Zealand" %>%
#'   msci_link() %>%
#'   get_investing_data('12/31/2019', '03/08/2020')
#' "https://www.investing.com/equities/american-airlines-group-historical-data" %>%
#'    get_investing_data('12/31/2019', '03/08/2020')
#'  "https://www.investing.com/equities/bank-of-america-historical-data" %>%
#'    get_investing_data('12/31/2019', '03/08/2020')
#'
#' @importFrom magrittr %>%
get_investing_data <- function(url, st_date, end_date, message = FALSE) {
  # first we get the curr_id and smlID
  # from the website, which will be ids[1] and ids[2]
  html <- rvest::read_html(url)
  ids <- html %>%
    rvest::html_nodes("script") %>%
    rvest::html_text() %>%
    stringr::str_subset("histDataExcessInfo") %>%
    stringr::str_extract_all("\\d+") %>%
    purrr::pluck(1L)

  # Next we get the submission for the Ajax client
  ajax_country_name <- html %>%
    rvest::html_nodes("h2") %>%
    rvest::html_text() %>%
    stringr::str_subset(".+ Historical Data") %>%
    stringr::str_extract(".*[^( Historical Data)]")

  # request data from the given data range
  url_header <- list(
    "X-Requested-With" = "XMLHttpRequest",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.132 Safari/537.36"
  )
  payload <- list(
    curr_id = ids[1],
    smlID = ids[2],
    header = ajax_country_name,
    st_date = st_date,
    end_date = end_date,
    interval_sec = "Daily",
    sort_col = "date",
    sort_ord = "DESC",
    action = "historical_data"
  )

  ajaxUrl = "https://www.investing.com/instruments/HistoricalDataAjax"
  posted <- httr::POST(url = ajaxUrl, body = payload, encode = "form",
                       purrr::lift(add_headers)(url_header))
  if(message){
    message(httr::http_status(posted))
  }
  posted %>%
    httr::content() %>%
    rvest::html_nodes("table") %>%
    rvest::html_table() %>%
    purrr::pluck(1L) %>%
    dplyr::as.tbl() %>%
    dplyr::mutate(Date = lubridate::mdy(Date))
}

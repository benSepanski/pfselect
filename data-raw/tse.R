library(tidyverse)
library(rvest)
# Get the data file and unzip it if not already present
if (!file.exists("data-raw/tse")) {
  tmp <- tempfile(fileext = ".zip")
  download.file("http://www.cs.technion.ac.il/~rani/portfolios/TSE.zip", tmp)
  unzip(tmp, exdir = "data-raw/tse")
  # Get the metadata
  download.file("http://www.cs.technion.ac.il/~rani/portfolios/TSE_Dataset.htm",
                "data-raw/tse/tse.htm")
}

stock_names <- read_html("data-raw/tse/tse.htm") %>%
  html_nodes("p") %>%
  html_text() %>%
  stringr::str_subset("\\d\\d?\\..*") %>%
  stringr::str_extract("[A-Z].*") %>%
  stringr::str_replace_all("\\s+", " ") %>%
  stringr::str_trim()

tse <- "data-raw/tse/TSE.txt" %>%
  read.csv(header = FALSE, sep = "") %>%
  as.matrix()
colnames(tse) <- stock_names

usethis::use_data(tse)

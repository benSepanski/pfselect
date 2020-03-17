library(tidyverse)
library(rvest)

# Get the data file and unzip it if not already present
if (!file.exists("data-raw/sp500")) {
  tmp <- tempfile(fileext = ".zip")
  download.file("http://www.cs.technion.ac.il/~rani/portfolios/SP500.zip", tmp)
  unzip(tmp, exdir = "data-raw/sp500")
  # Get the metadata
  download.file("http://www.cs.technion.ac.il/~rani/portfolios/SP500_Dataset.htm",
                "data-raw/sp500/sp500.htm")
}

stock_names <- read_html("data-raw/sp500/sp500.htm") %>%
  html_nodes("li") %>%
  html_text() %>%
  stringr::str_replace_all("\\s+", " ") %>%
  stringr::str_remove("\\(.*\\)") %>%
  stringr::str_trim()

sp500 <- "data-raw/sp500/SP500.txt" %>%
  read.csv(header = FALSE, sep = "") %>%
  as.matrix()
colnames(sp500) <- stock_names

usethis::use_data(sp500)

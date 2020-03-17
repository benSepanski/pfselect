library(tidyverse)
library(rvest)

# Get the data file and unzip it if not already present
if (!file.exists("data-raw/nyse_o")) {
  tmp <- tempfile(fileext = ".zip")
  download.file("http://www.cs.technion.ac.il/~rani/portfolios/NYSE.zip", tmp)
  unzip(tmp, exdir = "data-raw/nyse_o")
  # Get the metadata
  download.file("http://www.cs.technion.ac.il/~rani/portfolios/NYSE_Dataset.htm",
                "data-raw/nyse_o/NYSE_dataset.htm")
}

sotkc_names <- read_html("data-raw/nyse_o/NYSE_dataset.htm") %>%
  html_nodes("li") %>%
  html_text()

nyse_o <- "data-raw/nyse_o/NYSE.txt" %>%
  read.csv(header = FALSE, sep = "") %>%
  as.matrix()
colnames(nyse_o) <- stock_names

usethis::use_data(nyse_o)

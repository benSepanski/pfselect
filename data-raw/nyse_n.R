library(tidyverse)
library(R.matlab)

if (!file.exists("data-raw/nyse_n")) {
  dir.create("data-raw/nyse_n")
  download.file("https://raw.githubusercontent.com/OLPS/OLPS/master/Data/nyse-n.mat",
                "data-raw/nyse_n/nyse_n.mat", mode = "wb")
  stop(glue::glue("data-raw/nyse_n/nyse_n.mat must be converted to v6, \\
see the following link: \\
https://www.mathworks.com/help/matlab/import_export/mat-file-versions.html"))
}

nyse_n <- readMat("data-raw/nyse_n/nyse_n.mat")$data
usethis::use_data(nyse_n)

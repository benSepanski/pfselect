library(tidyverse)
library(R.matlab)

if (!file.exists("data-raw/msci")) {
  dir.create("data-raw/msci")
  download.file("https://raw.githubusercontent.com/OLPS/OLPS/master/Data/msci.mat",
                "data-raw/msci/msci.mat", mode = "wb")
}

msci <- readMat("data-raw/msci/msci.mat")$data
usethis::use_data(msci)

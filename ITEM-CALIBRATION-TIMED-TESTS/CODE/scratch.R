suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(lavaan))

input_scored <-
  suppressMessages(read_csv(
    here(
      "ITEM-CALIBRATION-TIMED-TESTS/INPUT-FILES/TODS_QRF-scored.csv"
    )
  ))

input_unscored <-
  suppressMessages(read_csv(
    here(
      "ITEM-CALIBRATION-TIMED-TESTS/INPUT-FILES/TODS6.24.21_QRF-unscored.csv"
    )
  ))


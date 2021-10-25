library(cNORM)
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(here))
library(writexl)
suppressMessages(library(lubridate))

combined_score_to_norm_file_name <- "TODE_8.27.21_fornorms-weighted-sum-scores.csv"
input_file_path <- "INPUT-FILES/NORMS/TODE_8.27.21_fornorms/"
output_file_path <- "OUTPUT-FILES/NORMS/TODE_8.27.21_fornorms/"

# tokens for score names
scores <- c("sege_sum", "rlne_sum", "rhme_sum", "snwe_sum",
            "lswe_sum", "lske_sum", "ORF_noNeg")

# Tokens setting the specific score to be normed on this iteration of the
# script.
score_to_norm_stem <- "sege_sum"
score_to_norm_file_name <- str_c(score_to_norm_stem, "-norms-input.csv")
score_to_norm_max_raw <- data.frame(test = score_to_norm_stem) %>%
  mutate(
    max_raw = case_when(
      str_detect(test, "sege") ~ 25,
      str_detect(test, "rlne") ~ 120,
      str_detect(test, "rhme") ~ 30,
      str_detect(test, "snwe") ~ 32,
      str_detect(test, "lswe") ~ 38,
      str_detect(test, "lske") ~ 33,
      str_detect(test, "ORF_noNeg") ~ 263
    )
  ) %>%
  pull(max_raw)

input <- suppressMessages(read_csv(here(str_c(
  input_file_path, score_to_norm_file_name
)))) %>%
  mutate(raw_prop = raw/25)

score_to_norm_stem_w <- "sege_sum_w"
score_to_norm_file_name_w <- str_c(score_to_norm_stem_w, "-norms-input.csv")

input_w <- suppressMessages(read_csv(here(str_c(
  input_file_path, score_to_norm_file_name_w
  )))) %>%
  mutate(raw_prop = raw/34)



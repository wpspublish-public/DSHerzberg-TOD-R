# Prep data for BLIMP INPUT

suppressMessages(library(here))
suppressMessages(library(tidyverse))

file_name <- "TODC2.23.21for BLIMP"
folder_name <- "TOD-C-2021-03-12"

input_orig <- suppressMessages(read_csv(here(
  str_c("MISSING-DATA-BLIMP/", folder_name, "/", file_name, ".csv")
))) %>% 
  rename(id = ID)

# get report of file size and missing pct.
cell_count <- nrow(input_orig) * ncol(input_orig)
na_count <- sum(is.na(input_orig))
na_pct <- round((na_count/cell_count) * 100, 2)

na_summ <- tribble(
  ~file_name, ~rows, ~cols, ~na, ~cells, ~na_pct,
  file_name, nrow(input_orig), ncol(input_orig), na_count, cell_count, na_pct
)

# get problematic columns, rows
all_0orNA_cols <- input_orig %>% keep(~all(is.na(.x) | .x == 0)) %>% names
all_1orNA_cols <- input_orig %>% keep(~all(is.na(.x) | .x == 1)) %>% names
all_0orNA_rows <- input_orig %>% filter(if_all(names(input_orig)[-1], ~(is.na(.) | . == 0)))


input_orig[is.na(input_orig)] <- 999

input_tall <- input_orig %>%
  pivot_longer(cols = -id,
               names_to = "item",
               values_to = "response") %>%
  mutate(across(item, ~ str_sub(., 2, 4)))

write_csv(input_tall,
          here(paste0("MISSING-DATA-BLIMP/", folder_name, "/", file_name, "-BLIMP-input.csv")),
          col_names = F
)


# reformat imputed data set for downstream analysis
blimp_output <- suppressMessages(
  read_csv(
    (here(str_c("MISSING-DATA-BLIMP/", folder_name, "/TOD-S1.csv"))), col_names = F)) %>% 
  setNames(c("ID", "item", "response")) %>% 
  pivot_wider(names_from = item,
              values_from = response) %>%
  setNames(names(input_orig))

NA_count <- sum(blimp_output == 999)
NA_count

write_csv(blimp_output, here(
  str_c(
    "MISSING-DATA-BLIMP/", folder_name, "/",
    file_name,
    "-noMiss-",
    format(Sys.Date(), "%Y-%m-%d"),
    ".csv"
  )
),
na = ""
)


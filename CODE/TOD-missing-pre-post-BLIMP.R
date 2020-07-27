# Prep data for BLIMP INPUT

suppressMessages(library(here))
suppressMessages(library(tidyverse))

file_name <- c("TOD.DATA.3.5.20_forBLIMP7.20.20rws_ssl")

input_orig <- suppressMessages(read_csv(here(
  paste0("INPUT-FILES/", file_name, ".csv")
))) 

# get problematic columns, rows
all_0orNA_cols <- input_orig %>% keep(~all(is.na(.x) | .x == 0)) %>% names
all_1orNA_cols <- input_orig %>% keep(~all(is.na(.x) | .x == 1)) %>% names
all_0orNA_rows <- input_orig %>% filter(across(names(input_orig)[-1], ~(is.na(.) | . == 0)))

NA_count <- sum(is.na(input_orig))
NA_count

input_orig[is.na(input_orig)] <- 999

input_tall <- input_orig %>%
  pivot_longer(cols = -ID,
               names_to = "item",
               values_to = "response") %>%
  mutate(across(item, ~ str_sub(., 2, 4)))

write_csv(input_tall,
          here(paste0("MISSING-DATA-BLIMP/", file_name, "-BLIMP-input.csv")),
          col_names = F
)


# reformat imputed data set for downstream analysis
blimp_output <- suppressMessages(
  read_csv(
    (here("MISSING-DATA-BLIMP/TOD-impute-2020-07-26-1.csv")), col_names = F)) %>% 
  setNames(c("ID", "item", "response")) %>% 
  pivot_wider(names_from = item,
              values_from = response) %>%
  setNames(names(input_orig))

NA_count <- sum(blimp_output == 999)
NA_count

write_csv(blimp_output, here(
  str_c(
    "MISSING-DATA-BLIMP/",
    file_name,
    "-noMiss-",
    format(Sys.Date(), "%Y-%m-%d"),
    ".csv"
  )
),
na = ""
)


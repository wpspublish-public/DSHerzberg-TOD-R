# Prep data for BLIMP INPUT

suppressMessages(library(here))
suppressMessages(library(tidyverse))

file_name <- c("TOD-E.DATA.3.5.20_forBLIMP6.18.20")

input_orig <- suppressMessages(read_csv(here(
  paste0("INPUT-FILES/", file_name, ".csv")
))) 

# get problematic columns, rows
all_0orNA_cols <- input_orig %>% purrr::keep(~all(is.na(.x) | .x == 0)) %>% names
all_1orNA_cols <- input_orig %>% purrr::keep(~all(is.na(.x) | .x == 1)) %>% names
all_0orNA_rows <- input_orig %>% filter_at(names(input_orig)[-1], ~(is.na(.) | . == 0))

NA_count <- sum(is.na(input_orig))
NA_count

input_orig[is.na(input_orig)] <- 999

input_gathered <- input_orig %>%
  gather("item","response",-ID) %>% 
  group_by(!!sym(names(input_orig)[1])) %>% 
  arrange(!!sym(names(input_orig)[1])) %>% 
  mutate(item = as.factor(str_sub(item, 2, 4)))

write_csv(input_gathered,
          here(paste0("MISSING-DATA-BLIMP/", file_name, "-BLIMP-input.csv")),
          col_names = F
)


# reformat imputed data set for downstream analysis
temp1 <- suppressMessages(
  read_csv(
    (here("MISSING-DATA-BLIMP/TOD-impute-2020-06-23-1.csv")), col_names = F))
names(temp1) <- c("ID", "item", "response")
temp2 <- temp1 %>% 
  spread(item, response) 
names(temp2) <- names(input_orig)

NA_count <- sum(temp2 == 999)
NA_count

write_csv(temp2, here(
  str_c(
    "MISSING-DATA-BLIMP/",
    file_name,
    "-noMiss-",
    format(Sys.Date(), "%Y-%m-%d"),
    ".csv"
  )
))

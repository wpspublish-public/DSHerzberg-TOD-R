suppressMessages(library(here))
suppressMessages(library(tidyverse))

file_name <- c("TODC2.23.21for BLIMPpart2")

input_orig <- suppressMessages(read_csv(here(
  str_c(
  "INPUT-FILES/", file_name, ".csv"
  )
))) 

blimp_output <- suppressMessages(
  read_csv(
    (here("MISSING-DATA-BLIMP/TOD-C-2021-03-12/TOD-C-part2-impute.csv")), col_names = F)) %>% 
  setNames(c("ID", "item", "response")) %>% 
  pivot_wider(names_from = item,
              values_from = response) %>%
  setNames(names(input_orig))

col_subsets <- c("i001:i050", "i051:i070", "i071:i095", "i096:i145", 
                 "i146:i166", "i167:i209", "i210:i251")

miss_recode <- col_subsets %>%
  map_df(
    ~
      input_orig %>%
      filter(across(!!rlang::parse_expr(.x),
                    ~ is.na(.))) %>%
      mutate(recode_cols1 = .x) %>%
      select(ID, recode_cols1)
  ) %>%
  arrange(ID) %>%
  mutate(
    streak = runner::streak_run(ID),
    recode_cols2 = case_when(lead(streak) == 2 ~ lead(recode_cols1),
                             T ~ NA_character_),
    recode_cols3 = case_when(lead(streak, 2) == 3 ~ lead(recode_cols1, 2),
                             T ~ NA_character_)
  ) %>% 
  filter(streak == 1) %>% 
  select(-streak)

blimp_recode <- blimp_output %>%
  left_join(miss_recode, by = "ID") %>%
  relocate(c(recode_cols1, recode_cols2, recode_cols3), .after = "ID") %>%
  pivot_longer(cols = c(-ID, -recode_cols1, -recode_cols2, -recode_cols3),
               names_to = c("item")) %>%
  extract(
    recode_cols1,
    into = c("start1", "end1"),
    "([[:alnum:]]{4})?\\:?(.*)",
    remove = F
  ) %>%
  extract(
    recode_cols2,
    into = c("start2", "end2"),
    "([[:alnum:]]{4})?\\:?(.*)",
    remove = F
  ) %>%
  extract(
    recode_cols3,
    into = c("start3", "end3"),
    "([[:alnum:]]{4})?\\:?(.*)",
    remove = F
  ) %>%
  group_by(ID) %>%
  mutate(
    recode_run =
      case_when(
        start1 == item ~ "recode1",
        end1 == item ~ "recode1",
        start2 == item ~ "recode2",
        end2 == item ~ "recode2",
        start3 == item ~ "recode3",
        end3 == item ~ "recode3",
        T ~ NA_character_
      ),
    across(c(recode_run),
           ~ runner::fill_run(., only_within = T)),
    across(c(value),
           ~ case_when(
             recode_run %in% c("recode1", "recode2", "recode3") ~ NA_real_,
             T ~ value
           ))
  ) %>%
  select(ID, item, value) %>%
  pivot_wider(
    names_from = item,
    values_from = value) %>%
  ungroup()

write_csv(blimp_recode, here(
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


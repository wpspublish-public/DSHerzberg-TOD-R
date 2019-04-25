# read .csv, strip (filter) first row, select needed columns by position
demos_input <-
  suppressMessages(read_csv(here(
    'DATA/TOD_demos_input_AMK_2019-04-21.csv'
  ))) %>% filter(!is.na(.[1])) %>% select(
    10,
    11,
    12,
    14,
    15,
    16,
    17,
    18,
    19
  )
# strip all attributes and garbage from input table, transforms it into a list of char vecs
attributes(demos_input) <- NULL
# name vecs within list
names(demos_input) <- c("IDnum", "form", "zip", "ageyear", "grade", "gender", "PEL", "hispanic", "ethnic")
# transform list of named vecs into dataframe with list column (enframe),
# transform into 2-col df with names, values cols (unnest), collapse into needed
# output table with one column per variable (unstack), reorder and reformat vars
TOD_demos <- demos_input %>% enframe() %>% unnest() %>% unstack(value ~ name) %>% 
  select(IDnum, form, zip, ageyear, grade, gender, PEL, hispanic, ethnic) %>% 
  mutate_at(vars(IDnum, zip, ageyear), funs(as.integer)) %>% 



library(tidyverse)
input <- tribble(
  ~name, ~value,
  "animal", "pig",
  "animal", "dog",
  "animal", "cat",
  "plant", "tree",
  "plant", "bush",
  "plant", "flower"
)

output <- tribble(
  ~animal, ~plant,
  "pig", "tree",
  "dog", "bush",
  "cat", "flower"
)

df3 <- unstack(input, value ~ name)


# columns: 10 (IDnum), 11 (form), 12 (zip), 14 (ageyear), 15 (grade), 16 (gender), 17 (PEL), 18 (hispanic), 19 (ethnic)

  # mutate(
  #   agestrat = case_when(
  #     ageyear == 5 ~ "05",
  #     ageyear == 6 ~ "06",
  #     ageyear == 7 ~ "07",
  #     ageyear == 8 ~ "08",
  #     ageyear == 9 ~ "09",
  #     ageyear >= 10 & ageyear <= 24 ~ as.character(ageyear),
  #     ageyear >= 25 & ageyear <= 40 ~ "2540",
  #     ageyear >= 41 & ageyear <= 50 ~ "4150",
  #     ageyear >= 51 & ageyear <= 60 ~ "5160",
  #     ageyear >= 61 & ageyear <= 70 ~ "6170",
  #     ageyear >= 71 & ageyear <= 80 ~ "7180",
  #     ageyear >= 81 & ageyear <= 90 ~ "8190",
  #     TRUE ~ NA_character_
  #   ),
  #   ethnic1 = case_when(hispanic == "Yes" ~ "Hispanic",
  #                       TRUE ~ ethnic),
  #   region = case_when(
  #     inrange(zip, 1000, 8999) | inrange(zip, 10000, 19699) ~ "Northeast",
  #     inrange(zip, 900, 999) |
  #       inrange(
  #         zip,
  #         19700,
  #         33999) |
  #           inrange(
  #             zip,
  #             34100,
  #             42799) |
  #               inrange(zip, 70000, 79999) | inrange(zip, 88500, 88599) ~ "South",
  #     inrange(zip, 43000, 58999) | inrange(zip, 60000, 69999) ~ "Midwest",
  #     inrange(zip, 59000, 59999) |
  #       inrange(
  #         zip,
  #         80000,
  #         88499) |
  #       inrange(
  #         zip,
  #         88900,
  #         96199) |
  #       inrange(zip, 96700, 96899) | inrange(zip, 97000, 99999) ~ "West",
  #     TRUE ~ NA_character_
  #       )) %>% arrange(agestrat) %>% group_by(agestrat)
  #     
  # 
  # 
  # 
  # 

suppressMessages(library(here))
suppressMessages(library(tidyverse))
library(writexl)

# PART I: SET UP TOKENS AND INPUT FILES

input_test_names <- c("PV-S", "LW-S", "QRF-S", "WRF-S")
output_test_names <- c("PV-S", "LW-S", "QRF-S", "WRF-S")
tod_form <- "TOD-S"
norm_type <- "age"
input_file_path <- "PRINT-FORMAT-NORMS-TABLES/POST-cNORM-HAND-SMOOTHED-TABLES/TOD-S/CHILD-AGE/"
output_file_path <- "PRINT-FORMAT-NORMS-TABLES/OUTPUT-FILES/TOD-S/CHILD-AGE/"

# read the input files (test>>age) into a list.
input_files_ta <- map(
  input_test_names,
  ~
  suppressMessages(read_csv(here(str_c(
  input_file_path, .x, "-", norm_type, ".csv"
))))
) %>% 
  set_names(input_test_names)

# read in static columns with all possible SS mapped onto associated percentiles
perc_ss_cols <- suppressMessages(read_csv(here(
  "PRINT-FORMAT-NORMS-TABLES/perc-ss-cols.csv"
)))

# read in a char vector of age-strats (or grade levels)
age_strat <- input_files_ta[[1]] %>% 
  select(-raw) %>% 
  names()

age_strat_qrf <- input_files_ta[[3]] %>% 
  select(-raw) %>% 
  names()

age_strat_wrf <- input_files_ta[[4]] %>% 
  select(-raw) %>% 
  names()

# Impose print-style raw score formatting on input tables
print_lookups_ta <- input_files_ta %>%
  map(~
        .x %>% 
  pivot_longer(contains("-"), names_to = "age_strat", values_to = "ss") %>%
  arrange(match(age_strat, naturalsort(age_strat))) %>%  
  group_by(age_strat) %>%
  complete(ss = 40:130) %>%
  group_by(age_strat, ss) %>%
  filter(n() == 1 | n() > 1 & row_number()  %in% c(1, n())) %>%
  summarize(raw = str_c(raw, collapse = '--')) %>%
  mutate(across(raw, ~ case_when(is.na(.x) ~ '-', TRUE ~ .x))) %>%
  arrange(match(age_strat, naturalsort(age_strat)), desc(ss)) %>%
  pivot_wider(names_from = age_strat,
              values_from = raw) %>%
  filter(!is.na(ss)) %>%
  right_join(perc_ss_cols, by = "ss") %>%
  relocate(perc, .before = "ss")
) %>% 
  set_names(input_test_names)

# decompose list of input tables into "list of lists", with bottom level elements
# being three-col lookup tables, one for each age-strat

age_strat_cols_ta_pv_ls <-  print_lookups_ta[1:2] %>%
  map( ~
         map(age_strat,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(age_strat))

age_strat_cols_ta_qrf <-  print_lookups_ta[3] %>%
  map( ~
         map(age_strat_qrf,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(age_strat_qrf))

age_strat_cols_ta_wrf <-  print_lookups_ta[4] %>%
  map( ~
         map(age_strat_wrf,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(age_strat_wrf))

age_strat_cols_ta <- c(age_strat_cols_ta_pv_ls, age_strat_cols_ta_qrf, age_strat_cols_ta_wrf)

# create vec of names of all crossings of age_strat and test names.
age_test_names_flat <- cross2(age_strat, input_test_names) %>% 
  map_chr(str_c, collapse = "_")

# flatten age_strat_dfs so that it is a one-level list holding all single
# age-strat lookups spread over all tests. rename the list elements with
# age_test_names so that each element (df) of the list can be identified.
age_test_cols_flat <- flatten(age_strat_cols_ta) %>% 
  set_names(age_test_names_flat)

# Reconstitute age_test_cols_flat into a list of lists in which the top level is
# a list for each age_strat, and within those lists are the list of lookup
# tables for that age_strat, for all tests.
age_test_cols_at <- map(
  age_strat,
  ~
  keep(age_test_cols_flat, str_detect(names(age_test_cols_flat), .x))
)

# print_lookups_at is a transformation of age_test_cols_at into a list suitable
# for writing out as a tabbed .xlsx file.
print_lookups_at <- age_test_cols_at %>% 
  map(
    ~
      .x %>% 
      reduce(left_join, by = c("perc", "ss")) %>% 
      rename_with(~ output_test_names, contains("-"))
  ) %>% 
  set_names(age_strat)

# Write raw-to-ss lookups by agestrat into tabbed, xlsx workbook. To create
# named tabs, supply writexl::write_xlsx() with a named list of dfs for each
# tab, tab names will be the names of the list elements
write_xlsx(print_lookups_at,
           here(
             str_c(
               output_file_path, tod_form, "-print-lookup-tables-", norm_type, ".xlsx"
             ))
           )





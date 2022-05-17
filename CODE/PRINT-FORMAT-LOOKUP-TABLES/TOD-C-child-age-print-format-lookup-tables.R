suppressMessages(library(here))
suppressMessages(library(tidyverse))
library(naturalsort)
library(writexl)

# PART I: SET UP TOKENS AND INPUT FILES

input_test_names <- c("PHM-C", "IWS-C", "RLN-C", "PWR-C", "WPC-C", "WM-C", 
                      "PAN-C", "IWR-C", "BLN-C", "SEG-C", "RWS-C", "SRE1-C", 
                      "SRE2-C", "RNL-C", "LM-C", "RPW-C", "RIW-C", "SSL-C", 
                      "LV-C", "GAN-C")
input_test_names_start <- c("PHM-C", "IWS-C", "RLN-C", "PWR-C", "WPC-C", "WM-C", 
                      "PAN-C", "IWR-C", "BLN-C", "SEG-C", "RWS-C")
input_test_names_end <- c("RNL-C", "LM-C", "RPW-C", "RIW-C", "SSL-C", 
                      "LV-C", "GAN-C")
output_test_names_both <- c("PHM-C", "IWS-C", "RLN-C", "PWR-C", "WPC-C", "WM-C", 
                      "PAN-C", "IWR-C", "BLN-C", "SEG-C", "RWS-C", "SRE1-C", 
                      "SRE2-C", "RNL-C", "LM-C", "RPW-C", "RIW-C", "SSL-C", 
                      "LV-C", "GAN-C")
output_test_names_sre1 <- c("PHM-C", "IWS-C", "RLN-C", "PWR-C", "WPC-C", "WM-C", 
                      "PAN-C", "IWR-C", "BLN-C", "SEG-C", "RWS-C", "SRE1-C", 
                      "RNL-C", "LM-C", "RPW-C", "RIW-C", "SSL-C", 
                      "LV-C", "GAN-C")
output_test_names_sre2 <- c("PHM-C", "IWS-C", "RLN-C", "PWR-C", "WPC-C", "WM-C", 
                            "PAN-C", "IWR-C", "BLN-C", "SEG-C", "RWS-C", "SRE2-C", 
                            "RNL-C", "LM-C", "RPW-C", "RIW-C", "SSL-C", 
                            "LV-C", "GAN-C")
tod_form <- "TOD-C"
norm_type <- "age"
input_file_path <- "PRINT-FORMAT-NORMS-TABLES/POST-cNORM-HAND-SMOOTHED-TABLES/TOD-C/CHILD-AGE/"
output_file_path <- "PRINT-FORMAT-NORMS-TABLES/OUTPUT-FILES/TOD-C/CHILD-AGE/"

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

age_strat_sre1 <- input_files_ta[[12]] %>% 
  select(-raw) %>% 
  names()

age_strat_sre2 <- input_files_ta[[13]] %>% 
  select(-raw) %>% 
  names()

age_strat_both <- intersect(age_strat_sre1, age_strat_sre2)

age_strat_start <- setdiff(age_strat_sre1, age_strat_both)

age_strat_end <- setdiff(age_strat_sre2, age_strat_both)

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

age_strat_cols_ta_start <-  print_lookups_ta[1:11] %>%
  map( ~
         map(age_strat,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(age_strat))

age_strat_cols_ta_sre1 <-  print_lookups_ta[12] %>%
  map( ~
         map(age_strat_sre1,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(age_strat_sre1))

age_strat_cols_ta_sre2 <-  print_lookups_ta[13] %>%
  map( ~
         map(age_strat_sre2,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(age_strat_sre2))

age_strat_cols_ta_end <-  print_lookups_ta[14:20] %>%
  map( ~
         map(age_strat,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(age_strat))

age_strat_cols_ta <- c(age_strat_cols_ta_start, age_strat_cols_ta_sre1, age_strat_cols_ta_sre2, age_strat_cols_ta_end)

# create vec of names of all crossings of age_strat and test names.
age_test_names_flat_start <- cross2(age_strat, input_test_names_start) %>% 
  map_chr(str_c, collapse = "_")

age_test_names_flat_sre1 <- cross2(age_strat_sre1, "SRE1-C") %>% 
  map_chr(str_c, collapse = "_")

age_test_names_flat_sre2 <- cross2(age_strat_sre2, "SRE2-C") %>% 
  map_chr(str_c, collapse = "_")

age_test_names_flat_end <- cross2(age_strat, input_test_names_end) %>% 
  map_chr(str_c, collapse = "_")

age_test_names_flat <- c(age_test_names_flat_start, age_test_names_flat_sre1, 
                         age_test_names_flat_sre2, age_test_names_flat_end)

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
print_lookups_at_start <- age_test_cols_at[1:10] %>% 
  map(
    ~
      .x %>% 
      reduce(left_join, by = c("perc", "ss")) %>%
      rename_with(~ output_test_names_sre1, contains("-"))
  ) %>% 
  set_names(age_strat_start)


print_lookups_at_both <- age_test_cols_at[11:16] %>% 
  map(
    ~
      .x %>% 
      reduce(left_join, by = c("perc", "ss")) %>%
      rename_with(~ output_test_names_both, contains("-"))
  ) %>% 
  set_names(age_strat_both)

print_lookups_at_end <- age_test_cols_at[17:20] %>% 
  map(
    ~
      .x %>% 
      reduce(left_join, by = c("perc", "ss")) %>%
      rename_with(~ output_test_names_sre2, contains("-"))
  ) %>% 
  set_names(age_strat_end)

print_lookups_at <- c(print_lookups_at_start, print_lookups_at_both, print_lookups_at_end)

# Write raw-to-ss lookups by agestrat into tabbed, xlsx workbook. To create
# named tabs, supply writexl::write_xlsx() with a named list of dfs for each
# tab, tab names will be the names of the list elements
write_xlsx(print_lookups_at,
           here(
             str_c(
               output_file_path, tod_form, "-print-lookup-tables-", norm_type, ".xlsx"
             ))
           )





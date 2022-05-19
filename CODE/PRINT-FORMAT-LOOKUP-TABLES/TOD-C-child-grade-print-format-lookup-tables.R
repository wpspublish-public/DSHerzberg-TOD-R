suppressMessages(library(here))
suppressMessages(library(tidyverse))
library(writexl)
library(naturalsort)

# PART I: SET UP TOKENS AND INPUT FILES

input_test_names <- c("PHM-C", "IWS-C", "RLN-C", "PWR-C", "WPC-C", "WM-C", 
                      "PAN-C", "IWR-C", "ORE-C", "BLN-C", "SEG-C", "RWS-C", 
                      "SRE1-C", "SRE2-C", "RNL-C", "LM-C", "RPW-C", "RIW-C", 
                      "SSL-C", "LV-C", "GAN-C")
input_test_names_lower <- c("PHM-C", "IWS-C", "RLN-C", "PWR-C", "WPC-C", "WM-C", 
                      "PAN-C", "IWR-C", "ORE-C", "BLN-C", "SEG-C", "RWS-C", 
                      "SRE1-C", "RNL-C", "LM-C", "RPW-C", "RIW-C", 
                      "SSL-C", "LV-C", "GAN-C")
input_test_names_upper <- c("PHM-C", "IWS-C", "RLN-C", "PWR-C", "WPC-C", "WM-C", 
                      "PAN-C", "IWR-C", "ORE-C", "BLN-C", "SEG-C", "RWS-C", 
                      "SRE2-C", "RNL-C", "LM-C", "RPW-C", "RIW-C", 
                      "SSL-C", "LV-C", "GAN-C")
input_test_names_start <- c("PHM-C", "IWS-C", "RLN-C", "PWR-C", "WPC-C", "WM-C", 
                      "PAN-C", "IWR-C", "ORE-C", "BLN-C", "SEG-C", "RWS-C")
input_test_names_end <- c("RNL-C", "LM-C", "RPW-C", "RIW-C", 
                      "SSL-C", "LV-C", "GAN-C")
output_test_names <- c("PHM-C", "IWS-C", "RLN-C", "PWR-C", "WPC-C", "WM-C", 
                       "PAN-C", "IWR-C", "ORE-C", "BLN-C", "SEG-C", "RWS-C", 
                       "SRE1-C", "SRE2-C", "RNL-C", "LM-C", "RPW-C", "RIW-C", 
                       "SSL-C", "LV-C", "GAN-C")
output_test_names_sre1 <- c("PHM-C", "IWS-C", "RLN-C", "PWR-C", "WPC-C", "WM-C", 
                       "PAN-C", "IWR-C", "ORE-C", "BLN-C", "SEG-C", "RWS-C", 
                       "SRE1-C", "RNL-C", "LM-C", "RPW-C", "RIW-C", 
                       "SSL-C", "LV-C", "GAN-C")
output_test_names_sre2 <- c("PHM-C", "IWS-C", "RLN-C", "PWR-C", "WPC-C", "WM-C", 
                       "PAN-C", "IWR-C", "ORE-C", "BLN-C", "SEG-C", "RWS-C", 
                       "SRE2-C", "RNL-C", "LM-C", "RPW-C", "RIW-C", 
                       "SSL-C", "LV-C", "GAN-C")
tod_form <- "TOD-C"
norm_type <- "grade"
input_file_path <- "PRINT-FORMAT-NORMS-TABLES/POST-cNORM-HAND-SMOOTHED-TABLES/TOD-C/CHILD-GRADE/"
output_file_path <- "PRINT-FORMAT-NORMS-TABLES/OUTPUT-FILES/TOD-C/CHILD-GRADE/"

# read the input files (test>>grade) into a list.
input_files_ta <- map(
  input_test_names,
  ~
  suppressMessages(read_csv(here(str_c(
  input_file_path, .x, "-", norm_type, ".csv"
)), col_select = -starts_with(".")))
) %>% 
  set_names(input_test_names)

# fix col names and store them in vectors: add leading zeros so they'll sort
# properly
new_names_input <- input_files_ta[[1]] %>%
  names() %>%
  tibble() %>%
  mutate(across(.,
                ~
                  case_when(
                    str_detect(., "^(?!(r|K|10|11|12)).*$") ~ str_c("0", .),
                    TRUE ~ .
                  ))) %>%
  pull(.)
grade_strat <- new_names_input[2:length(new_names_input)]

new_names_input_sre1 <- input_files_ta[[13]] %>%
  names() %>%
  tibble() %>%
  mutate(across(.,
                ~
                  case_when(
                    str_detect(., "^(?!(r|K|10|11|12)).*$") ~ str_c("0", .),
                    TRUE ~ .
                  ))) %>%
  pull(.)
grade_strat_sre1 <- new_names_input_sre1[2:length(new_names_input_sre1)]

new_names_input_sre2 <- input_files_ta[[14]] %>%
  names() %>%
  tibble() %>%
  mutate(across(.,
                ~
                  case_when(
                    str_detect(., "^(?!(r|K|10|11|12)).*$") ~ str_c("0", .),
                    TRUE ~ .
                  ))) %>%
  pull(.)
grade_strat_sre2 <- new_names_input_sre2[2:length(new_names_input_sre2)]

# apply new names to input files
input_files_ta <- map2(
  input_files_ta,
  c(
    rep(list(new_names_input), 12),
    list(new_names_input_sre1, new_names_input_sre2), 
    rep(list(new_names_input), 7)
  ), 
  ~ {
    n <- .y
    .x %>% 
    rename_with(
      ~
        n,
      everything()
    )}
) 

# read in static columns with all possible SS mapped onto associated percentiles
perc_ss_cols <- suppressMessages(read_csv(here(
  "PRINT-FORMAT-NORMS-TABLES/perc-ss-cols.csv"
)))

# Impose print-style raw score formatting on input tables
print_lookups_ta <- input_files_ta %>%
  map(
    ~
      .x %>%
      pivot_longer(contains("-"), names_to = "grade_strat", values_to = "ss") %>%
      arrange(match(grade_strat, grade_strat)) %>%
      group_by(grade_strat) %>%
      complete(ss = 40:130) %>%
      group_by(grade_strat, ss) %>%
      filter(n() == 1 | n() > 1 & row_number()  %in% c(1, n())) %>%
      summarize(raw = str_c(raw, collapse = '--')) %>%
      mutate(across(raw, ~ case_when(
        is.na(.x) ~ '-', TRUE ~ .x
      ))) %>%
      arrange(match(grade_strat, grade_strat), desc(ss)) %>%
      pivot_wider(names_from = grade_strat,
                  values_from = raw) %>%
      filter(!is.na(ss)) %>%
      right_join(perc_ss_cols, by = "ss") %>%
      relocate(perc, .before = "ss")
  ) %>%
  set_names(input_test_names)

# decompose list of input tables into "list of lists", with bottom level elements
# being three-col lookup tables, one for each grade-strat
grade_strat_cols_ta_start <-  print_lookups_ta[1:12] %>%
  map( ~
         map(grade_strat,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(grade_strat))

grade_strat_cols_ta_sre1 <-  print_lookups_ta[13] %>%
  map( ~
         map(grade_strat_sre1,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(grade_strat_sre1))

grade_strat_cols_ta_sre2 <-  print_lookups_ta[14] %>%
  map( ~
         map(grade_strat_sre2,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(grade_strat_sre2))

grade_strat_cols_ta_end <-  print_lookups_ta[15:21] %>%
  map( ~
         map(grade_strat,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(grade_strat))

grade_strat_cols_ta <- c(grade_strat_cols_ta_start, 
                         grade_strat_cols_ta_sre1, 
                         grade_strat_cols_ta_sre2, 
                         grade_strat_cols_ta_end)

# create vec of names of all crossings of grade_strat and test names.
grade_test_names_flat <- 
  c(
    cross2(grade_strat, input_test_names_start),
    str_c(grade_strat_sre1, "_SRE1-C"), 
    str_c(grade_strat_sre2, "_SRE2-C"), 
    cross2(grade_strat, input_test_names_end)
) %>% 
  map_chr(str_c, collapse = "_")

# flatten grade_strat_dfs so that it is a one-level list holding all single
# grade-strat lookups spread over all tests. rename the list elements with
# grade_test_names so that each element (df) of the list can be identified.
grade_test_cols_flat <- flatten(grade_strat_cols_ta) %>% 
  set_names(grade_test_names_flat)

# Reconstitute grade_test_cols_flat into a list of lists in which the top level is
# a list for each grade_strat, and within those lists are the list of lookup
# tables for that grade_strat, for all tests.
grade_test_cols_at <- map(
  grade_strat,
  ~
    keep(grade_test_cols_flat, str_detect(names(grade_test_cols_flat), .x))
)

# print_lookups_at is a transformation of grade_test_cols_at into a list suitable
# for writing out as a tabbed .xlsx file.
print_lookups_at <- map2(grade_test_cols_at,
                         c(rep(list(output_test_names_sre1), 10),
                           rep(list(output_test_names_sre2), 14)),
                         ~ {
                           n <- .y
                           .x %>%
                             reduce(left_join, by = c("perc", "ss")) %>%
                             rename_with( ~ n, contains("-"))
                         }) %>%
  set_names(grade_strat)

# Write raw-to-ss lookups by gradestrat into tabbed, xlsx workbook. To create
# named tabs, supply writexl::write_xlsx() with a named list of dfs for each
# tab, tab names will be the names of the list elements
write_xlsx(print_lookups_at,
           here(
             str_c(
               output_file_path, tod_form, "-print-lookup-tables-", norm_type, ".xlsx"
             ))
           )





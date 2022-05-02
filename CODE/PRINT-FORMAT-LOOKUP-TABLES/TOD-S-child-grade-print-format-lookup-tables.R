suppressMessages(library(here))
suppressMessages(library(tidyverse))
library(writexl)
library(naturalsort)

# PART I: SET UP TOKENS AND INPUT FILES

input_test_names <- c("PV-S", "LW-S", "QRF-S", "WRF-S")
output_test_names1 <- c("PV-S", "LW-S", "WRF-S")
output_test_names2 <- c("PV-S", "LW-S", "QRF-S")
tod_form <- "TOD-S"
norm_type <- "grade"
input_file_path <- "PRINT-FORMAT-NORMS-TABLES/POST-cNORM-HAND-SMOOTHED-TABLES/TOD-S/CHILD-GRADE/"
output_file_path <- "PRINT-FORMAT-NORMS-TABLES/OUTPUT-FILES/TOD-S/CHILD-GRADE/"

# read the input files (test>>grade) into a list.
input_files_ta <- map(
  input_test_names,
  ~
  suppressMessages(read_csv(here(str_c(
  input_file_path, .x, "-", norm_type, ".csv"
))))
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

new_names_input_qrf <- input_files_ta[[3]] %>%
  names() %>%
  tibble() %>%
  mutate(across(.,
                ~
                  case_when(
                    str_detect(., "^(?!(r|K|10|11|12)).*$") ~ str_c("0", .),
                    TRUE ~ .
                  ))) %>%
  pull(.)
grade_strat_qrf <- new_names_input_qrf[2:length(new_names_input_qrf)]

new_names_input_wrf <- input_files_ta[[4]] %>%
  names() %>%
  tibble() %>%
  mutate(across(.,
                ~
                  case_when(
                    str_detect(., "^(?!(r|K|10|11|12)).*$") ~ str_c("0", .),
                    TRUE ~ .
                  ))) %>%
  pull(.)
grade_strat_wrf <- new_names_input_wrf[2:length(new_names_input_wrf)]

# apply new names to input files
input_files_ta <- map2(
  input_files_ta,
  list(
    new_names_input,
    new_names_input,
    new_names_input_qrf,
    new_names_input_wrf
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
  set_names(input_test_names) %>%
  map2(
    list(grade_strat, grade_strat, grade_strat_qrf, grade_strat_wrf),
    ~ .x %>%
      select(perc, ss, all_of(.y))
  )

# decompose list of input tables into "list of lists", with bottom level elements
# being three-col lookup tables, one for each grade-strat
grade_strat_cols_ta_pv_ls <-  print_lookups_ta[1:2] %>%
  map( ~
         map(grade_strat,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(grade_strat))

grade_strat_cols_ta_qrf <-  print_lookups_ta[3] %>%
  map( ~
         map(grade_strat_qrf,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(grade_strat_qrf))

grade_strat_cols_ta_wrf <-  print_lookups_ta[4] %>%
  map( ~
         map(grade_strat_wrf,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(grade_strat_wrf))

grade_strat_cols_ta <- c(grade_strat_cols_ta_pv_ls, 
                       grade_strat_cols_ta_qrf, 
                       grade_strat_cols_ta_wrf)

# create vec of names of all crossings of grade_strat and test names.
grade_test_names_flat <- c(
  str_c(grade_strat, "_PV-S"),
  str_c(grade_strat, "_LW-S"),
  str_c(grade_strat_qrf, "_QRF-S"),
  str_c(grade_strat_wrf, "_WRF-S")
)

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
                         c(rep(list(output_test_names1), 4),
                           rep(list(output_test_names2), 22)),
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





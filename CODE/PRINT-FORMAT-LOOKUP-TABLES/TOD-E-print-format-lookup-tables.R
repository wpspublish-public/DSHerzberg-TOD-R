suppressMessages(library(here))
suppressMessages(library(tidyverse))
library(writexl)

# The purpose of this script is to convert lookup tables from cNORM, that have been
# modified by a project direct (e.g., hand-smoothing  has been applied) into print-format
# lookup tables. Because there is a step of human-intervention outside cNORM, the process
# cannot be automated as part of the basic cNORM workflow.

# The input tables embody a hierarchy of data in which there is one table per test, and each
# of these tables holds the lookup columns for all age strats. In addition, you lookup raw scores
# in the left-ward column and read to the right to find the associated standard scores. here
# is the head of one such table.

# The desired output, on the other hand, reverses the two hiearchies described for the input
# tables. In the output tables, there is one table per age strat, and each of these tables
# holds the lookup columns for all tests. On the output tables, you look up raw scores
# in the rightward columns and read to the left to find the associated standard scores. here
# is the head of one such output table.

# What the script accomplishes is a transformation of the essential hierarchies of the data(
# structures of the input, and a reformating of the cells themselves. Recall that the lookup
# relationship between raw scores and standard scores is many-to-one. That is, a range of raw scores
# (multiple raw scores) can  map onto a single standard score, but each raw score on its own
# maps onto one and only one standard score. Thus, the initial transformation of the work flow
# is to collapse the incremental sequence of raw scores so that each standard score occupies
# only a single row. Prior to this transformation, there could be duplicate standard score rows.
# )

# The second transformation is to invert the hierarchy of the inputs, in which each test is a 
# container for all age strats. This hierarchy is reversed in the output, such that after
# transformation, each age strat is a container for all tests.

test_names <- c("lske", "lswe", "rhme", "rlne", "sege", "snwe")
norm_type <- "age"
input_file_path <- "PRINT-FORMAT-NORMS-TABLES/POST-cNORM-HAND-SMOOTHED-TABLES/"
# output_file_path <- "OUTPUT-FILES/NORMS/TODC_final_11.17.21_adult_fornorms/"

input_files <- map(
  test_names,
  ~
  suppressMessages(read_csv(here(str_c(
  input_file_path, .x, "-", norm_type, ".csv"
))))
) %>% 
  set_names(test_names)

perc_ss_cols <- suppressMessages(read_csv(here(
  "PRINT-FORMAT-NORMS-TABLES/perc-ss-cols.csv"
)))

age_strat <- input_files[[1]] %>% 
  select(-raw) %>% 
  names()

# print_look_up_list contains the transformed input files. These are new lookup
# tables for each test, in the print format. Each table has lookup cols for all
# age strats, each containing raw scores (or ranges) that are looked up against
# the ss col on the left.
print_lookup_list <- input_files %>%
  map(~
        .x %>% 
  pivot_longer(contains("-"), names_to = "age_strat", values_to = "ss") %>%
  arrange(age_strat) %>%
  group_by(age_strat) %>%
  complete(ss = 40:130) %>%
  group_by(age_strat, ss) %>%
  filter(n() == 1 | n() > 1 & row_number()  %in% c(1, n())) %>%
  summarize(raw = str_c(raw, collapse = '--')) %>%
  mutate(across(raw, ~ case_when(is.na(.x) ~ '-', TRUE ~ .x))) %>%
  arrange(age_strat, desc(ss)) %>%
  pivot_wider(names_from = age_strat,
              values_from = raw) %>%
  filter(!is.na(ss)) %>%
  right_join(perc_ss_cols, by = "ss") %>%
  relocate(perc, .before = "ss")
) %>% 
  set_names(test_names)

# age_strat_dfs is a transformation of print_lookup_list using nested map calls.
# The table for each test of print_look_up_list is broken down into separate
# look-up dfs, one for each age_strat col. So age_strat_dfs is a list of lists,
# the top level being a list for each test, and the lower level being the list of
# age-strat specific lookup tables within each test.
age_strat_dfs <-  print_lookup_list %>%
  map( ~
         map(age_strat,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(age_strat))


# create vec of names (length = 54) of all crosssings of age_strat and test names.
as_tn_names <- cross2(age_strat, test_names) %>% 
  map_chr(str_c, collapse = "_")

# flatten age_strat_dfs so that it is a one-level list holding all 54 single
# age-strat lookups spread over the six tests. rename the list elements with
# as_tn_names so that each element of the list can be identified.
as_tn_dfs <- flatten(age_strat_dfs) %>% 
  set_names(as_tn_names)

# as_tn_dfs_per_age_strat reconsitutes a new two-level list from the flattened
# as_tn_dfs. Whereas in age_strat dfs, the hierarchy was test --> age_strat, in
# as_tn_dfs_per_agestrat, that hierarchy is reversed to age_strat --> test,
# which is the hierarchy required for the final output. Now we have a list of
# lists in which the top level is a list for each age_strat, and within those
# lists are the list of lookup tables for that age_strat, for all six tests.
# This transformation is accomplished via subsetting with single brackets [].
# Using map(), we subset as_tn_dfs 9 times, mapping across the 9 elements of
# age_strat. grep() matches the pattern supplied by .x (here a particular
# age_strat) against the names of the flattened as_tn_dfs, and returns the
# positions (indices) of any names that match the pattern. Passing those indices
# to the single brackets returns the age_strat specific lookups, one for each
# test, giving the new two-level list the desired hierarchy.
as_tn_dfs_per_age_strat <- map(
  age_strat,
  ~
    as_tn_dfs[grep(.x, names(as_tn_dfs))]  
)

# tabbed_output transforms as_tn_dfs_per_age_strat into a list suitable for
# writing out as a tabbed .xlsx file. Recall that each element of
# as_tn_dfs_per_age_strat is a list of the test-wise look up dfs for each
# age_strat. reduce() joins the list of dfs into a single df with right-ward raw
# score lookup cols for each test within each age_strat. The call of rename_with
# renames the raw score cols of the newly joined dfs, taking advantage of a
# shorthand in which the renaming function (the first argument of rename_with is
# specified as the vector or new) is simple a vector containing the new col
# names, and the second argument is a vector of equal length naming the cols to
# be renamed (here those cols are collected using the tidy select helper
# contains()). We then name the nine new age-strat specific lookup tables with
# the names of their corresponding age_strats, using set_names. Naming the list
# elements here is crucial for output, as these names become the tabbed names on
# the .xlsx output file.
tabbed_output <- as_tn_dfs_per_age_strat %>% 
  map(
    ~
      .x %>% 
      reduce(left_join, by = c("perc", "ss")) %>% 
      rename_with(~ test_names, contains("-"))
  ) %>% 
  set_names(age_strat)


# Write raw-to-ss lookups by agestrat into tabbed, xlsx workbook. To create
# named tabs, supply writexl::write_xlsx() with a named list of dfs for each
# tab, tab names will be the names of the list elements
write_xlsx(tabbed_output,
           here("print-format-table-age.xlsx"))





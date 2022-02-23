suppressMessages(library(here))
suppressMessages(library(tidyverse))
library(writexl)

# The purpose of this script is to convert lookup tables from cNORM, that have been
# modified by a project director (e.g., hand-smoothing  has been applied) into print-format
# lookup tables. Because there is a step of human-intervention outside cNORM, the process
# cannot be automated as part of the basic cNORM workflow.

# The input tables embody a hierarchy of data in which there is one table per test, and each
# of these tables holds the lookup columns for all age strats. In addition, you lookup raw scores
# in the left-ward column and read to the right to find the associated standard scores. here
# is the head of one such table.

# ```
# # A tibble: 34 × 10
# raw `5.0-5.3` `5.4-5.7` `5.8-5.11` `6.0-6.5` `6.6-6.11` `7.0-7.5` `7.6-7.11` `8.0-8.5`
# <dbl>     <dbl>     <dbl>      <dbl>     <dbl>      <dbl>     <dbl>      <dbl>     <dbl>
#   1     0        66        62         58        55         50        47         44        40
# 2     1        70        66         62        58         53        50         47        44
# 3     2        74        70         66        61         57        53         49        46
# 4     3        77        74         70        65         60        56         53        49
# 5     4        80        77         73        68         63        58         55        51
# 6     5        83        80         75        70         65        60         56        53
# 7     6        86        82         77        72         67        62         58        55
# 8     7        89        84         79        74         68        64         60        56
# 9     8        91        86         81        76         70        65         61        57
# 10     9        93        88         83        77         72        67         62        59
# # … with 24 more rows, and 1 more variable: 8.6-9.3 <dbl>
# 
# ```

# The desired output, on the other hand, reverses the two hierarchies described for the input
# tables. In the output tables, there is one table per age strat, and each of these tables
# holds the lookup columns for all tests. On the output tables, you look up raw scores
# in the rightward columns and read to the left to find the associated standard scores. here
# is the head of one such output table.

# ```
# # A tibble: 91 × 8
# perc     ss `LSK-E` `LSW-E` `RHY-E` `RLN-E` `SEG-E` `SPW-E`
# <chr> <dbl> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
#   1 98      130 27--33  25--38  21--30  73--120 21--25  20--32 
# 2 97      129 -       24      -       72      20      19     
# 3 97      128 26      -       20      71      -       -      
#   4 96      127 -       23      -       70      -       -      
#   5 96      126 -       -       19      68--69  19      18     
# 6 95      125 25      22      -       67      -       -      
#   7 95      124 -       -       18      66      -       -      
#   8 94      123 24      21      17      65      18      17     
# 9 93      122 -       -       -       64      -       -      
#   10 92      121 23      20      16      63      -       -      
#   # … with 81 more rows
# ```

# What the script accomplishes is a transformation of the essential hierarchies
# of the data structures of the input, and a reformation of the cells
# themselves. Recall that the lookup relationship between raw scores and
# standard scores is many-to-one. That is, a range of raw scores (multiple raw
# scores) can  map onto a single standard score, but each raw score on its own
# maps onto one and only one standard score. 

########## NEXT TWO PARAGRAPHS ARE OPTIMIZED TEXT IMPORTED FROM FINISHED DOCUMENTATION.

# The lookup relationship between raw and T-scores is many-to-one, meaning that
# each value of raw maps onto one and only one value of T, but each value of T
# may map onto more than one value of raw.

# Print format: The left-most column contains all possible values of T
# (reflecting any truncation of the T-score distribution), sorted descending.
# Rightward columns contain corresponding raw scores for each scale. Reflecting
# the many-to-one lookup relationship, some of the rightward cells contain
# single raw scores, while others contain a range of raw scores.

###########

# Thus, the initial transformation of
# the work flow is to collapse the incremental sequence of raw scores so that
# each standard score occupies only a single row in the output. Prior to this
# transformation, there could be duplicate standard score rows. To preserve the
# many-to-one raw to SS relationship, any ss row may map onto a range of raw
# scores (e.g., 88-91)

# The second transformation is to invert the hierarchy of the inputs, in which each test is a 
# container for all age strats. This hierarchy is reversed in the output, such that after
# transformation, each age strat is a container for all tests.

# In order to track the transformation of the hierarchy through the code, we
# name objects with the suffix "ta" (test>>age) when they express the input hierarchy,
# and we use the suffix "at" (age>>test) to label the output hierarchy, and "flat" to
# denote an absence of either hierarchy.

# PART I: SET UP TOKENS AND INPUT FILES

input_test_names <- c("lske", "lswe", "rhme", "rlne", "sege", "snwe")
output_test_names <- c("LSK-E", "LSW-E", "RHY-E", "RLN-E", "SEG-E", "SPW-E")
tod_form <- "TOD-E"
norm_type <- "age"
input_file_path <- "PRINT-FORMAT-NORMS-TABLES/POST-cNORM-HAND-SMOOTHED-TABLES/"
output_file_path <- "PRINT-FORMAT-NORMS-TABLES/OUTPUT-FILES/"

# read the six input files (test>>age) into a list.
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

# read in a char vector of age-strats, by extracting these elements from the col
# names of one of the input dfs
age_strat <- input_files_ta[[1]] %>% 
  select(-raw) %>% 
  names()

# print_look_up_list_ta contains the transformed input files. These are new
# lookup tables for each test, in the print format. Each table has lookup cols
# for all age strats, each containing raw scores (or ranges) that are looked up
# against the ss col on the left. The tables in this list preserve the
# (test>>age) hiearchy of the input files, with one look-up row for each
# possible SS value.
print_lookups_ta <- input_files_ta %>%
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
  set_names(input_test_names)

# age_strat_cols_ta is a transformation of print_lookup_list using nested map
# calls. The table for each test of print_look_up_list is broken down into
# separate look-up dfs, one for each age_strat col. So age_strat_cols_ta is a
# list of lists, the top level being a list for each test, and the lower level
# being the list of age-strat specific lookup tables within each test. We say
# that the (test>>age) hierarchy of the input is preserved here, because the
# lowest level col labels are age-strat labels, as they are on the input files.
age_strat_cols_ta <-  print_lookups_ta %>%
  map( ~
         map(age_strat,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(age_strat))


# create vec of names (length = 54) of all crosssings of age_strat and test names.
age_test_names_flat <- cross2(age_strat, input_test_names) %>% 
  map_chr(str_c, collapse = "_")

# flatten age_strat_dfs so that it is a one-level list holding all 54 single
# age-strat lookups spread over the six tests. rename the list elements with
# as_tn_names so that each element (df) of the list can be identified.
age_test_cols_flat <- flatten(age_strat_cols_ta) %>% 
  set_names(age_test_names_flat)

# age_test_cols_at reconstitutes a new two-level list from the flattened
# as_tn_dfs. Whereas in age_strat_cols_ta, the hierarchy was test --> age_strat,
# in age_test_cols_at, that hierarchy is reversed to age_strat --> test, which
# is the hierarchy required for the final output. Now we have a list of lists in
# which the top level is a list for each age_strat, and within those lists are
# the list of lookup tables for that age_strat, for all six tests. This
# transformation is accomplished via purrr::keep(), which subsets list elements.
# Using map(), we subset age_test_cols_flat 9 times, mapping across the 9
# elements of age_strat. str_detect() matches the pattern supplied by .x (here a
# particular age_strat) against the names of the flattened age_test_cols_flat,
# and returns list elements whose names contain a match for the the pattern
# (i.e., a particular age-strat). These retained elements are complete sets of
# test-specific lookup columns, one set for each agestrat, giving the new
# two-level list the desired output hierarchy (age_strat --> test).
age_test_cols_at <- map(
  age_strat,
  ~
  keep(age_test_cols_flat, str_detect(names(age_test_cols_flat), .x))
)

# print_lookups_at is a transformation of age_test_cols_at into a list suitable
# for writing out as a tabbed .xlsx file. Recall that each element of
# age_test_cols_at is a list of the test-wise look up dfs for each
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





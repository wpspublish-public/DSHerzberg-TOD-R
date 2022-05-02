suppressMessages(library(cNORM))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(here))
library(writexl)
suppressMessages(library(lubridate))

# Prep input data file. Parse to three cols: personID, group (explanatory var  -
# age), raw score

# General tokens
combined_score_to_norm_file_name <- "TODS_TOE&TODCchild_fornorms.csv"
file_name_stem <- "TODS_TOE&TODCchild_fornorms"
input_file_path <- "INPUT-FILES/NORMS/TODS_TOE&TODCchild_fornorms/"
output_file_path <- "OUTPUT-FILES/NORMS/TODS_TOE&TODCchild_fornorms/"

# Tokens for score names

scores <- c("lw_ability", "pv_ability", "wrf_sum", "qrf_sum")

# Tokens setting the specific score to be normed on this iteration of the
# script.
score_to_norm_stem <- "qrf_sum"
score_to_norm_file_name <- str_c(score_to_norm_stem, "-norms-input.csv")
score_to_norm_max_raw <- data.frame(test = score_to_norm_stem) %>%
  mutate(
    max_raw = case_when(
      str_detect(test, "lw_ability") ~ 168,
      str_detect(test, "pv_ability") ~ 173,
      str_detect(test, "wrf_sum") ~ 68,
      str_detect(test, "qrf_sum") ~ 93,
    )
  ) %>%
  pull(max_raw)
score_to_norm_min_raw <- data.frame(test = score_to_norm_stem) %>%
  mutate(
    min_raw = case_when(
      str_detect(test, "lw_ability") ~ 29,
      str_detect(test, "pv_ability") ~ 29,
      str_detect(test, "wrf_sum") ~ 0,
      str_detect(test, "qrf_sum") ~ 0,
    )
  ) %>%
  pull(min_raw)

# to use age as predictor in cNORM, read in DOB, date_admin, calculate
# chronological age as decimal value.
age_contin <- suppressMessages(read_csv(here(
  str_c(input_file_path, combined_score_to_norm_file_name)
))) %>% 
  mutate(
    across(
      c(DOB, admin_date),
      ~
        mdy(.x)
    ),
    age = (DOB %--% admin_date) / years (1)
  ) %>%
  bind_cols(getGroups(.$age)) %>% 
  rename(group = ...10) %>% 
  # rename(group = ...8) %>% 
  select(ID, age, group)

# Next block reads an input containing multiple raw score columns per person,
# processes into separate dfs that are input files into cNORM for norming one
# raw score. 
map(
  scores,
  ~
    suppressMessages(read_csv(here(
      str_c(input_file_path, combined_score_to_norm_file_name)
    ))) %>%
    select(ID, !!sym(.x)) %>%
    drop_na(!!sym(.x)) %>% 
    left_join(age_contin, by = "ID") %>% 
    rename(raw = !!sym(.x)) %>% 
    select(ID, age, group, raw)
) %>%
  set_names(scores) %>%
  map2(scores,
       ~
         write_csv(.x,
                   here(
                     str_c(input_file_path, .y, "-norms-input.csv")
                   ))) %>% 
  invisible(.)

# read single score input.

input <- suppressMessages(read_csv(here(
  str_c(input_file_path, score_to_norm_file_name)
)))
#       # run age-filtering if needed.)
# ))) %>%
#   filter(age > 7) %>%
#   select(-group) %>%
#   mutate(group =
#            getGroups(.$age)) %>%
# relocate(group, .after = "age")

# Alex Lenhard's recommended approach with cNORM

# Use the all-in-one cnorm() function to create the model.
# Compare diagnostics from two approaches to defining age groups: -
# 1. use getGroups() to create equal size groups out of the age distributions,
# use "groups = " argument within cnorm() to refer to column holding this
# grouping code.
# 2. omit group argument: cnorm() defaults to rankBySlidingWindow, but this can
# be problematic when there are few cases on the tails of the age distribution -

# The two key diagnostics are plot(model, "series") and checkConsistency(). Both
# target the same problem: violations of monotonicty, or intersecting percentile
# curves. With plot(model, "series"), you can use "end" argument to set upper
# limit of predictors.

model <- cnorm(
  raw = input$raw, 
  group = input$group, 
  k = 4, 
  terms = 3, 
  scale = "IQ"
  )
# model <- cnorm(raw = input$raw, age = input$age, width = 1, k = 4, terms = 4, scale = "IQ")
plot(model, "series", end = 8)
checkConsistency(model)

# Token for names of output age groups
tab_names <- c(
  # todE runs below this line
  # "5.0-5.3", "5.4-5.7", "5.8-5.11", "6.0-6.5",
  # "6.6-6.11", "7.0-7.5", "7.6-7.11", "8.0-8.5", "8.6-9.3"
  # todC gr1-12 runs below this line
  # "6.0-6.3", "6.4-6.7", "6.8-6.11", "7.0-7.3", "7.4-7.7", "7.8-7.11",
  # "8.0-8.5", "8.6-8.11", "9.0-9.5",
  # "9.6-9.11", 
  # "10.0-10.5", "10.6-10.11", "11.0-11.5",
  # "11.6-11.11", "12.0-12.5", "12.6-12.11",
  # "13.0-13.11", "14.0-14.11", "15.0-16.11", "17.0-18.11"
  # todC adult runs below this line
  # "18.0-23.11", "24.0-39.11", "40.0-49.11", "50.0-59.11", 
  # "60.0-69.11", "70.0-89.11"
  # todE-todC combined runs below this line
  "5.0-5.3", "5.4-5.7", "5.8-5.11",
  "6.0-6.3", "6.4-6.7", "6.8-6.11",
  "7.0-7.3", "7.4-7.7", "7.8-7.11",
  "8.0-8.5", "8.6-8.11",
  "9.0-9.5", "9.6-9.11",
  "10.0-10.5", "10.6-10.11",
  "11.0-11.5", "11.6-11.11",
  "12.0-12.5", "12.6-12.11",
  "13.0-13.11",
  "14.0-14.11",
  "15.0-16.11",
  "17.0-18.11"
  # todE-todC combined wrf-sum runs below this line
  # "5.0-5.3", "5.4-5.7", "5.8-5.11",
  # "6.0-6.3", "6.4-6.7", "6.8-6.11",
  # "7.0-7.3", "7.4-7.7", "7.8-7.11"
  # todE-todC combined qrf-sum runs below this line
  # "7.0-7.3", "7.4-7.7", "7.8-7.11",
  # "8.0-8.5", "8.6-8.11",
  # "9.0-9.5", "9.6-9.11",
  # "10.0-10.5", "10.6-10.11",
  # "11.0-11.5", "11.6-11.11",
  # "12.0-12.5", "12.6-12.11",
  # "13.0-13.11",
  # "14.0-14.11",
  # "15.0-16.11",
  # "17.0-18.11"
)

# Prepare a list of data frames, each df is raw-to-ss lookup table for an age group.
norms_list <- rawTable(
  c(
    # todE runs below this line
    # 5.167, 5.5, 5.833, 6.25, 6.75, 7.25, 7.75, 8.25, 8.917
    # todC gr1-12 runs below this line
    # 6.167, 6.5, 6.833, 7.167, 7.5, 7.833, 8.25, 8.75,
    # 9.25, 9.75, 
    # 10.25, 10.75, 11.25, 11.75, 12.25,
    # 12.75,
    # 13.5, 14.5, 16, 18.0
    # todC adult runs below this line
    # 21, 32, 45, 55, 65, 80
    # todE-todC combined runs below this line
    5.167, 5.5, 5.833,
    6.167, 6.5, 6.833, 7.167, 7.5, 7.833, 8.25, 8.75,
    9.25, 9.75,
    10.25, 10.75, 11.25, 11.75, 12.25,
    12.75,
    13.5, 14.5, 16, 18.0
    # todE-todC combined wrf-sum runs below this line
    # 5.167, 5.5, 5.833,
    # 6.167, 6.5, 6.833,
    # 7.167, 7.5, 7.833
    # todE-todC combined qrf-sum runs below this line
    # 7.167, 7.5, 7.833,
    # 8.25, 8.75,
    # 9.25, 9.75,
    # 10.25, 10.75,
    # 11.25, 11.75,
    # 12.25, 12.75,
    # 13.5, 14.5, 16, 18.0
  ), 
  model, 
  step = 1, 
  minNorm = 40, 
  maxNorm = 130, 
  minRaw = score_to_norm_min_raw, 
  maxRaw = score_to_norm_max_raw,
  pretty = FALSE
  ) %>% 
  set_names(tab_names) %>% 
  map( 
    ~
      select(.x, raw, norm) %>% 
      summarize(raw = raw,
                ss = round(norm, 0))
)

# prepare reversal report
reversal_report <- norms_list %>%
  reduce(left_join,
         by = "raw") %>%
  set_names("raw", tab_names) %>%
  pivot_longer(-raw, names_to = "agestrat", values_to = "ss") %>%
  group_by(raw) %>%
  mutate(reversal = case_when(lag(ss) < ss ~ 1)) %>%
  filter(reversal == 1) %>%
  select(raw, agestrat) %>%
  write_csv(here(
    str_c(output_file_path, score_to_norm_stem, "-", file_name_stem, "-reversal-report-age.csv")
  ))

# Write raw-to-ss lookups by agestrat into tabbed, xlsx workbook.
write_xlsx(norms_list,
           here(str_c(
             output_file_path, score_to_norm_stem,  "-", file_name_stem, "-raw-ss-lookup-tabbed-age.xlsx"
           )))

# write raw-to-ss-lookups to single-sheet table
table <- norms_list %>%
  reduce(left_join,
         by = "raw") %>%
  set_names("raw", tab_names)

write_csv(table, 
          here(
  str_c(output_file_path, score_to_norm_stem,  "-", file_name_stem, "-raw-ss-lookup-table-age.csv")
))

# write model summary to text file, so you can replicate model later.
capture.output(
  str_c(score_to_norm_stem, " age model summary"), 
  summary(model),
  file = here(
    str_c(output_file_path, score_to_norm_stem,  "-", file_name_stem, "-model-summ-age.txt")  )
)




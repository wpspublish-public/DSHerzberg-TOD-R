suppressMessages(library(cNORM))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(here))
library(writexl)
suppressMessages(library(lubridate))

# Prep input data file. Parse to three cols: personID, group (explanatory var  -
# age), raw score

# General tokens
combined_score_to_norm_file_name <- "TODC_adult_10.28.21_for norms.csv"
input_file_path <- "INPUT-FILES/NORMS/TODC_adult_10.28.21_for norms/"
output_file_path <- "OUTPUT-FILES/NORMS/TODC_adult_10.28.21_for norms/"

# Tokens for score names

scores <- c("iws_sum", "bln_sum", "seg_sum", "rln_sum", "iwr_sum", 
            "riw_sum", "lem_sum", "pan_sum", "lvc_sum", "wpc_sum", 
            "rws_sum", "sub_sum", "del_sum", "rnl_sum", "nwr_sum", 
            "rnw_sum", "wom_sum", "gea_sum", "ssl_sum", "pflsum2", 
            "orf_sum")

# Tokens setting the specific score to be normed on this iteration of the
# script.
score_to_norm_stem <- "pan_sum"
score_to_norm_file_name <- str_c(score_to_norm_stem, "-adult-norms-input.csv")
score_to_norm_max_raw <- data.frame(test = score_to_norm_stem) %>%
  mutate(
    max_raw = case_when(
      str_detect(test, "iws_sum") ~ 44,
      str_detect(test, "bln_sum") ~ 29,
      str_detect(test, "seg_sum") ~ 29,
      str_detect(test, "rln_sum") ~ 200,
      str_detect(test, "iwr_sum") ~ 55,
      str_detect(test, "riw_sum") ~ 100,
      str_detect(test, "lem_sum") ~ 20,
      str_detect(test, "pan_sum") ~ 40,
      str_detect(test, "lvc_sum") ~ 38,
      str_detect(test, "wpc_sum") ~ 59,
      str_detect(test, "rws_sum") ~ 44,
      str_detect(test, "sub_sum") ~ 20,
      str_detect(test, "del_sum") ~ 25,
      str_detect(test, "rnl_sum") ~ 200,
      str_detect(test, "nwr_sum") ~ 47,
      str_detect(test, "rnw_sum") ~ 60,
      str_detect(test, "wom_sum") ~ 20,
      str_detect(test, "gea_sum") ~ 40,
      str_detect(test, "ssl_sum") ~ 42,
      str_detect(test, "pflsum2") ~ 51, 
      str_detect(test, "orf_sum") ~ 384
    )
  ) %>%
  pull(max_raw)

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
  rename(group = ...32) %>% 
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
                     str_c(input_file_path, .y, "-adult-norms-input.csv")
                   ))) %>% 
  invisible(.)

# read single score input.

input <- suppressMessages(read_csv(here(str_c(
  input_file_path, score_to_norm_file_name
))))

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
  terms = 4, 
  scale = "IQ"
  )
# model <- cnorm(raw = input$raw, age = input$age, width = 1, k = 4, terms = 4, scale = "IQ")
plot(model, "series", end = 8)
checkConsistency(model)

# Token for names of output age groups
tab_names <- c("18.0-23.11", "24.0-39.11", "40.0-49.11", "50.0-59.11", "60.0-69.11", "70.0-89.11")

# Prepare a list of data frames, each df is raw-to-ss lookup table for an age group.
norms_list <- rawTable(
  c(
    21, 32, 45, 55, 65, 80
  ), 
  model, 
  step = 1, 
  minNorm = 40, 
  maxNorm = 130, 
  minRaw = 0, 
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
    str_c(output_file_path, score_to_norm_stem, "-adult-reversal-report-age.csv")
  ))

# Write raw-to-ss lookups by agestrat into tabbed, xlsx workbook.
write_xlsx(norms_list,
           here(str_c(
             output_file_path, score_to_norm_stem, "-adult-raw-ss-lookup-tabbed-age.xlsx"
           )))

# write raw-to-ss-lookups to single-sheet table
table <- norms_list %>%
  reduce(left_join,
         by = "raw") %>%
  set_names("raw", tab_names)

write_csv(table, 
          here(
  str_c(output_file_path, score_to_norm_stem, "-adult-raw-ss-lookup-table-age.csv")
))

# write model summary to text file, so you can replicate model later.
capture.output(
  str_c(score_to_norm_stem, "adult age model summary"), 
  summary(model),
  file = here(
    str_c(output_file_path, score_to_norm_stem, "-adult-model-summ-age.txt")  )
)




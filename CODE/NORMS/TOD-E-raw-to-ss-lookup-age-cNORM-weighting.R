library(cNORM)
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(here))
library(writexl)
suppressMessages(library(lubridate))

# Prep input data file. Parse to three cols: personID, group (explanatory var  -
# age), raw score

# General tokens
combined_score_to_norm_file_name <- "TODE_8.27.21_fornorms-weighted-sum-scores.csv"
input_file_path <- "INPUT-FILES/NORMS/TODE_8.27.21_fornorms/"
output_file_path <- "OUTPUT-FILES/NORMS/TODE_8.27.21_fornorms/"

scores <- c("sege_sum", "rlne_sum", "rhme_sum", "snwe_sum",
            "lswe_sum", "lske_sum")

# Tokens setting the specific score to be normed on this iteration of the
# script.
score_to_norm_stem <- "lske_sum"
score_to_norm_file_name <- str_c(score_to_norm_stem, "-norms-input.csv")
score_to_norm_max_raw <- data.frame(test = score_to_norm_stem) %>%
  mutate(
    max_raw = case_when(
      str_detect(test, "sege") ~ 25,
      str_detect(test, "rlne") ~ 120,
      str_detect(test, "rhme") ~ 30,
      str_detect(test, "snwe") ~ 32,
      str_detect(test, "lswe") ~ 38,
      str_detect(test, "lske") ~ 33,
      # str_detect(test, "ORF_noNeg") ~ 263
    )
  ) %>%
  pull(max_raw)

# to use age as predictor in cNORM, read in DOB, date_admin, calculate
# chronological age as decimal value. This snippet also uses cNORM::getGroups()
# to let cNORM define equal-sized age groups
age_contin <- suppressMessages(read_csv(here(
  str_c(input_file_path, "TODE_8.27.21_fornorms.csv")
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
  rename(group = ...51) %>% 
  select(ID, age, group)

# read in census pcts for all demographic crossings.

demo_crossing_pct_all_160 <- suppressMessages(read_csv(here(
  str_c(input_file_path, "demo_crossing_pct_all_24.csv")
)))


# Next block reads an input containing multiple raw score columns per person,
# processes into separate dfs that are input files into cNORM for norming one
# raw score. 
map(
  scores,
  ~
    suppressMessages(read_csv(here(
      str_c(input_file_path, combined_score_to_norm_file_name)
    ))) %>%
    select(ID, demo_wt, gender, educ, ethnic, region, !!sym(.x)) %>%
    drop_na(!!sym(.x)) %>% 
    left_join(age_contin, by = "ID") %>% 
    left_join(
      demo_crossing_pct_all_160, 
      by = c("gender", "educ", "ethnic", "region")) %>% 
    rename(raw = !!sym(.x)) %>% 
    select(ID, demo_wt, cell_pct, age, group, raw)
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
  k = 5, 
  terms = 8, 
  scale = "IQ", 
  weights = input$cell_pct
  )
# model <- cnorm(raw = input$raw, age = input$age, width = 1, k = 4, terms = 4, scale = "IQ")
plot(model, "series", end = 10)
checkConsistency(model)

# Token for names of output age groups
tab_names <- c("5.0-5.3", "5.4-5.7", "5.8-5.11", "6.0-6.5", 
               "6.6-6.11", "7.0-7.5", "7.6-7.11", "8.0-8.5", "8.6-9.3")

# Prepare a list of data frames, each df is raw-to-ss lookup table for an age group.
norms_list <- rawTable(
  c(5.167, 5.5, 5.833, 6.25, 6.75, 7.25, 7.75, 8.25, 8.917), 
  model, 
  step = 1, 
  minNorm = 40, 
  maxNorm = 130, 
  minRaw = 1, 
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
    str_c(output_file_path, score_to_norm_stem, "-reversal-report-cNORM-weighting-age.csv")
  ))


# Write raw-to-ss lookups by agestrat into tabbed, xlsx workbook.
write_xlsx(norms_list,
           here(str_c(
             output_file_path, score_to_norm_stem, "-raw-ss-lookup-tabbed-cNORM-weighting-age.xlsx"
           )))

# write raw-to-ss-lookups to single-sheet table
table <- norms_list %>%
  reduce(left_join,
         by = "raw") %>%
  set_names("raw", tab_names)

write_csv(table, 
          here(
  str_c(output_file_path, score_to_norm_stem, "-raw-ss-lookup-table-cNORM-weighting-age.csv")
))

# write model summary to text file, so you can replicate model later.
capture.output(
  str_c(score_to_norm_stem, " model summary"), 
  summary(model),
  file = here(
    str_c(output_file_path, score_to_norm_stem, "-model-summ-cNORM-weighting-age.txt")  )
)




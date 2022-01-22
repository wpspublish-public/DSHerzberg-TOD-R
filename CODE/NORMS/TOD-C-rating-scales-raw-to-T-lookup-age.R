suppressMessages(library(cNORM))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(here))
library(writexl)
suppressMessages(library(lubridate))

# Prep input data file. Parse to three cols: personID, group (explanatory var  -
# age), raw score

# General tokens
combined_score_to_norm_file_name <- "TODCratingscales_gr1_12final.csv"
input_file_path <- "INPUT-FILES/NORMS/TODCratingscales_gr1_12final/"
output_file_path <- "OUTPUT-FILES/NORMS/TODCratingscales_gr1_12final/"

# Tokens for score names

scores <- c("TODparent_tot", "TODteacher_tot", "TODself_tot")

# Tokens setting the specific score to be normed on this iteration of the
# script.
score_to_norm_stem <- "TODparent_tot"
score_to_norm_file_name <- str_c(score_to_norm_stem, "-norms-input.csv")
score_to_norm_max_raw <- data.frame(test = score_to_norm_stem) %>%
  mutate(
    max_raw = case_when(
      str_detect(test, "parent") ~ 104,
      str_detect(test, "teacher") ~ 120,
      str_detect(test, "self") ~ 140
    )
  ) %>%
  pull(max_raw)
score_to_norm_min_raw <- data.frame(test = score_to_norm_stem) %>%
  mutate(
    min_raw = case_when(
      str_detect(test, "parent") ~ 26,
      str_detect(test, "teacher") ~ 30,
      str_detect(test, "self") ~ 25
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
    # group = case_when(
    #   age < 9 ~ 68,
    #   TRUE ~ 918
    # )
  ) %>%
  bind_cols(getGroups(.$age)) %>%
  # rename(group = ...33) %>%
  rename(group = ...8) %>%
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
  terms = 3, 
  scale = "T"
  )
# model <- cnorm(raw = input$raw, age = input$age, width = 1, k = 4, terms = 4, scale = "IQ")
plot(model, "series", end = 8)
checkConsistency(model)

# Token for names of output age groups
tab_names <- c(
  "6.0-8.11", "9.0-18.11"
  )

# Prepare a list of data frames, each df is raw-to-ss lookup table for an age group.
norms_list <- rawTable(
  c(
    7.5, 13.5
  ), 
  model, 
  step = 1, 
  minNorm = 40, 
  maxNorm = 80, 
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
    str_c(output_file_path, score_to_norm_stem, "-reversal-report-age.csv")
  ))

# Write raw-to-ss lookups by agestrat into tabbed, xlsx workbook.
write_xlsx(norms_list,
           here(str_c(
             output_file_path, score_to_norm_stem, "-raw-ss-lookup-tabbed-age.xlsx"
           )))

# write raw-to-ss-lookups to single-sheet table
table <- norms_list %>%
  reduce(left_join,
         by = "raw") %>%
  set_names("raw", tab_names)

write_csv(table, 
          here(
  str_c(output_file_path, score_to_norm_stem, "-raw-ss-lookup-table-age.csv")
))

# write model summary to text file, so you can replicate model later.
capture.output(
  str_c(score_to_norm_stem, " age model summary"), 
  summary(model),
  file = here(
    str_c(output_file_path, score_to_norm_stem, "-model-summ-age.txt")  )
)




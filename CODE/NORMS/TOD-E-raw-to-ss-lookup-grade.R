suppressMessages(library(cNORM))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(here))
library(writexl)
suppressMessages(library(lubridate))

# Prep input data file. Parse to three cols: personID, group (explanatory var  -
# grade), raw score

# General tokens

# data file with both weighted and unweighted sum scores
# combined_score_to_norm_file_name <- "TODE_8.27.21_fornorms-weighted-sum-scores.csv"

# data file with only unweighted sum scores
combined_score_to_norm_file_name <- "TODE_8.27.21_fornorms.csv"

input_file_path <- "INPUT-FILES/NORMS/TODE_8.27.21_fornorms/"
output_file_path <- "OUTPUT-FILES/NORMS/TODE_8.27.21_fornorms/"

# Tokens to toggle between using weighted vs. unweighted scores as the basis for
# the norms.

# scores <- c("sege_sum_w", "rlne_sum_w", "rhme_sum_w", "snwe_sum_w",
# "lswe_sum_w", "lske_sum_w")
scores <- c("sege_sum", "rlne_sum", "rhme_sum", "snwe_sum",
            "lswe_sum", "lske_sum", "ORF")

# Tokens setting the specific score to be normed on this iteration of the
# script.
score_to_norm_stem <- "ORF"
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
      str_detect(test, "ORF") ~ 263
    )
  ) %>%
  pull(max_raw)

# Next block reads an input containing multiple raw score columns per person,
# processes into separate dfs that are input files into cNORM for norming one
# raw score. 
map(
  scores,
  ~
    suppressMessages(read_csv(here(
      str_c(input_file_path, combined_score_to_norm_file_name)
    ))) %>%
    select(ID, GradeSemester, !!sym(.x)) %>%
    # recode grade variable to reflect weeks in school, centers grade norms at
    # the chronological center of the semester.
    mutate(
      across(
        GradeSemester,
        ~
          case_when(
            . == 1 ~ 8,
            . == 2 ~ 34,
            . == 3 ~ 60,
            . == 4 ~ 86,
            . == 5 ~ 112,
            . == 6 ~ 138
          )
      )
    ) %>% 
    drop_na(!!sym(.x)) %>% 
    rename(raw = !!sym(.x), group = GradeSemester) %>% 
    select(ID, group, raw) 
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
  terms = 4, 
  scale = "IQ"
  )
# model <- cnorm(raw = input$raw, age = input$age, width = 1, k = 4, terms = 4, scale = "IQ")
plot(model, "series", end = 8)
checkConsistency(model)

# Token for names of output age groups
tab_names <- c("K-Fall", "K-Spring", "1-Fall", "1-Spring", 
               "2-Fall", "2-Spring")

# Prepare a list of data frames, each df is raw-to-ss lookup table for an age group.
norms_list <- rawTable(
  c(8, 34, 60, 86, 112, 138), 
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
    str_c(output_file_path, score_to_norm_stem, "-reversal-report-grade.csv")
  ))

# Write raw-to-ss lookups by agestrat into tabbed, xlsx workbook.
write_xlsx(norms_list,
           here(str_c(
             output_file_path, score_to_norm_stem, "-raw-ss-lookup-tabbed-grade.xlsx"
           )))

# write raw-to-ss-lookups to single-sheet table
table <- norms_list %>%
  reduce(left_join,
         by = "raw") %>%
  set_names("raw", tab_names)

write_csv(table, 
          here(
  str_c(output_file_path, score_to_norm_stem, "-raw-ss-lookup-table-grade.csv")
))

# write model summary to text file, so you can replicate model later.
capture.output(str_c(score_to_norm_stem, "grade model summary"),
               summary(model),
               file = here(
                 str_c(
                   output_file_path,
                   score_to_norm_stem,
                   "-model-summ-grade.txt"
                 )
               ))




suppressMessages(library(cNORM))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(here))
library(writexl)
suppressMessages(library(lubridate))

# Prep input data file. Parse to three cols: personID, group (explanatory var  -
# grade), raw score

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

# Next block reads an input containing multiple raw score columns per person,
# processes into separate dfs that are input files into cNORM for norming one
# raw score. 

# here, the "GradeSemester" variable is coded as an integer (1:26) representing
# k-fall, k-spring, 1-fall, 1-spring . . . 12-fall, 12-spring. This coding is
# suitable for input into cNORM modeling, because it represents the equal
# interval stratification of the school year. You could take the extra step of
# recoding grade into weeks of schooling (with a linear transformation), but
# this is not necessary and yields no improvement in the modeling of the norms,
# according to the cNORM developers.
map(
  scores,
  ~
    suppressMessages(read_csv(here(
      str_c(input_file_path, combined_score_to_norm_file_name)
    ))) %>%
    select(ID, GradeSemester, !!sym(.x)) %>%
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
# ))))
      # run grade-filtering if needed.)
)))) %>%
  filter(group > 4)

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

# Token for names of output grade groups
tab_names <- c(
  # todE runs below this line
  # "K-Fall", "K-Spring", "1-Fall", "1-Spring", 
  # "2-Fall", "2-Spring"
  # todC gr1-12 runs below this line
  # "1-Fall", "1-Spring",
  # "2-Fall", "2-Spring",
  # "3-Fall", "3-Spring",
  # "4-Fall", "4-Spring",
  # "5-Fall", "5-Spring", 
  # "6-Fall", "6-Spring",
  # "7-Fall", "7-Spring",
  # "8-Fall", "8-Spring",
  # "9-Fall", "9-Spring",
  # "10-Fall", "10-Spring",
  # "11-Fall", "11-Spring",
  # "12-Fall", "12-Spring"
  # todE-todC combined runs below this line
  # "K-Fall", "K-Spring",
  # "1-Fall", "1-Spring",
  # "2-Fall", "2-Spring",
  # "3-Fall", "3-Spring",
  # "4-Fall", "4-Spring",
  # "5-Fall", "5-Spring",
  # "6-Fall", "6-Spring",
  # "7-Fall", "7-Spring",
  # "8-Fall", "8-Spring",
  # "9-Fall", "9-Spring",
  # "10-Fall", "10-Spring",
  # "11-Fall", "11-Spring",
  # "12-Fall", "12-Spring"
  # todE-todC combined wrf-sum runs below this line
  # "K-Fall", "K-Spring",
  # "1-Fall", "1-Spring"
  # todE-todC combined qrf-sum runs below this line
  "2-Fall", "2-Spring",
  "3-Fall", "3-Spring",
  "4-Fall", "4-Spring",
  "5-Fall", "5-Spring",
  "6-Fall", "6-Spring",
  "7-Fall", "7-Spring",
  "8-Fall", "8-Spring",
  "9-Fall", "9-Spring",
  "10-Fall", "10-Spring",
  "11-Fall", "11-Spring",
  "12-Fall", "12-Spring"
)

# In rawTable(), we pass A = 3:26 (e.g.) as the specification for the lookup table
# grade strata. This is identical to the coding of GradeSemester on the input
# side, where 3:26 represents the equal interval grouping structure of 1-fall,
# 1-spring, 2-fall, .  . ., 12-spring. In the TOD data, testing was performed on
# indiscriminate dates throughout each semester (as opposed to, say, all testing
# was done on the monday of the 10th week). Because of this, we can plausibly
# assume that the 3:26 input vector also represent the midpoint of each semester
# on the output side, which is what we want for the lookup tables. By centering
# these tables on the midpoint of each semester, we minimize the residuals of
# any child's distance (in terms of when in the semester he/she was tested) from
# that center point.

norms_list <- rawTable(
  c(
    # todE runs below this line
    # 1:6
    # todC gr1-12 runs below this line
    # 3:26
    # todE-todC combined runs below this line
    # 1:26
    # todE-todC combined wrf-sum runs below this line
    # 1:4
    # todE-todC combined qrf-sum runs below this line
    5:26
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
    str_c(output_file_path, score_to_norm_stem, "-", file_name_stem, "-reversal-report-grade.csv")
  ))

# Write raw-to-ss lookups by agestrat into tabbed, xlsx workbook.
write_xlsx(norms_list,
           here(str_c(
             output_file_path, score_to_norm_stem,  "-", file_name_stem, "-raw-ss-lookup-tabbed-grade.xlsx"
           )))

# write raw-to-ss-lookups to single-sheet table
table <- norms_list %>%
  reduce(left_join,
         by = "raw") %>%
  set_names("raw", tab_names)

write_csv(table, 
          here(
  str_c(output_file_path, score_to_norm_stem,  "-", file_name_stem, "-raw-ss-lookup-table-grade.csv")
))

# write model summary to text file, so you can replicate model later.
capture.output(
  str_c(score_to_norm_stem, " grade model summary"), 
  summary(model),
  file = here(
    str_c(output_file_path, score_to_norm_stem,  "-", file_name_stem, "-model-summ-grade.txt")  )
)




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
combined_score_to_norm_file_name <- "TODC_final_gr1_12_10.28.21_fornorms.csv"
input_file_path <- "INPUT-FILES/NORMS/TODC_final_gr1_12_10.28.21_fornorms/"
output_file_path <- "OUTPUT-FILES/NORMS/TODC_final_gr1_12_10.28.21_fornorms/"

# Tokens for score names

scores <- c("iws_sum", "bln_sum", "seg_sum", "rln_sum", "iwr_sum", "riw_sum", "lem_sum", "pan_sum", 
            "lvc_sum", "wpc_sum", "rws_sum", "sub_sum", "del_sum", "rnl_sum", "nwr_sum", "rnw_sum", 
            "wom_sum", "gea_sum", "ssl_sum", "pflsum1", "pflsum2")

# Tokens setting the specific score to be normed on this iteration of the
# script.
score_to_norm_stem <- "iws_sum"
score_to_norm_file_name <- str_c(score_to_norm_stem, "-norms-input.csv")
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
      str_detect(test, "pflsum1") ~ 63,
      str_detect(test, "pflsum2") ~ 51
    )
  ) %>%
  pull(max_raw)

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

# Token for names of output grade groups
tab_names <- c( 
               "1-Fall", "1-Spring",
               "2-Fall", "2-Spring",
               "3-Fall", "3-Spring",
               "4-Fall", "4-Spring",
               "5-Fall", "5-Spring", 
               # end pflsum1 gradestrat, begin pflsum2 gradestrat
               "6-Fall", "6-Spring",
               "7-Fall", "7-Spring",
               "8-Fall", "8-Spring",
               "9-Fall", "9-Spring",
               "10-Fall", "10-Spring",
               "11-Fall", "11-Spring",
               "12-Fall", "12-Spring"
               )

# In rawTable(), we pass A = 3:26 as the specification for the lookup table
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

# Note that the code below includes versions of the A argument for restricted
# gradestrats of pflsum1 and pflsum2.

norms_list <- rawTable(
  3:26,
  # 3:12,
  # 13:26,
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
capture.output(
  str_c(score_to_norm_stem, "grade model summary"), 
  summary(model),
  file = here(
    str_c(output_file_path, score_to_norm_stem, "-model-summ-grade.txt")  )
)




library(cNORM)
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(here))
library(writexl)
suppressMessages(library(lubridate))

# Prep input data file. Parse to three cols: personID, group (explanatory var  -
# age), raw score

combined_input_file_name <- "TODE_8.27.21_fornorms-weighted-sum-scores.csv"
input_file_path <- "INPUT-FILES/NORMS/TODE_8.27.21_fornorms/"
output_file_path <- "OUTPUT-FILES/NORMS/TODE_8.27.21_fornorms/"
scores <- c("sege_sum_w", "rlne_sum_w", "rhme_sum_w", "snwe_sum_w",
            "lswe_sum_w", "lske_sum_w", "ORF_noNeg_w")
# scores <- c("sege_sum", "rlne_sum", "rhme_sum", "snwe_sum",
#             "lswe_sum", "lske_sum", "ORF_noNeg")

# to use age as predictor in cNORM, read in DOB, date_admin, calculate
# chronological age as decimal value.
age_contin <- suppressMessages(read_csv(here(
  str_c(input_file_path, "TODE_8.27.21_fornorms_datesOnly.csv")
))) %>% 
  mutate(
    across(
      c(DOB, admin_date),
      ~
        # lubridate::mdy() coerces string into date-time type
        mdy(.x)
    ),
    # [start_date] %--% [end_date] uses lubridate operator %--% to create a time
    # interval (duration). Dividing interval by a period (years(1) = 1 year)
    # returns age in years-months-days as a decimal value (e.g., 5 years, 6
    # months, 0 days = 5.5)
    age = (DOB %--% admin_date) / years (1)
  ) %>%
  # here bind_cols joins a col created by cNORM::getGroups(), which returns vec
  # with equal size age strata, where value of col is a label for the age stratum that
  # each row belongs to, and that label is the arithmetic mean of chronological
  # age within that group
  bind_cols(getGroups(.$age)) %>% 
  rename(group = ...5) %>% 
  select(ID, age, group)

age_contin %>% 
  select(ID, age) %>% 
  write_csv(here(
    str_c(input_file_path, "TOD-AL-age-contin.csv")
  ))

age_contin %>% 
  rename(getGroup = group) %>% 
  select(ID, age, getGroup) %>% 
  write_csv(here(
    str_c(input_file_path, "TOD-AL-age-contin-getGroup.csv")
  ))


# Next block reads an input containing multiple raw score columns per person,
# processes into separate dfs that are input files into cNORM for norming one
# raw score. Within the process, a list of dfs is created and those dfs are
# eventually written out as .csvs, but the list itself is invisible, it's never
# preserved in the global environment. These new input files for cnorm()
# incorporate the continuous age variable and age grouping veriable creaed in
# the previous step.
map(
  scores,
  ~
    suppressMessages(read_csv(here(
      str_c(input_file_path, combined_input_file_name)
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

input_file_stem <- "lswe_sum_w"
input_file_name <- str_c(input_file_stem, "-norms-input.csv")

input <- suppressMessages(read_csv(here(str_c(
  input_file_path, input_file_name
))))

# Alex Lenhard's solution. Key points:

# Use the all-in-one cnorm(), that combines several important functions from my
# code above. Compare diagnostics from two approaches to defining age groups: -
# 1. omit group argument: cnorm() defaults to rankBySlidingWindow, but this can
# be problematic when there are few cases on the tails of the age distribution -
# 2. use getGroups() to create equal size groups out of the age distributions,
# use "groups = " argument within cnorm() to refer to column holding this
# grouping code

# The two key diagnostics are plotPercentileSeries() and checkConsistency(). Both
# target the same problem: violations of monotonicty, or intersecting percentile
# curves. With plotPercntileSeries(), you can use "end" argument to set upper
# limit of predictors, and "percentiles" to provide a vector of %ile scores to
# model.

model <- cnorm(raw = input$raw, group = input$group, k = 5, terms = 4, scale = "IQ")
# model <- cnorm(raw = input$raw, age = input$age, width = 1, k = 4, terms = 4, scale = "IQ")
plot(model, "series", end = 10)
plot(model, "subset")
plot(model, "percentiles")
plotDerivative(model)
checkConsistency(model)
plotPercentiles(model, percentiles=c(0.001, .5, .999))
normTable(model, A = 5.5)
rawTable(model, A=9.25)
plotRaw(model)
plotNorm(model)
getNormCurve(130, model)
plotNormCurves(model, c(70, 85, 100, 115, 130))
predictNorm(20, 8, model)


tab_names <- c("5.0", "5.6", "6.0", "6.6", "7.0", "7.6", "8.0")

# The max raw scores are as follows:
#   Snwe = 32
# Sege = 25
# rlne= 120
# lswe = 38
# rhme = 30
# lske = 33
# orf = each grade is given a different passage to read within 1 minute and 
# it is unlikely anyone would read the whole thing with zero errors, 
# but the total max for each passage is: Kinder 114, 1st grade fall = 119, 
# 1st grade spring = 112, 2nd grade = 163

# Prepare a list of data frames, each df is raw-to-ss lookup table for an age group.
norms_list <- rawTable(
  c(5.25, 5.75, 6.25, 6.75, 7.25, 7.75, 8.25), 
  model, 
  step = 1, 
  minNorm = 40, 
  maxNorm = 130, 
  minRaw = 1, 
  maxRaw = 38
  ) %>% 
  set_names(tab_names) %>% 
  map( 
    ~
      select(.x, raw, norm) %>% 
      summarize(raw = raw,
        ss = round(norm, 0))
  )

# Write assignments by coder into tabbed, xlsx workbook. To create named tabs,
# supply writexl::write_xlsx() with a named list of dfs for each tab, tab names
# will be the names of the list elements
write_xlsx(norms_list,
           here(str_c(
             output_file_path, input_file_stem, "-raw-ss-lookup.xlsx"
           )))


library(cNORM)
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(here))
library(writexl)
suppressMessages(library(lubridate))

# Prep input data file. Parse to three cols: personID, group (explanatory var  -
# age), raw score

combined_input_file_name <- "TODE_8.27.21_fornorms-weighted-sum-scores.csv"
input_file_path <- "INPUT-FILES/NORMS/TODE_8.27.21_fornorms/"
# scores <- c("sege_sum_w", "rlne_sum_w", "rhme_sum_w", "snwe_sum_w",
#             "lswe_sum_w", "lske_sum_w", "ORF_noNeg_w")
scores <- c("sege_sum", "rlne_sum", "rhme_sum", "snwe_sum",
            "lswe_sum", "lske_sum", "ORF_noNeg")

# to use age as predictor in cNORM, read in DOB, date_admin, calculate
# chronological age as decimal value.
age_contin <- suppressMessages(read_csv(here(
  str_c(input_file_path, "TODE_8.27.21_fornorms_datesOnly.csv")
))) %>% 
  # drop age outlier
  filter(ID != 210039) %>% 
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


# Next block reads an input containing multiple raw score columns per person,
# processes into separate dfs that are input files into cNORM for norming one
# raw score. Within the process, a list of dfs is created and those dfs are
# eventually written out as .csvs, but the list itself is invisible, it's never
# preserved in the global environment,
# map(
#   scores,
#   ~
#     suppressMessages(read_csv(here(
#       str_c(input_file_path, combined_input_file_name)
#     ))) %>%
#     select(ID, agestrat, !!sym(.x)) %>%
#     drop_na(!!sym(.x)) %>% 
#     mutate(
#       agestrat2 = case_when(
#         agestrat %in% c("5:0-5:7", "5:8-5:11") ~ 5,
#         agestrat %in% c("6:0-6:3", "6:4-6:7", "6:8-6:11") ~ 6,
#         agestrat %in% c("7:0-7:5", "7:6-7:11", "8:0-9:3") ~ 7,
#         TRUE ~ NA_real_
#       ),
#       across(
#         agestrat,
#         ~ case_when(
#           .x == "5:0-5:7" ~ 5,
#           .x == "5:8-5:11" ~ 5.67,
#           .x == "6:0-6:3" ~ 6,
#           .x == "6:4-6:7" ~ 6.33,
#           .x == "6:8-6:11" ~ 6.67,
#           .x == "7:0-7:5" ~ 7,
#           .x == "7:6-7:11" ~ 7.5,
#           .x == "8:0-9:3" ~ 8,
#           TRUE ~ NA_real_
#         )
#       )) %>%
#     # rename(group = agestrat,
#     rename(group = agestrat2,
#            raw = !!sym(.x)) %>% 
#     select(ID, group, raw)
# ) %>%
#   set_names(scores) %>%
#   map2(scores,
#        ~
#          write_csv(.x,
#                    here(
#                      str_c(input_file_path, .y, "-norms-input.csv")
#                    ))) %>% 
#   invisible(.)

map(
  scores,
  ~
    suppressMessages(read_csv(here(
      str_c(input_file_path, combined_input_file_name)
    ))) %>%
    # drop age outlier
    filter(ID != 210039) %>% 
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

input_file_name <- "sege_sum-norms-input.csv"

input <- suppressMessages(read_csv(here(str_c(
  input_file_path, input_file_name
))))

# Alex Lenhard's solution. Key points:

# Use the all-in-one cnorm(), that combines several important functions from my
# code above

# "group" var is used by cnorm() to determine percentile ranks. Value for this
# var should be the mean value of chronological age for cases with each group
# (age strata), or some close approximation of this value. Other values (e.g.,
# lower-bound of age group) introduce error.

# The two key diagnostics are plotPercentileSeries() and checkConsitency(). Both
# target the same problem: violations of monotonicty, or intersecting percentile
# curves. With plotPercntileSeries(), you can use "end" argument to set upper
# limit of predictors, and "percentiles" to provide a vector of %ile scores to
# model.

# Alex recoded the input age groupings into "group2", collapsing existing age
# strata to get larger groups. She recommends n >= 100 for age strata, so
# collapse groups to get to this number.

#modelTOD
model <- cnorm(raw = input$raw, group = input$group, k = 5, t = 2, terms = 5, scale = "IQ")
plot(model, "series", end = 10)
plot(model, "subset")
plot(model, "percentiles")
plotDerivative(model)
checkConsistency(model)
plotPercentiles(model, percentiles=c(0.001, .5, .999))
normTable(model, A = 5.5)
rawTable(model, A=9.25)

#alternative modelling with exact age = date_eval minus DOB, expressed as a decimal.
model_age <- cnorm(
  raw = input$raw,
  group = input$group,
  age = input$age,
  k = 5,
  t = 2,
  terms = 5,
  scale = "IQ"
)
plot(model_age, "series", end = 10)











# DROP OLDEST AGE GROUP
# input <- suppressMessages(read_csv(here(str_c(
#   input_file_path, input_file_name
# )))) %>% 
#   filter(group != 8)

# Calculation of the manifest percentiles and subsequent normal rank
# transformation to determine location (proxy for a norm-referenced score, i.e.,
# NOT a raw score)

input <- rankByGroup(input, scale = "IQ")

# TRUNCATE UPPER BOUND OF SS DISTRIBUTION
# input <- rankByGroup(input, scale = "IQ") %>%
#   mutate(across(normValue,
#                 ~
#                   case_when(normValue > 120 ~ 120,
#                             TRUE ~ .x)))


# Calculation of powers and interactions of
# location and grouping variable

input <- computePowers(input)


# Determining the best model with specified R2

model <- bestModel(input, terms = 5)


# Numerical check of the bijectivity between raw score and normal score

checkConsistency(model)


# Illustration of R2 by number of predictors

plotSubset(model, type= 0)

# Checking the limits of model validity via first order derivative
# to location outside the age range of the test (= horizontal interpolation)
# The gradient should not fall below zero.
# plotDerivative(model, minAge=60, maxAge=228, minNorm=50, maxNorm=150)



# Illustration of percentile curves of the identified best model

plotPercentiles(input, model)


# Alternatively, a whole series of charts can be generated, whereby
# the number of predictors is incremented by one at a time.
# If no further information is given, the chart is set to actually
# occurring raw scores and age groups of the original data set.

plotPercentileSeries(input, model)

# Transforms a specified series of normal scores into raw scores for
# third graders, ninth month of school year (explanatory variable = 3.75)

# normTable(3.75, model, step=1, minNorm=25, maxNorm=75)


# Alternative: Output of standard scores for a series of raw scores

tab_names <- c("5.0", "5.3", "5.6", "5.9", "6.0", "6.3", "6.6", "6.9", 
               "7.0", "7.3", "7.6", "7.9", "8.0", "8.6", "9.0", "9.6", 
               "10.0", "10.6", "11.0", "11.6", "12.0", "12.6", "13.0", 
               "14.0", "15.0", "16-18", "19-21")

# Prepare a list of data frames, each df is raw-to-ss lookup table for an age group.
norms_list <- rawTable(
  unique(input$group), 
  model, 
  step = 1, 
  minNorm = 40, 
  maxNorm = 160, 
  minRaw = 0, 
  maxRaw = 61
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
# write_xlsx(norms_list,
#            here("OUTPUT-FILES/ANT-raw-ss-lookup.xlsx"))


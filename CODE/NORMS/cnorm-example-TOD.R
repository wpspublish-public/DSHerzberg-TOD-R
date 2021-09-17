library(cNORM)
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(here))
library(writexl)

# Prep input data file. Parse to three cols: personID, group (explanatory var  -
# age), raw score

combined_input_file_name <- "TODE_8.27.21_fornorms-weighted-sum-scores.csv"
input_file_path <- "INPUT-FILES/NORMS/TODE_8.27.21_fornorms/"
# scores <- c("sege_sum_w", "rlne_sum_w", "rhme_sum_w", "snwe_sum_w", 
#             "lswe_sum_w", "lske_sum_w", "ORF_noNeg_w")
scores <- c("sege_sum", "rlne_sum", "rhme_sum", "snwe_sum", 
            "lswe_sum", "lske_sum", "ORF_noNeg")

# Next block reads an input containing multiple raw score columns per person,
# processes into separate dfs that are input files into cNORM for norming one
# raw score. Within the process, a list of dfs is created and those dfs are
# eventually written out as .csvs, but the list itself is invisible, it's never
# preserved in the global environment,
map(
  scores,
  ~
    suppressMessages(read_csv(here(
      str_c(input_file_path, combined_input_file_name)
    ))) %>%
    select(ID, agestrat, !!sym(.x)) %>%
    mutate(across(
      agestrat,
      ~ case_when(
        .x == "5:0-5:7" ~ 5,
        .x == "5:8-5:11" ~ 5.67,
        .x == "6:0-6:3" ~ 6,
        .x == "6:4-6:7" ~ 6.33,
        .x == "6:8-6:11" ~ 6.67,
        .x == "7:0-7:5" ~ 7,
        .x == "7:6-7:11" ~ 7.5,
        .x == "8:0-9:3" ~ 8,
        TRUE ~ NA_real_
      )
    )) %>%
    rename(group = agestrat,
           raw = !!sym(.x))
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
)))) %>% 
  filter(group != 8)

# Calculation of the manifest percentiles and subsequent normal rank
# transformation to determine location (proxy for a norm-referenced score, i.e.,
# NOT a raw score)

input <- rankByGroup(input, scale = "IQ") %>%
  mutate(across(normValue,
                ~
                  case_when(normValue > 120 ~ 120,
                            TRUE ~ .x)))


# Calculation of powers and interactions of
# location and grouping variable

input <- computePowers(input)


# Determining the best model with specified R2

model <- bestModel(input, terms = 3)


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


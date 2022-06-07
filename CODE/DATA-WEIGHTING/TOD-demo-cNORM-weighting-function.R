################################################################################
#' Tutorial on the usage of raking weights in regression-based norming
#' with cNORM
#'
#
#'
################################################################################

# set up ----------------------------------------------------------------
suppressMessages(suppressWarnings(library(cNORM)))
suppressMessages(suppressWarnings(library(here)))
suppressMessages(suppressWarnings(library(lubridate)))
suppressMessages(suppressWarnings(library(tidyverse)))

input_file_path <- "INPUT-FILES/DATA-WEIGHTING/"
output_file_path <- "OUTPUT-FILES/DATA-WEIGHTING/"
input_file_name <- "TOD-psychometrica"
item_names <- c(str_c("i", sprintf("%02d", 1:44)))

# Load data set, get continuous age variable, raw scores --------------------------------------
input <-   suppressMessages(read_csv(here(
  str_c(input_file_path, input_file_name, ".csv")
))) %>% 
  rename(i16 = i16i, gender = Gender) %>% 
  mutate(
  across(
    c(DOB, admin_date),
    ~
      mdy(.x)
  ),
  age = (DOB %--% admin_date) / years (1),
  iws_tot = rowSums(.[item_names])
) %>% 
  # drop gender = 3
  filter (gender != 3) %>% 
  # collapse categories to be weighted
  mutate(
    gender_w = gender,
    educ_w = case_when(
      HighestEducation == 1 ~ 1,
      TRUE ~ 2
    ), 
    ethnic_w = case_when(
      Ethnicity %in% c(1,3) ~ 1,
      TRUE ~ 2
    ) 
  )

# Demo proportions of input sample original variables
prop.table(xtabs(~gender, data = input))
table(input$gender)

prop.table(xtabs(~HighestEducation, data = input))
table(input$HighestEducation)

prop.table(xtabs(~Ethnicity, data = input))
table(input$Ethnicity)

prop.table(xtabs(~region, data = input))
table(input$region)

# Demo proportions of new vars for weighting
prop.table(xtabs(~gender_w, data = input))
table(input$gender_w)

prop.table(xtabs(~educ_w, data = input))
table(input$educ_w)

prop.table(xtabs(~ethnic_w, data = input))
table(input$ethnic_w)

# new data object has only vars to be weighted.
input_w <- input %>% 
  select(ID, age, gender_w, educ_w, ethnic_w, iws_tot) %>% 
  bind_cols(getGroups(.$age)) %>% 
  rename(group = ...7) 


# new input object for unweighted norming.
input_uw <- input %>% 
  select(ID, age, iws_tot) %>% 
  bind_cols(getGroups(.$age)) %>% 
  rename(group = ...4) 


# Secondly, assigning population marginals to variable marginals.ppvt
# Note: The marginals have to be in the shown format, especially the order
# of the columns must be the variables names first, secondly, the single levels
# of the variables, and finally the single proportions.
# Each level of each factor needs its own row.

marginals_input_w <-  data.frame(var = c("gender_w", "gender_w", 
                                         "educ_w", "educ_w", 
                                         "ethnic_w", "ethnic_w"),
                                 level = c(1, 2, 1, 2, 1, 2),
                                 prop = c(0.511, 0.489, 0.115, 0.885, 0.568, 0.432))
# View(marginals_input_w)

# Step 1: Compute and standardize raking weights -------------------------------

# Calculate standardized raking weights using computeWeights()
weights_input_w <- computeWeights(data = input_w, population.margins = marginals_input_w)


# Let's check, which weights resulted (just for demo; not necessary)
input_w$weights <- weights_input_w
norm_data_split <- input_w %>% group_by(gender_w, educ_w, ethnic_w) %>% summarize(weights = unique(weights))
View(norm_data_split)


# Step 2: ranking and modeling is done in a single step ------------------------
model_w <- cnorm(raw     =  input_w$iws_tot,
               group   = input_w$group,
               weights = weights_input_w, 
               terms = 4,
               scale = "IQ"
               )

# plot(model_w, "series", end = 8)
# checkConsistency(model_w)

tab_names <- c(
  "6.0-6.3",
  "6.4-6.7",
  "6.8-6.11",
  "7.0-7.3",
  "7.4-7.7",
  "7.8-7.11",
  "8.0-8.5",
  "8.6-8.11",
  "9.0-9.5",
  "9.6-9.11",
  "10.0-10.5",
  "10.6-10.11",
  "11.0-11.5",
  "11.6-11.11",
  "12.0-12.5",
  "12.6-12.11",
  "13.0-13.11",
  "14.0-14.11",
  "15.0-16.11",
  "17.0-18.11"
)

 norms_list <- rawTable(
  c(
    6.167,
    6.5,
    6.833,
    7.167,
    7.5,
    7.833,
    8.25,
    8.75,
    9.25,
    9.75,
    10.25,
    10.75,
    11.25,
    11.75,
    12.25,
    12.75,
    13.5,
    14.5,
    16,
    18.0
  ),
  model_w,
  step = 1,
  minNorm = 40,
  maxNorm = 130,
  minRaw = 1,
  maxRaw = 44,
  pretty = FALSE
) %>%
  set_names(tab_names) %>%
  map(~
        select(.x, raw, norm) %>%
        summarize(raw = raw,
                  ss = round(norm, 0)))


table_w <- norms_list %>%
  reduce(left_join,
         by = "raw") %>%
  set_names("raw", tab_names)

write_csv(table_w, 
          here(
            str_c(output_file_path, "input-w-raw-ss-lookup-table-age.csv")
          ))

# create unweighted lookup tables for comparison.

model_uw <- cnorm(raw     =  input_uw$iws_tot,
                  group   = input_uw$group,
                  terms = 4,
                  scale = "IQ"
                  )

norms_list <- rawTable(
  c(
    6.167,
    6.5,
    6.833,
    7.167,
    7.5,
    7.833,
    8.25,
    8.75,
    9.25,
    9.75,
    10.25,
    10.75,
    11.25,
    11.75,
    12.25,
    12.75,
    13.5,
    14.5,
    16,
    18.0
  ),
  model_uw,
  step = 1,
  minNorm = 40,
  maxNorm = 130,
  minRaw = 1,
  maxRaw = 44,
  pretty = FALSE
) %>%
  set_names(tab_names) %>%
  map(~
        select(.x, raw, norm) %>%
        summarize(raw = raw,
                  ss = round(norm, 0)))

table_uw <- norms_list %>%
  reduce(left_join,
         by = "raw") %>%
  set_names("raw", tab_names)

write_csv(table_uw, 
          here(
            str_c(output_file_path, "input-uw-raw-ss-lookup-table-age.csv")
          ))


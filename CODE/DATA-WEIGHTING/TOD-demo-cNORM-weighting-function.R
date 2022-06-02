################################################################################
#' Tutorial on the usage of raking weights in regression-based norming
#' with cNORM
#'
#'The script contains a step-by-step tutorial for the usage of raking weights
#'in regression-based norming with R package cNORM based on the norm sample
#'of the Peabody Picture Vocabulary Test (PPVT).
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
  select(ID, age, gender_w, educ_w, ethnic_w, iws_tot)

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
View(marginals.ppvt)

# Step 1: Compute and standardize raking weights -------------------------------

# Calculate standardized raking weights using computeWeights()
weights.ppvt <- computeWeights(data = norm.data, population.margins = marginals.ppvt)


# Let's check, which weights resulted (just for demo; not necessary)
norm.data$weights <- weights.ppvt
norm.data.split <- norm.data %>% group_by(sex, migration) %>% summarize(weights = unique(weights))
View(norm.data.split)


# Step 2: ranking and modeling is done in a single step ------------------------
model.ppvt <- cnorm(raw     =  norm.data$raw,
                    group   = norm.data$group,
                    weights = weights.ppvt)

# further steps like model selection
plot(model.ppvt, "subset")
normTable(c(5, 6, 7), model.ppvt)


# TOD: two-level demographic tracking

suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
suppressMessages(suppressWarnings(library(data.table))) # DATA READ-IN TOOLS
suppressMessages(library(reshape2)) # RESHAPE DATA FROM WIDE TO TALL
library(magrittr) # PIPE OPERATORS
# note use of `suppressWarnings` to silence chatter during interactive session
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(ggpmisc)) # EXTENSIONS TO ggplot2: ADD EQUATIONS AND FIT STATISTICS TO FITTED LINE PLOTS
library(ggrepel) # MORE ggplot2 EXTENSIONS

# Read input file of demos per case, sort by ageyear, compute agestrat, group by agestrat
# TOD_demos <- suppressMessages(read_csv(here('DATA/TOD_demos_input1.csv'))) %>%  
TOD_demos <- suppressMessages(read_csv(here('DATA/TOD_demos_input_oversample.csv'))) %>%  
  mutate(agestrat = case_when(
    ageyear == 6 ~ "06",
    ageyear == 7 ~ "07",
    ageyear == 8 ~ "08",
    ageyear == 9 ~ "09",
    ageyear >=10 & ageyear <=24 ~ as.character(ageyear),
    ageyear >=25 & ageyear <=40 ~ "2540",
    ageyear >=41 & ageyear <=50 ~ "4150",
    ageyear >=51 & ageyear <=60 ~ "5160",
    ageyear >=61 & ageyear <=70 ~ "6170",
    ageyear >=71 & ageyear <=80 ~ "7180",
    ageyear >=81 & ageyear <=90 ~ "8190",
    TRUE ~ NA_character_
  )) %>% arrange(agestrat) %>% group_by(agestrat)

# Initialize table of static columns: use bind_cols to paste tables side-by-side, there is no index var
# so must be sure that columns are sorted on same var prior to paste.
static_columns <-
  bind_cols(
    tibble(
      agestrat = c(
        "06",
        "07",
        "08",
        "09",
        "10",
        "11",
        "12",
        "13",
        "14",
        "15",
        "16",
        "17",
        "18",
        "19",
        "20",
        "21",
        "22",
        "23",
        "24",
        "2540",
        "4150",
        "5160",
        "6170",
        "7180",
        "8190"
      )
    ),
    # Note use of `data.table::fread` to read in .csv, takes the arguments `select = c(vector of cols to keep)`, or
    # `drop = c(vec of cols to drop)`
    fread(here("DATA/TOD_Target_n_by_age.csv"), select = c("target_n")),
    (
      fread(
        here("DATA/Age_x_gender_TOD_final.csv"),
        select = c("Male", "Female")
      ) %>%
        rename(Male_census_pct = Male, Female_census_pct = Female)
    ),
    (
      fread(
        here("DATA/Age_x_PEL_TOD_final.csv"),
        select = c("Less_than_HS", "HS_Degree", "Some_College", "BA_or_Higher")
      ) %>%
        rename(
          No_HS_deg_census_pct = Less_than_HS,
          HS_grad_census_pct = HS_Degree,
          Some_college_census_pct = Some_College,
          BA_plus_census_pct = BA_or_Higher
        )
    ),
    (
      fread(
        here("DATA/Age_x_race_TOD_final.csv"),
        select = c("White", "Black", "Asian", "Other", "Hispanic")
      ) %>%
        rename(
          White_census_pct = White,
          Black_census_pct = Black,
          Asian_census_pct = Asian,
          Other_multiracial_census_pct = Other,
          Hispanic_census_pct = Hispanic
        ) %>%
        select(
          Hispanic_census_pct,
          Asian_census_pct,
          Black_census_pct,
          White_census_pct,
          Other_multiracial_census_pct
        )
    ),
    (
    fread(
      here("DATA/Age_x_region_TOD_final.csv"),
      select = c("Northeast", "Midwest", "South", "West")
    ) %>%
      rename(
        Northeast_census_pct = Northeast,
        Midwest_census_pct = Midwest,
        South_census_pct = South,
        West_census_pct = West
      ) 
  )
)

# Individual demo input tables

# pull demo variable with agestrats from demo input file, spread gender values from single "gender" column 
# into multiple columns: (e.g., "Male" and "Female"), showing counts of each value of per agestrat.
gender_input <- TOD_demos %>% select(agestrat, gender)  %>%
  count(agestrat, gender) %>%
  spread(gender, n, fill = 0) %>%
  select(agestrat, male, female) %>%
  rename(Male_actual = male, Female_actual = female)

PEL_input <- TOD_demos %>% select(agestrat, PEL) %>%
  count(agestrat, PEL) %>%
  spread(PEL, n, fill = 0) %>%
  select(agestrat, No_HS_deg, HS_grad, Some_college, BA_plus) %>%
  rename(
    No_HS_deg_actual = No_HS_deg,
    HS_grad_actual = HS_grad,
    Some_college_actual = Some_college,
    BA_plus_actual = BA_plus
  )

ethnic_input <- TOD_demos %>% select(agestrat, ethnic) %>%
  count(agestrat, ethnic) %>%
  spread(ethnic, n, fill = 0) %>%
  select(agestrat, Hispanic, Asian, Black, White, Other) %>%
  rename(
    Hispanic_actual = Hispanic,
    Asian_actual = Asian,
    Black_actual = Black,
    White_actual = White,
    Other_multiracial_actual = Other
  )

region_input <- TOD_demos %>% select(agestrat, region) %>%
  count(agestrat, region) %>%
  spread(region, n, fill = 0) %>%
  select(agestrat, Northeast, South, Midwest, West) %>%
  rename(
    Northeast_actual = Northeast,
    South_actual = South,
    Midwest_actual = Midwest,
    West_actual = West
  )

# Initial char vec of final output columns in final order.
final_output_cols <- c( "agestrat", "target_n", "Male_census_pct", "Male_actual", 
                        "Male_needed", "Female_census_pct", "Female_actual", "Female_needed", 
                        "No_HS_deg_census_pct", "No_HS_deg_actual", "No_HS_deg_needed", 
                        "HS_grad_census_pct", "HS_grad_actual", "HS_grad_needed", "Some_college_census_pct", 
                        "Some_college_actual", "Some_college_needed", "BA_plus_census_pct", "BA_plus_actual", 
                        "BA_plus_needed", "Hispanic_census_pct", "Hispanic_actual", "Hispanic_needed", 
                        "Asian_census_pct", "Asian_actual", "Asian_needed", "Black_census_pct", "Black_actual", 
                        "Black_needed", "White_census_pct", "White_actual", "White_needed", 
                        "Other_multiracial_census_pct", "Other_multiracial_actual", "Other_multiracial_needed", 
                        "Northeast_census_pct", "Northeast_actual", "Northeast_needed", "South_census_pct", 
                        "South_actual", "South_needed", "Midwest_census_pct", "Midwest_actual", 
                        "Midwest_needed", "West_census_pct", "West_actual", "West_needed")

# Final output table joins static columns with actual counts from each demo cat,
# then calculates still needed count for each cat. To join multiple tables, put
# them into a list and then use `purrr::reduce` to apply `dplyr::left_join` over
# the list, indexing on a `by` var.
TOD_demo_tracking_output <-
  list(static_columns, gender_input, PEL_input, ethnic_input, region_input) %>% reduce(left_join, by = "agestrat") %>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  mutate(
    Male_needed = (target_n * Male_census_pct) - Male_actual,
    Female_needed = (target_n * Female_census_pct) - Female_actual,
    No_HS_deg_needed = (target_n * No_HS_deg_census_pct) - No_HS_deg_actual,
    HS_grad_needed = (target_n * HS_grad_census_pct) - HS_grad_actual,
    Some_college_needed = (target_n * Some_college_census_pct) - Some_college_actual,
    BA_plus_needed = (target_n * BA_plus_census_pct) - BA_plus_actual,
    Hispanic_needed = (target_n * Hispanic_census_pct) - Hispanic_actual,
    Asian_needed = (target_n * Asian_census_pct) - Asian_actual,
    Black_needed = (target_n * Black_census_pct) - Black_actual,
    White_needed = (target_n * White_census_pct) - White_actual,
    Other_multiracial_needed = (target_n * Other_multiracial_census_pct) - Other_multiracial_actual,
    Northeast_needed = (target_n * Northeast_census_pct) - Northeast_actual,
    South_needed = (target_n * South_census_pct) - South_actual,
    Midwest_needed = (target_n * Midwest_census_pct) - Midwest_actual,
    West_needed = (target_n * West_census_pct) - West_actual
    ) %>%
  select(final_output_cols) %>% 
  # `mutate_at` applies funs over columns designated by
  # `vars(contains("_needed"))`, i.e., only the columns specifying cases still
  # needed for a given demographic category, in this case it finds any negative
  # value in any cell, and recodes it to 0 using `case_when`, when it finds
  # non-negative values (the `TRUE` argument - equivalent to ELSE),  it doesn't
  # change the value: `.x` is a token for any cell value, `as.double` ensures
  # output of `TRUE` is same data type as input.
  mutate_at(vars(contains("_needed")),
            ~ case_when(
              .x < 0 ~ 0,
              TRUE ~ as.double(.x)
            )
  ) %>%
  # In this `mutate`, `pmax` is used to get the maximum of a set of values,
  # because the elements being evaluated are of different lengths, and using
  # `max` would return incorrect values.
  mutate(total_usable_cases = target_n - pmax((Male_needed + Female_needed),
                                             (No_HS_deg_needed + HS_grad_needed + Some_college_needed + BA_plus_needed),
                                             (Hispanic_needed + Asian_needed + Black_needed + White_needed + Other_multiracial_needed),
                                             (Northeast_needed + South_needed + Midwest_needed + West_needed))) %>%
  # save the rounding to the last step, so calculations can occur on unrounded
  # numbers, use `mutate_at` with `vars` and `funs` args to provide correct
  # arguments within pipe.
  mutate_at(vars(contains("_needed"), total_usable_cases), funs(round(., 0)))

# Write output appended with date
write_csv(TOD_demo_tracking_output, here(paste0("DATA/OUTPUT/TOD_demo_tracking_output_", format(Sys.Date(), "%Y-%m-%d"), ".csv")))


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
TOD_demos <- suppressMessages(read_csv(here('DATA/TOD_demos_input1.csv'))) %>%  
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
        select = c("male", "female")
      ) %>%
        rename(male_census_pct = male, female_census_pct = female)
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
          Other_Multiracial_census_pct = Other,
          Hispanic_census_pct = Hispanic
        ) %>%
        select(
          Hispanic_census_pct,
          Asian_census_pct,
          Black_census_pct,
          White_census_pct,
          Other_Multiracial_census_pct
        )
    ),
    (
    fread(
      here("DATA/Age_x_region_TOD_final.csv"),
      select = c("Northeast", "Midwest", "South", "West")
    ) %>%
      rename(
        northeast_census_pct = Northeast,
        midwest_census_pct = Midwest,
        south_census_pct = South,
        west_census_pct = West
      ) 
  )
)

# Individual demo input tables

# pull demo variable with agestrats from demo input file, spread gender values from single "gender" column 
# into multiple columns: (e.g., "male" and "female"), showing counts of each value of per agestrat.
gender_input <- TOD_demos %>% select(agestrat, gender)  %>%
  count(agestrat, gender) %>%
  spread(gender, n, fill = 0) %>%
  select(agestrat, male, female) %>%
  rename(male_actual = male, female_actual = female)

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

# NEXT RECODE NEGATIVE `_needed` NUMBERS TO 0.

# join tally of males/females per agestrat to column of all agestrats, target sample sizes, replace `NA` with 0
gender_output <-
  left_join(static_columns, gender_input, by = "agestrat") %>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  mutate(
    male_needed = round((target_n * male_census_pct) - male_actual, 0),
    female_needed = round((target_n * female_census_pct) - female_actual, 0)
  ) %>%
  select(
    agestrat,
    target_n,
    male_census_pct,
    male_actual,
    male_needed,
    female_census_pct,
    female_actual,
    female_needed
  )
# %>% 
#   mutate(still_needed = target_n - (male + female))
# 



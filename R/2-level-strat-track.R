# TOD: two-level demographic tracking

suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
library(reshape2) # RESHAPE DATA FROM WIDE TO TALL
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


# NEXT: MAKE THIS NEXT TABLE A STATIC COLUMN TABLE HOLDING ALL STATIC VALUES, INCLUDING CENSUS DEMO PERCENTAGES PER AGESTRAT

# Initialize columns of target sample size per agestrat.
target_n_age <- tibble(agestrat = c("06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", 
                                       "18", "19", "20", "21", "22", "23", "24", "2540", "4150", "5160", "6170", "7180", "8190"),
                          target_n = c(250, 250, 250, 250, 250, 225, 225, 225, 225, 225, 175, 175, 175, 175, 100, 
                                       100, 100, 75, 75, 75, 75, 75, 75, 50, 50))

# Individual demo input tables

# pull demo variable with agestrats from demo input file, spread gender values from single "gender" column 
# into multiple columns: (e.g., "male" and "female"), showing counts of each value of per agestrat.
gender_input <- TOD_demos %>% select(agestrat, gender)  %>%
  count(agestrat, gender) %>%
  spread(gender, n, fill = 0) %>% select(agestrat, male, female)

PEL_input <- TOD_demos %>% select(agestrat, PEL) %>%
  count(agestrat, PEL) %>%
  spread(PEL, n, fill = 0) %>% select(agestrat, No_HS_deg, HS_grad, Some_college, BA_plus)

ethnic_input <- TOD_demos %>% select(agestrat, ethnic) %>%
  count(agestrat, ethnic) %>%
  spread(ethnic, n, fill = 0) %>% select(agestrat, Hispanic, Asian, Black, White, Other)

region_input <- TOD_demos %>% select(agestrat, region) %>%
  count(agestrat, region) %>%
  spread(region, n, fill = 0) %>% select(agestrat, Northeast, South, Midwest, West)



# join tally of males/females per agestrat to column of all agestrats, target sample sizes, replace `NA` with 0
# gender_output <- left_join(target_n_age, gender_input) %>% mutate_if(is.numeric , replace_na, replace = 0) %>% 
#   mutate(still_needed = target_n - (male + female))
# 


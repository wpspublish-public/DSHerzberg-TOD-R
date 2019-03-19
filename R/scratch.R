# library(tidyverse)
# Initialize single-column table with agestrats represented by char vec that will sort in "numerical order"
agestrat_column <- tibble(agestrat = c("06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", 
                                         "18", "19", "20", "21", "22", "23", "24", "2540", "4150", "5160", "6170", "7180", "8190"),
                          target_n = c(250, 250, 250, 250, 250, 225, 225, 225, 225, 225, 175, 175, 175, 175, 100, 
                                       100, 100, 75, 75, 75, 75, 75, 75, 50, 50))

# tally actual counts
gender_input <- TOD_demos %>% select(agestrat, gender) 

# spread gender_input gender values from single "gender" column 
# into two columns: "male" and "female"
gender_input_spread <- gender_input %>%
  count(agestrat, gender) %>%
  spread(gender, n, fill = 0) %>% arrange(agestrat)

# join tally of males/females per agestrat to column of all agestrats,
# replace `NA` with 0
gender_output <- left_join(agestrat_column, gender_input_spread)
gender_output[is.na(gender_output)] <- 0

library(data.table)
target_n <- fread(here("DATA/TOD_Target_n_by_age.csv"), select = c("target_n"))

gender_census_pct <- fread(here("DATA/Age_x_gender_TOD_final.csv"), select = c("male","female")) %>% 
  rename(male_census_pct = male, female_census_pct = female)

region_census_pct <- fread(here("DATA/Age_x_region_TOD_final.csv"), select = c("Northeast","Midwest", "South", "West")) %>% 
  rename(northeast_census_pct = Northeast, midwest_census_pct = Midwest, south_census_pct = South, west_census_pct = West) %>% 
  select(northeast_census_pct, south_census_pct, midwest_census_pct, west_census_pct)



target_n = c(250, 250, 250, 250, 250, 225, 225, 225, 225, 225, 175, 175, 175, 175, 100, 
             100, 100, 75, 75, 75, 75, 75, 75, 50, 50),
male_census_pct = c(51.1, 51.1, 50.8, 51.1, 51.1, 50.8, 50.9, 51.3, 51.4, 51.2, 51.1, 51.2, 
                    50.9, 51.5, 51.6, 51.5, 51.6, 51.1, 51, 50.3, 49.5, 48.7, 47.4, 45, 39.1),
female_census_pct = c(48.9, 48.9, 49.2, 48.9, 48.9, 49.2, 49.1, 48.7, 48.6, 48.8, 48.9, 48.8, 
                      49.1, 48.5, 48.4, 48.5, 48.4, 48.9, 49, 49.7, 50.5, 51.3, 52.6, 55, 60.9))


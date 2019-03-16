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

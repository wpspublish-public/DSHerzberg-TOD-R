# library(tidyverse)
# Initialize single-column table with agestrats represented by char vec that will sort in "numerical order"
agestrat_column <- tibble(agestrat = c("05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", 
                                         "18", "19", "20", "21", "22", "23", "24", "2540", "4150", "5160", "6170", "7180", "8190"))

# tally actual counts
gender_input <- TOD_demos %>% select(agestrat, gender) 

# Here is input data: a table giving the age classification (agestrat - either a single age year or a range of years) and gender of 20 persons.

gender_input_spread <- gender_input %>%
  count(agestrat, gender) %>%
  spread(gender, n, fill = 0) %>% arrange(agestrat)
                 
gender_output <- left_join(agestrat_column, gender_input_spread)
gender_output[is.na(gender_output)] <- 0

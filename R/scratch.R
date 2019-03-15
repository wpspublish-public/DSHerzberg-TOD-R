library(tidyverse)
# Input agestrat levels as char vec that will sort in "numerical order"
gender_counts <- tibble(c("05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", 
              "18", "19", "20", "21", "22", "23", "24", "2540", "4150", "5160", "6170", "7180", "8190"))

agestrat <- c("05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", 
              "18", "19", "20", "21", "22", "23", "24", "2540", "4150", "5160", "6170", "7180", "8190")


# Assemble empty table to hold gender counts.
gender_counts <- tibble(agestrat)

# tally actual counts
gender_actuals <- as.data.frame(table(TOD_demos$gender))

gender_input <- TOD_demos %>% select(agestrat, gender) 

# Here is input data: a table giving the age classification (agestrat - either a single age year or a range of years) and gender of 20 persons.

df <- tibble(agestrat = c("05", "06", "06", "07", "07", "07", "07", "09", "11", "11", "12", "14", "17", "18", "18", "22", "24", "2540", "4150", "5160"),
                 gender = c("male", "female", "female", "male", "male", "male", "female", "female", "male", "female", "female", "male", "female", "female", "male", "male", "female", "male", "male", "female"))
   

df1 <- df %>%
  count(agestrat, gender) %>%
  spread(gender, n, fill = 0) %>% arrange(agestrat)
                 
df2 <- left_join(gender_counts, df1)
df2[is.na(df2)] <- 0

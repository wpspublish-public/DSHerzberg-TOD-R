suppressMessages(library(here))
suppressMessages(library(tidyverse))

temp1 <- suppressMessages(
  read_csv(
    here(
      "INPUT-FILES/OES-INPUT-TABLES/TOD-E/test-CI-lookup.csv"
    )
  )
) %>% 
  mutate(across(test, ~ str_to_lower(.)))
# ) %>% 
#   mutate(test1 = str_to_lower(test))

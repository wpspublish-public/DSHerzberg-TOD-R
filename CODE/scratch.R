suppressMessages(library(here))
suppressMessages(library(tidyverse))

temp1 <- suppressMessages(
  read_csv(
    here(
      str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-E/index_composites-age-lookup.csv")
    )
  )) %>% 
  pivot_longer(
    cols = -raw,
    names_to = "test",
    values_to = "ss"
  ) %>% 
  mutate(
    across(test,
           ~ str_sub(., 1, -5) %>% 
             str_to_lower()
    ),
    norm_group = "age"
    )

temp2 <- suppressMessages(
  read_csv(
    here(
      str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-E/index_composites-grade-lookup.csv")
    )
  )) %>% 
  pivot_longer(
    cols = -raw,
    names_to = "test",
    values_to = "ss"
  ) %>% 
  mutate(
    across(test,
           ~ str_sub(., 1, -7) %>% 
             str_to_lower()
    ),
    norm_group = "grade"
  )


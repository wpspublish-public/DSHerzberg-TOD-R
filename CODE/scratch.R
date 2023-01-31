suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))


temp1 <- read_excel(
  here(
    "INPUT-FILES/OES-INPUT-TABLES/TOD-S/PV ability to standard score-age.xlsx"
  )
)


temp2 <- temp1 %>% 
pivot_longer(
  cols = -raw,
  names_to = "age_strat",
  values_to = "ss"
)
  





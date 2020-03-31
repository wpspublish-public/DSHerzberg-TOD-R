suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(psych)) # DESCRIPTIVE TABLES

TOD_comp<- suppressMessages(read_csv(here(
  'INPUT-FILES/TOD-E_PVandLW_BLIMPcompare3.5.20.csv'
))) 

TOD_comp_desc <-
  TOD_comp %>% 
  select(contains('sum')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname) %>% 
  select(scale, n, mean, sd) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2)))

TOD_comp_desc <-
  TOD_comp %>% 
  select(contains('sum')) %>%
  cor()
  
# write_csv(Adult_Other_raw_desc, here(
#   paste0(
#     'OUTPUT-FILES/ADULT/DESCRIPTIVES/Adult-Other-raw-desc-',
#     format(Sys.Date(), "%Y-%m-%d"),
#     '.csv'
#   )
# ), 
# na = ''
# )

glimpse(TOD_comp)

# Prep data for BLIMP INPUT

suppressMessages(library(here))
suppressMessages(library(tidyverse))

id <- c('ID')
first_item <- c('i001')
last_item <- c('i534')
file_name <- c('TOD-E-missing-2020-03-05')

input_orig <- suppressMessages(read_csv(here(
  paste0('INPUT-FILES/', file_name, '.csv')
))) %>% 
  select(id, first_item:last_item)
names_input_orig <- names(input_orig)

NA_count <- sum(is.na(input_orig))
NA_count

input_orig[is.na(input_orig)] <- 999

input_gathered <- input_orig %>%
  gather('item','response',-id) %>% 
  group_by(!!sym(id)) %>% 
  arrange(!!sym(id)) %>% 
  mutate(item = as.factor(str_sub(item, 2, 4)))

write_csv(input_gathered,
          here(paste0(file_name, '-BLIMP-input.csv')),
          col_names = F
)

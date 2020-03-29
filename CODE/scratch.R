suppressMessages(library(tidyverse))

input <- tribble(
  ~id, ~item1, ~item2, ~item3, ~item4,
  'A', 0, 0, NA, NA, 
  'B', 0, 0, 0, 0, 
  'C', NA, 0, 1, 0, 
  'D', 1, 0, 1, 1,
  'E', 0, NA, 0, 0
)

output <- tribble(
  ~id, ~item1, ~item2, ~item3, ~item4,
  'A', 0, 0, NA, NA, 
  'B', 0, 0, 0, 0, 
  'E', 0, NA, 0, 0
)

items <- c(names(input))[-1]

all_0orNA_rows <- input %>% filter_at(names(input)[-1], ~(is.na(.) | . == 0))
identical(all_0orNA_rows, output)


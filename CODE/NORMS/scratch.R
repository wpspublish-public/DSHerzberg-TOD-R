suppressMessages(library(here))
suppressMessages(library(tidyverse))

file_name <- "Letter and Sight word Recognition age.csv"
input_file_path <- "PRINT-FORMAT-NORMS-TABLES/POST-cNORM-HAND-SMOOTHED-TABLES/"
# output_file_path <- "OUTPUT-FILES/NORMS/TODC_final_11.17.21_adult_fornorms/"

input <- suppressMessages(read_csv(here(str_c(
  input_file_path, file_name
))))

perc_ss_cols <- suppressMessages(read_csv(here(
  "PRINT-FORMAT-NORMS-TABLES/perc-ss-cols.csv"
)))

print_lookup_50 <- input %>% 
  select(raw, `5.0-5.3`) %>% 
  rename(ss = `5.0-5.3`) %>%
  right_join(perc_ss_cols, by = "ss") %>% 
  arrange(desc(ss)) %>% 
  select(perc, ss, raw)

##### NEXT STEP: TRANSFORM INPUT INTO A LIST OF DFS WITH IDENTICAL FORMAT 
# TO norms_list ON CNORM SCRIPT

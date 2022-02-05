suppressMessages(library(here))
suppressMessages(library(tidyverse))

test_names <- c("lske", "lswe", "rhme", "rlne", "sege", "snwe")
norm_type <- "age"
input_file_path <- "PRINT-FORMAT-NORMS-TABLES/POST-cNORM-HAND-SMOOTHED-TABLES/"
# output_file_path <- "OUTPUT-FILES/NORMS/TODC_final_11.17.21_adult_fornorms/"

input_files <- map(
  test_names,
  ~
  suppressMessages(read_csv(here(str_c(
  input_file_path, .x, "-", norm_type, ".csv"
))))
) %>% 
  set_names(test_names)

perc_ss_cols <- suppressMessages(read_csv(here(
  "PRINT-FORMAT-NORMS-TABLES/perc-ss-cols.csv"
)))

# print_lookup_50 <- input %>% 
#   select(raw, `5.0-5.3`) %>% 
#   rename(ss = `5.0-5.3`) %>%
#   right_join(perc_ss_cols, by = "ss") %>% 
#   arrange(desc(ss)) %>% 
#   select(perc, ss, raw)

age_strat <- input_files[[1]] %>% 
  select(-raw) %>% 
  names()




print_lookup_list <- input_files %>%
  map(~
        .x %>% 
  pivot_longer(contains("-"), names_to = "age_strat", values_to = "ss") %>%
  arrange(age_strat) %>%
  group_by(age_strat) %>%
  complete(ss = 40:130) %>%
  group_by(age_strat, ss) %>%
  filter(n() == 1 | n() > 1 & row_number()  %in% c(1, n())) %>%
  summarize(raw = str_c(raw, collapse = '--')) %>%
  mutate(across(raw, ~ case_when(is.na(.x) ~ '-', TRUE ~ .x))) %>%
  arrange(age_strat, desc(ss)) %>%
  pivot_wider(names_from = age_strat,
              values_from = raw) %>%
  filter(!is.na(ss)) %>%
  right_join(perc_ss_cols, by = "ss") %>%
  relocate(perc, .before = "ss")
) %>% 
  set_names(test_names)

temp1 <- print_lookup_list[["lske"]]

# age_strat_dfs <-  
#   map(
#     age_strat,
#     ~
#       temp1 %>% 
#       select(perc, ss, !!sym(.x))
#   ) %>%
#   set_names(age_strat)

# nested map calls
age_strat_dfs <-  print_lookup_list %>%
  map( ~
         map(age_strat,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(age_strat))

# NEXT: AT THIS STAGE WE HAVE A LIST OF LISTS
# OUTER LIST: EACH ELEMENT IS A LIST FOR A SINGLE TEST
# INNER LISTS: EACH TEST LIST CONTAINS THE DFS FOR EACH AGE STRAT
# TASK: RENAME ELEMENTS OF INNER LIST SO THAT NAMES INCLUDE TEST AND AGESTRAT


temp1 <-
  map(test_names,
      ~
        age_strat_dfs %>%
        pluck(.x, "5.0-5.3")) %>% 
  set_names(test_names)


# # nested map calls
# df_list <- input_files %>%
#   map(~
#         map(age_strat,
#             ~
#               .y %>%
#               select(raw, !!sym(.x)) %>%
#               rename(ss = !!sym(.x)), .y = .x) %>%
#         set_names(age_strat))





write_csv(
  lookup_print,
  here("PRINT-FORMAT-NORMS-TABLES/print-format-lookup-example.csv"),
  na = ""
)


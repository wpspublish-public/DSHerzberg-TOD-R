temp1 <- keep(grade_test_cols_flat, 
              str_detect(names(grade_test_cols_flat), "^1-Spring")
              )


temp2 <- keep(grade_test_cols_flat, 
              str_detect(names(grade_test_cols_flat), startsWith(.x, "1-Spring"))
)


grade_strat1 <- input_files_ta[[1]] %>% 
  select(-raw) %>% 
  names() %>% 
  # data.frame()
  tibble() %>% 
  mutate(new_strat = case_when(
    # str_detect(., "K|12") ~ "1",
    str_detect(., "!(K|12)") ~ "1",
    TRUE ~ .
  ))
  pull(.)



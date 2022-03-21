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
  mutate(across(., 
           ~
           case_when(
    # str_detect(., "K|12") ~ "1",
    str_detect(., "^(?!(K|10|11|12)).*$") ~ str_c("0", .),
    TRUE ~ .
  )))
  pull(.)

  temp5 <- map2(input_files_ta[3],
                list(new_names_input_qrf),
                ~ {
                  n <- .y
                  rename_with(.x,
                              ~
                                n,
                              everything())})
  
  
  print_lookups_at <- map2(grade_test_cols_at,
                           c(rep(list(output_test_names1), 4), 
                             rep(list(output_test_names2), 22)),
        ~ {
          n <- .y
          .x %>% 
        reduce(left_join, by = c("perc", "ss")) %>%
        rename_with(~ n, contains("-"))}
    ) %>% 
    set_names(grade_strat)
  
  
  
  
  
  mutate(data = map2(data, year, ~  {
    yr <- .y
    .x %>% rename_with(~ str_remove(., str_c("_", yr)), everything())
  }))
  
  
  temp6 <- input_files_ta[[3]] %>%
    rename_with(~new_names_input_qrf,
                everything())
  
  
  new_names_input <- input_files_ta[[1]] %>%
    names() %>%
    tibble() %>%
    mutate(across(.,
                  ~
                    case_when(
                      str_detect(., "^(?!(r|K|10|11|12)).*$") ~ str_c("0", .),
                      TRUE ~ .
                    ))) %>%
    pull(.)
  grade_strat <- new_names_input[2:length(new_names_input)]
  
  
  new_names_input_qrf <- input_files_ta[[3]] %>%
    names() %>%
    tibble() %>%
    mutate(across(.,
                  ~
                    case_when(
                      str_detect(., "^(?!(r|K|10|11|12)).*$") ~ str_c("0", .),
                      TRUE ~ .
                    ))) %>%
    pull(.)
  grade_strat_qrf <- new_names_input_qrf[2:length(new_names_input_qrf)]
  
  new_names_input_wrf <- input_files_ta[[4]] %>%
    names() %>%
    tibble() %>%
    mutate(across(.,
                  ~
                    case_when(
                      str_detect(., "^(?!(r|K|10|11|12)).*$") ~ str_c("0", .),
                      TRUE ~ .
                    ))) %>%
    pull(.)
  grade_strat_wrf <- new_names_input_wrf[2:length(new_names_input_wrf)]
  
  
  

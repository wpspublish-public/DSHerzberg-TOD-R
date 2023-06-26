index_composites_lookups <-
  list(
    suppressMessages(read_csv(here(
      str_c(
        "INPUT-FILES/OES-INPUT-TABLES/TOD-C/indexcomposites-age-lookup.csv"
      )
    ))) %>%
      pivot_longer(
        cols = -raw,
        names_to = "test",
        values_to = "ss"
      ) %>%
      mutate(across(test,
                    ~ str_to_lower(.)),
             norm_group = "age") %>% 
      left_join(cv_lookup_index_composites_age_grade, by = "test"), 
    suppressMessages(read_csv(here(
      str_c(
        "INPUT-FILES/OES-INPUT-TABLES/TOD-C/indexcomposites-grade-lookup.csv"
      )
    ))) %>%
      pivot_longer(
        cols = -raw,
        names_to = "test",
        values_to = "ss"
      ) %>%
      mutate(across(test,
                    ~ str_to_lower(.)),
             norm_group = "grade") %>% 
      left_join(cv_lookup_index_composites_age_grade, by = "test"), 
    suppressMessages(read_csv(here(
      str_c(
        "INPUT-FILES/OES-INPUT-TABLES/TOD-C/indexcomposites-adult-lookup.csv"
      )
    ))) %>%
      pivot_longer(
        cols = -raw,
        names_to = "test",
        values_to = "ss"
      ) %>%
      mutate(across(test,
                    ~ str_to_lower(.)),
             norm_group = "adult") %>% 
      left_join(cv_lookup_index_composites_adult, by = "test")
  ) %>%
  bind_rows() %>%
  left_join(ss_percentile_lookup, by = "ss")
  

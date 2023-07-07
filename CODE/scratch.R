temp2 <-
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
  left_join(ss_percentile_lookup, by = "ss") %>% 
  mutate(
    prob = case_when(
      test %in% c("ddiq", "ddiw") &
        between(ss, 120, 130) ~ "Extremely Low Probability of Dyslexia",
      test %in% c("ddiq", "ddiw") &
        between(ss, 110, 119) ~ "Very Low Probability of Dyslexia",
      test %in% c("ddiq", "ddiw") &
        between(ss, 90, 109) ~ "Low to Moderate Probability of Dyslexia",
      test %in% c("ddiq", "ddiw") &
        between(ss, 80, 89) ~ "High Probability of Dyslexia",
      test %in% c("ddiq", "ddiw") &
        between(ss, 70, 79) ~ "Very High Probability of Dyslexia",
      test %in% c("ddiq", "ddiw") &
        between(ss, 40, 69) ~ "Extremely High Probability of Dyslexia",
      TRUE ~ NA_character_
    # ),
    # prob = case_when(
    #   (test == "ddiq" | test =="ddiw") &
    #     between(ss, 120, 130) ~ "Extremely Low Probability of Dyslexia",
    #   (test == "ddiq" | test =="ddiw") &
    #     between(ss, 110, 119) ~ "Very Low Probability of Dyslexia",
    #   (test == "ddiq" | test =="ddiw") &
    #     between(ss, 90, 109) ~ "Low to Moderate Probability of Dyslexia",
    #   (test == "ddiq" | test =="ddiw") &
    #     between(ss, 80, 89) ~ "High Probability of Dyslexia",
    #   (test == "ddiq" | test =="ddiw") &
    #     between(ss, 70, 79) ~ "Very High Probability of Dyslexia",
    #   (test == "ddiq" | test =="ddiw") &
    #     between(ss, 40, 69) ~ "Extremely High Probability of Dyslexia",
    #   TRUE ~ NA_character_
    )) %>% 
  drop_na(ss) %>% 
  arrange(match(norm_group, age_grade_order), match(test, age_grade_index_composites), raw)
  
  

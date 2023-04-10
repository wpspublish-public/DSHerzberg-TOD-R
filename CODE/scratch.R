index_composites_lookups <-
  list(
    suppressMessages(read_csv(here(
      str_c(
        "INPUT-FILES/OES-INPUT-TABLES/TOD-E/index_composites-age-lookup.csv"
      )
    ))) %>%
      pivot_longer(
        cols = -raw,
        names_to = "test",
        values_to = "ss"
      ) %>%
      mutate(across(test,
                    ~ str_sub(., 1,-5) %>%
                      str_to_lower()),
             norm_group = "age"),
    suppressMessages(read_csv(here(
      str_c(
        "INPUT-FILES/OES-INPUT-TABLES/TOD-E/index_composites-grade-lookup.csv"
      )
    ))) %>%
      pivot_longer(
        cols = -raw,
        names_to = "test",
        values_to = "ss"
      ) %>%
      mutate(across(test,
                    ~ str_sub(., 1,-7) %>%
                      str_to_lower()),
             norm_group = "grade")
  ) %>%
  bind_rows() %>%
  left_join(ss_percentile_lookup, by = "ss") %>%
  left_join(cv_lookup, by = "test") %>% 
  mutate(
    risk = case_when(
      test %in% c("eddiq", "eddiw") &
        between(ss, 120, 130) ~ "Extremely Low Probability of Dyslexia",
      test %in% c("eddiq", "eddiw") &
        between(ss, 110, 119) ~ "Very Low Probability of Dyslexia",
      test %in% c("eddiq", "eddiw") &
        between(ss, 90, 109) ~ "Low to Moderate Probability of Dyslexia",
      test %in% c("eddiq", "eddiw") &
        between(ss, 80, 89) ~ "High Probability of Dyslexia",
      test %in% c("eddiq", "eddiw") &
        between(ss, 70, 79) ~ "Very High Probability of Dyslexia",
      test %in% c("eddiq", "eddiw") &
        between(ss, 40, 69) ~ "Extremely High Probability of Dyslexia",
      TRUE ~ NA_character_
    ),
    desc_range = case_when(
      between(ss, 120, 130) ~ "Well Above Average",
      between(ss, 110, 119) ~ "Above Average",
      between(ss, 90, 109) ~ "Average",
      between(ss, 80, 89) ~ "Below Average",
      between(ss, 70, 79) ~ "Well Below Average",
      between(ss, 40, 69) ~ "Significantly Below Average",
      TRUE ~ NA_character_
    ), 
    CI90_LB_pre = ss - CV_90,
    CI90_UB_pre = ss + CV_90,
    CI95_LB_pre = ss - CV_95,
    CI95_UB_pre = ss + CV_95,
    CI90_LB = as.character(case_when(
      CI90_LB_pre < 40 ~ 40,
      TRUE ~ CI90_LB_pre
    )), 
    CI90_UB = as.character(case_when(
      CI90_UB_pre > 130 ~ 130,
      TRUE ~ CI90_UB_pre
    )), 
    CI95_LB = as.character(case_when(
      CI95_LB_pre < 40 ~ 40,
      TRUE ~ CI95_LB_pre
    )), 
    CI95_UB = as.character(case_when(
      CI95_UB_pre > 130 ~ 130,
      TRUE ~ CI95_UB_pre
    )), 
    CI90 = str_c(CI90_LB, CI90_UB, sep = " - "), 
    CI95 = str_c(CI95_LB, CI95_UB, sep = " - ") 
  ) %>% 
  select(norm_group, test, raw, ss, CI90, CI95, percentile, desc_range, risk) %>% 
  rename(
    score = test
  ) %>% 
  arrange(norm_group, match(score, index_composites), raw) %>% 
  write_csv(
    here(
      "OUTPUT-FILES/OES-INPUT-TABLES/TOD-E/TOD-E-OES-age-lookup-table.csv"
    ),
    na = ""
  )

  



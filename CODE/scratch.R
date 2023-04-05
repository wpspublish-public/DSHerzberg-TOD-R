grade_lookups <- map(
  test_grade_stems,
  ~
    suppressMessages(
      read_csv(
        here(
          str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-E/", .x, "-lookup.csv")
        )
      )) %>% 
    pivot_longer(
      cols = -raw,
      names_to = "age_grade",
      values_to = "ss"
    ) %>% 
    mutate(
      test = str_sub(.x, 1, -7),
    ) %>% 
    select(test, age_grade, raw, ss) %>% 
    arrange(test, 
            match(age_grade, age_grade_order), raw)
) %>% 
  bind_rows() %>%
  left_join(ss_percentile_lookup, by = "ss") %>%
  left_join(cv_lookup, by = "test") %>% 
  mutate(
    grade_equiv = case_when(
     test == "spwe"  & between(raw, 0, 8) ~ "< start grade",
     test == "spwe"  & between(raw, 9, 12) ~ "K-Fall",
     test == "spwe"  & between(raw, 13, 14) ~ "K-Spring",
     test == "spwe"  & between(raw, 15, 17) ~ "1-Fall",
     test == "spwe"  & between(raw, 18, 20) ~ "1-Spring",
     test == "spwe"  & between(raw, 21, 22) ~ "2-Fall",
     test == "spwe"  & between(raw, 23, 25) ~ "2-Spring",
     test == "spwe"  & between(raw, 26, 32) ~ "> stop grade",
     test == "rhye"  & between(raw, 0, 5) ~ "< start grade",
     test == "rhye"  & between(raw, 6, 10) ~ "K-Fall",
     test == "rhye"  & between(raw, 11, 14) ~ "K-Spring",
     test == "rhye"  & between(raw, 15, 18) ~ "1-Fall",
     test == "rhye"  & between(raw, 19, 21) ~ "1-Spring",
     test == "rhye"  & between(raw, 22, 24) ~ "2-Fall",
     test == "rhye"  & between(raw, 25, 26) ~ "2-Spring",
     test == "rhye"  & between(raw, 27, 30) ~ "> stop grade",
     test == "ernle"  & between(raw, 0, 35) ~ "< start grade",
     test == "ernle"  & between(raw, 36, 46) ~ "K-Fall",
     test == "ernle"  & between(raw, 47, 55) ~ "K-Spring",
     test == "ernle"  & between(raw, 56, 64) ~ "1-Fall",
     test == "ernle"  & between(raw, 65, 71) ~ "1-Spring",
     test == "ernle"  & between(raw, 72, 78) ~ "2-Fall",
     test == "ernle"  & between(raw, 79, 88) ~ "2-Spring",
     test == "ernle"  & between(raw, 89, 120) ~ "> stop grade",
     test == "lswe"  & between(raw, 0, 11) ~ "< start grade",
     test == "lswe"  & between(raw, 12, 15) ~ "K-Fall",
     test == "lswe"  & between(raw, 16, 20) ~ "K-Spring",
     test == "lswe"  & between(raw, 21, 26) ~ "1-Fall",
     test == "lswe"  & between(raw, 27, 31) ~ "1-Spring",
     test == "lswe"  & between(raw, 32, 33) ~ "2-Fall",
     test == "lswe"  & between(raw, 34, 36) ~ "2-Spring",
     test == "lswe"  & between(raw, 37, 38) ~ "> stop grade",
     test == "esege"  & between(raw, 0, 8) ~ "< start grade",
     test == "esege"  & between(raw, 9, 12) ~ "K-Fall",
     test == "esege"  & between(raw, 13, 16) ~ "K-Spring",
     test == "esege"  & between(raw, 17, 18) ~ "1-Fall",
     test == "esege"  & between(raw, 19, 20) ~ "1-Spring",
     test == "esege"  & between(raw, 21, 21) ~ "2-Fall",
     test == "esege"  & between(raw, 22, 22) ~ "2-Spring",
     test == "esege"  & between(raw, 23, 25) ~ "> stop grade",
     test == "lske"  & between(raw, 0, 13) ~ "< start grade",
     test == "lske"  & between(raw, 14, 18) ~ "K-Fall",
     test == "lske"  & between(raw, 19, 23) ~ "K-Spring",
     test == "lske"  & between(raw, 24, 27) ~ "1-Fall",
     test == "lske"  & between(raw, 28, 29) ~ "1-Spring",
     test == "lske"  & between(raw, 30, 31) ~ "2-Fall",
     test == "lske"  & between(raw, 32, 33) ~ "2-Spring",
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
  select(test, age_grade, raw, ss, CI90, CI95, percentile, grade_equiv, desc_range) %>% 
  rename(grade = age_grade) %>% 
  write_csv(
    here(
      "OUTPUT-FILES/OES-INPUT-TABLES/TOD-E/TOD-E-OES-grade-lookup-table.csv"
    ),
    na = ""
  )

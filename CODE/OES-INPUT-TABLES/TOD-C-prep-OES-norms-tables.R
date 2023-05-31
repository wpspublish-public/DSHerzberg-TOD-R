suppressMessages(library(here))
suppressMessages(library(tidyverse))

tests <- c("PHMC", "IWSC", "RLNC", "PWRC", "WPCC", "WMC", "PANC", "IWRC", "OREC", "BLNC", "SEGC", 
           "RWSC", "SRE1C", "SRE2C", "RNLC", "LMC", "RPWC", "RIWC", "SSLC", "LVC", "GANC")
age_grade_index_composites <- c("DDIW", "DDIQ", "LPI", "RSIW", "RSIQ", "SWA", "PK", "BRS", "DE", "SP", "RFW", 
                                "RFQ", "RCQ1", "RCQ2", "PA", "RAN", "AWM", "OP", "VO", "RE", "VR2", "VR4")
adult_index_composites <- c("DDIQ", "LPI", "RSIQ", "SWA", "PK", "BRS", "DE", "SP", "RFQ", "RCQ2", "PA", 
                            "RAN", "AWM", "OP", "VO", "RE", "VR2", "VR4")
index_composites <- c(age_grade_index_composites, adult_index_composites)
all_scores <- c(tests, index_composites)
norm_groups <- c("age", "grade")
test_age_stems <- str_c(tests, "-age")
test_grade_stems <- str_c(tests, "-grade")
age_grade_order <-
  c(
    "6.0-6.3", 
    "6.4-6.7", 
    "6.6-6.11", 
    "7.0-7.3", 
    "7.4-7.7", 
    "7.6-7.11", 
    "8.0-8.5", 
    "8.6-8.11", 
    "9.0-9.5", 
    "9.6-9.11", 
    "10.0-10.5", 
    "10.6-10.11", 
    "11.0-11.5", 
    "11.6-11.11", 
    "12.0-12.5", 
    "12.6-12.11", 
    "13.0-13.11", 
    "14.0-14.11", 
    "15.0-16.11", 
    "17.0-18.11",
    "18.0-23.11", 
    "24.0-39.11", 
    "40.0-49.11", 
    "50.0-59.11", 
    "60.0-69.11", 
    "70.0-89.11", 
    "1-Fall", 
    "1-Spring", 
    "2-Fall", 
    "2-Spring", 
    "3-Fall", 
    "3-Spring", 
    "4-Fall", 
    "4-Spring", 
    "5-Fall", 
    "5-Spring", 
    "6-Fall", 
    "6-Spring", 
    "7-Fall", 
    "7-Spring", 
    "8-Fall", 
    "8-Spring", 
    "9-Fall", 
    "9-Spring", 
    "10-Fall", 
    "10-Spring", 
    "11-Fall", 
    "11-Spring", 
    "12-Fall", 
    "12-Spring"
  )

ss_percentile_lookup <- suppressMessages(
  read_csv(
    here(
      "INPUT-FILES/OES-INPUT-TABLES/TOD-E/SS-to-percentile.csv"
    )
  )
) %>%
  rename(ss = SS)

age_mo_min_max_lookup <- suppressMessages(
  read_csv(
    here(
      "INPUT-FILES/OES-INPUT-TABLES/TOD-E/age-range-TODE.csv"
    )
  )
) %>%
  rename(age_grade = original_age_range)

cv_lookup <- bind_rows(
  suppressMessages(
    read_csv(
      here(
        "INPUT-FILES/OES-INPUT-TABLES/TOD-E/test-CI-lookup.csv"
      )
    )
  ) %>% 
    mutate(across(test, ~ str_to_lower(.))) %>% 
    rename(CV_90 = CI90, CV_95 = CI95), 
  suppressMessages(
    read_csv(
      here(
        "INPUT-FILES/OES-INPUT-TABLES/TOD-E/index_composites-CI-lookup.csv"
      )
    )
  ) %>% 
    mutate(across(test, ~ str_to_lower(.))) %>% 
    rename(CV_90 = CI90, CV_95 = CI95)
)

#### OUTPUT FOR AGE LOOKUP TABLES

age_lookups <- map(
  test_age_stems,
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
      test = str_sub(.x, 1, -5),
    ) %>% 
    select(test, age_grade, raw, ss) %>% 
    arrange(test, 
            match(age_grade, age_grade_order), raw)
) %>% 
  bind_rows() %>%
  left_join(ss_percentile_lookup, by = "ss") %>%
  left_join(cv_lookup, by = "test") %>% 
  left_join(age_mo_min_max_lookup, by = "age_grade") %>% 
  mutate(
    equiv = case_when(
      test == "spwe"  & between(raw, 0, 7) ~ "< start age",
      test == "spwe"  & between(raw, 8, 10) ~ "5:0 to 5:3",
      test == "spwe"  & between(raw, 11, 12) ~ "5:4 to 5:7",
      test == "spwe"  & between(raw, 13, 13) ~ "5:8 to 5:11",
      test == "spwe"  & between(raw, 14, 15) ~ "6:0 to 6:5",
      test == "spwe"  & between(raw, 16, 18) ~ "6:6 to 6:11",
      test == "spwe"  & between(raw, 19, 20) ~ "7:0 to 7:5",
      test == "spwe"  & between(raw, 21, 22) ~ "7:6 to 7:11",
      test == "spwe"  & between(raw, 23, 24) ~ "8:0 to 8:5",
      test == "spwe"  & between(raw, 25, 26) ~ "8:6 to 9:3",
      test == "spwe"  & between(raw, 27, 32) ~ "> stop age",
      test == "rhye"  & between(raw, 0, 3) ~ "< start age",
      test == "rhye"  & between(raw, 4, 6) ~ "5:0 to 5:3",
      test == "rhye"  & between(raw, 7, 9) ~ "5:4 to 5:7",
      test == "rhye"  & between(raw, 10, 12) ~ "5:8 to 5:11",
      test == "rhye"  & between(raw, 13, 16) ~ "6:0 to 6:5",
      test == "rhye"  & between(raw, 17, 20) ~ "6:6 to 6:11",
      test == "rhye"  & between(raw, 21, 23) ~ "7:0 to 7:5",
      test == "rhye"  & between(raw, 24, 24) ~ "7:6 to 7:11",
      test == "rhye"  & between(raw, 25, 25) ~ "8:0 to 8:5",
      test == "rhye"  & between(raw, 26, 27) ~ "8:6 to 9:3",
      test == "rhye"  & between(raw, 28, 30) ~ "> stop age",
      test == "ernle"  & between(raw, 0, 32) ~ "< start age",
      test == "ernle"  & between(raw, 33, 40) ~ "5:0 to 5:3",
      test == "ernle"  & between(raw, 41, 44) ~ "5:4 to 5:7",
      test == "ernle"  & between(raw, 45, 50) ~ "5:8 to 5:11",
      test == "ernle"  & between(raw, 51, 56) ~ "6:0 to 6:5",
      test == "ernle"  & between(raw, 57, 65) ~ "6:6 to 6:11",
      test == "ernle"  & between(raw, 66, 73) ~ "7:0 to 7:5",
      test == "ernle"  & between(raw, 74, 79) ~ "7:6 to 7:11",
      test == "ernle"  & between(raw, 80, 88) ~ "8:0 to 8:5",
      test == "ernle"  & between(raw, 89, 99) ~ "8:6 to 9:3",
      test == "ernle"  & between(raw, 100, 120) ~ "> stop age",
      test == "lswe"  & between(raw, 0, 9) ~ "< start age",
      test == "lswe"  & between(raw, 10, 12) ~ "5:0 to 5:3",
      test == "lswe"  & between(raw, 13, 15) ~ "5:4 to 5:7",
      test == "lswe"  & between(raw, 16, 18) ~ "5:8 to 5:11",
      test == "lswe"  & between(raw, 19, 22) ~ "6:0 to 6:5",
      test == "lswe"  & between(raw, 23, 27) ~ "6:6 to 6:11",
      test == "lswe"  & between(raw, 28, 31) ~ "7:0 to 7:5",
      test == "lswe"  & between(raw, 32, 33) ~ "7:6 to 7:11",
      test == "lswe"  & between(raw, 34, 35) ~ "8:0 to 8:5",
      test == "lswe"  & between(raw, 36, 38) ~ "8:6 to 9:3",
      test == "esege"  & between(raw, 0, 4) ~ "< start age",
      test == "esege"  & between(raw, 5, 9) ~ "5:0 to 5:3",
      test == "esege"  & between(raw, 10, 12) ~ "5:4 to 5:7",
      test == "esege"  & between(raw, 13, 14) ~ "5:8 to 5:11",
      test == "esege"  & between(raw, 15, 17) ~ "6:0 to 6:5",
      test == "esege"  & between(raw, 18, 19) ~ "6:6 to 6:11",
      test == "esege"  & between(raw, 20, 20) ~ "7:0 to 7:5",
      test == "esege"  & between(raw, 21, 21) ~ "7:6 to 7:11",
      test == "esege"  & between(raw, 22, 22) ~ "8:0 to 8:5",
      test == "esege"  & between(raw, 23, 23) ~ "8:6 to 9:3",
      test == "esege"  & between(raw, 24, 25) ~ "> stop age",
      test == "lske"  & between(raw, 0, 9) ~ "< start age",
      test == "lske"  & between(raw, 10, 14) ~ "5:0 to 5:3",
      test == "lske"  & between(raw, 15, 17) ~ "5:4 to 5:7",
      test == "lske"  & between(raw, 18, 21) ~ "5:8 to 5:11",
      test == "lske"  & between(raw, 22, 24) ~ "6:0 to 6:5",
      test == "lske"  & between(raw, 25, 28) ~ "6:6 to 6:11",
      test == "lske"  & between(raw, 29, 30) ~ "7:0 to 7:5",
      test == "lske"  & between(raw, 31, 31) ~ "7:6 to 7:11",
      test == "lske"  & between(raw, 32, 32) ~ "8:0 to 8:5",
      test == "lske"  & between(raw, 33, 33) ~ "8:6 to 9:3",
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
  select(test, age_min, age_max, raw, ss, CI90, CI95, percentile, equiv, desc_range) %>% 
  rename(
    agemon_min = age_min,
    agemon_max = age_max,
    age_equiv = equiv
  ) %>% 
  write_csv(
    here(
      "OUTPUT-FILES/OES-INPUT-TABLES/TOD-E/TOD-E-OES-age-lookup-table.csv"
    ),
    na = ""
  )

#### OUTPUT FOR GRADE LOOKUP TABLES

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

#### OUTPUT FOR INDEX_COMPOSITE LOOKUP TABLES

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
  drop_na(ss) %>% 
  write_csv(
    here(
      "OUTPUT-FILES/OES-INPUT-TABLES/TOD-E/TOD-E-OES-index-composite-lookup-table.csv"
    ),
    na = ""
  )





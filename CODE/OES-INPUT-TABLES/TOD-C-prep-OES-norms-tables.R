suppressMessages(library(here))
suppressMessages(library(tidyverse))

tests <- c("PHMC", "IWSC", "RLNC", "PWRC", "WPCC", "WMC", "PANC", "IWRC", "OREC", "BLNC", "SEGC", 
           "RWSC", "SRE1C", "SRE2C", "RNLC", "LMC", "RPWC", "RIWC", "SSLC", "LVC", "GANC") %>% 
  str_to_lower(.)
age_grade_index_composites <- c("DDIW", "DDIQ", "LPI", "RSIW", "RSIQ", "SWA", "PK", "BRS", "DE", "SP", "RFW", 
                                "RFQ", "RCQ1", "RCQ2", "PA", "RAN", "AWM", "OP", "VO", "RE", "VR2", "VR4") %>% 
  str_to_lower(.)
adult_index_composites <- c("DDIQ", "LPI", "RSIQ", "SWA", "PK", "BRS", "DE", "SP", "RFQ", "RCQ2", "PA", 
                            "RAN", "AWM", "OP", "VO", "RE", "VR2", "VR4") %>% 
  str_to_lower(.)
index_composites <- c(age_grade_index_composites, adult_index_composites)
all_scores <- c(tests, index_composites)
norm_groups <- c("age", "grade")
test_age_stems <- str_c(tests, "-age")[!str_detect(str_c(tests, "-age"), "orec-age")]
test_grade_stems <- str_c(tests, "-grade")
age_grade_order <-
  c(
    "6.0-6.3", 
    "6.4-6.7", 
    "6.8-6.11", 
    "7.0-7.3", 
    "7.4-7.7", 
    "7.8-7.11", 
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
      "INPUT-FILES/OES-INPUT-TABLES/TOD-C/SS-to-percentile.csv"
    )
  )
) %>%
  rename(ss = SS)

age_mo_min_max_lookup <- bind_rows(suppressMessages(read_csv(
  here("INPUT-FILES/OES-INPUT-TABLES/TOD-C/age-range-child.csv")
)) ,
suppressMessages(read_csv(
  here("INPUT-FILES/OES-INPUT-TABLES/TOD-C/age-range-adult.csv")
))) %>%
  rename(age_grade = original_age_range)

cv_lookup <- bind_rows(
  suppressMessages(
    read_csv(
      here(
        "INPUT-FILES/OES-INPUT-TABLES/TOD-C/test-CI-lookup.csv"
      )
    )
  ) %>% 
    rename(CV_90 = CI90, CV_95 = CI95) %>% 
    mutate(source = "test"), 
  suppressMessages(
    read_csv(
      here(
        "INPUT-FILES/OES-INPUT-TABLES/TOD-C/indexcomposites-CI-child-lookup.csv"
      )
    )
  ) %>% 
    mutate(across(test, ~ str_to_lower(.))) %>% 
    rename(CV_90 = CI90, CV_95 = CI95) %>% 
    mutate(source = "indexcomposites_child"), 
  suppressMessages(
  read_csv(
    here(
      "INPUT-FILES/OES-INPUT-TABLES/TOD-C/indexcomposites-CI-adult-lookup.csv"
    )
  )
) %>% 
  mutate(across(test, ~ str_to_lower(.))) %>% 
  rename(CV_90 = CI90, CV_95 = CI95) %>% 
  mutate(source = "indexcomposites_adult"), 
) %>% 
  select(test, source, CV_90, CV_95)

#### OUTPUT FOR AGE LOOKUP TABLES

age_lookups <- map(
  test_age_stems,
  ~
    suppressMessages(read_csv(here(
      str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-C/", .x, "-lookup.csv")
    ))) %>%
    pivot_longer(
      cols = -raw,
      names_to = "age_grade",
      values_to = "ss"
    ) %>%
    mutate(test = str_sub(.x, 1,-5),) %>%
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
      test == "phmc" & between(raw, 0, 17) ~ "< Start age",
      test == "phmc" & between(raw, 18, 22) ~ "6.0 - 6.3",
      test == "phmc" & between(raw, 23, 24) ~ "6.4 - 6.7",
      test == "phmc" & between(raw, 25, 25) ~ "6.8 - 6.11",
      test == "phmc" & between(raw, 26, 27) ~ "7.0 - 7.3",
      test == "phmc" & between(raw, 28, 28) ~ "7.4 - 7.7",
      test == "phmc" & between(raw, 29, 30) ~ "7.8 - 7.11",
      test == "phmc" & between(raw, 31, 32) ~ "8.0 - 8.5",
      test == "phmc" & between(raw, 33, 33) ~ "8.6 - 8.11",
      test == "phmc" & between(raw, 34, 35) ~ "9.0 - 9.5",
      test == "phmc" & between(raw, 36, 36) ~ "9.6 - 9.11",
      test == "phmc" & between(raw, 37, 37) ~ "10.0 - 10.5",
      test == "phmc" & between(raw, 38, 38) ~ "10.6 - 10.11",
      test == "phmc" & between(raw, 39, 39) ~ "11.0 - 11.5",
      test == "phmc" & between(raw, 40, 40) ~ "11.6 - 11.11",
      test == "phmc" & between(raw, 41, 41) ~ "13.0 - 13.11",
      test == "phmc" & between(raw, 42, 42) ~ "15.0 - 16.11",
      test == "phmc" & between(raw, 43, 43) ~ "17.0 - 18.11",
      test == "phmc" & between(raw, 44, 45) ~ "> Stop age",
      test == "iwsc" & between(raw, 0, 4) ~ "< Start age",
      test == "iwsc" & between(raw, 5, 6) ~ "6.0 - 6.3",
      test == "iwsc" & between(raw, 7, 7) ~ "6.4 - 6.7",
      test == "iwsc" & between(raw, 8, 9) ~ "7.0 - 7.3",
      test == "iwsc" & between(raw, 10, 11) ~ "7.4 - 7.7",
      test == "iwsc" & between(raw, 12, 13) ~ "7.8 - 7.11",
      test == "iwsc" & between(raw, 14, 15) ~ "8.0 - 8.5",
      test == "iwsc" & between(raw, 16, 17) ~ "8.6 - 8.11",
      test == "iwsc" & between(raw, 18, 20) ~ "9.0 - 9.5",
      test == "iwsc" & between(raw, 21, 22) ~ "9.6 - 9.11",
      test == "iwsc" & between(raw, 23, 23) ~ "10.0 - 10.5",
      test == "iwsc" & between(raw, 24, 25) ~ "10.6 - 10.11",
      test == "iwsc" & between(raw, 26, 26) ~ "11.0 - 11.5",
      test == "iwsc" & between(raw, 27, 28) ~ "11.6 - 11.11",
      test == "iwsc" & between(raw, 29, 29) ~ "12.0 - 12.5",
      test == "iwsc" & between(raw, 30, 30) ~ "12.6 - 12.11",
      test == "iwsc" & between(raw, 31, 31) ~ "14.0 - 14.11",
      test == "iwsc" & between(raw, 32, 32) ~ "15.0 - 16.11",
      test == "iwsc" & between(raw, 33, 34) ~ "17.0 - 18.11",
      test == "iwsc" & between(raw, 35, 44) ~ "> Stop age",
      test == "rlnc" & between(raw, 0, 31) ~ "< Start age",
      test == "rlnc" & between(raw, 32, 38) ~ "6.0 - 6.3",
      test == "rlnc" & between(raw, 39, 42) ~ "6.4 - 6.7",
      test == "rlnc" & between(raw, 43, 45) ~ "6.8 - 6.11",
      test == "rlnc" & between(raw, 46, 48) ~ "7.0 - 7.3",
      test == "rlnc" & between(raw, 49, 51) ~ "7.4 - 7.7",
      test == "rlnc" & between(raw, 52, 55) ~ "7.8 - 7.11",
      test == "rlnc" & between(raw, 56, 59) ~ "8.0 - 8.5",
      test == "rlnc" & between(raw, 60, 63) ~ "8.6 - 8.11",
      test == "rlnc" & between(raw, 64, 67) ~ "9.0 - 9.5",
      test == "rlnc" & between(raw, 68, 71) ~ "9.6 - 9.11",
      test == "rlnc" & between(raw, 72, 75) ~ "10.0 - 10.5",
      test == "rlnc" & between(raw, 76, 79) ~ "10.6 - 10.11",
      test == "rlnc" & between(raw, 80, 83) ~ "11.0 - 11.5",
      test == "rlnc" & between(raw, 84, 86) ~ "11.6 - 11.11",
      test == "rlnc" & between(raw, 87, 89) ~ "12.0 - 12.5",
      test == "rlnc" & between(raw, 90, 92) ~ "12.6 - 12.11",
      test == "rlnc" & between(raw, 93, 97) ~ "13.0 - 13.11",
      test == "rlnc" & between(raw, 98, 103) ~ "14.0 - 14.11",
      test == "rlnc" & between(raw, 104, 109) ~ "15.0 - 16.11",
      test == "rlnc" & between(raw, 110, 120) ~ "17.0 - 18.11",
      test == "rlnc" & between(raw, 121, 200) ~ "> Stop age",
      test == "pwrc" & between(raw, 0, 17) ~ "< Start age",
      test == "pwrc" & between(raw, 18, 22) ~ "6.0 - 6.3",
      test == "pwrc" & between(raw, 23, 24) ~ "6.4 - 6.7",
      test == "pwrc" & between(raw, 25, 25) ~ "6.8 - 6.11",
      test == "pwrc" & between(raw, 26, 26) ~ "7.0 - 7.3",
      test == "pwrc" & between(raw, 27, 28) ~ "7.4 - 7.7",
      test == "pwrc" & between(raw, 29, 30) ~ "7.8 - 7.11",
      test == "pwrc" & between(raw, 31, 31) ~ "8.0 - 8.5",
      test == "pwrc" & between(raw, 32, 33) ~ "8.6 - 8.11",
      test == "pwrc" & between(raw, 34, 34) ~ "9.0 - 9.5",
      test == "pwrc" & between(raw, 35, 36) ~ "9.6 - 9.11",
      test == "pwrc" & between(raw, 37, 37) ~ "10.0 - 10.5",
      test == "pwrc" & between(raw, 38, 38) ~ "10.6 - 10.11",
      test == "pwrc" & between(raw, 39, 39) ~ "11.0 - 11.5",
      test == "pwrc" & between(raw, 40, 40) ~ "12.0 - 12.5",
      test == "pwrc" & between(raw, 41, 41) ~ "12.6 - 12.11",
      test == "pwrc" & between(raw, 42, 42) ~ "13.0 - 13.11",
      test == "pwrc" & between(raw, 43, 43) ~ "14.0 - 14.11",
      test == "pwrc" & between(raw, 44, 44) ~ "17.0 - 18.11",
      test == "pwrc" & between(raw, 45, 47) ~ "> Stop age",
      test == "wpcc" & between(raw, 0, 8) ~ "< Start age",
      test == "wpcc" & between(raw, 9, 10) ~ "6.0 - 6.3",
      test == "wpcc" & between(raw, 11, 11) ~ "6.4 - 6.7",
      test == "wpcc" & between(raw, 12, 12) ~ "6.8 - 6.11",
      test == "wpcc" & between(raw, 13, 13) ~ "7.0 - 7.3",
      test == "wpcc" & between(raw, 14, 15) ~ "7.8 - 7.11",
      test == "wpcc" & between(raw, 16, 17) ~ "8.0 - 8.5",
      test == "wpcc" & between(raw, 18, 19) ~ "8.6 - 8.11",
      test == "wpcc" & between(raw, 20, 20) ~ "9.0 - 9.5",
      test == "wpcc" & between(raw, 21, 22) ~ "9.6 - 9.11",
      test == "wpcc" & between(raw, 23, 24) ~ "10.0 - 10.5",
      test == "wpcc" & between(raw, 25, 26) ~ "10.6 - 10.11",
      test == "wpcc" & between(raw, 27, 27) ~ "11.0 - 11.5",
      test == "wpcc" & between(raw, 28, 29) ~ "11.6 - 11.11",
      test == "wpcc" & between(raw, 30, 30) ~ "12.0 - 12.5",
      test == "wpcc" & between(raw, 31, 31) ~ "12.6 - 12.11",
      test == "wpcc" & between(raw, 32, 33) ~ "13.0 - 13.11",
      test == "wpcc" & between(raw, 34, 36) ~ "14.0 - 14.11",
      test == "wpcc" & between(raw, 37, 39) ~ "15.0 - 16.11",
      test == "wpcc" & between(raw, 40, 46) ~ "17.0 - 18.11",
      test == "wpcc" & between(raw, 47, 59) ~ "> Stop age",
      test == "wmc" & between(raw, 0, 3) ~ "< Start age",
      test == "wmc" & between(raw, 4, 4) ~ "6.0 - 6.3",
      test == "wmc" & between(raw, 5, 5) ~ "8.0 - 8.5",
      test == "wmc" & between(raw, 6, 6) ~ "10.0 - 10.5",
      test == "wmc" & between(raw, 7, 7) ~ "12.6 - 12.11",
      test == "wmc" & between(raw, 8, 8) ~ "17.0 - 18.11",
      test == "wmc" & between(raw, 9, 20) ~ "> Stop age",
      test == "panc" & between(raw, 0, 14) ~ "< Start age",
      test == "panc" & between(raw, 15, 17) ~ "6.0 - 6.3",
      test == "panc" & between(raw, 18, 18) ~ "6.8 - 6.11",
      test == "panc" & between(raw, 19, 19) ~ "7.4 - 7.7",
      test == "panc" & between(raw, 20, 20) ~ "8.0 - 8.5",
      test == "panc" & between(raw, 21, 21) ~ "9.0 - 9.5",
      test == "panc" & between(raw, 22, 22) ~ "9.6 - 9.11",
      test == "panc" & between(raw, 23, 23) ~ "10.6 - 10.11",
      test == "panc" & between(raw, 24, 24) ~ "12.0 - 12.5",
      test == "panc" & between(raw, 25, 25) ~ "13.0 - 13.11",
      test == "panc" & between(raw, 26, 27) ~ "17.0 - 18.11",
      test == "panc" & between(raw, 28, 40) ~ "> Stop age",
      test == "iwrc" & between(raw, 0, 18) ~ "< Start age",
      test == "iwrc" & between(raw, 19, 21) ~ "6.0 - 6.3",
      test == "iwrc" & between(raw, 22, 22) ~ "6.4 - 6.7",
      test == "iwrc" & between(raw, 23, 23) ~ "6.8 - 6.11",
      test == "iwrc" & between(raw, 24, 25) ~ "7.0 - 7.3",
      test == "iwrc" & between(raw, 26, 26) ~ "7.4 - 7.7",
      test == "iwrc" & between(raw, 27, 28) ~ "7.8 - 7.11",
      test == "iwrc" & between(raw, 29, 30) ~ "8.6 - 8.11",
      test == "iwrc" & between(raw, 31, 31) ~ "9.0 - 9.5",
      test == "iwrc" & between(raw, 32, 32) ~ "9.6 - 9.11",
      test == "iwrc" & between(raw, 33, 33) ~ "10.0 - 10.5",
      test == "iwrc" & between(raw, 34, 34) ~ "10.6 - 10.11",
      test == "iwrc" & between(raw, 35, 35) ~ "11.0 - 11.5",
      test == "iwrc" & between(raw, 36, 36) ~ "11.6 - 11.11",
      test == "iwrc" & between(raw, 37, 37) ~ "13.0 - 13.11",
      test == "iwrc" & between(raw, 38, 38) ~ "14.0 - 14.11",
      test == "iwrc" & between(raw, 39, 39) ~ "15.0 - 16.11",
      test == "iwrc" & between(raw, 40, 41) ~ "17.0 - 18.11",
      test == "iwrc" & between(raw, 42, 55) ~ "> Stop age",
      test == "blnc" & between(raw, 0, 14) ~ "< Start age",
      test == "blnc" & between(raw, 15, 15) ~ "6.0 - 6.3",
      test == "blnc" & between(raw, 16, 16) ~ "6.4 - 6.7",
      test == "blnc" & between(raw, 17, 17) ~ "7.8 - 7.11",
      test == "blnc" & between(raw, 18, 18) ~ "9.0 - 9.5",
      test == "blnc" & between(raw, 19, 19) ~ "11.0 - 11.5",
      test == "blnc" & between(raw, 20, 20) ~ "12.6 - 12.11",
      test == "blnc" & between(raw, 21, 21) ~ "14.0 - 14.11",
      test == "blnc" & between(raw, 22, 22) ~ "15.0 - 16.11",
      test == "blnc" & between(raw, 23, 23) ~ "17.0 - 18.11",
      test == "blnc" & between(raw, 24, 29) ~ "> Stop age",
      test == "segc" & between(raw, 0, 14) ~ "< Start age",
      test == "segc" & between(raw, 15, 16) ~ "6.0 - 6.3",
      test == "segc" & between(raw, 17, 17) ~ "6.8 - 6.11",
      test == "segc" & between(raw, 18, 18) ~ "7.8 - 7.11",
      test == "segc" & between(raw, 19, 19) ~ "8.6 - 8.11",
      test == "segc" & between(raw, 20, 20) ~ "10.0 - 10.5",
      test == "segc" & between(raw, 21, 21) ~ "11.6 - 11.11",
      test == "segc" & between(raw, 22, 22) ~ "15.0 - 16.11",
      test == "segc" & between(raw, 23, 23) ~ "17.0 - 18.11",
      test == "segc" & between(raw, 24, 29) ~ "> Stop age",
      test == "rwsc" & between(raw, 0, 4) ~ "< Start age",
      test == "rwsc" & between(raw, 5, 6) ~ "6.0 - 6.3",
      test == "rwsc" & between(raw, 7, 8) ~ "6.4 - 6.7",
      test == "rwsc" & between(raw, 9, 9) ~ "6.8 - 6.11",
      test == "rwsc" & between(raw, 10, 10) ~ "7.0 - 7.3",
      test == "rwsc" & between(raw, 11, 12) ~ "7.4 - 7.7",
      test == "rwsc" & between(raw, 13, 14) ~ "7.8 - 7.11",
      test == "rwsc" & between(raw, 15, 16) ~ "8.0 - 8.5",
      test == "rwsc" & between(raw, 17, 18) ~ "8.6 - 8.11",
      test == "rwsc" & between(raw, 19, 20) ~ "9.0 - 9.5",
      test == "rwsc" & between(raw, 21, 22) ~ "9.6 - 9.11",
      test == "rwsc" & between(raw, 23, 23) ~ "10.0 - 10.5",
      test == "rwsc" & between(raw, 24, 25) ~ "10.6 - 10.11",
      test == "rwsc" & between(raw, 26, 27) ~ "11.0 - 11.5",
      test == "rwsc" & between(raw, 28, 28) ~ "11.6 - 11.11",
      test == "rwsc" & between(raw, 29, 29) ~ "12.0 - 12.5",
      test == "rwsc" & between(raw, 30, 31) ~ "12.6 - 12.11",
      test == "rwsc" & between(raw, 32, 32) ~ "13.0 - 13.11",
      test == "rwsc" & between(raw, 33, 34) ~ "14.0 - 14.11",
      test == "rwsc" & between(raw, 35, 36) ~ "15.0 - 16.11",
      test == "rwsc" & between(raw, 37, 39) ~ "17.0 - 18.11",
      test == "rwsc" & between(raw, 40, 44) ~ "> Stop age",
      test == "sre1c" & between(raw, 0, 5) ~ "< Start age",
      test == "sre1c" & between(raw, 6, 7) ~ "6.0 - 6.3",
      test == "sre1c" & between(raw, 8, 8) ~ "6.4 - 6.7",
      test == "sre1c" & between(raw, 9, 10) ~ "6.8 - 6.11",
      test == "sre1c" & between(raw, 11, 12) ~ "7.0 - 7.3",
      test == "sre1c" & between(raw, 13, 13) ~ "7.4 - 7.7",
      test == "sre1c" & between(raw, 14, 15) ~ "7.8 - 7.11",
      test == "sre1c" & between(raw, 16, 16) ~ "8.0 - 8.5",
      test == "sre1c" & between(raw, 17, 18) ~ "8.6 - 8.11",
      test == "sre1c" & between(raw, 19, 19) ~ "9.0 - 9.5",
      test == "sre1c" & between(raw, 20, 20) ~ "9.6 - 9.11",
      test == "sre1c" & between(raw, 21, 21) ~ "10.0 - 10.5",
      test == "sre1c" & between(raw, 22, 22) ~ "10.6 - 10.11",
      test == "sre1c" & between(raw, 23, 23) ~ "11.6 - 11.11",
      test == "sre1c" & between(raw, 24, 24) ~ "12.0 - 12.5",
      test == "sre1c" & between(raw, 25, 26) ~ "12.6 - 12.11",
      test == "sre1c" & between(raw, 27, 63) ~ "> Stop age",
      test == "sre2c" & between(raw, 0, 17) ~ "< Start age",
      test == "sre2c" & between(raw, 18, 20) ~ "10.0 - 10.5",
      test == "sre2c" & between(raw, 21, 21) ~ "11.0 - 11.5",
      test == "sre2c" & between(raw, 22, 22) ~ "11.6 - 11.11",
      test == "sre2c" & between(raw, 23, 23) ~ "12.0 - 12.5",
      test == "sre2c" & between(raw, 24, 25) ~ "13.0 - 13.11",
      test == "sre2c" & between(raw, 26, 27) ~ "14.0 - 14.11",
      test == "sre2c" & between(raw, 28, 30) ~ "15.0 - 16.11",
      test == "sre2c" & between(raw, 31, 35) ~ "17.0 - 18.11",
      test == "sre2c" & between(raw, 36, 51) ~ "> Stop age",
      test == "rnlc" & between(raw, 0, 47) ~ "< Start age",
      test == "rnlc" & between(raw, 48, 55) ~ "6.0 - 6.3",
      test == "rnlc" & between(raw, 56, 59) ~ "6.4 - 6.7",
      test == "rnlc" & between(raw, 60, 62) ~ "6.8 - 6.11",
      test == "rnlc" & between(raw, 63, 65) ~ "7.0 - 7.3",
      test == "rnlc" & between(raw, 66, 68) ~ "7.4 - 7.7",
      test == "rnlc" & between(raw, 69, 72) ~ "7.8 - 7.11",
      test == "rnlc" & between(raw, 73, 76) ~ "8.0 - 8.5",
      test == "rnlc" & between(raw, 77, 80) ~ "8.6 - 8.11",
      test == "rnlc" & between(raw, 81, 84) ~ "9.0 - 9.5",
      test == "rnlc" & between(raw, 85, 89) ~ "9.6 - 9.11",
      test == "rnlc" & between(raw, 90, 93) ~ "10.0 - 10.5",
      test == "rnlc" & between(raw, 94, 96) ~ "10.6 - 10.11",
      test == "rnlc" & between(raw, 97, 99) ~ "11.0 - 11.5",
      test == "rnlc" & between(raw, 100, 102) ~ "11.6 - 11.11",
      test == "rnlc" & between(raw, 103, 106) ~ "12.0 - 12.5",
      test == "rnlc" & between(raw, 107, 110) ~ "12.6 - 12.11",
      test == "rnlc" & between(raw, 111, 115) ~ "13.0 - 13.11",
      test == "rnlc" & between(raw, 116, 122) ~ "14.0 - 14.11",
      test == "rnlc" & between(raw, 123, 128) ~ "15.0 - 16.11",
      test == "rnlc" & between(raw, 129, 141) ~ "17.0 - 18.11",
      test == "rnlc" & between(raw, 142, 200) ~ "> Stop age",
      test == "lmc" & between(raw, 0, 2) ~ "< Start age",
      test == "lmc" & between(raw, 3, 3) ~ "6.0 - 6.3",
      test == "lmc" & between(raw, 4, 4) ~ "6.4 - 6.7",
      test == "lmc" & between(raw, 5, 5) ~ "7.8 - 7.11",
      test == "lmc" & between(raw, 6, 6) ~ "9.6 - 9.11",
      test == "lmc" & between(raw, 7, 7) ~ "12.0 - 12.5",
      test == "lmc" & between(raw, 8, 8) ~ "17.0 - 18.11",
      test == "lmc" & between(raw, 9, 20) ~ "> Stop age",
      test == "rpwc" & between(raw, 0, 10) ~ "< Start age",
      test == "rpwc" & between(raw, 11, 14) ~ "6.0 - 6.3",
      test == "rpwc" & between(raw, 15, 15) ~ "6.4 - 6.7",
      test == "rpwc" & between(raw, 16, 16) ~ "6.8 - 6.11",
      test == "rpwc" & between(raw, 17, 17) ~ "7.0 - 7.3",
      test == "rpwc" & between(raw, 18, 19) ~ "7.4 - 7.7",
      test == "rpwc" & between(raw, 20, 21) ~ "7.8 - 7.11",
      test == "rpwc" & between(raw, 22, 23) ~ "8.0 - 8.5",
      test == "rpwc" & between(raw, 24, 25) ~ "8.6 - 8.11",
      test == "rpwc" & between(raw, 26, 27) ~ "9.0 - 9.5",
      test == "rpwc" & between(raw, 28, 29) ~ "9.6 - 9.11",
      test == "rpwc" & between(raw, 30, 30) ~ "10.0 - 10.5",
      test == "rpwc" & between(raw, 31, 31) ~ "10.6 - 10.11",
      test == "rpwc" & between(raw, 32, 32) ~ "11.0 - 11.5",
      test == "rpwc" & between(raw, 33, 33) ~ "11.6 - 11.11",
      test == "rpwc" & between(raw, 34, 35) ~ "12.0 - 12.5",
      test == "rpwc" & between(raw, 36, 36) ~ "12.6 - 12.11",
      test == "rpwc" & between(raw, 37, 37) ~ "13.0 - 13.11",
      test == "rpwc" & between(raw, 38, 38) ~ "14.0 - 14.11",
      test == "rpwc" & between(raw, 39, 39) ~ "15.0 - 16.11",
      test == "rpwc" & between(raw, 40, 42) ~ "17.0 - 18.11",
      test == "rpwc" & between(raw, 43, 60) ~ "> Stop age",
      test == "riwc" & between(raw, 0, 19) ~ "< Start age",
      test == "riwc" & between(raw, 20, 28) ~ "6.0 - 6.3",
      test == "riwc" & between(raw, 29, 33) ~ "6.4 - 6.7",
      test == "riwc" & between(raw, 34, 38) ~ "6.8 - 6.11",
      test == "riwc" & between(raw, 39, 43) ~ "7.0 - 7.3",
      test == "riwc" & between(raw, 44, 47) ~ "7.4 - 7.7",
      test == "riwc" & between(raw, 48, 52) ~ "7.8 - 7.11",
      test == "riwc" & between(raw, 53, 57) ~ "8.0 - 8.5",
      test == "riwc" & between(raw, 58, 62) ~ "8.6 - 8.11",
      test == "riwc" & between(raw, 63, 66) ~ "9.0 - 9.5",
      test == "riwc" & between(raw, 67, 70) ~ "9.6 - 9.11",
      test == "riwc" & between(raw, 71, 73) ~ "10.0 - 10.5",
      test == "riwc" & between(raw, 74, 76) ~ "10.6 - 10.11",
      test == "riwc" & between(raw, 77, 79) ~ "11.0 - 11.5",
      test == "riwc" & between(raw, 80, 81) ~ "11.6 - 11.11",
      test == "riwc" & between(raw, 82, 83) ~ "12.0 - 12.5",
      test == "riwc" & between(raw, 84, 84) ~ "12.6 - 12.11",
      test == "riwc" & between(raw, 85, 85) ~ "13.0 - 13.11",
      test == "riwc" & between(raw, 86, 87) ~ "14.0 - 14.11",
      test == "riwc" & between(raw, 88, 89) ~ "15.0 - 16.11",
      test == "riwc" & between(raw, 90, 94) ~ "17.0 - 18.11",
      test == "riwc" & between(raw, 95, 100) ~ "> Stop age",
      test == "sslc" & between(raw, 0, 9) ~ "< Start age",
      test == "sslc" & between(raw, 10, 12) ~ "6.0 - 6.3",
      test == "sslc" & between(raw, 13, 13) ~ "7.0 - 7.3",
      test == "sslc" & between(raw, 14, 14) ~ "8.0 - 8.5",
      test == "sslc" & between(raw, 15, 15) ~ "9.6 - 9.11",
      test == "sslc" & between(raw, 16, 16) ~ "11.0 - 11.5",
      test == "sslc" & between(raw, 17, 17) ~ "14.0 - 14.11",
      test == "sslc" & between(raw, 18, 20) ~ "17.0 - 18.11",
      test == "sslc" & between(raw, 21, 42) ~ "> Stop age",
      test == "lvc" & between(raw, 0, 7) ~ "< Start age",
      test == "lvc" & between(raw, 8, 9) ~ "6.0 - 6.3",
      test == "lvc" & between(raw, 10, 10) ~ "6.4 - 6.7",
      test == "lvc" & between(raw, 11, 11) ~ "6.8 - 6.11",
      test == "lvc" & between(raw, 12, 12) ~ "7.4 - 7.7",
      test == "lvc" & between(raw, 13, 13) ~ "7.8 - 7.11",
      test == "lvc" & between(raw, 14, 15) ~ "8.0 - 8.5",
      test == "lvc" & between(raw, 16, 16) ~ "8.6 - 8.11",
      test == "lvc" & between(raw, 17, 17) ~ "9.0 - 9.5",
      test == "lvc" & between(raw, 18, 18) ~ "9.6 - 9.11",
      test == "lvc" & between(raw, 19, 19) ~ "10.0 - 10.5",
      test == "lvc" & between(raw, 20, 20) ~ "11.0 - 11.5",
      test == "lvc" & between(raw, 21, 21) ~ "11.6 - 11.11",
      test == "lvc" & between(raw, 22, 22) ~ "12.0 - 12.5",
      test == "lvc" & between(raw, 23, 23) ~ "12.6 - 12.11",
      test == "lvc" & between(raw, 24, 24) ~ "13.0 - 13.11",
      test == "lvc" & between(raw, 25, 25) ~ "14.0 - 14.11",
      test == "lvc" & between(raw, 26, 27) ~ "15.0 - 16.11",
      test == "lvc" & between(raw, 28, 30) ~ "17.0 - 18.11",
      test == "lvc" & between(raw, 31, 38) ~ "> Stop age",
      test == "ganc" & between(raw, 0, 6) ~ "< Start age",
      test == "ganc" & between(raw, 7, 8) ~ "6.0 - 6.3",
      test == "ganc" & between(raw, 9, 9) ~ "6.4 - 6.7",
      test == "ganc" & between(raw, 10, 10) ~ "6.8 - 6.11",
      test == "ganc" & between(raw, 11, 11) ~ "7.0 - 7.3",
      test == "ganc" & between(raw, 12, 12) ~ "7.4 - 7.7",
      test == "ganc" & between(raw, 13, 13) ~ "7.8 - 7.11",
      test == "ganc" & between(raw, 14, 14) ~ "8.0 - 8.5",
      test == "ganc" & between(raw, 15, 16) ~ "8.6 - 8.11",
      test == "ganc" & between(raw, 17, 17) ~ "9.0 - 9.5",
      test == "ganc" & between(raw, 18, 18) ~ "9.6 - 9.11",
      test == "ganc" & between(raw, 19, 19) ~ "10.0 - 10.5",
      test == "ganc" & between(raw, 20, 20) ~ "10.6 - 10.11",
      test == "ganc" & between(raw, 21, 21) ~ "11.0 - 11.5",
      test == "ganc" & between(raw, 22, 23) ~ "12.0 - 12.5",
      test == "ganc" & between(raw, 24, 24) ~ "13.0 - 13.11",
      test == "ganc" & between(raw, 25, 25) ~ "14.0 - 14.11",
      test == "ganc" & between(raw, 26, 26) ~ "15.0 - 16.11",
      test == "ganc" & between(raw, 27, 27) ~ "17.0 - 18.11",
      test == "ganc" & between(raw, 28, 40) ~ "> Stop age",
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
      "OUTPUT-FILES/OES-INPUT-TABLES/TOD-C/TOD-C-OES-age-lookup-table.csv"
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
          str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-C/", .x, "-lookup.csv")
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
      "OUTPUT-FILES/OES-INPUT-TABLES/TOD-C/TOD-C-OES-grade-lookup-table.csv"
    ),
    na = ""
  )

#### OUTPUT FOR INDEX_COMPOSITE LOOKUP TABLES

index_composites_lookups <-
  list(
    suppressMessages(read_csv(here(
      str_c(
        "INPUT-FILES/OES-INPUT-TABLES/TOD-C/index_composites-age-lookup.csv"
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
        "INPUT-FILES/OES-INPUT-TABLES/TOD-C/index_composites-grade-lookup.csv"
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
      "OUTPUT-FILES/OES-INPUT-TABLES/TOD-C/TOD-C-OES-index-composite-lookup-table.csv"
    ),
    na = ""
  )





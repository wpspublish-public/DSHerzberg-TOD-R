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

cv_lookup_test <- suppressMessages(read_csv(
  here("INPUT-FILES/OES-INPUT-TABLES/TOD-C/test-CI-lookup.csv")
)) %>%
  rename(CV_90 = CI90, CV_95 = CI95) %>%
  select(test, CV_90, CV_95)

cv_lookup_index_composites_age_grade <- suppressMessages(read_csv(
  here("INPUT-FILES/OES-INPUT-TABLES/TOD-C/indexcomposites-CI-child-lookup.csv")
)) %>%
  rename(CV_90 = CI90, CV_95 = CI95) %>%
  select(test, CV_90, CV_95) %>% 
  mutate(across(test,
                ~ str_to_lower(.)))

cv_lookup_index_composites_adult <- suppressMessages(read_csv(
  here("INPUT-FILES/OES-INPUT-TABLES/TOD-C/indexcomposites-CI-adult-lookup.csv")
)) %>%
  rename(CV_90 = CI90, CV_95 = CI95) %>%
  select(test, CV_90, CV_95) %>% 
  mutate(across(test,
                ~ str_to_lower(.)))

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
  left_join(cv_lookup_test, by = "test") %>%
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
  left_join(cv_lookup_test, by = "test") %>% 
  mutate(
    grade_equiv = case_when(
      test == "phmc" & between(raw, 0, 19) ~ "< start grade", 
      test == "phmc" & between(raw, 20, 24) ~ "1-Fall", 
      test == "phmc" & between(raw, 25, 27) ~ "1-Spring", 
      test == "phmc" & between(raw, 28, 29) ~ "2-Fall", 
      test == "phmc" & between(raw, 30, 31) ~ "2-Spring", 
      test == "phmc" & between(raw, 32, 32) ~ "3-Fall", 
      test == "phmc" & between(raw, 33, 34) ~ "3-Spring", 
      test == "phmc" & between(raw, 35, 36) ~ "4-Fall", 
      test == "phmc" & between(raw, 37, 38) ~ "5-Fall", 
      test == "phmc" & between(raw, 39, 39) ~ "5-Spring", 
      test == "phmc" & between(raw, 40, 41) ~ "7-Fall", 
      test == "phmc" & between(raw, 42, 42) ~ "11-Spring", 
      test == "phmc" & between(raw, 43, 43) ~ "12-Spring", 
      test == "phmc" & between(raw, 44, 45) ~ "> stop grade", 
      test == "iwsc" & between(raw, 0, 4) ~ "< start grade", 
      test == "iwsc" & between(raw, 5, 6) ~ "1-Fall", 
      test == "iwsc" & between(raw, 7, 8) ~ "1-Spring", 
      test == "iwsc" & between(raw, 9, 11) ~ "2-Fall", 
      test == "iwsc" & between(raw, 12, 14) ~ "2-Spring", 
      test == "iwsc" & between(raw, 15, 17) ~ "3-Fall", 
      test == "iwsc" & between(raw, 18, 19) ~ "3-Spring", 
      test == "iwsc" & between(raw, 20, 21) ~ "4-Fall", 
      test == "iwsc" & between(raw, 22, 23) ~ "4-Spring", 
      test == "iwsc" & between(raw, 24, 24) ~ "5-Fall", 
      test == "iwsc" & between(raw, 25, 26) ~ "5-Spring", 
      test == "iwsc" & between(raw, 27, 27) ~ "6-Fall", 
      test == "iwsc" & between(raw, 28, 28) ~ "6-Spring", 
      test == "iwsc" & between(raw, 29, 29) ~ "7-Fall", 
      test == "iwsc" & between(raw, 30, 31) ~ "8-Fall", 
      test == "iwsc" & between(raw, 32, 32) ~ "9-Spring", 
      test == "iwsc" & between(raw, 33, 34) ~ "11-Fall", 
      test == "iwsc" & between(raw, 35, 37) ~ "12-Spring", 
      test == "iwsc" & between(raw, 38, 44) ~ "> stop grade", 
      test == "rlnc" & between(raw, 0, 35) ~ "< start grade", 
      test == "rlnc" & between(raw, 36, 42) ~ "1-Fall", 
      test == "rlnc" & between(raw, 43, 47) ~ "1-Spring", 
      test == "rlnc" & between(raw, 48, 52) ~ "2-Fall", 
      test == "rlnc" & between(raw, 53, 57) ~ "2-Spring", 
      test == "rlnc" & between(raw, 58, 61) ~ "3-Fall", 
      test == "rlnc" & between(raw, 62, 66) ~ "3-Spring", 
      test == "rlnc" & between(raw, 67, 70) ~ "4-Fall", 
      test == "rlnc" & between(raw, 71, 74) ~ "4-Spring", 
      test == "rlnc" & between(raw, 75, 77) ~ "5-Fall", 
      test == "rlnc" & between(raw, 78, 81) ~ "5-Spring", 
      test == "rlnc" & between(raw, 82, 84) ~ "6-Fall", 
      test == "rlnc" & between(raw, 85, 87) ~ "6-Spring", 
      test == "rlnc" & between(raw, 88, 90) ~ "7-Fall", 
      test == "rlnc" & between(raw, 91, 93) ~ "7-Spring", 
      test == "rlnc" & between(raw, 94, 96) ~ "8-Fall", 
      test == "rlnc" & between(raw, 97, 98) ~ "8-Spring", 
      test == "rlnc" & between(raw, 99, 101) ~ "9-Fall", 
      test == "rlnc" & between(raw, 102, 103) ~ "9-Spring", 
      test == "rlnc" & between(raw, 104, 105) ~ "10-Fall", 
      test == "rlnc" & between(raw, 106, 107) ~ "10-Spring", 
      test == "rlnc" & between(raw, 108, 109) ~ "11-Fall", 
      test == "rlnc" & between(raw, 110, 111) ~ "11-Spring", 
      test == "rlnc" & between(raw, 112, 112) ~ "12-Fall", 
      test == "rlnc" & between(raw, 113, 122) ~ "12-Spring", 
      test == "rlnc" & between(raw, 123, 200) ~ "> stop grade", 
      test == "pwrc" & between(raw, 0, 20) ~ "< start grade", 
      test == "pwrc" & between(raw, 21, 24) ~ "1-Fall", 
      test == "pwrc" & between(raw, 25, 26) ~ "1-Spring", 
      test == "pwrc" & between(raw, 27, 28) ~ "2-Fall", 
      test == "pwrc" & between(raw, 29, 30) ~ "2-Spring", 
      test == "pwrc" & between(raw, 31, 32) ~ "3-Fall", 
      test == "pwrc" & between(raw, 33, 34) ~ "3-Spring", 
      test == "pwrc" & between(raw, 35, 35) ~ "4-Fall", 
      test == "pwrc" & between(raw, 36, 37) ~ "4-Spring", 
      test == "pwrc" & between(raw, 38, 39) ~ "5-Spring", 
      test == "pwrc" & between(raw, 40, 40) ~ "6-Spring", 
      test == "pwrc" & between(raw, 41, 41) ~ "7-Fall", 
      test == "pwrc" & between(raw, 42, 42) ~ "8-Fall", 
      test == "pwrc" & between(raw, 43, 43) ~ "9-Spring", 
      test == "pwrc" & between(raw, 44, 44) ~ "11-Spring", 
      test == "pwrc" & between(raw, 45, 45) ~ "12-Spring", 
      test == "pwrc" & between(raw, 46, 47) ~ "> stop grade", 
      test == "wpcc" & between(raw, 0, 8) ~ "< start grade", 
      test == "wpcc" & between(raw, 9, 11) ~ "1-Fall", 
      test == "wpcc" & between(raw, 12, 12) ~ "1-Spring", 
      test == "wpcc" & between(raw, 13, 14) ~ "2-Fall", 
      test == "wpcc" & between(raw, 15, 16) ~ "2-Spring", 
      test == "wpcc" & between(raw, 17, 18) ~ "3-Fall", 
      test == "wpcc" & between(raw, 19, 20) ~ "3-Spring", 
      test == "wpcc" & between(raw, 21, 22) ~ "4-Fall", 
      test == "wpcc" & between(raw, 23, 23) ~ "4-Spring", 
      test == "wpcc" & between(raw, 24, 25) ~ "5-Fall", 
      test == "wpcc" & between(raw, 26, 27) ~ "5-Spring", 
      test == "wpcc" & between(raw, 28, 28) ~ "6-Fall", 
      test == "wpcc" & between(raw, 29, 30) ~ "6-Spring", 
      test == "wpcc" & between(raw, 31, 31) ~ "7-Fall", 
      test == "wpcc" & between(raw, 32, 32) ~ "7-Spring", 
      test == "wpcc" & between(raw, 33, 33) ~ "8-Fall", 
      test == "wpcc" & between(raw, 34, 35) ~ "8-Spring", 
      test == "wpcc" & between(raw, 36, 36) ~ "9-Fall", 
      test == "wpcc" & between(raw, 37, 37) ~ "9-Spring", 
      test == "wpcc" & between(raw, 38, 38) ~ "10-Spring", 
      test == "wpcc" & between(raw, 39, 39) ~ "11-Fall", 
      test == "wpcc" & between(raw, 40, 40) ~ "12-Fall", 
      test == "wpcc" & between(raw, 41, 59) ~ "> stop grade", 
      test == "wmc" & between(raw, 0, 3) ~ "< start grade", 
      test == "wmc" & between(raw, 4, 4) ~ "1-Fall", 
      test == "wmc" & between(raw, 5, 5) ~ "3-Fall", 
      test == "wmc" & between(raw, 6, 6) ~ "5-Fall", 
      test == "wmc" & between(raw, 7, 7) ~ "7-Fall", 
      test == "wmc" & between(raw, 8, 8) ~ "12-Fall", 
      test == "wmc" & between(raw, 9, 20) ~ "> stop grade", 
      test == "panc" & between(raw, 0, 15) ~ "< start grade", 
      test == "panc" & between(raw, 16, 18) ~ "1-Fall", 
      test == "panc" & between(raw, 19, 19) ~ "2-Fall", 
      test == "panc" & between(raw, 20, 21) ~ "3-Fall", 
      test == "panc" & between(raw, 22, 22) ~ "4-Spring", 
      test == "panc" & between(raw, 23, 23) ~ "5-Spring", 
      test == "panc" & between(raw, 24, 24) ~ "6-Spring", 
      test == "panc" & between(raw, 25, 25) ~ "8-Fall", 
      test == "panc" & between(raw, 26, 26) ~ "10-Spring", 
      test == "panc" & between(raw, 27, 28) ~ "12-Spring", 
      test == "panc" & between(raw, 29, 40) ~ "> stop grade", 
      test == "iwrc" & between(raw, 0, 20) ~ "< start grade", 
      test == "iwrc" & between(raw, 21, 23) ~ "1-Fall", 
      test == "iwrc" & between(raw, 24, 24) ~ "1-Spring", 
      test == "iwrc" & between(raw, 25, 26) ~ "2-Fall", 
      test == "iwrc" & between(raw, 27, 28) ~ "2-Spring", 
      test == "iwrc" & between(raw, 29, 30) ~ "3-Fall", 
      test == "iwrc" & between(raw, 31, 32) ~ "4-Fall", 
      test == "iwrc" & between(raw, 33, 34) ~ "5-Fall", 
      test == "iwrc" & between(raw, 35, 35) ~ "6-Fall", 
      test == "iwrc" & between(raw, 36, 36) ~ "7-Fall", 
      test == "iwrc" & between(raw, 37, 37) ~ "8-Spring", 
      test == "iwrc" & between(raw, 38, 39) ~ "10-Fall", 
      test == "iwrc" & between(raw, 40, 41) ~ "12-Fall", 
      test == "iwrc" & between(raw, 42, 43) ~ "12-Spring", 
      test == "iwrc" & between(raw, 44, 55) ~ "> stop grade", 
      test == "orec" & between(raw, -20, 34) ~ "< start grade", 
      test == "orec" & between(raw, 35, 51) ~ "1-Fall", 
      test == "orec" & between(raw, 52, 60) ~ "1-Spring", 
      test == "orec" & between(raw, 61, 68) ~ "2-Fall", 
      test == "orec" & between(raw, 69, 76) ~ "2-Spring", 
      test == "orec" & between(raw, 77, 82) ~ "3-Fall", 
      test == "orec" & between(raw, 83, 87) ~ "3-Spring", 
      test == "orec" & between(raw, 88, 92) ~ "4-Fall", 
      test == "orec" & between(raw, 93, 96) ~ "4-Spring", 
      test == "orec" & between(raw, 97, 100) ~ "5-Fall", 
      test == "orec" & between(raw, 101, 103) ~ "5-Spring", 
      test == "orec" & between(raw, 104, 106) ~ "6-Fall", 
      test == "orec" & between(raw, 107, 109) ~ "6-Spring", 
      test == "orec" & between(raw, 110, 112) ~ "7-Fall", 
      test == "orec" & between(raw, 113, 115) ~ "7-Spring", 
      test == "orec" & between(raw, 116, 117) ~ "8-Fall", 
      test == "orec" & between(raw, 118, 120) ~ "8-Spring", 
      test == "orec" & between(raw, 121, 122) ~ "9-Fall", 
      test == "orec" & between(raw, 123, 125) ~ "9-Spring", 
      test == "orec" & between(raw, 126, 128) ~ "10-Fall", 
      test == "orec" & between(raw, 129, 131) ~ "10-Spring", 
      test == "orec" & between(raw, 132, 135) ~ "11-Fall", 
      test == "orec" & between(raw, 136, 139) ~ "11-Spring", 
      test == "orec" & between(raw, 140, 144) ~ "12-Fall", 
      test == "orec" & between(raw, 145, 160) ~ "12-Spring", 
      test == "orec" & between(raw, 161, 384) ~ "> stop grade", 
      test == "blnc" & between(raw, 0, 15) ~ "< start grade", 
      test == "blnc" & between(raw, 16, 16) ~ "1-Fall", 
      test == "blnc" & between(raw, 17, 17) ~ "2-Spring", 
      test == "blnc" & between(raw, 18, 18) ~ "4-Spring", 
      test == "blnc" & between(raw, 19, 19) ~ "6-Fall", 
      test == "blnc" & between(raw, 20, 20) ~ "7-Fall", 
      test == "blnc" & between(raw, 21, 21) ~ "8-Spring", 
      test == "blnc" & between(raw, 22, 22) ~ "10-Fall", 
      test == "blnc" & between(raw, 23, 23) ~ "12-Fall", 
      test == "blnc" & between(raw, 24, 24) ~ "12-Spring", 
      test == "blnc" & between(raw, 25, 29) ~ "> stop grade", 
      test == "segc" & between(raw, 0, 14) ~ "< start grade", 
      test == "segc" & between(raw, 15, 16) ~ "1-Fall", 
      test == "segc" & between(raw, 17, 17) ~ "1-Spring", 
      test == "segc" & between(raw, 18, 18) ~ "2-Spring", 
      test == "segc" & between(raw, 19, 19) ~ "3-Spring", 
      test == "segc" & between(raw, 20, 20) ~ "5-Fall", 
      test == "segc" & between(raw, 21, 21) ~ "7-Fall", 
      test == "segc" & between(raw, 22, 22) ~ "11-Fall", 
      test == "segc" & between(raw, 23, 23) ~ "12-Spring", 
      test == "segc" & between(raw, 24, 29) ~ "> stop grade", 
      test == "rwsc" & between(raw, 0, 5) ~ "< start grade", 
      test == "rwsc" & between(raw, 6, 8) ~ "1-Fall", 
      test == "rwsc" & between(raw, 9, 10) ~ "1-Spring", 
      test == "rwsc" & between(raw, 11, 12) ~ "2-Fall", 
      test == "rwsc" & between(raw, 13, 15) ~ "2-Spring", 
      test == "rwsc" & between(raw, 16, 17) ~ "3-Fall", 
      test == "rwsc" & between(raw, 18, 19) ~ "3-Spring", 
      test == "rwsc" & between(raw, 20, 21) ~ "4-Fall", 
      test == "rwsc" & between(raw, 22, 23) ~ "4-Spring", 
      test == "rwsc" & between(raw, 24, 24) ~ "5-Fall", 
      test == "rwsc" & between(raw, 25, 26) ~ "5-Spring", 
      test == "rwsc" & between(raw, 27, 27) ~ "6-Fall", 
      test == "rwsc" & between(raw, 28, 29) ~ "6-Spring", 
      test == "rwsc" & between(raw, 30, 30) ~ "7-Fall", 
      test == "rwsc" & between(raw, 31, 31) ~ "7-Spring", 
      test == "rwsc" & between(raw, 32, 32) ~ "8-Fall", 
      test == "rwsc" & between(raw, 33, 33) ~ "8-Spring", 
      test == "rwsc" & between(raw, 34, 34) ~ "9-Fall", 
      test == "rwsc" & between(raw, 35, 36) ~ "10-Fall", 
      test == "rwsc" & between(raw, 37, 37) ~ "12-Fall", 
      test == "rwsc" & between(raw, 38, 38) ~ "12-Spring", 
      test == "rwsc" & between(raw, 39, 44) ~ "> stop grade", 
      test == "sre1c" & between(raw, 0, 6) ~ "< start grade", 
      test == "sre1c" & between(raw, 7, 9) ~ "1-Fall", 
      test == "sre1c" & between(raw, 10, 11) ~ "1-Spring", 
      test == "sre1c" & between(raw, 12, 13) ~ "2-Fall", 
      test == "sre1c" & between(raw, 14, 15) ~ "2-Spring", 
      test == "sre1c" & between(raw, 16, 17) ~ "3-Fall", 
      test == "sre1c" & between(raw, 18, 18) ~ "3-Spring", 
      test == "sre1c" & between(raw, 19, 19) ~ "4-Fall", 
      test == "sre1c" & between(raw, 20, 20) ~ "4-Spring", 
      test == "sre1c" & between(raw, 21, 21) ~ "5-Fall", 
      test == "sre1c" & between(raw, 22, 24) ~ "5-Spring", 
      test == "sre1c" & between(raw, 25, 63) ~ "> stop grade", 
      test == "sre2c" & between(raw, 0, 17) ~ "< start grade", 
      test == "sre2c" & between(raw, 18, 21) ~ "6-Fall", 
      test == "sre2c" & between(raw, 22, 22) ~ "6-Spring", 
      test == "sre2c" & between(raw, 23, 24) ~ "7-Fall", 
      test == "sre2c" & between(raw, 25, 25) ~ "7-Spring", 
      test == "sre2c" & between(raw, 26, 26) ~ "8-Fall", 
      test == "sre2c" & between(raw, 27, 27) ~ "9-Fall", 
      test == "sre2c" & between(raw, 28, 28) ~ "9-Spring", 
      test == "sre2c" & between(raw, 29, 29) ~ "10-Spring", 
      test == "sre2c" & between(raw, 30, 30) ~ "11-Spring", 
      test == "sre2c" & between(raw, 31, 32) ~ "12-Fall", 
      test == "sre2c" & between(raw, 33, 36) ~ "12-Spring", 
      test == "sre2c" & between(raw, 37, 51) ~ "> stop grade", 
      test == "rnlc" & between(raw, 0, 50) ~ "< start grade", 
      test == "rnlc" & between(raw, 51, 59) ~ "1-Fall", 
      test == "rnlc" & between(raw, 60, 64) ~ "1-Spring", 
      test == "rnlc" & between(raw, 65, 69) ~ "2-Fall", 
      test == "rnlc" & between(raw, 70, 75) ~ "2-Spring", 
      test == "rnlc" & between(raw, 76, 79) ~ "3-Fall", 
      test == "rnlc" & between(raw, 80, 83) ~ "3-Spring", 
      test == "rnlc" & between(raw, 84, 87) ~ "4-Fall", 
      test == "rnlc" & between(raw, 88, 91) ~ "4-Spring", 
      test == "rnlc" & between(raw, 92, 95) ~ "5-Fall", 
      test == "rnlc" & between(raw, 96, 99) ~ "5-Spring", 
      test == "rnlc" & between(raw, 100, 103) ~ "6-Fall", 
      test == "rnlc" & between(raw, 104, 106) ~ "6-Spring", 
      test == "rnlc" & between(raw, 107, 109) ~ "7-Fall", 
      test == "rnlc" & between(raw, 110, 112) ~ "7-Spring", 
      test == "rnlc" & between(raw, 113, 114) ~ "8-Fall", 
      test == "rnlc" & between(raw, 115, 117) ~ "8-Spring", 
      test == "rnlc" & between(raw, 118, 120) ~ "9-Fall", 
      test == "rnlc" & between(raw, 121, 122) ~ "9-Spring", 
      test == "rnlc" & between(raw, 123, 124) ~ "10-Fall", 
      test == "rnlc" & between(raw, 125, 126) ~ "10-Spring", 
      test == "rnlc" & between(raw, 127, 128) ~ "11-Fall", 
      test == "rnlc" & between(raw, 129, 130) ~ "11-Spring", 
      test == "rnlc" & between(raw, 131, 131) ~ "12-Fall", 
      test == "rnlc" & between(raw, 132, 132) ~ "12-Spring", 
      test == "rnlc" & between(raw, 133, 200) ~ "> stop grade", 
      test == "lmc" & between(raw, 0, 3) ~ "< start grade", 
      test == "lmc" & between(raw, 4, 4) ~ "1-Fall", 
      test == "lmc" & between(raw, 5, 5) ~ "2-Spring", 
      test == "lmc" & between(raw, 6, 6) ~ "4-Spring", 
      test == "lmc" & between(raw, 7, 7) ~ "6-Spring", 
      test == "lmc" & between(raw, 8, 8) ~ "11-Spring", 
      test == "lmc" & between(raw, 9, 9) ~ "12-Spring", 
      test == "lmc" & between(raw, 10, 20) ~ "> stop grade", 
      test == "rpwc" & between(raw, 0, 9) ~ "< start grade", 
      test == "rpwc" & between(raw, 10, 14) ~ "1-Fall", 
      test == "rpwc" & between(raw, 15, 17) ~ "1-Spring", 
      test == "rpwc" & between(raw, 18, 19) ~ "2-Fall", 
      test == "rpwc" & between(raw, 20, 22) ~ "2-Spring", 
      test == "rpwc" & between(raw, 23, 24) ~ "3-Fall", 
      test == "rpwc" & between(raw, 25, 26) ~ "3-Spring", 
      test == "rpwc" & between(raw, 27, 27) ~ "4-Fall", 
      test == "rpwc" & between(raw, 28, 29) ~ "4-Spring", 
      test == "rpwc" & between(raw, 30, 31) ~ "5-Fall", 
      test == "rpwc" & between(raw, 32, 32) ~ "5-Spring", 
      test == "rpwc" & between(raw, 33, 33) ~ "6-Fall", 
      test == "rpwc" & between(raw, 34, 34) ~ "6-Spring", 
      test == "rpwc" & between(raw, 35, 35) ~ "7-Fall", 
      test == "rpwc" & between(raw, 36, 36) ~ "7-Spring", 
      test == "rpwc" & between(raw, 37, 37) ~ "8-Fall", 
      test == "rpwc" & between(raw, 38, 38) ~ "9-Fall", 
      test == "rpwc" & between(raw, 39, 39) ~ "10-Fall", 
      test == "rpwc" & between(raw, 40, 40) ~ "11-Fall", 
      test == "rpwc" & between(raw, 41, 41) ~ "12-Fall", 
      test == "rpwc" & between(raw, 42, 44) ~ "12-Spring", 
      test == "rpwc" & between(raw, 45, 60) ~ "> stop grade", 
      test == "riwc" & between(raw, 0, 24) ~ "< start grade", 
      test == "riwc" & between(raw, 25, 31) ~ "1-Fall", 
      test == "riwc" & between(raw, 32, 39) ~ "1-Spring", 
      test == "riwc" & between(raw, 40, 46) ~ "2-Fall", 
      test == "riwc" & between(raw, 47, 53) ~ "2-Spring", 
      test == "riwc" & between(raw, 54, 59) ~ "3-Fall", 
      test == "riwc" & between(raw, 60, 63) ~ "3-Spring", 
      test == "riwc" & between(raw, 64, 68) ~ "4-Fall", 
      test == "riwc" & between(raw, 69, 71) ~ "4-Spring", 
      test == "riwc" & between(raw, 72, 74) ~ "5-Fall", 
      test == "riwc" & between(raw, 75, 77) ~ "5-Spring", 
      test == "riwc" & between(raw, 78, 79) ~ "6-Fall", 
      test == "riwc" & between(raw, 80, 81) ~ "6-Spring", 
      test == "riwc" & between(raw, 82, 82) ~ "7-Fall", 
      test == "riwc" & between(raw, 83, 84) ~ "7-Spring", 
      test == "riwc" & between(raw, 85, 85) ~ "8-Fall", 
      test == "riwc" & between(raw, 86, 86) ~ "8-Spring", 
      test == "riwc" & between(raw, 87, 87) ~ "9-Spring", 
      test == "riwc" & between(raw, 88, 88) ~ "10-Spring", 
      test == "riwc" & between(raw, 89, 90) ~ "11-Spring", 
      test == "riwc" & between(raw, 91, 95) ~ "12-Spring", 
      test == "riwc" & between(raw, 96, 100) ~ "> stop grade", 
      test == "sslc" & between(raw, 0, 8) ~ "< start grade", 
      test == "sslc" & between(raw, 9, 11) ~ "1-Fall", 
      test == "sslc" & between(raw, 12, 12) ~ "1-Spring", 
      test == "sslc" & between(raw, 13, 13) ~ "2-Fall", 
      test == "sslc" & between(raw, 14, 14) ~ "3-Fall", 
      test == "sslc" & between(raw, 15, 15) ~ "4-Fall", 
      test == "sslc" & between(raw, 16, 16) ~ "5-Spring", 
      test == "sslc" & between(raw, 17, 17) ~ "9-Spring", 
      test == "sslc" & between(raw, 18, 20) ~ "12-Spring", 
      test == "sslc" & between(raw, 21, 42) ~ "> stop grade", 
      test == "lvc" & between(raw, 0, 8) ~ "< start grade", 
      test == "lvc" & between(raw, 9, 10) ~ "1-Fall", 
      test == "lvc" & between(raw, 11, 12) ~ "1-Spring", 
      test == "lvc" & between(raw, 13, 14) ~ "2-Spring", 
      test == "lvc" & between(raw, 15, 15) ~ "3-Fall", 
      test == "lvc" & between(raw, 16, 17) ~ "3-Spring", 
      test == "lvc" & between(raw, 18, 19) ~ "4-Spring", 
      test == "lvc" & between(raw, 20, 21) ~ "5-Spring", 
      test == "lvc" & between(raw, 22, 22) ~ "6-Spring", 
      test == "lvc" & between(raw, 23, 24) ~ "7-Spring", 
      test == "lvc" & between(raw, 25, 25) ~ "8-Spring", 
      test == "lvc" & between(raw, 26, 26) ~ "9-Spring", 
      test == "lvc" & between(raw, 27, 27) ~ "10-Spring", 
      test == "lvc" & between(raw, 28, 28) ~ "11-Spring", 
      test == "lvc" & between(raw, 29, 30) ~ "12-Spring", 
      test == "lvc" & between(raw, 31, 38) ~ "> stop grade", 
      test == "ganc" & between(raw, 0, 8) ~ "< start grade", 
      test == "ganc" & between(raw, 9, 10) ~ "1-Fall", 
      test == "ganc" & between(raw, 11, 11) ~ "1-Spring", 
      test == "ganc" & between(raw, 12, 13) ~ "2-Fall", 
      test == "ganc" & between(raw, 14, 14) ~ "2-Spring", 
      test == "ganc" & between(raw, 15, 15) ~ "3-Fall", 
      test == "ganc" & between(raw, 16, 16) ~ "3-Spring", 
      test == "ganc" & between(raw, 17, 17) ~ "4-Fall", 
      test == "ganc" & between(raw, 18, 18) ~ "4-Spring", 
      test == "ganc" & between(raw, 19, 19) ~ "5-Fall", 
      test == "ganc" & between(raw, 20, 20) ~ "5-Spring", 
      test == "ganc" & between(raw, 21, 21) ~ "6-Fall", 
      test == "ganc" & between(raw, 22, 23) ~ "7-Fall", 
      test == "ganc" & between(raw, 24, 24) ~ "8-Spring", 
      test == "ganc" & between(raw, 25, 25) ~ "9-Spring", 
      test == "ganc" & between(raw, 26, 26) ~ "11-Spring", 
      test == "ganc" & between(raw, 27, 28) ~ "12-Spring", 
      test == "ganc" & between(raw, 29, 40) ~ "> stop grade",
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
  
  # ### START HERE BY EXAMIING WRITTEN OUTPUT TO THIS POINT
  
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





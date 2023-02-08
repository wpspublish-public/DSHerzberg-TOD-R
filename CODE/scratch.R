suppressMessages(library(here))
suppressMessages(library(tidyverse))

tests <- c("DRIQ", "DRIW", "LWC", "PV", "QRF", "WRF")
adult_tests <- c("DRIQ", "LWC", "PV", "QRF")
norm_groups <- c("age", "grade", "adult")
age_stems <- str_c(tests, "-age")
grade_stems <- str_c(tests, "-grade")
adult_stems <- str_c(adult_tests, "-adult")
age_grade_order <-
  c(
    "5.0-5.3",
    "5.4-5.7",
    "5.8-5.11",
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
    "K-Fall",
    "K-Spring",
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
      "INPUT-FILES/OES-INPUT-TABLES/TOD-S/ss-to-percentile.csv"
    )
  )
)

age_lookups <- map(
  age_stems,
  ~
    suppressMessages(
      read_csv(
        here(
          str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-S/", .x, "-lookup.csv")
        )
      )) %>% 
    pivot_longer(
      cols = -raw,
      names_to = "age_grade",
      values_to = "ss"
    ) %>% 
    mutate(
      test = str_sub(.x, 1, -5),
      norm_group = "age"
    ) %>% 
    select(norm_group, test, age_grade, raw, ss) %>% 
    arrange(norm_group, test, 
            match(age_grade, age_grade_order), raw)
) %>% 
  bind_rows()

grade_lookups <- map(
  grade_stems,
  ~
    suppressMessages(
      read_csv(
        here(
          str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-S/", .x, "-lookup.csv")
        )
      )) %>% 
    pivot_longer(
      cols = -raw,
      names_to = "age_grade",
      values_to = "ss"
    ) %>% 
    mutate(
      test = str_sub(.x, 1, -7),
      norm_group = "grade"
    ) %>% 
    select(norm_group, test, age_grade, raw, ss) %>% 
    arrange(norm_group, test, 
            match(age_grade, age_grade_order), raw)
) %>% 
  bind_rows()

adult_lookups <- map(
  adult_stems,
  ~
    suppressMessages(
      read_csv(
        here(
          str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-S/", .x, "-lookup.csv")
        )
      )) %>% 
    pivot_longer(
      cols = -raw,
      names_to = "age_grade",
      values_to = "ss"
    ) %>% 
    mutate(
      test = str_sub(.x, 1, -7),
      norm_group = "adult"
    ) %>% 
    select(norm_group, test, age_grade, raw, ss) %>% 
    arrange(norm_group, test, 
            match(age_grade, age_grade_order), raw)
) %>% 
  bind_rows()

all_lookups <- bind_rows(age_lookups,
                         grade_lookups,
                         adult_lookups) %>%
  left_join(ss_percentile_lookup, by = "ss") %>%
  mutate(
    equiv = case_when(
      norm_group == "age" & test == "PV"  & between(raw, 26, 96) ~ "< Start age",
      norm_group == "age" & test == "PV"  & between(raw, 97, 101) ~ "5:0 to 5:3",
      norm_group == "age" & test == "PV"  & between(raw, 102, 103) ~ "5:4 to 5:7",
      norm_group == "age" & test == "PV"  & between(raw, 104, 105) ~ "5:8 to 5:11",
      norm_group == "age" & test == "PV"  & between(raw, 106, 107) ~ "6:0 to 6:3",
      norm_group == "age" & test == "PV"  & between(raw, 108, 109) ~ "6:4 to 6:7",
      norm_group == "age" & test == "PV"  & between(raw, 110, 111) ~ "6:8 to 6:11",
      norm_group == "age" & test == "PV"  & between(raw, 112, 113) ~ "7:0 to 7:3",
      norm_group == "age" & test == "PV"  & between(raw, 114, 115) ~ "7:4 to 7:7",
      norm_group == "age" & test == "PV"  & between(raw, 116, 116) ~ "7:8 to 7:11",
      norm_group == "age" & test == "PV"  & between(raw, 117, 118) ~ "8:0 to 8:5",
      norm_group == "age" & test == "PV"  & between(raw, 119, 120) ~ "8:6 to 8:11",
      norm_group == "age" & test == "PV"  & between(raw, 121, 122) ~ "9:0 to 9:5",
      norm_group == "age" & test == "PV"  & between(raw, 123, 123) ~ "9:6 to 9:11",
      norm_group == "age" & test == "PV"  & between(raw, 124, 125) ~ "10:0 to 10:5",
      norm_group == "age" & test == "PV"  & between(raw, 126, 126) ~ "10:6 to 10:11",
      norm_group == "age" & test == "PV"  & between(raw, 127, 127) ~ "11:0 to 11:5",
      norm_group == "age" & test == "PV"  & between(raw, 128, 128) ~ "11:6 to 11:11",
      norm_group == "age" & test == "PV"  & between(raw, 129, 129) ~ "12:0 to 12:5",
      norm_group == "age" & test == "PV"  & between(raw, 130, 130) ~ "12:6 to 12:11",
      norm_group == "age" & test == "PV"  & between(raw, 131, 131) ~ "13:0 to 13:11",
      norm_group == "age" & test == "PV"  & between(raw, 132, 132) ~ "14:0 to 14:11",
      norm_group == "age" & test == "PV"  & between(raw, 133, 135) ~ "15:0 to 16:11",
      norm_group == "age" & test == "PV"  & between(raw, 136, 138) ~ "17:0 to 18:11",
      norm_group == "age" & test == "PV"  & between(raw, 139, 173) ~ "> stop age",
      norm_group == "age" & test == "LWC"  & between(raw, 29, 82) ~ "< Start age",
      norm_group == "age" & test == "LWC"  & between(raw, 83, 88) ~ "5:0 to 5:3",
      norm_group == "age" & test == "LWC"  & between(raw, 89, 91) ~ "5:4 to 5:7",
      norm_group == "age" & test == "LWC"  & between(raw, 92, 95) ~ "5:8 to 5:11",
      norm_group == "age" & test == "LWC"  & between(raw, 96, 98) ~ "6:0 to 6:3",
      norm_group == "age" & test == "LWC"  & between(raw, 99, 102) ~ "6:4 to 6:7",
      norm_group == "age" & test == "LWC"  & between(raw, 103, 105) ~ "6:8 to 6:11",
      norm_group == "age" & test == "LWC"  & between(raw, 106, 107) ~ "7:0 to 7:3",
      norm_group == "age" & test == "LWC"  & between(raw, 108, 110) ~ "7:4 to 7:7",
      norm_group == "age" & test == "LWC"  & between(raw, 111, 112) ~ "7:8 to 7:11",
      norm_group == "age" & test == "LWC"  & between(raw, 113, 115) ~ "8:0 to 8:5",
      norm_group == "age" & test == "LWC"  & between(raw, 116, 118) ~ "8:6 to 8:11",
      norm_group == "age" & test == "LWC"  & between(raw, 119, 120) ~ "9:0 to 9:5",
      norm_group == "age" & test == "LWC"  & between(raw, 121, 122) ~ "9:6 to 9:11",
      norm_group == "age" & test == "LWC"  & between(raw, 123, 124) ~ "10:0 to 10:5",
      norm_group == "age" & test == "LWC"  & between(raw, 125, 125) ~ "10:6 to 10:11",
      norm_group == "age" & test == "LWC"  & between(raw, 126, 126) ~ "11:0 to 11:5",
      norm_group == "age" & test == "LWC"  & between(raw, 127, 127) ~ "11:6 to 11:11",
      norm_group == "age" & test == "LWC"  & between(raw, 128, 129) ~ "13:0 to 13:11",
      norm_group == "age" & test == "LWC"  & between(raw, 130, 131) ~ "14:0 to 14:11",
      norm_group == "age" & test == "LWC"  & between(raw, 132, 134) ~ "15:0 to 16:11",
      norm_group == "age" & test == "LWC"  & between(raw, 135, 139) ~ "17:0 to 18:11",
      norm_group == "age" & test == "LWC"  & between(raw, 140, 168) ~ "> stop age",
      norm_group == "age" & test == "WRF" & between(raw, 0, 4) ~ "< Start age",
      norm_group == "age" & test == "WRF" & between(raw, 5, 8) ~ "5:0 to 5:3",
      norm_group == "age" & test == "WRF" & between(raw, 9, 11) ~ "5:4 to 5:7",
      norm_group == "age" & test == "WRF" & between(raw, 12, 14) ~ "5:8 to 5:11",
      norm_group == "age" & test == "WRF" & between(raw, 15, 17) ~ "6:0 to 6:3",
      norm_group == "age" & test == "WRF" & between(raw, 18, 20) ~ "6:4 to 6:7",
      norm_group == "age" & test == "WRF" & between(raw, 21, 22) ~ "6:8 to 6:11",
      norm_group == "age" & test == "WRF" & between(raw, 23, 25) ~ "7:0 to 7:3",
      norm_group == "age" & test == "WRF" & between(raw, 34, 35) ~ "7:4 to 7:7",
      norm_group == "age" & test == "WRF" & between(raw, 28, 31) ~ "7:8 to 7:11",
      norm_group == "age" & test == "WRF" & between(raw, 32, 68) ~ "> stop age",
      norm_group == "age" & test == "QRF" & between(raw, 0, 17) ~ "< Start age",
      norm_group == "age" & test == "QRF" & between(raw, 18, 22) ~ "7:0 to 7:3",
      norm_group == "age" & test == "QRF" & between(raw, 23, 23) ~ "7:4 to 7:7",
      norm_group == "age" & test == "QRF" & between(raw, 24, 25) ~ "7:8 to 7:11",
      norm_group == "age" & test == "QRF" & between(raw, 26, 28) ~ "8:0 to 8:5",
      norm_group == "age" & test == "QRF" & between(raw, 29, 30) ~ "8:6 to 8:11",
      norm_group == "age" & test == "QRF" & between(raw, 31, 33) ~ "9:0 to 9:5",
      norm_group == "age" & test == "QRF" & between(raw, 34, 35) ~ "9:6 to 9:11",
      norm_group == "age" & test == "QRF" & between(raw, 36, 37) ~ "10:0 to 10:5",
      norm_group == "age" & test == "QRF" & between(raw, 38, 39) ~ "10:6 to 10:11",
      norm_group == "age" & test == "QRF" & between(raw, 40, 41) ~ "11:0 to 11:5",
      norm_group == "age" & test == "QRF" & between(raw, 42, 43) ~ "11:6 to 11:11",
      norm_group == "age" & test == "QRF" & between(raw, 44, 44) ~ "12:0 to 12:5",
      norm_group == "age" & test == "QRF" & between(raw, 45, 46) ~ "12:6 to 12:11",
      norm_group == "age" & test == "QRF" & between(raw, 47, 48) ~ "13:0 to 13:11",
      norm_group == "age" & test == "QRF" & between(raw, 49, 51) ~ "14:0 to 14:11",
      norm_group == "age" & test == "QRF" & between(raw, 52, 54) ~ "15:0 to 16:11",
      norm_group == "age" & test == "QRF" & between(raw, 55, 61) ~ "17:0 to 18:11",
      norm_group == "age" & test == "QRF" & between(raw, 62, 93) ~ "> stop age",
      norm_group == "grade" & test == "PV" & between(raw, 29, 97) ~ "< start grade",
      norm_group == "grade" & test == "PV" & between(raw, 98, 103) ~ "K – Fall",
      norm_group == "grade" & test == "PV" & between(raw, 104, 107) ~ "K – Spring",
      norm_group == "grade" & test == "PV" & between(raw, 108, 111) ~ "1 – Fall",
      norm_group == "grade" & test == "PV" & between(raw, 112, 114) ~ "1 – Spring",
      norm_group == "grade" & test == "PV" & between(raw, 115, 117) ~ "2 – Fall",
      norm_group == "grade" & test == "PV" & between(raw, 118, 119) ~ "2 – Spring",
      norm_group == "grade" & test == "PV" & between(raw, 120, 121) ~ "3 – Fall",
      norm_group == "grade" & test == "PV" & between(raw, 122, 122) ~ "3 – Spring",
      norm_group == "grade" & test == "PV" & between(raw, 123, 124) ~ "4 – Fall",
      norm_group == "grade" & test == "PV" & between(raw, 125, 125) ~ "4 – Spring",
      norm_group == "grade" & test == "PV" & between(raw, 126, 126) ~ "5 – Fall",
      norm_group == "grade" & test == "PV" & between(raw, 127, 127) ~ "5 – Spring",
      norm_group == "grade" & test == "PV" & between(raw, 128, 128) ~ "6 – Fall",
      norm_group == "grade" & test == "PV" & between(raw, 129, 129) ~ "6 – Spring",
      norm_group == "grade" & test == "PV" & between(raw, 130, 130) ~ "7 – Spring",
      norm_group == "grade" & test == "PV" & between(raw, 131, 131) ~ "8 – Fall",
      norm_group == "grade" & test == "PV" & between(raw, 132, 132) ~ "9 – Fall",
      norm_group == "grade" & test == "PV" & between(raw, 133, 133) ~ "9 – Spring",
      norm_group == "grade" & test == "PV" & between(raw, 134, 134) ~ "10 – Fall",
      norm_group == "grade" & test == "PV" & between(raw, 135, 135) ~ "10 – Spring",
      norm_group == "grade" & test == "PV" & between(raw, 136, 136) ~ "11 – Fall",
      norm_group == "grade" & test == "PV" & between(raw, 137, 137) ~ "11 – Spring",
      norm_group == "grade" & test == "PV" & between(raw, 138, 138) ~ "12 – Fall",
      norm_group == "grade" & test == "PV" & between(raw, 139, 139) ~ "12 – Spring",
      norm_group == "grade" & test == "PV" & between(raw, 143, 173) ~ "> 12",
      norm_group == "grade" & test == "LWC" & between(raw, 29, 84) ~ "< start grade",
      norm_group == "grade" & test == "LWC" & between(raw, 85, 91) ~ "K – Fall",
      norm_group == "grade" & test == "LWC" & between(raw, 92, 97) ~ "K – Spring",
      norm_group == "grade" & test == "LWC" & between(raw, 98, 103) ~ "1 – Fall",
      norm_group == "grade" & test == "LWC" & between(raw, 104, 107) ~ "1 – Spring",
      norm_group == "grade" & test == "LWC" & between(raw, 108, 111) ~ "2 – Fall",
      norm_group == "grade" & test == "LWC" & between(raw, 112, 114) ~ "2 – Spring",
      norm_group == "grade" & test == "LWC" & between(raw, 115, 117) ~ "3 – Fall",
      norm_group == "grade" & test == "LWC" & between(raw, 118, 120) ~ "3 – Spring",
      norm_group == "grade" & test == "LWC" & between(raw, 121, 122) ~ "4 – Fall",
      norm_group == "grade" & test == "LWC" & between(raw, 123, 123) ~ "4 – Spring",
      norm_group == "grade" & test == "LWC" & between(raw, 124, 124) ~ "5 – Fall",
      norm_group == "grade" & test == "LWC" & between(raw, 125, 125) ~ "5 – Spring",
      norm_group == "grade" & test == "LWC" & between(raw, 126, 126) ~ "6 – Fall",
      norm_group == "grade" & test == "LWC" & between(raw, 127, 127) ~ "7 – Fall",
      norm_group == "grade" & test == "LWC" & between(raw, 128, 128) ~ "8 – Spring",
      norm_group == "grade" & test == "LWC" & between(raw, 129, 129) ~ "9 – Spring",
      norm_group == "grade" & test == "LWC" & between(raw, 130, 130) ~ "10 – Fall",
      norm_group == "grade" & test == "LWC" & between(raw, 131, 131) ~ "10 – Spring",
      norm_group == "grade" & test == "LWC" & between(raw, 132, 132) ~ "11 – Fall",
      norm_group == "grade" & test == "LWC" & between(raw, 133, 134) ~ "11 – Spring",
      norm_group == "grade" & test == "LWC" & between(raw, 135, 137) ~ "12 – Fall",
      norm_group == "grade" & test == "LWC" & between(raw, 138, 144) ~ "12 – Spring",
      norm_group == "grade" & test == "LWC" & between(raw, 145, 168) ~ "> 12",
      norm_group == "grade" & test == "WRF" & between(raw, 0, 7) ~ "< start grade",
      norm_group == "grade" & test == "WRF" & between(raw, 8, 13) ~ "K – Fall",
      norm_group == "grade" & test == "WRF" & between(raw, 14, 18) ~ "K – Spring",
      norm_group == "grade" & test == "WRF" & between(raw, 19, 22) ~ "1 – Fall",
      norm_group == "grade" & test == "WRF" & between(raw, 23, 26) ~ "1 – Spring",
      norm_group == "grade" & test == "WRF" & between(raw, 27, 68) ~ "> 12",
      norm_group == "grade" & test == "QRF" & between(raw, 0, 19) ~ "< start grade",
      norm_group == "grade" & test == "QRF" & between(raw, 20, 24) ~ "2 – Fall",
      norm_group == "grade" & test == "QRF" & between(raw, 25, 27) ~ "2 – Spring",
      norm_group == "grade" & test == "QRF" & between(raw, 28, 29) ~ "3 – Fall",
      norm_group == "grade" & test == "QRF" & between(raw, 30, 31) ~ "3 – Spring",
      norm_group == "grade" & test == "QRF" & between(raw, 32, 34) ~ "4 – Fall",
      norm_group == "grade" & test == "QRF" & between(raw, 35, 36) ~ "4 – Spring",
      norm_group == "grade" & test == "QRF" & between(raw, 37, 38) ~ "5 – Fall",
      norm_group == "grade" & test == "QRF" & between(raw, 39, 40) ~ "5 – Spring",
      norm_group == "grade" & test == "QRF" & between(raw, 41, 42) ~ "6 – Fall",
      norm_group == "grade" & test == "QRF" & between(raw, 43, 44) ~ "6 – Spring",
      norm_group == "grade" & test == "QRF" & between(raw, 45, 45) ~ "7 – Fall",
      norm_group == "grade" & test == "QRF" & between(raw, 46, 47) ~ "7 – Spring",
      norm_group == "grade" & test == "QRF" & between(raw, 48, 48) ~ "8 – Fall",
      norm_group == "grade" & test == "QRF" & between(raw, 49, 50) ~ "8 – Spring",
      norm_group == "grade" & test == "QRF" & between(raw, 51, 51) ~ "9 – Fall",
      norm_group == "grade" & test == "QRF" & between(raw, 52, 52) ~ "9 – Spring",
      norm_group == "grade" & test == "QRF" & between(raw, 53, 53) ~ "10 – Fall",
      norm_group == "grade" & test == "QRF" & between(raw, 54, 54) ~ "10 – Spring",
      norm_group == "grade" & test == "QRF" & between(raw, 55, 55) ~ "11 – Fall",
      norm_group == "grade" & test == "QRF" & between(raw, 56, 56) ~ "11 – Spring",
      norm_group == "grade" & test == "QRF" & between(raw, 57, 62) ~ "12 – Spring",
      norm_group == "grade" & test == "QRF" & between(raw, 63, 93) ~ "> 12",
      TRUE ~ NA_character_
    )
  )

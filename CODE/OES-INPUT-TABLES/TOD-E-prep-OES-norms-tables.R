suppressMessages(library(here))
suppressMessages(library(tidyverse))

tests <- c("erlne", "esege", "lske", "lswe", "rhye", "spwe", "eddiw",
           "eddiq", "elp", "ersw", "ersq", "eswa", "ephk", "ebrs", "epha")
norm_groups <- c("age", "grade")
age_stems <- str_c(tests, "-age")
grade_stems <- str_c(tests, "-grade")
age_grade_order <-
  c(
    "5.0-5.3",
    "5.4-5.7",
    "5.8-5.11",
    "6.0-6.5",
    "6.6-6.11",
    "7.0-7.5",
    "7.6-7.11",
    "8.0-8.5",
    "8.6-9.3",
    "K-Fall",
    "K-Spring",
    "1-Fall",
    "1-Spring",
    "2-Fall",
    "2-Spring"
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

age_lookups <- map(
  age_stems,
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
      norm_group = "grade"
    ) %>% 
    select(norm_group, test, age_grade, raw, ss) %>% 
    arrange(norm_group, test, 
            match(age_grade, age_grade_order), raw)
) %>% 
  bind_rows()

all_lookups <- bind_rows(age_lookups,
                         grade_lookups) %>%
  left_join(ss_percentile_lookup, by = "ss") %>%
  mutate(
    equiv = case_when(
      norm_group == "age" & test == "spwe"  & between(raw, 0, 7) ~ "< start age",
      norm_group == "age" & test == "spwe"  & between(raw, 8, 10) ~ "5:0 to 5:3",
      norm_group == "age" & test == "spwe"  & between(raw, 11, 12) ~ "5:4 to 5:7",
      norm_group == "age" & test == "spwe"  & between(raw, 13, 13) ~ "5:8 to 5:11",
      norm_group == "age" & test == "spwe"  & between(raw, 14, 15) ~ "6:0 to 6:5",
      norm_group == "age" & test == "spwe"  & between(raw, 16, 18) ~ "6:6 to 6:11",
      norm_group == "age" & test == "spwe"  & between(raw, 19, 20) ~ "7:0 to 7:5",
      norm_group == "age" & test == "spwe"  & between(raw, 21, 22) ~ "7:6 to 7:11",
      norm_group == "age" & test == "spwe"  & between(raw, 23, 24) ~ "8:0 to 8:5",
      norm_group == "age" & test == "spwe"  & between(raw, 25, 26) ~ "8:6 to 9:3",
      norm_group == "age" & test == "spwe"  & between(raw, 27, 32) ~ "> stop age",
      norm_group == "age" & test == "rhye"  & between(raw, 0, 3) ~ "< start age",
      norm_group == "age" & test == "rhye"  & between(raw, 4, 6) ~ "5:0 to 5:3",
      norm_group == "age" & test == "rhye"  & between(raw, 7, 9) ~ "5:4 to 5:7",
      norm_group == "age" & test == "rhye"  & between(raw, 10, 12) ~ "5:8 to 5:11",
      norm_group == "age" & test == "rhye"  & between(raw, 13, 16) ~ "6:0 to 6:5",
      norm_group == "age" & test == "rhye"  & between(raw, 17, 20) ~ "6:6 to 6:11",
      norm_group == "age" & test == "rhye"  & between(raw, 21, 23) ~ "7:0 to 7:5",
      norm_group == "age" & test == "rhye"  & between(raw, 24, 24) ~ "7:6 to 7:11",
      norm_group == "age" & test == "rhye"  & between(raw, 25, 25) ~ "8:0 to 8:5",
      norm_group == "age" & test == "rhye"  & between(raw, 26, 27) ~ "8:6 to 9:3",
      norm_group == "age" & test == "rhye"  & between(raw, 28, 30) ~ "> stop age",
      norm_group == "age" & test == "erlne"  & between(raw, 0, 32) ~ "< start age",
      norm_group == "age" & test == "erlne"  & between(raw, 33, 40) ~ "5:0 to 5:3",
      norm_group == "age" & test == "erlne"  & between(raw, 41, 44) ~ "5:4 to 5:7",
      norm_group == "age" & test == "erlne"  & between(raw, 45, 50) ~ "5:8 to 5:11",
      norm_group == "age" & test == "erlne"  & between(raw, 51, 56) ~ "6:0 to 6:5",
      norm_group == "age" & test == "erlne"  & between(raw, 57, 65) ~ "6:6 to 6:11",
      norm_group == "age" & test == "erlne"  & between(raw, 66, 73) ~ "7:0 to 7:5",
      norm_group == "age" & test == "erlne"  & between(raw, 74, 79) ~ "7:6 to 7:11",
      norm_group == "age" & test == "erlne"  & between(raw, 80, 88) ~ "8:0 to 8:5",
      norm_group == "age" & test == "erlne"  & between(raw, 89, 99) ~ "8:6 to 9:3",
      norm_group == "age" & test == "erlne"  & between(raw, 100, 120) ~ "> stop age",
      norm_group == "age" & test == "lswe"  & between(raw, 0, 9) ~ "< start age",
      norm_group == "age" & test == "lswe"  & between(raw, 10, 12) ~ "5:0 to 5:3",
      norm_group == "age" & test == "lswe"  & between(raw, 13, 15) ~ "5:4 to 5:7",
      norm_group == "age" & test == "lswe"  & between(raw, 16, 18) ~ "5:8 to 5:11",
      norm_group == "age" & test == "lswe"  & between(raw, 19, 22) ~ "6:0 to 6:5",
      norm_group == "age" & test == "lswe"  & between(raw, 23, 27) ~ "6:6 to 6:11",
      norm_group == "age" & test == "lswe"  & between(raw, 28, 31) ~ "7:0 to 7:5",
      norm_group == "age" & test == "lswe"  & between(raw, 32, 33) ~ "7:6 to 7:11",
      norm_group == "age" & test == "lswe"  & between(raw, 34, 35) ~ "8:0 to 8:5",
      norm_group == "age" & test == "lswe"  & between(raw, 36, 38) ~ "8:6 to 9:3",
      norm_group == "age" & test == "esege"  & between(raw, 0, 4) ~ "< start age",
      norm_group == "age" & test == "esege"  & between(raw, 5, 9) ~ "5:0 to 5:3",
      norm_group == "age" & test == "esege"  & between(raw, 10, 12) ~ "5:4 to 5:7",
      norm_group == "age" & test == "esege"  & between(raw, 13, 14) ~ "5:8 to 5:11",
      norm_group == "age" & test == "esege"  & between(raw, 15, 17) ~ "6:0 to 6:5",
      norm_group == "age" & test == "esege"  & between(raw, 18, 19) ~ "6:6 to 6:11",
      norm_group == "age" & test == "esege"  & between(raw, 20, 20) ~ "7:0 to 7:5",
      norm_group == "age" & test == "esege"  & between(raw, 21, 21) ~ "7:6 to 7:11",
      norm_group == "age" & test == "esege"  & between(raw, 22, 22) ~ "8:0 to 8:5",
      norm_group == "age" & test == "esege"  & between(raw, 23, 23) ~ "8:6 to 9:3",
      norm_group == "age" & test == "esege"  & between(raw, 24, 25) ~ "> stop age",
      norm_group == "age" & test == "lske"  & between(raw, 0, 9) ~ "< start age",
      norm_group == "age" & test == "lske"  & between(raw, 10, 14) ~ "5:0 to 5:3",
      norm_group == "age" & test == "lske"  & between(raw, 15, 17) ~ "5:4 to 5:7",
      norm_group == "age" & test == "lske"  & between(raw, 18, 21) ~ "5:8 to 5:11",
      norm_group == "age" & test == "lske"  & between(raw, 22, 24) ~ "6:0 to 6:5",
      norm_group == "age" & test == "lske"  & between(raw, 25, 28) ~ "6:6 to 6:11",
      norm_group == "age" & test == "lske"  & between(raw, 29, 30) ~ "7:0 to 7:5",
      norm_group == "age" & test == "lske"  & between(raw, 31, 31) ~ "7:6 to 7:11",
      norm_group == "age" & test == "lske"  & between(raw, 32, 32) ~ "8:0 to 8:5",
      norm_group == "age" & test == "lske"  & between(raw, 33, 33) ~ "8:6 to 9:3",
      norm_group == "grade" & test == "spwe"  & between(raw, 0, 8) ~ "< start grade",
      norm_group == "grade" & test == "spwe"  & between(raw, 9, 12) ~ "K-Fall",
      norm_group == "grade" & test == "spwe"  & between(raw, 13, 14) ~ "K-Spring",
      norm_group == "grade" & test == "spwe"  & between(raw, 15, 17) ~ "1-Fall",
      norm_group == "grade" & test == "spwe"  & between(raw, 18, 20) ~ "1-Spring",
      norm_group == "grade" & test == "spwe"  & between(raw, 21, 22) ~ "2-Fall",
      norm_group == "grade" & test == "spwe"  & between(raw, 23, 25) ~ "2-Spring",
      norm_group == "grade" & test == "spwe"  & between(raw, 26, 32) ~ "> stop grade",
      norm_group == "grade" & test == "rhye"  & between(raw, 0, 5) ~ "< start grade",
      norm_group == "grade" & test == "rhye"  & between(raw, 6, 10) ~ "K-Fall",
      norm_group == "grade" & test == "rhye"  & between(raw, 11, 14) ~ "K-Spring",
      norm_group == "grade" & test == "rhye"  & between(raw, 15, 18) ~ "1-Fall",
      norm_group == "grade" & test == "rhye"  & between(raw, 19, 21) ~ "1-Spring",
      norm_group == "grade" & test == "rhye"  & between(raw, 22, 24) ~ "2-Fall",
      norm_group == "grade" & test == "rhye"  & between(raw, 25, 26) ~ "2-Spring",
      norm_group == "grade" & test == "rhye"  & between(raw, 27, 30) ~ "> stop grade",
      norm_group == "grade" & test == "erlne"  & between(raw, 0, 35) ~ "< start grade",
      norm_group == "grade" & test == "erlne"  & between(raw, 36, 46) ~ "K-Fall",
      norm_group == "grade" & test == "erlne"  & between(raw, 47, 55) ~ "K-Spring",
      norm_group == "grade" & test == "erlne"  & between(raw, 56, 64) ~ "1-Fall",
      norm_group == "grade" & test == "erlne"  & between(raw, 65, 71) ~ "1-Spring",
      norm_group == "grade" & test == "erlne"  & between(raw, 72, 78) ~ "2-Fall",
      norm_group == "grade" & test == "erlne"  & between(raw, 79, 88) ~ "2-Spring",
      norm_group == "grade" & test == "erlne"  & between(raw, 89, 120) ~ "> stop grade",
      norm_group == "grade" & test == "lswe"  & between(raw, 0, 11) ~ "< start grade",
      norm_group == "grade" & test == "lswe"  & between(raw, 12, 15) ~ "K-Fall",
      norm_group == "grade" & test == "lswe"  & between(raw, 16, 20) ~ "K-Spring",
      norm_group == "grade" & test == "lswe"  & between(raw, 21, 26) ~ "1-Fall",
      norm_group == "grade" & test == "lswe"  & between(raw, 27, 31) ~ "1-Spring",
      norm_group == "grade" & test == "lswe"  & between(raw, 32, 33) ~ "2-Fall",
      norm_group == "grade" & test == "lswe"  & between(raw, 34, 36) ~ "2-Spring",
      norm_group == "grade" & test == "lswe"  & between(raw, 37, 38) ~ "> stop grade",
      norm_group == "grade" & test == "esege"  & between(raw, 0, 8) ~ "< start grade",
      norm_group == "grade" & test == "esege"  & between(raw, 9, 12) ~ "K-Fall",
      norm_group == "grade" & test == "esege"  & between(raw, 13, 16) ~ "K-Spring",
      norm_group == "grade" & test == "esege"  & between(raw, 17, 18) ~ "1-Fall",
      norm_group == "grade" & test == "esege"  & between(raw, 19, 20) ~ "1-Spring",
      norm_group == "grade" & test == "esege"  & between(raw, 21, 21) ~ "2-Fall",
      norm_group == "grade" & test == "esege"  & between(raw, 22, 22) ~ "2-Spring",
      norm_group == "grade" & test == "esege"  & between(raw, 23, 25) ~ "> stop grade",
      norm_group == "grade" & test == "lske"  & between(raw, 0, 13) ~ "< start grade",
      norm_group == "grade" & test == "lske"  & between(raw, 14, 18) ~ "K-Fall",
      norm_group == "grade" & test == "lske"  & between(raw, 19, 23) ~ "K-Spring",
      norm_group == "grade" & test == "lske"  & between(raw, 24, 27) ~ "1-Fall",
      norm_group == "grade" & test == "lske"  & between(raw, 28, 29) ~ "1-Spring",
      norm_group == "grade" & test == "lske"  & between(raw, 30, 31) ~ "2-Fall",
      norm_group == "grade" & test == "lske"  & between(raw, 32, 33) ~ "2-Spring",
      TRUE ~ NA_character_
    ), 
    desc_range = case_when(
      !(test %in% c("eddiq", "eddiw")) & between(ss, 120, 130) ~ "Well Above Average", 
      !(test %in% c("eddiq", "eddiw")) & between(ss, 110, 119) ~ "Above Average", 
      !(test %in% c("eddiq", "eddiw")) & between(ss, 90, 109) ~ "Average", 
      !(test %in% c("eddiq", "eddiw")) & between(ss, 80, 89) ~ "Below Average", 
      !(test %in% c("eddiq", "eddiw")) & between(ss, 70, 79) ~ "Well Below Average", 
      !(test %in% c("eddiq", "eddiw")) & between(ss, 40, 69) ~ "Significantly Below Average", 
      test %in% c("eddiq", "eddiw") & between(ss, 120, 130) ~ "Well Above Average", 
      test %in% c("eddiq", "eddiw") & between(ss, 110, 119) ~ "Above Average", 
      test %in% c("eddiq", "eddiw") & between(ss, 100, 109) ~ "Average to Moderately Above Average", 
      test %in% c("eddiq", "eddiw") & between(ss, 90, 99) ~ "Average to Moderately Below Average", 
      test %in% c("eddiq", "eddiw") & between(ss, 80, 89) ~ "Below Average", 
      test %in% c("eddiq", "eddiw") & between(ss, 70, 79) ~ "Well Below Average", 
      test %in% c("eddiq", "eddiw") & between(ss, 40, 69) ~ "Significantly Below Average", 
      TRUE ~ NA_character_
    ), 
    risk = case_when(
      test %in% c("eddiq", "eddiw") & between(ss, 120, 130) ~ "Extremely Low Probability of Dyslexia", 
      test %in% c("eddiq", "eddiw") & between(ss, 110, 119) ~ "Very Low Probability of Dyslexia", 
      test %in% c("eddiq", "eddiw") & between(ss, 100, 109) ~ "Low Probability of Dyslexia", 
      test %in% c("eddiq", "eddiw") & between(ss, 90, 99) ~ "Moderate Probability of Dyslexia", 
      test %in% c("eddiq", "eddiw") & between(ss, 80, 89) ~ "High Probability of Dyslexia", 
      test %in% c("eddiq", "eddiw") & between(ss, 70, 79) ~ "Very High Probability of Dyslexia", 
      test %in% c("eddiq", "eddiw") & between(ss, 40, 69) ~ "Extremely High Probability of Dyslexia", 
      TRUE ~ NA_character_
    ), 
    CV_90 = case_when(
      age_grade %in% c(age_low, grade_low)  & test == "PV" ~  10,
      age_grade %in% c(age_low, grade_low)  & test == "LWC" ~  8,
      age_grade %in% c(age_middle, grade_middle)  & test == "PV" ~  11,
      age_grade %in% c(age_middle, grade_middle)  & test == "LWC" ~  10,
      age_grade %in% c(age_high, grade_high)  & test == "PV" ~  11,
      age_grade %in% c(age_high, grade_high)  & test == "LWC" ~  9,
      test == "WRF" ~  6,
      test == "QRF" ~  5,
      norm_group %in% c("age", "grade")  & test == "eddiw" ~  10, 
      norm_group %in% c("age", "grade")  & test == "eddiq" ~  8, 
      norm_group %in% c("adult")  & test == "eddiq" ~  7,
      TRUE ~ NA_real_
    ), 
    CV_95 = case_when(
      age_grade %in% c(age_low, grade_low) & test == "PV" ~ 12, 
      age_grade %in% c(age_low, grade_low) & test == "LWC" ~ 10, 
      age_grade %in% c(age_middle, grade_middle) & test == "PV" ~ 13, 
      age_grade %in% c(age_middle, grade_middle) & test == "LWC" ~ 11, 
      age_grade %in% c(age_high, grade_high) & test == "PV" ~ 13, 
      age_grade %in% c(age_high, grade_high) & test == "LWC" ~ 11, 
      test == "WRF" ~ 7, 
      test == "QRF" ~ 6, 
      norm_group %in% c("age", "grade") & test == "eddiw" ~ 12, 
      norm_group %in% c("age", "grade") & test == "eddiq" ~ 9, 
      norm_group %in% c("adult") & test == "eddiq" ~ 9,
      TRUE ~ NA_real_
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
  select(norm_group, test, age_grade, raw, ss, CI90, CI95, percentile, equiv, desc_range, risk)

age_lookups_v2 <- all_lookups %>%
  filter((norm_group %in% c("age", "adult")) &
           (test %in% c("PV", "LWC", "QRF", "WRF"))
  ) %>% 
  left_join(age_mo_min_max_lookup, by = "age_grade") %>% 
  select(norm_group, test, age_min, age_max, raw, ss, CI90, CI95, percentile, equiv, desc_range)

grade_lookups_v2 <- all_lookups %>%
  filter((norm_group %in% c("grade")) &
           (test %in% c("PV", "LWC", "QRF", "WRF"))
  ) %>% 
  select(test, age_grade, raw, ss, CI90, CI95, percentile, equiv, desc_range) %>% 
  rename(grade = age_grade)

indexcomposite_lookups_v2 <- all_lookups %>%
  filter(test %in% c("eddiq", "eddiw")
  ) %>% 
  select(norm_group, test, raw, ss, CI90, CI95, percentile, desc_range, risk) %>% 
  rename(score = test)

write_csv(
  all_lookups,
  here(
    "OUTPUT-FILES/OES-INPUT-TABLES/TOD-E/TOD-E-OES-lookup-table.csv"
  ),
  na = ""
)

write_csv(
  age_lookups_v2,
  here(
    "OUTPUT-FILES/OES-INPUT-TABLES/TOD-E/TOD-E-OES-age-lookup-table-v2.csv"
  ),
  na = ""
)

write_csv(
  grade_lookups_v2,
  here(
    "OUTPUT-FILES/OES-INPUT-TABLES/TOD-E/TOD-E-OES-grade-lookup-table-v2.csv"
  ),
  na = ""
)

write_csv(
  indexcomposite_lookups_v2,
  here(
    "OUTPUT-FILES/OES-INPUT-TABLES/TOD-E/TOD-E-OES-indexcomposite-lookup-table-v2.csv"
  ),
  na = ""
)


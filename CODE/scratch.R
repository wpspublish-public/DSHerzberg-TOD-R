suppressMessages(library(here))
suppressMessages(library(tidyverse))

scores <- c("DRIQ", "DRIW", "LWC", "PV", "QRF", "WRF")
norm_groups <- c("age", "grade", "adult")
age_stems <- str_c(scores, "-age")
grade_stems <- str_c(scores, "-grade")
lookup_names <- cross2(scores, norm_groups) %>% 
  map_chr(str_c, collapse = "-")
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



age_lookups <- suppressMessages(
  read_csv(
    here(
      "INPUT-FILES/OES-INPUT-TABLES/TOD-S/PV-adult-lookup.csv"
    )
  )) %>% 
  pivot_longer(
    cols = -raw,
    names_to = "age_grade",
    values_to = "ss"
  ) %>% 
  mutate(
    test = "PV",
    norm_group = "age"
  ) %>% 
  select(norm_group, test, age_grade, raw, ss) %>% 
  arrange(norm_group, test, 
          match(age_grade, age_grade_order), raw)

temp1 <- suppressMessages(
  read_csv(
    here(
      "INPUT-FILES/OES-INPUT-TABLES/TOD-S/PV-adult-lookup.csv"
    )
  )) %>% 
  pivot_longer(
    cols = -raw,
    names_to = "age_grade",
    values_to = "ss"
  ) %>% 
  mutate(
    test = "PV",
    norm_group = "age"
  ) %>% 
  select(norm_group, test, age_grade, raw, ss) %>% 
  arrange(norm_group, test, 
          match(age_grade, age_grade_order), raw)




temp2 <- map(lookup_names,
             ~
                     suppressMessages(read_csv(here(
                       str_c(
                         "INPUT-FILES/OES-INPUT-TABLES/TOD-S/",
                         .x,
                         "-lookup.csv"
                       )
                     )))) %>%
  set_names(lookup_names)

comp1 <- temp3[[4]]

comp2 <- suppressMessages(
  read_csv(
    here(
      "INPUT-FILES/OES-INPUT-TABLES/TOD-S/PV-age-lookup.csv"
    )
  )
)

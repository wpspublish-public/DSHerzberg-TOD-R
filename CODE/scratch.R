suppressMessages(library(here))
suppressMessages(library(tidyverse))

scores <- c("DRIQ", "DRIW", "LWC", "PV", "QRF", "WRF")
norm_groups <- c("age", "grade")
lookup_names <- cross2(scores, norm_groups) %>% 
  map_chr(str_c, collapse = "-")


temp1 <- suppressMessages(
  read_csv(
  here(
    "INPUT-FILES/OES-INPUT-TABLES/TOD-S/PV-age-lookup.csv"
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
  select(norm_group, test, age_grade, raw, ss)
  



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

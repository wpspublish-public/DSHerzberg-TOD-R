filename <- paste0("TOD_demo_tracking_output_", format(Sys.Date(), "%Y-%m-%d"), ".csv")

path1<- file.path("/Users", "dherzberg", "Desktop", "TOD-R", "DATA", "OUPUT", "TOD_demo_tracking_output_2019-04-05.csv")

TOD_demos <-
  suppressMessages(read_csv(here(
    'DATA/TOD_demos_input_AMK_2019-04-12.csv'
  ))) %>%
  mutate(
    agestrat = case_when(
      ageyear == 6 ~ "06",
      ageyear == 7 ~ "07",
      ageyear == 8 ~ "08",
      ageyear == 9 ~ "09",
      ageyear >= 10 & ageyear <= 24 ~ as.character(ageyear),
      ageyear >= 25 & ageyear <= 40 ~ "2540",
      ageyear >= 41 & ageyear <= 50 ~ "4150",
      ageyear >= 51 & ageyear <= 60 ~ "5160",
      ageyear >= 61 & ageyear <= 70 ~ "6170",
      ageyear >= 71 & ageyear <= 80 ~ "7180",
      ageyear >= 81 & ageyear <= 90 ~ "8190",
      TRUE ~ NA_character_
    ),
    ethnic1 = case_when(hispanic == "Yes" ~ "Hispanic",
                        TRUE ~ ethnic),
    region = case_when(
      inrange(zip, 1000, 8999) | inrange(zip, 10000, 19699) ~ "Northeast",
      inrange(zip, 900, 999) |
        inrange(
          zip,
          19700,
          33999) |
            inrange(
              zip,
              34100,
              42799) |
                inrange(zip, 70000, 79999) | inrange(zip, 88500, 88599) ~ "South",
      inrange(zip, 43000, 58999) | inrange(zip, 60000, 69999) ~ "Midwest",
      inrange(zip, 59000, 59999) |
        inrange(
          zip,
          80000,
          88499) |
        inrange(
          zip,
          88900,
          96199) |
        inrange(zip, 96700, 96899) | inrange(zip, 97000, 99999) ~ "West",
      TRUE ~ NA_character_
        )) %>% arrange(agestrat) %>% group_by(agestrat)
      





temp1 <- index_composites_lookups %>% 
  filter(score == "epa")

range(temp1$raw)


tests <- c("PHMC", "IWSC", "RLNC", "PWRC", "WPCC", "WMC", "PANC", "IWRC", "OREC", "BLNC", "SEGC", 
           "RWSC", "SRE1C", "SRE2C", "RNLC", "LMC", "RPWC", "RIWC", "SSLC", "LVC", "GANC")
age_grade_index_composites <- c("DDIW", "DDIQ", "LPI", "RSIW", "RSIQ", "SWA", "PK", "BRS", "DE", "SP", "RFW", 
                                "RFQ", "RCQ1", "RCQ2", "PA", "RAN", "AWM", "OP", "VO", "RE", "VR2", "VR4")
adult_index_composites <- c("DDIQ", "LPI", "RSIQ", "SWA", "PK", "BRS", "DE", "SP", "RFQ", "RCQ2", "PA", 
                            "RAN", "AWM", "OP", "VO", "RE", "VR2", "VR4")
temp <- test_age_stems[!str_detect(test_age_stems, "orec-age")]
temp3 <- str_c(tests, "-age")[!str_detect(str_c(tests, "-age"), "orec-age")]


temp <- c("PHMC", "IWSC", "RLNC", "PWRC", "WPCC", "WMC", "PANC", "IWRC", "OREC", "BLNC", "SEGC", 
          "RWSC", "SRE1C", "SRE2C", "RNLC", "LMC", "RPWC", "RIWC", "SSLC", "LVC", "GANC") %>% 
  str_to_lower(.)

temp2 <- map(
  test_age_stems,
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
      test = str_sub(.x, 1, -5),
    ) %>% 
    select(test, age_grade, raw, ss) %>% 
    arrange(test, 
            match(age_grade, age_grade_order), raw)
) %>% 
  bind_rows()  %>%
  left_join(ss_percentile_lookup, by = "ss") %>%
  left_join(cv_lookup, by = "test") %>% 
  left_join(age_mo_min_max_lookup, by = "age_grade")


age_mo_min_max_lookup <- bind_rows(
    suppressMessages(
    read_csv(
      here(
        "INPUT-FILES/OES-INPUT-TABLES/TOD-C/age-range-child.csv"
      )
    )
  ) ,
  suppressMessages(
    read_csv(
      here(
        "INPUT-FILES/OES-INPUT-TABLES/TOD-C/age-range-adult.csv"
      )
    )
  )
)     %>%
  rename(age_grade = original_age_range)


temp1 <- index_composites_lookups %>% 
  filter(score == "epa")

range(temp1$raw)


tests <- c("PHMC", "IWSC", "RLNC", "PWRC", "WPCC", "WMC", "PANC", "IWRC", "OREC", "BLNC", "SEGC", 
           "RWSC", "SRE1C", "SRE2C", "RNLC", "LMC", "RPWC", "RIWC", "SSLC", "LVC", "GANC")
age_grade_index_composites <- c("DDIW", "DDIQ", "LPI", "RSIW", "RSIQ", "SWA", "PK", "BRS", "DE", "SP", "RFW", 
                                "RFQ", "RCQ1", "RCQ2", "PA", "RAN", "AWM", "OP", "VO", "RE", "VR2", "VR4")
adult_index_composites <- c("DDIQ", "LPI", "RSIQ", "SWA", "PK", "BRS", "DE", "SP", "RFQ", "RCQ2", "PA", 
                            "RAN", "AWM", "OP", "VO", "RE", "VR2", "VR4")


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
      )))
    

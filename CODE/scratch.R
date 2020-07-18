# Check for any dupIDs (anyDuplicated() returns row number of FIRST dup ID encountered)
anyDuplicated(temp4$ID)

# Check for any NAs on IDNumber, returns TRUE if NA exist
any(is.na(miss_recode$ID))

# extract cases with Dup ID numbers or NA on IDNumber, write out for investigation
dupMissIDs <- miss_recode %>%
  mutate(dup = duplicated(ID)) %>%
  filter(dup == TRUE | is.na(ID)) %>%
  select(-dup) 

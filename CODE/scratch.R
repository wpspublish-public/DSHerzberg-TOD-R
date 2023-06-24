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

test <- miss_recode %>% 
  group_by(ID) %>% 
  summarise(
    range1 = nth(recode_cols, 1),
    range2 = nth(recode_cols, 2),
    range3 = nth(recode_cols, 3),
    range4 = nth(recode_cols, 4)
  )

ranges <- c("range1", "range2", "range3", "range4")

temp3 <- temp2 %>%
  left_join(miss_recode, by = "ID") %>%
  relocate(range1:range4, .after = "ID") %>%
  mutate(
    across(
      c(i001:i035),
      ~ case_when(
        (range1 == "i001:i035") | (range2 == "i001:i035") |
          (range3 == "i001:i035") | (range4 == "i001:i035") ~ NA_real_,
        T ~ .x
      )
    )) 
    

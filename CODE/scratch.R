test <- miss_recode %>% 
  left_join(input_orig, by = "ID")

test2 <- slice_sample(input_orig, n = 100)

test3 <- bind_rows(test, test2) %>% 
  arrange(ID) %>% 
  filter(
    !(ID %in% c(230010, 268005) & is.na(recode_cols1))
  )

anyDuplicated(test3$ID)

write_csv(test3, here("test3.csv"))

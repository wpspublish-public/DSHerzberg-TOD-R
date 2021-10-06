age5 <- data.frame(
  raw = c(5, 4, 3, 2, 1), 
  ss = c(120, 110, 100, 90, 80)
)
age6 <- data.frame(
  raw = c(5, 4, 3, 2, 1), 
  ss = c(117, 107, 97, 87, 77)
)
age7 <- data.frame(
  raw = c(5, 4, 3, 2, 1), 
  ss = c(115, 105, 95, 85, 75)
)

lookup_list <- lst(age5, age6, age7)


lookup_rawXage_70ss <- getNormCurve(70, model, step = .25) %>% 
  mutate(
    across(
      raw,
      ~
        round(., 0)
    )
  ) %>% 
  select(age, raw) %>% 
  rename(raw70 = raw) 

lookup_rawXage_100ss <- getNormCurve(100, model, step = .25) %>% 
  mutate(
    across(
      raw,
      ~
        round(., 0)
    )
  ) %>% 
  select(age, raw) %>% 
  rename(raw100 = raw) 

lookup_rawXage_130ss <- getNormCurve(130, model, step = .25) %>% 
  mutate(
    across(
      raw,
      ~
        round(., 0)
    )
  ) %>% 
  select(age, raw) %>% 
  rename(raw130 = raw) 

lookup_combo <- reduce(
  list(
    lookup_rawXage_70ss,
    lookup_rawXage_100ss,
    lookup_rawXage_130ss
  ),
  left_join,
  by = "age"
)


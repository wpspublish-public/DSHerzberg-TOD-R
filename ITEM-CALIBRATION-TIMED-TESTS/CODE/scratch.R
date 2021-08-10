demo_weight_by_crossing <- weighted_output %>%
  group_by(demo_wt) %>%
  summarize(
    gender = first(gender),
    educ = first(educ),
    ethnic = first(ethnic),
    region = first(region)
  ) %>%
  relocate(demo_wt, .after = region) %>%
  arrange(
    match(gender, cat_order),
    match(educ, cat_order),
    match(ethnic, cat_order),
    match(region, cat_order)
  )

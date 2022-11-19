growth_lookup1 <- here("INPUT-FILES/OES-INPUT-TABLES/DP4-EXAMPLE/growth-score-lookup.xlsx") %>% 
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = here("INPUT-FILES/OES-INPUT-TABLES/DP4-EXAMPLE/growth-score-lookup.xlsx"),
         .id = "form") %>% 
  rename_with(~ str_c(.x,"_G"), PHY:COM) 

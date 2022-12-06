test <- suppressMessages(
  read_csv(
    here(
      "INPUT-FILES/OES-INPUT-TABLES/TODC-child-rating-scale-raw-to-t-score.csv"
    ))) %>% 
  mutate(version = "TOD-C") %>% 
  pivot_longer(
    cols = ends_with("_t"),
    names_to = "rater",
    values_to = "t_score"
  ) %>% relocate(c(version, rater), .before = "raw") %>% 
  arrange(rater)
  

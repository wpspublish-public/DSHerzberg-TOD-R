library(psych)
descriptives <- weighted_sum_scores_pom_rescale %>% 
  select(contains("sum")) %>%
  describe(fast = T) %>% 
  rownames_to_column(var = "scale")


temp1 <- weighted_sum_scores %>% 
  select(ID, sege_sum, sege_sum_w_pom) %>% 
  rename(sege_sum_w = sege_sum_w_pom) %>% 
  drop_na()

temp1 %>% 
  select(-ID) %>% 
  describe(fast = TRUE)

write_csv(
  temp1,
  "/Users/dherzberg/Desktop/R/WEIGHTING-DATA/INPUT-FILES/example-rescale-weighted-raw-scores-input.csv"
)

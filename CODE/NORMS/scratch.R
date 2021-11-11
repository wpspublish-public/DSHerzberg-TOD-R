library(psych)
descriptives <- weighted_sum_scores_pom_rescale %>% 
  select(contains("sum")) %>%
  describe(fast = T) %>% 
  rownames_to_column(var = "scale")


temp1 <- weighted_sum_scores %>% 
  select(ID, sege_sum, sege_sum_w_pom) %>% 
  rename(sege_sum_w = sege_sum_w_pom) %>% 
  drop_na() %>% 
  mutate(across(
    sege_sum_w,
    ~ round(., 0)
    ))

unique(temp1$sege_sum_w)

temp1 %>% 
  select(-ID) %>% 
  describe(fast = TRUE)

write_csv(
  temp1,
  "/Users/dherzberg/Desktop/R/WEIGHTING-DATA/INPUT-FILES/example-rescale-weighted-raw-scores-input.csv"
)

# sege_pom = sege_sum/25, 
# rlne_pom = rlne_sum/120, 
# rhme_pom = rhme_sum/30, 
# snwe_pom = snwe_sum/32, 
# lswe_pom = lswe_sum/38, 
# lske_pom = lske_sum/33, 

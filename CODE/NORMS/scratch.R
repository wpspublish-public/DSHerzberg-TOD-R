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

temp1 <- read_csv(here(
  str_c(input_file_path, combined_score_to_norm_file_name)
))

n_age_group <- age_contin %>% 
  group_by(group) %>% 
  count(group)
"5.0-5.3", "5.4-5.7", "5.8-5.11"

suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(psych)) # DESCRIPTIVE TABLES

grade <- suppressMessages(read_csv(here(
  'INPUT-FILES/ID-Grade-PV-LW.csv'
))) %>% 
  mutate_at(vars(grade),
          ~ as.integer(.x))

NA_count <- sum(is.na(grade))
NA_count

  
TOD_comp<- suppressMessages(read_csv(here(
  'INPUT-FILES/TOD-E_PVandLW_BLIMPcompare3.5.20.csv'
))) %>% 
  left_join(grade, by = "ID") %>% 
  select(ID, grade, everything()) %>% 
  filter(!(is.na(grade) | grade == 888))

TOD_comp_desc <-
  TOD_comp %>% 
  select(contains('sum')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname) %>% 
  select(scale, n, mean, sd) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2)))

TOD_comp_cor <-
  TOD_comp %>% 
  # group_by(grade) %>% 
  select(contains('sum')) %>%
  cor(use = "pairwise.complete.obs")

cor_grade <- TOD_comp %>% 
  group_by(grade) %>% 
  summarize(n = n(), 
            pv_cor = cor(pvBLIMP_sum, pv_sum, use = "pairwise.complete.obs"),
            lw_cor = cor(lwBLIMP_sum, lw_sum, use = "pairwise.complete.obs"),
            sd_pooled = sum(sd(pvBLIMP_sum), sd(lwBLIMP_sum), sd(pv_sum, na.rm = T), sd(lw_sum, na.rm = T))/4) %>% 
  mutate_at(vars(grade), ~ as.character(.x))

cor <- TOD_comp %>% 
  summarize(n = n(), 
            pv_cor = cor(pvBLIMP_sum, pv_sum, use = "pairwise.complete.obs"),
            lw_cor = cor(lwBLIMP_sum, lw_sum, use = "pairwise.complete.obs"), 
            sd_pooled = sum(sd(pvBLIMP_sum), sd(lwBLIMP_sum), sd(pv_sum, na.rm = T), sd(lw_sum, na.rm = T))/4) %>% 
  mutate(grade = "all") %>% 
  select(grade, everything())

combo <- bind_rows(cor, cor_grade) %>% 
  mutate_at(vars(pv_cor, lw_cor), ~ round(.x, 2))

write_csv(combo, here(
    'OUTPUT-FILES/PV-LW-corByGrade.csv'
    ))

# write_csv(Adult_Other_raw_desc, here(
#   paste0(
#     'OUTPUT-FILES/ADULT/DESCRIPTIVES/Adult-Other-raw-desc-',
#     format(Sys.Date(), "%Y-%m-%d"),
#     '.csv'
#   )
# ), 
# na = ''
# )

# glimpse(TOD_comp)

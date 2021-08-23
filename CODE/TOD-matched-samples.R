###### LOAD PACKAGES -----------------------------------------------------------
suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
suppressMessages(library(MatchIt))

# READ TARGET AND MATCHPOOL SAMPLES --------------------------------------------------

smallfile_name <- "TODC&TODS_masks_smallfile_rev"
largefile_name <- "TODC&TODS_masks_largefile_rev"
name_stem <- "TODC&TODS_masks_rev"

TOD_target_preMatch <-
  suppressMessages(as_tibble(read_csv(here(
    str_c("INPUT-FILES/SAMPLE-MATCHING/", smallfile_name, ".csv")
  )))) %>%
  mutate(source = "target") %>%
  rename(
    gender = Gender,
    ethnicity = Ethnicity
  )

TOD_matchpool_preMatch <-
  suppressMessages(as_tibble(read_csv(here(
    str_c("INPUT-FILES/SAMPLE-MATCHING/", largefile_name, ".csv")
  )))) %>%
  mutate(source = "matchpool") %>% 
  rename(
    gender = Gender,
    ethnicity = Ethnicity
  )

# EXTRACT MATCHED TYPICAL SAMPLE ------------------------------------------

# This step encodes a logical var (Group), needed by matchit, that captures
# target vs matchpool status
TOD_target_matchpool_preMatch <- bind_rows(
  TOD_target_preMatch,
  TOD_matchpool_preMatch 
) %>% 
  mutate(Group = case_when(
    source == "target" ~ TRUE,
    TRUE ~ FALSE
  ))

# matchit cannot process NA. First get sum of NA for data. If that is 0,
# proceed. If sum NA is positive, recode all NA to 999
sum(is.na(TOD_target_matchpool_preMatch))

# identify cols with NA
na_cols <- TOD_target_matchpool_preMatch %>% select_if(~ any(is.na(.)))

# in NA cols, replace NA with 999
TOD_target_matchpool_preMatch <- TOD_target_matchpool_preMatch %>%
  replace_na(
    list(
      ID = 999,
      age = 999,
      gender = 999,
      ethnicity = 999,
      SES = 999,
      source = "999"
    ))

# run matchit to get 1:1 matching
set.seed(39485703)
match <- matchit(
  Group ~ age + gender + SES + ethnicity + clinical, 
  data = TOD_target_matchpool_preMatch, 
  method = "nearest", 
  ratio = 1)
match_summ <- summary(match)

# save matched samples into new df; split by source
TOD_target_matchpool_match <- match.data(match) %>% 
  select(-Group, -distance, -weights, -subclass) 
TOD_matchpool_match <- TOD_target_matchpool_match %>% 
  filter(source == 'matchpool')
TOD_matchpool_match_output <- TOD_matchpool_match %>% 
  select(-source)
TOD_target_match <- TOD_target_matchpool_match %>% 
  filter(source == 'target')

# demo counts

var_order <- c("age", "gender", "SES", "ethnicity", "clinical")

match_dist_matchpool <- TOD_matchpool_match %>%
  select(age, gender, SES, ethnicity, clinical) %>%
  pivot_longer(everything(), names_to = "var", values_to = "cat") %>%
  group_by(var, cat) %>%
  count(var, cat) %>%
  arrange(match(var, var_order), cat) %>%
  ungroup() %>%
  mutate(
    var = case_when(
      lag(var) == "age" & var == "age" ~ "",
      lag(var) == "gender" & var == "gender" ~ "",
      lag(var) == "SES" & var == "SES" ~ "",
      lag(var) == "ethnicity" & var == "ethnicity" ~ "",
      lag(var) == "clinical" & var == "clinical" ~ "",
      TRUE ~ var
    )
  ) %>%
  mutate(group = case_when(row_number() == 1 ~ "matched_sample",
                           TRUE ~ "")) %>%
  select(group, everything())

match_dist_target <- TOD_target_match %>%
  select(age, gender, SES, ethnicity, clinical) %>%
  pivot_longer(everything(), names_to = "var", values_to = "cat") %>%
  group_by(var, cat) %>%
  count(var, cat) %>%
  arrange(match(var, var_order), cat) %>%
  ungroup() %>%
  mutate(
    var = case_when(
      lag(var) == "age" & var == "age" ~ "",
      lag(var) == "gender" & var == "gender" ~ "",
      lag(var) == "SES" & var == "SES" ~ "",
      lag(var) == "ethnicity" & var == "ethnicity" ~ "",
      lag(var) == "clinical" & var == "clinical" ~ "",
      TRUE ~ var
    )
  ) %>%
  mutate(group = case_when(row_number() == 1 ~ "orig_smallfile_sample",
                           TRUE ~ "")) %>%
  select(group, everything())


# write table of combined matchpool, target demo counts.
write_csv(
  bind_rows(match_dist_matchpool,
            match_dist_target),
  here(
    str_c("OUTPUT-FILES/SAMPLE-MATCHING/", name_stem, "-matchedSample-target-demos.csv")
  )
)

# write matchpool subsample that is matched to target group.
write_csv(
  TOD_matchpool_match_output,
  here(
    str_c("OUTPUT-FILES/SAMPLE-MATCHING/", name_stem, "-matched-sample.csv")
  ),
  na = ""
)


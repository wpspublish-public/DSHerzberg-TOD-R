suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(psych))

input <- suppressMessages(
  read_csv(
    here(
      paste0("INPUT-FILES/TOD-E.DATA.3.5.20recodes6.9.20.csv")
    )
  )
) 

recode_output <- input %>%
  group_by(ID) %>%
  pivot_longer(
    cols = -ID,
    names_to = c("pre", "num"),
    names_sep = 4
  ) %>%
  mutate(
    NA_status = case_when(
      lead(num) == 1 & is.na(value) ~ "offset_NA",
      is.na(value) & !is.na(lag(value)) ~ "onset_NA",
      T ~ NA_character_
    )
  ) %>%
  group_by(ID, grp = cumsum(!is.na(NA_status))) %>%
  mutate(NA_status = replace(NA_status, first(NA_status) == 'onset_NA', 'onset_NA')) %>%
  ungroup() %>%
  select(-grp) %>%
  mutate(new_val = case_when(NA_status %in% c("onset_NA", "offset_NA") ~ 0,
                             T ~ value)) %>%
  pivot_wider(
    id_cols = ID,
    names_from = c(pre, num),
    names_sep = "",
    values_from = new_val
  )

write_csv(recode_output,
          here("OUTPUT-FILES/TOD-E-recode-2020-06-11.csv"),
          na = "")

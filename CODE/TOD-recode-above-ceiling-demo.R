suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(psych))
suppressMessages(library(runner))

urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish/DSHerzberg-TOD-R/master/INPUT-FILES/"
fileName_path   <- "TOD-E-recode-above-ceiling-input.csv"

input <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
)))

input_tall <- input %>%
  pivot_longer(
    cols = -ID,
    names_to = c("pre", "num"),
    names_sep = 4
  )  %>%
  mutate(across(
    c(pre),
    ~ case_when(
      pre == "lske" & between(num, 1, 15) ~ "lske_A",
      pre == "lske" & between(num, 16, 25) ~ "lske_B",
      pre == "lske" & between(num, 26, 35) ~ "lske_C",
      T ~ pre
    )
  )) %>%
  group_by(ID, pre) %>%
  mutate(
    streak_val = case_when(value == 0 ~ streak_run(value, na_rm = F),
                           T ~ NA_integer_),
    ceiling = case_when((
      pre %in% c("snwe", "sege", "lswe", "rhme") & streak_val == 5
    ) |
      (
        pre %in% c("lske_A", "lske_B", "lske_C") & streak_val == 3
      ) ~ 1,
    T ~ 0)
  )

ceiling <-  input_tall %>% 
  group_by(ID, pre) %>% 
  summarise(ceiling_count = sum(ceiling)) %>% 
  mutate(ceiling_reached = case_when(
    ceiling_count >= 1 ~ 1,
    T ~ NA_real_
  )) %>% 
  select(-ceiling_count)

 recode_output <- input_tall %>% 
  left_join(ceiling, by = c("ID", "pre")) %>% 
  group_by(ID) %>%
  mutate(
    NA_status = case_when(
      (pre != lead(pre) | is.na(lead(pre))) & is.na(value) & ceiling_reached == 1 ~ "offset_NA",
      is.na(value) & !is.na(lag(value)) & pre == lag(pre) & ceiling_reached == 1 ~ "onset_NA",
      T ~ NA_character_
    )
  ) %>% 
   group_by(ID, pre) %>% 
   mutate(
     across(
       c(NA_status),
       ~ fill_run(.)
     )
   ) %>% 
 mutate(new_val = case_when(NA_status %in% c("onset_NA", "offset_NA") ~ 0,
                            T ~ value)) %>%
   pivot_wider(
     id_cols = ID,
     names_from = c(pre, num),
     names_sep = "",
     values_from = new_val
   )
 
 write_csv(recode_output,
           here(str_c("OUTPUT-FILES/TOD-E-recode-",
                      format(Sys.Date(), "%Y-%m-%d"),
                      ".csv")),
           na = "")
 
    


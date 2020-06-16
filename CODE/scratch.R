suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(psych))
suppressMessages(library(runner))

input <- suppressMessages(
  read_csv(
    here(
      paste0("INPUT-FILES/TOD-E.DATA.3.5.20recodes6.9.20.csv")
    )
  )
) 

input_tall <- input %>%
  group_by(ID) %>%
  pivot_longer(
    cols = -ID,
    names_to = c("pre", "num"),
    names_sep = 4
  )  %>%
  mutate(streak_val = case_when(value == 0 ~ streak_run(value, na_rm = F),
                                T ~ NA_integer_),
         across(
           c(pre),
           ~ case_when(
             pre == "lske" & between(num, 1, 15) ~ "lske_A",
             pre == "lske" & between(num, 16, 25) ~ "lske_B",
             pre == "lske" & between(num, 26, 35) ~ "lske_C",
             T ~ pre
           )
         ),
         ceiling = case_when(
           (pre %in% c("snwe", "sege", "lswe", "rhme") & streak_val == 5) | 
             (pre %in% c("lske_A", "lske_B", "lske_C") & streak_val == 3) ~ 1,
           T ~ 0
         )
         ) 

ceiling <-  input_tall %>% 
  group_by(ID, pre) %>% 
  summarise(ceiling_count = sum(ceiling)) %>% 
  mutate(ceiling_reached = case_when(
    ceiling_count >= 1 ~ 1,
    T ~ NA_real_
  ))

    


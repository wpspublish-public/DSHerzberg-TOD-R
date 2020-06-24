suppressMessages(library(here))
suppressMessages(library(tidyverse))

file_name <- c("TOD-E.DATA.3.5.20_forBLIMP6.18.20")

input_orig <- suppressMessages(read_csv(here(
  paste0("INPUT-FILES/", file_name, ".csv")
))) 

miss1 <- input_orig %>%
  filter(across(c(i001:i035),
                ~ is.na(.))) %>% 
  mutate(recode_cols = "i001:i035")
miss2 <- input_orig %>%
  filter(across(c(i036:i060),
                ~ is.na(.))) %>% 
  mutate(recode_cols = "i036:i060")
miss3 <- input_orig %>%
  filter(across(c(i061:i100),
                ~ is.na(.))) %>% 
  mutate(recode_cols = "i061:i100")
miss4 <- input_orig %>%
  filter(across(c(i101:i130),
                ~ is.na(.))) %>% 
  mutate(recode_cols = "i101:i130")
miss5 <- input_orig %>%
  filter(across(c(i131:i165),
                ~ is.na(.))) %>% 
  mutate(recode_cols = "i131:i165")

miss_recode <- bind_rows(
  miss1, 
  miss2, 
  miss3, 
  miss4, 
  miss5
) %>% 
  select(ID, recode_cols)

temp3 <- temp2 %>%
  left_join(miss_recode, by = "ID") %>%
  relocate(recode_cols, .after = "ID") %>%
  mutate(
    across(
      c(i001:i035),
      ~ case_when(
        recode_cols == "i001:i035"  ~ NA_real_,
        T ~ .x
      )
    ), 
    across(
      c(i036:i060),
      ~ case_when(
        recode_cols == "i036:i060"  ~ NA_real_,
        T ~ .x
      )
    ), 
    across(
      c(i061:i100),
      ~ case_when(
        recode_cols == "i061:i100"  ~ NA_real_,
        T ~ .x
      )
    ), 
    across(
      c(i101:i130),
      ~ case_when(
        recode_cols == "i101:i130"  ~ NA_real_,
        T ~ .x
      )
    ), 
    across(
      c(i131:i165),
      ~ case_when(
        recode_cols == "i131:i165"  ~ NA_real_,
        T ~ .x
      )
    )
  )

NA_count <- sum(is.na(temp3))
NA_count

write_csv(temp3, here(
  str_c(
    "MISSING-DATA-BLIMP/",
    file_name,
    "-noMiss-",
    format(Sys.Date(), "%Y-%m-%d"),
    ".csv"
  )
))

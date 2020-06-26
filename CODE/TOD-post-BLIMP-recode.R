suppressMessages(library(here))
suppressMessages(library(tidyverse))

file_name <- c("TOD-E.DATA.3.5.20_forBLIMP6.18.20")

input_orig <- suppressMessages(read_csv(here(
  paste0("INPUT-FILES/", file_name, ".csv")
))) 

col_range <- c("i001:i035", "i036:i060", "i061:i100", "i101:i130", "i131:i165")

miss_recode <- col_range %>% 
  map_df(~
           input_orig %>%
           filter(across(!!rlang::parse_expr(.x),
                         ~ is.na(.))) %>% 
           mutate(recode_cols = .x) %>% 
           select(ID, recode_cols)
  )

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
  ) %>%
  select(-recode_cols)

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
),
na = ""
)

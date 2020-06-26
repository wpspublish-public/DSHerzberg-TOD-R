suppressMessages(library(here))
suppressMessages(library(tidyverse))

file_name <- c("TOD-E.DATA.3.5.20_forBLIMP6.18.20")

input_orig <- suppressMessages(read_csv(here(
  paste0("INPUT-FILES/", file_name, ".csv")
))) 

temp1 <- suppressMessages(
  read_csv(
    (here("MISSING-DATA-BLIMP/TOD-impute-2020-06-23-1.csv")), col_names = F))
names(temp1) <- c("ID", "item", "response")
temp2 <- temp1 %>% 
  spread(item, response) 
names(temp2) <- names(input_orig)

col_range <- c("i001:i035", "i036:i060", "i061:i100", "i101:i130", "i131:i165")

miss_recode <- col_range %>% 
  map_df(~
           input_orig %>%
           filter(across(!!rlang::parse_expr(.x),
                         ~ is.na(.))) %>% 
           mutate(recode_cols = .x) %>% 
           select(ID, recode_cols)
  )

# repeated use of across to recode over different subsets of columns
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

# pivot longer to avoid repeated code
temp4 <- temp2 %>%
  left_join(miss_recode, by = "ID") %>%
  relocate(recode_cols, .after = "ID") %>%
  pivot_longer(cols = c(-ID,-recode_cols),
               names_to = c("item")) %>%
  extract(
    recode_cols,
    into = c("start", "end"),
    "([:alnum:]{4})?\\:?(.*)",
    remove = F
  ) %>%
  group_by(ID) %>%
  mutate(
    recode_run =
      case_when(start == item ~ "onset",
                end == item ~ "offset",
                T ~ NA_character_),
    across(c(recode_run),
           ~ runner::fill_run(.,)),
    across(
      c(recode_run),
      ~ case_when(recode_run == "offset" ~ NA_character_,
                  T ~ recode_run)
    ),
    across(
      c(recode_run),
      ~ case_when(lag(recode_run) == "onset" ~ "onset",
                  T ~ recode_run)
    ),
    across(c(value),
           ~ case_when(recode_run == "onset" ~ NA_real_,
                       T ~ value))
  ) %>%
  pivot_wider(
    id_cols = ID,
    names_from = c(item),
    values_from = value
  ) %>%
  ungroup()

identical(temp3, temp4)


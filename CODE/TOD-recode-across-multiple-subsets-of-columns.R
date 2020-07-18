suppressMessages(library(here))
suppressMessages(library(tidyverse))

file_name <- c("TOD.DATA.3.5.20_forBLIMP7.16.20iws_lvc")

input_orig <- suppressMessages(read_csv(here(
  paste0("INPUT-FILES/", file_name, ".csv")
))) 

temp1 <- suppressMessages(
  read_csv(
    (here("MISSING-DATA-BLIMP/TOD-impute-2020-07-17-1.csv")), col_names = F))
names(temp1) <- c("ID", "item", "response")
temp2 <- temp1 %>% 
  pivot_wider(
    names_from = item,
    values_from = response
  ) 
names(temp2) <- names(input_orig)

col_range <- c("i001:i050", "i051:i084", "i085:i114", "i115:i185", 
               "i186:i206", "i207:i251", "i252:i293")

# this section now has code to handle when a single case has two column ranges
# that need to be recoded - these are now identified in recode_cols1 and recode_cols2
miss_recode <- col_range %>% 
  map_df(~
           input_orig %>%
           filter(across(!!rlang::parse_expr(.x),
                         ~ is.na(.))) %>% 
           mutate(recode_cols1 = .x) %>% 
           select(ID, recode_cols1)
  ) %>% 
  arrange(ID) %>% 
  mutate(recode_cols2 = case_when(
    lead(ID) == ID ~ lead(recode_cols1),
    T ~ NA_character_
  )) %>% 
   filter(rownames(.) == "1" | lag(ID) != ID)


######### NEXT COMMENTED-OUT SNIPPET HAS NOT BEEN UPDATED TO HANDLE CASES WITH
######### MORE THAN ONE MISSING COLUMN RANGE

# # repeated use of across to recode over different subsets of columns
# temp3 <- temp2 %>%
#   left_join(miss_recode, by = "ID") %>%
#   relocate(recode_cols1, .after = "ID") %>%
#   mutate(
#     across(
#       c(i001:i050),
#       ~ case_when(
#         recode_cols1 == "i001:i050"  ~ NA_real_,
#         T ~ .x
#       )
#     ), 
#     across(
#       c(i051:i084),
#       ~ case_when(
#         recode_cols1 == "i051:i084"  ~ NA_real_,
#         T ~ .x
#       )
#     ), 
#     across(
#       c(i085:i114),
#       ~ case_when(
#         recode_cols1 == "i085:i114"  ~ NA_real_,
#         T ~ .x
#       )
#     ), 
#     across(
#       c(i115:i185),
#       ~ case_when(
#         recode_cols1 == "i115:i185"  ~ NA_real_,
#         T ~ .x
#       )
#     ), 
#     across(
#       c(i186:i206),
#       ~ case_when(
#         recode_cols1 == "i186:i206"  ~ NA_real_,
#         T ~ .x
#       )
#     ), 
#     across(
#       c(i207:i251),
#       ~ case_when(
#         recode_cols1 == "i207:i251"  ~ NA_real_,
#         T ~ .x
#       )
#     ), 
#     across(
#       c(i252:i293),
#       ~ case_when(
#         recode_cols1 == "i252:i293"  ~ NA_real_,
#         T ~ .x
#       )
#     )
#   ) %>%
#   select(-recode_cols1)

# pivot longer to avoid repeated code
temp4 <- temp2 %>%
  left_join(miss_recode, by = "ID") %>%
  relocate(c(recode_cols1, recode_cols2), .after = "ID") %>%
  pivot_longer(cols = c(-ID, -recode_cols1,-recode_cols2),
               names_to = c("item")) %>%
  extract(
    recode_cols1,
    into = c("start1", "end1"),
    "([:alnum:]{4})?\\:?(.*)",
    remove = F
  ) %>%
  extract(
    recode_cols2,
    into = c("start2", "end2"),
    "([:alnum:]{4})?\\:?(.*)",
    remove = F
  ) %>%
  group_by(ID) %>%
  mutate(
    recode_run =
      case_when(
        start1 == item ~ "recode1",
        end1 == item ~ "recode1",
        start2 == item ~ "recode2",
        end2 == item ~ "recode2",
        T ~ NA_character_
      ),
    across(c(recode_run),
           ~ runner::fill_run(., only_within = T)),
    across(c(value),
           ~ case_when(
             recode_run %in% c("recode1", "recode2") ~ NA_real_,
             T ~ value
           ))
  ) %>%
  select(ID, item, value) %>%
  pivot_wider(
    names_from = item,
    values_from = value) %>%
  unnest()

write_csv(temp4, here(
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


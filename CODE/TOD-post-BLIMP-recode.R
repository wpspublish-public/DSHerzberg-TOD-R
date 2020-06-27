suppressMessages(library(here))
suppressMessages(library(tidyverse))

# read original pre-BLIMP so col names are available to rename output cols
file_name <- c("TOD-E.DATA.3.5.20_forBLIMP6.18.20")

input_orig <- suppressMessages(read_csv(here(
  paste0("INPUT-FILES/", file_name, ".csv")
))) 

# read post BLIMP imputed data, apply original col names
temp1 <- suppressMessages(
  read_csv(
    (here("MISSING-DATA-BLIMP/TOD-impute-2020-06-23-1.csv")), col_names = F))
names(temp1) <- c("ID", "item", "response")
temp2 <- temp1 %>% 
  spread(item, response) 
names(temp2) <- names(input_orig)

# verify no missing
NA_count <- sum(temp2 == 999)
NA_count

# The task is to recode certain subsets of columns to NA. These cols represent a
# test that was entirely missing in the input data; although these missing
# responses were imputed by BLIMP, the PD prefers to treat them as missing in
# subsequent analyses.

# These are the groups of cols that will be recoded to NA.
col_range <- c("i001:i035", "i036:i060", "i061:i100", "i101:i130", "i131:i165")

# in this snippet, the col groups are mapped onto a function that filters the
# original input for cases that are NA for all cols within each group. Within
# filter(), the first argument of across iterates through the vector of column
# ranges, designated by .x. For across() to process i001:i035 as a range of
# cols, it must see the range as an R expression, not a name or an object.
# rlang::parse_expr() coerces the quoted column range to an expression, and !!
# unquotes it, so that it can be processed by across(). The second argument of
# across is the function is.na(), which returns TRUE only if all of the cols in
# the first argument of across() are NA. The filtered (retained) rows are then
# labeled (by the value of recode_cols) on the group of cols on which they are
# all missing. By using map_df(), we stack these retained rows on top of each
# other, into a single data frame, as the function iterates over the elements of
# col_range. select() keeps only ID and recode_col, so these can be joined back
# with the post-BLIMP data.

# The code can handle cases that are missing all cols on multiple col ranges. In
# this case, because the entire item set is divided into five col ranges, the
# code is set up to handle cases with all NA on up to four of those ranges (the
# assumption is that a case missing on all five ranges would have no data, and
# would have been dropped at an earlier srage of data cleanup). The summarize()
# call creates four new variables (range1, etc) to hold the labels for up to
# four col ranges per case that are all NA.
miss_recode <- col_range %>% 
  map_df(~
           input_orig %>%
           filter(across(!!rlang::parse_expr(.x),
                         ~ is.na(.))) %>% 
           mutate(recode_cols = .x) %>% 
           select(ID, recode_cols)
  ) %>% 
  arrange(ID) %>% 
  group_by(ID) %>% 
  summarize(
    range1 = nth(recode_cols, 1),
    range2 = nth(recode_cols, 2),
    range3 = nth(recode_cols, 3),
    range4 = nth(recode_cols, 4)
  )


# This next section brings in the recode_cols label and joins it to the post
# BLIMP data. Now the cases that need to have certain col ranges recoded to NA
# are labeled. The mutate() call goes through the five col ranges one by one,
# recoding to NA all cols within the range wherever the value of range1, range2,
# range3, or range4 designates a case for which those cols need to be recoded.
temp3 <- temp2 %>%
  left_join(miss_recode, by = "ID") %>%
  relocate(range1:range4, .after = "ID") %>%
  mutate(
    across(
      c(i001:i035),
      ~ case_when(
        (range1 == "i001:i035") | (range2 == "i001:i035") |
          (range3 == "i001:i035") | (range4 == "i001:i035") ~ NA_real_,
        T ~ .x
      )
    ), 
    across(
      c(i036:i060),
      ~ case_when(
        (range1 == "i036:i060") | (range2 == "i036:i060") |
          (range3 == "i036:i060") | (range4 == "i036:i060") ~ NA_real_,
        T ~ .x
      )
    ), 
    across(
      c(i061:i100),
      ~ case_when(
        (range1 == "i061:i100") | (range2 == "i061:i100") |
          (range3 == "i061:i100") | (range4 == "i061:i100") ~ NA_real_,
        T ~ .x
      )
    ), 
    across(
      c(i101:i130),
      ~ case_when(
        (range1 == "i101:i130") | (range2 == "i101:i130") |
          (range3 == "i101:i130") | (range4 == "i101:i130") ~ NA_real_,
        T ~ .x
      )
    ), 
    across(
      c(i131:i165),
      ~ case_when(
        (range1 == "i131:i165") | (range2 == "i131:i165") |
          (range3 == "i131:i165") | (range4 == "i131:i165") ~ NA_real_,
        T ~ .x
      )
    )
  ) %>%
  select(-(range1:range4))

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

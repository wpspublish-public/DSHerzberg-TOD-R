suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))

form <- c("TODC-child", "TODC-adult", "TODE")

# Create char vec holding names of input .xlsx containing scale lookups.
# `purrr::map_chr()` returns a char vec. Mapping `str_c` allows you to paste
# the names of the three forms into the file name stem, creating charvec with
# three file names.
form_file_name <- map_chr(
  form, 
  ~ 
    str_c(.x, "-rating-scale-raw-to-t-score")
  )

# read in percentile lookup column
perc_lookup <- suppressMessages(
  read_excel(
    here(
      "INPUT-FILES/OES-INPUT-TABLES/t-score-to-percentile.xlsx"
      ))) %>% 
  rename(t_score = `t score`)

# Use map2() to read in input files into a list, and append a new column
# labeling the TOD version of the input file
lookup <- map2(
  form_file_name,
  form,
  ~
    suppressMessages(read_csv(here(
      str_c("INPUT-FILES/OES-INPUT-TABLES/", .x, ".csv")
    ))) %>%
    mutate(version = .y) %>%
    # pivot each input file to the long format required for the OES input file
    pivot_longer(
      cols = ends_with("_t"),
      names_to = "norm_rater",
      values_to = "t_score"
    ) %>% relocate(c(version, norm_rater), .before = "raw") %>%
    arrange(norm_rater)
) %>% 
  # stack the three long input (which have identical col names) into a single long format table.
  bind_rows(.) %>%
  mutate(
    # recode norm_rater to indicate the normative age range and rater type
    # associated with each raw-to-t lookup relationship
    across(norm_rater,
           ~
             case_when(
               version == "TODC-child" & norm_rater == "parent_t" ~ "child-parent",
               version == "TODC-child" & norm_rater == "self_t" ~ "child-self",
               version == "TODC-child" & norm_rater == "teacher_t" ~ "child-teacher",
               version == "TODC-adult" & norm_rater == "self_t" ~ "adult-self",
               version == "TODE" & norm_rater == "parent_t" ~ "child-parent",
               version == "TODE" & norm_rater == "teacher_t" ~ "child-teacher",
               TRUE ~ NA_character_
             )
    ),
    # truncate cell values for version
    across(version,
           ~
             str_sub(., 1, 4)
    ),
    # code risk_level with case_when()
    risk_level = case_when(
      t_score <= 37 ~ "No or extremely low risk",
      between(t_score, 38, 43) ~ "Very low risk",
      between(t_score, 44, 57) ~ "Low risk",
      between(t_score, 58, 64) ~ "High risk",
      between(t_score, 65, 70) ~ "Very high risk",
      t_score >= 71 ~ "Extremely high risk",
      TRUE ~ NA_character_
    ),
    # code confidence values with case_when()
    CV90 = case_when(
      version == "TODC" & norm_rater == "child-self" ~ 4,
      version == "TODC" & norm_rater == "adult-self" ~ 4,
      version == "TODC" & norm_rater == "child-parent" ~ 4,
      version == "TODC" & norm_rater == "child-teacher" ~ 3,
      version == "TODE" & norm_rater == "child-parent" ~ 4,
      version == "TODE" & norm_rater == "child-teacher" ~ 3,
      TRUE ~ NA_real_
    ),
    CV95 = case_when(
      version == "TODC" & norm_rater == "child-self" ~ 5,
      version == "TODC" & norm_rater == "adult-self" ~ 4,
      version == "TODC" & norm_rater == "child-parent" ~ 4,
      version == "TODC" & norm_rater == "child-teacher" ~ 4,
      version == "TODE" & norm_rater == "child-parent" ~ 4,
      version == "TODE" & norm_rater == "child-teacher" ~ 3,
      TRUE ~ NA_real_
    )
  ) %>% 
  # combining inputs with different t-score ranges results in rows with NA on
  # t_score - drop these rows
  drop_na(t_score) %>% 
  # calculate confidence interval lower and upper bounds, concatenate these into
  # a single string giving the full confidence interval
  mutate(
    CI90_LB_pre = t_score - CV90,
    CI90_UB_pre = t_score + CV90,
    CI95_LB_pre = t_score - CV95,
    CI95_UB_pre = t_score + CV95,
    CI90_LB = as.character(case_when(
      CI90_LB_pre < 40 ~ 40,
      TRUE ~ CI90_LB_pre
    )), 
    CI90_UB = as.character(case_when(
      CI90_UB_pre > 80 ~ 80,
      TRUE ~ CI90_UB_pre
    )), 
    CI95_LB = as.character(case_when(
      CI95_LB_pre < 40 ~ 40,
      TRUE ~ CI95_LB_pre
    )), 
    CI95_UB = as.character(case_when(
      CI95_UB_pre > 80 ~ 80,
      TRUE ~ CI95_UB_pre
    )), 
    CI90 = str_c(CI90_LB, CI90_UB, sep = " - "), 
    CI95 = str_c(CI95_LB, CI95_UB, sep = " - ") 
  ) %>% 
  # join percentil lookup col
  left_join(perc_lookup, by = "t_score") %>% 
  # select and sequence cols for final OES input table
  select(version,	norm_rater,	raw, t_score, CI90, CI95, risk_level, percentile)
  

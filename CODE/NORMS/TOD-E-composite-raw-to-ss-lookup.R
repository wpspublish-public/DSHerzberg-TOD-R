# Load packages, read data

suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
suppressMessages(library(bestNormalize))

input_file_path  <- "INPUT-FILES/NORMS/TODE_FINAL_composites/"
input_file_name <- "TODE_FINAL_composites_grade.csv"
input_file_stem <- "TODE_FINAL_composites_grade"
output_file_path  <- "OUTPUT-FILES/NORMS/TODE_FINAL_composites/"


score_names <- c("EDDIW", "EDDIQ", "ELP", "ERSW", "ERSQ",
                 "ESWA", "EPHK", "EBRS", "EPHA")
norm_type <- "_grd"
all_raw_range <- 80:1040
raw_range_per_score <- list(320:1040, 320:1040, 120:390, 200:650, 200:650, 
                         80:260, 80:260, 80:260, 80:260)
ss_lower_bound <- 40
ss_upper_bound <- 130

input_all <- suppressMessages(read_csv(here(str_c(
  input_file_path, input_file_name
))))

input_per_score <- map(
  score_names,
  ~
  input_all %>%
  select(ID, !!sym(str_c(.x, norm_type))) %>%
  drop_na()
) %>% set_names(score_names)

norm_input_per_score <- map(
  score_names,
  ~
    input_all %>%
    select(!!sym(str_c(.x, norm_type))) %>%
    drop_na() %>% 
    as_vector() %>%
    set_names(NULL)
) %>% set_names(score_names)


# DETERMINE BEST NORMALIZATION MODEL

set.seed(12345)
norm_model_per_score_chosen_transform <- map(
  norm_input_per_score,
  ~
    bestNormalize(.x) %>%
    pluck("chosen_transform") %>% 
    class() %>% 
    pluck(1)
) %>% 
  set_names(score_names)

# CALC ss per case, write out .csv  ------------------------
  
ss_perCase <- pmap(
  list(
    norm_model_per_score_chosen_transform,
    norm_input_per_score,
    input_per_score,
    score_names
  ),
  ~
    get(..1)(..2) %>%
    pluck("x.t") %>% 
    tibble() %>% 
    bind_cols(..3) %>% 
    rename(!!sym(str_c(..4, norm_type, "_nz")) := ".") %>% 
    select(ID, !!sym(str_c(..4, norm_type, "_nz"))) %>% 
    mutate(across(!!sym(str_c(..4, norm_type, "_nz")),
      ~
        (round(. * 15) + 100))) %>% 
    mutate(across(!!sym(str_c(..4, norm_type, "_nz")),
                  ~
                    case_when(
                      . < ss_lower_bound ~ ss_lower_bound,
                      . > ss_upper_bound ~ ss_upper_bound,
                      TRUE ~ .
                    ) %>%
                    as.integer)) %>% 
    rename(!!sym(str_c(..4, norm_type, "_ss")) := !!sym(str_c(..4, norm_type, "_nz")))
)

output_ss_perCase <- pmap(
  list(input_per_score, ss_perCase),
  ~
  left_join(..1, ..2, by = "ID")
) %>% 
  reduce(full_join, by = c("ID")) %>% 
  arrange(ID)

write_csv(output_ss_perCase,
          here(
            str_c(output_file_path, input_file_stem,
                  "-ss-percase.csv")
          ),
          na = '')

# GENERATE BASIC FORMAT RAW-TO-ss LOOKUP TABLE, write to .csv -----------------------------------------

all_lookup_basic <- map2(
  score_names,
  raw_range_per_score,
  ~
    output_ss_perCase %>%
    group_by(!!sym(str_c(.x, norm_type))) %>%
    summarize(!!sym(str_c(.x, norm_type, "_ss")) := min(!!sym(
      str_c(.x, norm_type, "_ss")
    ))) %>%
    complete(!!sym(str_c(.x, norm_type)) := all_raw_range) %>%
    drop_na(!!sym(str_c(.x, norm_type))) %>%
    fill(!!sym(str_c(.x, norm_type, "_ss")), .direction = "downup") %>%
    rename(raw = !!sym(str_c(.x, norm_type))) %>% 
    mutate(across(
      !!sym(str_c(.x, norm_type, "_ss")),
      ~
        case_when(raw %in% c(.y) ~ .,
                  TRUE ~ NA_integer_))
    )
) %>%
  reduce(left_join,
         by = 'raw')


write_csv(all_lookup_basic,
          here(
            str_c(output_file_path, input_file_stem,
                  "-raw-ss-lookup-basic.csv")
          ),
          na = '')

# GENERATE PRINT FORMAT RAW-TO-ss LOOKUP TABLE -----------------------------------------

all_lookup_print <- all_lookup_basic %>% 
  pivot_longer(contains("_ss"), names_to = "scale", values_to = "ss") %>% 
  arrange(scale) %>% 
  group_by(scale) %>%
  complete(ss = 40:130) %>% 
  group_by(scale, ss) %>%
  filter(n() == 1 | n() > 1 & row_number()  %in% c(1, n())) %>%
  summarize(raw = str_c(raw, collapse = '--')) %>%
  mutate(across(raw, ~ case_when(is.na(.x) ~ '-', TRUE ~ .x))) %>%
  arrange(scale, desc(ss)) %>% 
  pivot_wider(names_from = scale,
              values_from = raw) %>% 
  rename_with(~ str_replace_all(., "_ss", "_raw")) %>%
  filter(!is.na(ss))

write_csv(all_lookup_print,
          here(
            str_c(output_file_path, input_file_stem,
                  "-raw-ss-lookup-print.csv")
          ),
          na = '')


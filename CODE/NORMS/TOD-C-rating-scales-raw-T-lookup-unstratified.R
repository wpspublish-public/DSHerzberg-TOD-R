# Load packages, read data

suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
suppressMessages(library(bestNormalize))

input_file_name <- "TODC_g1_12FINALMASTERstand_ratingscales.csv"
input_file_stem <- "TODC_g1_12FINALMASTERstand_ratingscales"
input_file_path <- "INPUT-FILES/NORMS/TODCratingscales_gr1_12final/"
output_file_path <- "OUTPUT-FILES/NORMS/TODCratingscales_gr1_12final/"

scores <- c("TODparent_tot", "TODteacher_tot", "TODself_tot")
# scale_prefix <- "TOD"
# scale_suffix <- c("parent_tot", "teacher_tot")
# age_range_name <- "child"
# form_name <- "parent"
all_raw_range <- 25:140
raw_range_parent <- 26:104
raw_range_teacher <- 30:120
raw_range_self <- 25:140
# TOT_raw_lower_bound <- 50
# subscale_raw_upper_bound <- 40
t_score_lower_bound <- 40
t_score_upper_bound <- 80

input_all <- suppressMessages(read_csv(here(str_c(
  input_file_path, input_file_name
)))) %>% 
  relocate(TODself_tot, .after = "TODteacher_tot")

input_parent <- input_all %>%
  select(-TODteacher_tot, -TODself_tot) %>%
  drop_na()

norm_input_parent <- input_parent %>%
  select(TODparent_tot) %>%
  as_vector() %>%
  set_names(NULL)

input_teacher <- input_all %>%
  select(-TODparent_tot, -TODself_tot) %>%
  drop_na()

norm_input_teacher <- input_teacher %>%
  select(TODteacher_tot) %>%
  as_vector() %>%
  set_names(NULL)

input_self <- input_all %>%
  select(-TODteacher_tot, -TODparent_tot) %>%
  drop_na()

norm_input_self <- input_self %>%
  select(TODself_tot) %>%
  as_vector() %>%
  set_names(NULL)


# DETERMINE BEST NORMALIZATION MODEL

set.seed(12345)
TOT_nz_obj_parent <- bestNormalize(norm_input_parent)
TOT_nz_obj_parent$chosen_transform
chosen_transform_parent <- class(TOT_nz_obj_parent$chosen_transform)[1]

TOT_nz_obj_teacher <- bestNormalize(norm_input_teacher)
TOT_nz_obj_teacher$chosen_transform
chosen_transform_teacher <- class(TOT_nz_obj_teacher$chosen_transform)[1]

TOT_nz_obj_self <- bestNormalize(norm_input_self)
TOT_nz_obj_self$chosen_transform
chosen_transform_self <- class(TOT_nz_obj_self$chosen_transform)[1]

# CALC NORMALIZED T-SCORES PER CASE, write out .csv  ------------------------
  
ntScore_perCase_parent <- get(chosen_transform_parent)(norm_input_parent) %>%
  pluck("x.t") %>%
  tibble() %>%
  bind_cols(input_parent) %>%
  rename(TODparent_tot_nz = ".") %>%
  select(ID, TODparent_tot_nz) %>%
  mutate(across(TODparent_tot_nz,
                ~
                  (round(. * 10) + 50))) %>% 
  mutate(across(TODparent_tot_nz,
                ~
                  case_when(
                    . < t_score_lower_bound ~ t_score_lower_bound,
                    . > t_score_upper_bound ~ t_score_upper_bound,
                    TRUE ~ .
                  ) %>%
                  as.integer)) %>% 
  rename(TODparent_tot_nt = TODparent_tot_nz)

ntScore_perCase_teacher <- get(chosen_transform_teacher)(norm_input_teacher) %>%
  pluck("x.t") %>%
  tibble() %>%
  bind_cols(input_teacher) %>%
  rename(TODteacher_tot_nz = ".") %>%
  select(ID, TODteacher_tot_nz) %>%
  mutate(across(TODteacher_tot_nz,
                ~
                  (round(. * 10) + 50))) %>% 
  mutate(across(TODteacher_tot_nz,
                ~
                  case_when(
                    . < t_score_lower_bound ~ t_score_lower_bound,
                    . > t_score_upper_bound ~ t_score_upper_bound,
                    TRUE ~ .
                  ) %>%
                  as.integer)) %>% 
  rename(TODteacher_tot_nt = TODteacher_tot_nz)

ntScore_perCase_self <- get(chosen_transform_self)(norm_input_self) %>%
  pluck("x.t") %>%
  tibble() %>%
  bind_cols(input_self) %>%
  rename(TODself_tot_nz = ".") %>%
  select(ID, TODself_tot_nz) %>%
  mutate(across(TODself_tot_nz,
                ~
                  (round(. * 10) + 50))) %>% 
  mutate(across(TODself_tot_nz,
                ~
                  case_when(
                    . < t_score_lower_bound ~ t_score_lower_bound,
                    . > t_score_upper_bound ~ t_score_upper_bound,
                    TRUE ~ .
                  ) %>%
                  as.integer)) %>% 
  rename(TODself_tot_nt = TODself_tot_nz)

output_ntScore_perCase <- list(input_all, 
                               ntScore_perCase_parent, 
                               ntScore_perCase_teacher, 
                               ntScore_perCase_self
                               ) %>% 
  reduce(left_join, by = c("ID"))

write_csv(output_ntScore_perCase,
          here(
            str_c(output_file_path, input_file_stem,
                  "-ntScore-percase.csv")
          ),
          na = '')

# GENERATE BASIC FORMAT RAW-TO-T LOOKUP TABLE, write to .csv -----------------------------------------

all_lookup_basic <- map(
  scores, 
  ~
  output_ntScore_perCase %>% 
  group_by(!!sym(.x)) %>% 
  summarize(!!sym(str_c(.x, "_nt")) := min(!!sym(str_c(.x, "_nt")))) %>% 
  complete(!!sym(.x) := all_raw_range) %>% 
  drop_na(!!sym(.x)) %>% 
  fill(!!sym(str_c(.x, "_nt")), .direction = "downup") %>% 
  rename(raw = !!sym(.x))
) %>%
  reduce(left_join,
         by = 'raw') %>% 
  rename(
    parent_t = TODparent_tot_nt,
    teacher_t = TODteacher_tot_nt,
    self_t = TODself_tot_nt
  ) %>% 
  mutate(
    across(
      parent_t,
      ~
        case_when(
          raw %in% raw_range_parent ~ .x,
          TRUE ~ NA_integer_)
    ), 
    across(
      teacher_t,
      ~
        case_when(
          raw %in% raw_range_teacher ~ .x,
          TRUE ~ NA_integer_)
    ), 
    across(
      self_t,
      ~
        case_when(
          raw %in% raw_range_self ~ .x,
          TRUE ~ NA_integer_)
    ), 
  )

write_csv(all_lookup_basic,
          here(
            str_c(output_file_path, input_file_stem,
                  "-raw-T-lookup-basic.csv")
          ),
          na = '')

# GENERATE PRINT FORMAT RAW-TO-T LOOKUP TABLE -----------------------------------------

all_lookup_print <- all_lookup_basic %>% 
  pivot_longer(contains("_t"), names_to = "scale", values_to = "Tscore") %>% 
  arrange(scale) %>% 
  group_by(scale) %>%
  complete(Tscore = 40:80) %>% 
  group_by(scale, Tscore) %>%
  filter(n() == 1 | n() > 1 & row_number()  %in% c(1, n())) %>%
  summarize(raw = str_c(raw, collapse = '--')) %>%
  mutate(across(raw, ~ case_when(is.na(.x) ~ '-', TRUE ~ .x))) %>%
  arrange(scale, desc(Tscore)) %>% 
  pivot_wider(names_from = scale,
              values_from = raw) %>% 
  rename_with(~ str_replace_all(., "_t", "_raw")) %>%
  rename(T_score = Tscore) %>% 
  filter(!is.na(T_score))

write_csv(all_lookup_print,
          here(
            str_c(output_file_path, input_file_stem,
                  "-raw-T-lookup-print.csv")
          ),
          na = '')


# Load packages, read data

suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
suppressMessages(library(bestNormalize))

input_file_path  <- "INPUT-FILES/NORMS/TODEratingscales/"
input_file_name <- "TODEratingscales.csv"
input_file_stem <- "TODEratingscales"
output_file_path  <- "OUTPUT-FILES/NORMS/TODEratingscales/"


# scale_prefix <- "TODE"
# scale_suffix <- c("parent_tot", "teacher_tot")
# age_range_name <- "child"
# form_name <- "parent"
all_raw_range <- 26:108
# TOT_raw_lower_bound <- 50
# subscale_raw_upper_bound <- 40
t_score_lower_bound <- 40
t_score_upper_bound <- 80

input_all <- suppressMessages(read_csv(here(str_c(
  input_file_path, input_file_name
))))

input_parent <- input_all %>%
  select(-TODEteacher_tot) %>%
  drop_na()

norm_input_parent <- input_parent %>%
  select(TODEparent_tot) %>%
  as_vector() %>%
  set_names(NULL)

input_teacher <- input_all %>%
  select(-TODEparent_tot) %>%
  drop_na()

norm_input_teacher <- input_teacher %>%
  select(TODEteacher_tot) %>%
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

# CALC NORMALIZED T-SCORES PER CASE, write out .csv  ------------------------
  
ntScore_perCase_parent <- get(chosen_transform_parent)(norm_input_parent) %>%
  pluck("x.t") %>%
  tibble() %>%
  bind_cols(input_parent) %>%
  rename(TODEparent_tot_nz = ".") %>%
  select(ID, TODEparent_tot_nz) %>%
  mutate(across(TODEparent_tot_nz,
                ~
                  (round(. * 10) + 50))) %>% 
  mutate(across(TODEparent_tot_nz,
                ~
                  case_when(
                    . < t_score_lower_bound ~ t_score_lower_bound,
                    . > t_score_upper_bound ~ t_score_upper_bound,
                    TRUE ~ .
                  ) %>%
                  as.integer)) %>% 
  rename(TODEparent_tot_nt = TODEparent_tot_nz)

ntScore_perCase_teacher <- get(chosen_transform_teacher)(norm_input_teacher) %>%
  pluck("x.t") %>%
  tibble() %>%
  bind_cols(input_teacher) %>%
  rename(TODEteacher_tot_nz = ".") %>%
  select(ID, TODEteacher_tot_nz) %>%
  mutate(across(TODEteacher_tot_nz,
                ~
                  (round(. * 10) + 50))) %>% 
  mutate(across(TODEteacher_tot_nz,
                ~
                  case_when(
                    . < t_score_lower_bound ~ t_score_lower_bound,
                    . > t_score_upper_bound ~ t_score_upper_bound,
                    TRUE ~ .
                  ) %>%
                  as.integer)) %>% 
  rename(TODEteacher_tot_nt = TODEteacher_tot_nz)

output_ntScore_perCase <- list(input_all, ntScore_perCase_parent, ntScore_perCase_teacher) %>% 
  reduce(left_join, by = c("ID"))

write_csv(output_ntScore_perCase,
          here(
            str_c(output_file_path, input_file_stem,
                  "-ntScore-percase.csv")
          ),
          na = '')

# GENERATE BASIC FORMAT RAW-TO-T LOOKUP TABLE, write to .csv -----------------------------------------

all_lookup_basic <- map(
  c("TODEparent_tot", "TODEteacher_tot"), 
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
    parent_t = TODEparent_tot_nt,
    teacher_t = TODEteacher_tot_nt
  ) %>% 
  mutate(
    across(
      parent_t,
      ~
        case_when(
          raw %in% c(27:108) ~ .x,
          TRUE ~ NA_integer_)
    ), 
    across(
      teacher_t,
      ~
        case_when(
          raw %in% c(26:104) ~ .x,
          TRUE ~ NA_integer_)
    ), 
  )

write_csv(all_lookup_basic,
          here(
            str_c(output_file_path, input_file_stem,
                  "-raw-T-lookup-basic.csv")
          ),
          na = '')

############## START HERE

# GENERATE PRINT FORMAT RAW-TO-T LOOKUP TABLE -----------------------------------------

all_lookup_print <- all_lookup_basic %>% 
pivot_longer(contains("nt"), names_to = "scale", values_to = "NT") %>% 
  arrange(scale) %>% 
  group_by(scale) %>%
  complete(NT = 40:80) %>% 
  group_by(scale, NT) %>%
  filter(n() == 1 | n() > 1 & row_number()  %in% c(1, n())) %>%
  summarize(raw = str_c(raw, collapse = '--')) %>%
  mutate(across(raw, ~ case_when(is.na(.x) ~ '-', TRUE ~ .x))) %>%
  arrange(scale, desc(NT)) %>% 
  pivot_wider(names_from = scale,
              values_from = raw) %>% 
  rename_with(~ str_replace_all(., "_nt", "_raw")) %>%
  rename(T_score = NT) %>% 
  filter(!is.na(T_score))

write_csv(all_lookup_print,
          here(str_c(
            "OUTPUT-FILES/TABLES/",
            str_c("raw-T-lookup-print",
                  age_range_name,
                  form_name,
                  sep = "-"),
            ".csv"
          )),
          na = '')

# RAW SCORE DESCRIPTIVES AND DEMOGRAPHIC COUNTS -----------------------------------------

assign(
  str_c("raw_score_desc", age_range_name, form_name, sep = "_"),
  get(str_c("data", age_range_name, form_name, sep = "_")) %>%
    select(contains("raw")) %>%
    describe(fast = TRUE) %>%
    rownames_to_column(var = "scale") %>%
    select(scale, n, mean, sd) %>%
    mutate(across(c(mean, sd), ~ (round(
      ., 2
    ))))
)

write_csv(get(str_c(
  "raw_score_desc", age_range_name, form_name, sep = "_"
)),
here(str_c(
  "OUTPUT-FILES/TABLES/",
  str_c("raw-score-desc",
        age_range_name,
        form_name,
        sep = "-"),
  ".csv"
)),
na = '')

var_order <- c("age_range", "gender", "educ", "ethnic", "region")

cat_order <- c(
  # age_range
  str_sort(unique(get(str_c("data", age_range_name, form_name, sep = "_"))$age_range)),
  # gender
  str_sort(unique(get(str_c("data", age_range_name, form_name, sep = "_"))$gender), 
           decreasing = TRUE),
  # educ
  "no_HS", "HS_grad", "some_college", "BA_plus" , 
  # ethnic
  "hispanic", "asian", "black", "white", "other",
  # Region
  "northeast", "midwest", "south", "west")

assign(
  str_c("demo_counts", age_range_name, form_name, sep = "_"),
  get(str_c("data", age_range_name, form_name, sep = "_")) %>%
    select(all_of(var_order)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "category") %>%
    group_by(variable, category) %>%
    count(variable, category) %>%
    arrange(match(variable, var_order), match(category, cat_order)) %>%
    ungroup() %>%
    mutate(across(
      variable,
      ~
        case_when(lag(.x) == .x ~ NA_character_,
                  TRUE ~ .x)
    ))
)

# write demo counts table to .csv
write_csv(get(str_c(
  "demo_counts", age_range_name, form_name, sep = "_"
)),
here(str_c(
  "OUTPUT-FILES/TABLES/",
  str_c("demo-counts",
        age_range_name,
        form_name,
        sep = "-"),
  ".csv"
)),
na = '')

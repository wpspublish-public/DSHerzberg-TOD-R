# Load packages, read data

suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
suppressMessages(library(bestNormalize))

input_file_path  <- "INPUT-FILES/NORMS/TODEratingscales/"
input_file_name <- "TODEratingscales.csv"

# scale_prefix <- "TODE"
# scale_suffix <- c("parent_tot", "teacher_tot")
# age_range_name <- "child"
# form_name <- "parent"
# all_raw_range <- 10:200
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



# DETERMINE BEST NORMALIZATION MODEL, CALC NORMALIZED T-SCORES PER CASE  ------------------------

set.seed(12345)
TOT_nz_obj <- bestNormalize(norm_input_parent)
TOT_nz_obj$chosen_transform
chosen_transform <- class(TOT_nz_obj$chosen_transform)[1]

ntScore_perCase <- sqrt_x(norm_input_parent) %>%
  pluck("x.t") %>%
  tibble() %>%
  bind_cols(input_parent) %>%
  rename(TODEparent_tot_nz = ".") %>%
  relocate(TODEparent_tot_nz, .after = TODEparent_tot) %>%
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

########## START HERE i've gotten all the way to NT scores per case for parent
#scale, now recreate that for teach scale, better yet, do it in one run for both
#teacher adn parent by mapping over a list



# mutate(
#   across(
#     TODEparent_tot_nz,
#     ~
#       (round(. * 10) + 50) %>%
#       {
#         case_when(
#           . < t_score_lower_bound ~ t_score_lower_bound,
#           . > t_score_upper_bound ~ t_score_upper_bound,
#           TRUE ~ .
#         )
#       } %>%
#       as.integer))


ntScore_perCase <- nzScore_perCase %>%
  mutate(across(everything(),
                ~
                  (round(. * 10) + 50) %>%
                  {
                    case_when(
                      . < t_score_lower_bound ~ t_score_lower_bound,
                      . > t_score_upper_bound ~ t_score_upper_bound,
                      TRUE ~ .
                    )
                  } %>%
                  as.integer)) %>%
  rename_with( ~ str_c(scale_prefix, str_replace_all(., "nz", "nt")))

assign(
  str_c("data", age_range_name, form_name, "nt", sep = "_"),
  get(str_c("data", age_range_name, form_name, sep = "_")) %>% bind_cols(ntScore_perCase) %>%
    mutate(clin_status = 'typ',
           clin_dx = NA) %>%
    select(
      ID:region,
      clin_status,
      clin_dx,
      contains("raw"),
      contains("nt"),
      everything()
    )
)

write_csv(get(str_c(
  "data", age_range_name, form_name, "nt", sep = "_"
)),
here(str_c(
  "OUTPUT-FILES/TABLES/",
  str_c("nt-Scores-per-case",
        age_range_name,
        form_name,
        sep = "-"),
  ".csv"
)),
na = '')

get(str_c("data", age_range_name, form_name, "nt", sep = "_")) %>%
  select(contains("TOT_nt")) %>%
  as_vector() %>%
  MASS::truehist(.,
                 h = 1,
                 prob = FALSE,
                 xlab = "TOT_nt")

# GENERATE BASIC FORMAT RAW-TO-T LOOKUP TABLE -----------------------------------------

all_lookup_basic <- map(
  scale_suffix,
  ~ get(str_c(
    "data", age_range_name, form_name, "nt", sep = "_"
  )) %>%
    group_by(!!sym(str_c(
      scale_prefix, .x, "_raw"
    ))) %>%
    summarize(!!sym(str_c(
      scale_prefix, .x, "_nt"
    )) := min(!!sym(
      str_c(scale_prefix, .x, "_nt")
    ))) %>%
    complete(!!sym(str_c(
      scale_prefix, .x, "_raw"
    )) := all_raw_range) %>%
    fill(!!sym(str_c(
      scale_prefix, .x, "_nt"
    )),
    .direction = "downup") %>%
    rename(raw = !!sym(str_c(
      scale_prefix, .x, "_raw"
    ))) 
) %>%
  reduce(left_join,
         by = 'raw') %>% 
mutate(across(
  contains(scale_suffix[-length(scale_suffix)]),
  ~ case_when(raw > subscale_raw_upper_bound ~ NA_integer_,
              TRUE ~ .x)
),
across(
  contains(scale_suffix[length(scale_suffix)]),
  ~ case_when(raw < TOT_raw_lower_bound ~ NA_integer_,
              TRUE ~ .x)
))

write_csv(all_lookup_basic,
here(str_c(
  "OUTPUT-FILES/TABLES/",
  str_c("raw-T-lookup",
        age_range_name,
        form_name,
        sep = "-"),
  ".csv"
)),
na = '')

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

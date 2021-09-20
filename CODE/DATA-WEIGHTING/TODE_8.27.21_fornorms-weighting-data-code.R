suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(survey)))

set.seed(123)

gender <- c("male", "female")
educ <- c("no_HS", "HS_grad", "some_college", "BA_plus")
ethnic <- c("hispanic", "asian", "black", "white", "other")
region <- c("northeast", "south", "midwest", "west")

var_order <- c("agestrat", "gradestrat", "gender", "educ", "ethnic", "region")

var_order_census_match  <- c("gender", "educ", "ethnic", "region")

cat_order <- c(
  NA, "5:0-5:7", "5:8-5:11", "6:0-6:3", "6:4-6:7", "6:8-6:11", "7:0-7:5", 
  "7:6-7:11", "8:0-9:3",
  NA, "K_Fall", "K_Spring", "1_Fall", "1_Spring", "2_Fall", "2_Spring", 
  NA, "male", "female",
  NA, "no_HS", "HS_grad", "some_college", "BA_plus",
  NA, "hispanic", "asian", "black", "white", "other",
  NA, "northeast", "south", "midwest", "west")

last_item <- 32

file_name <- "TODE_8.27.21_fornorms-"

input_file_path <- "INPUT-FILES/NORMS/TODE_8.27.21_fornorms/"
output_file_path <- "OUTPUT-FILES/NORMS/TODE_8.27.21_fornorms/"
fileName_path   <- "TODE_8.27.21_fornorms.csv"

outlier_IDs <- c(326013, 234011, 235029, 262026, 
                 305022, 326018, 231001, 231007, 338006)

original_input <- suppressMessages(read_csv(
  str_c(input_file_path, fileName_path)
)) %>% rename(
  gender = Gender,
  educ = SES,
  ethnic = Ethnicity,
) %>% 
  mutate(
    across(agestrat, ~ case_when(
      . == 1 ~ "5:0-5:7",
      . == 2 ~ "5:8-5:11",
      . == 3 ~ "6:0-6:3",
      . == 4 ~ "6:4-6:7",
      . == 5 ~ "6:8-6:11",
      . == 6 ~ "7:0-7:5",
      . == 7 ~ "7:6-7:11",
      . == 8 ~ "8:0-9:3")),
    across(gradestrat, ~ case_when(
      . == 1 ~ "K_Fall",
      . == 2 ~ "K_Spring",
      . == 3 ~ "1_Fall",
      . == 4 ~ "1_Spring",
      . == 5 ~ "2_Fall",
      . == 6 ~ "2_Spring")),
    across(gender, ~ case_when(
      . == 1 ~ "male",
      . == 2 ~ "female")),
    across(educ, ~ case_when(
      . == 1 ~ "no_HS",
      . == 2 ~ "HS_grad",
      . == 3 ~ "some_college",
      . == 4 ~ "BA_plus")),
    across(ethnic, ~ case_when(
      . == 1 ~ "asian",
      . == 2 ~ "black",
      . == 3 ~ "white",
      . %in% c(4, 5, 6) ~ "other",
      . == 7 ~ "hispanic")),
    across(region, ~ case_when(
      . == 1 ~ "northeast",
      . == 2 ~ "midwest",
      . == 3 ~ "south",
      . == 4 ~ "west"))
  ) %>% 
# Get rid of zero values for "sum" scores by adding 1 to all
  mutate(
    across(
      c(sege_sum:lske_sum),
      ~
        .x + 1
    )
  ) %>% 
  # remove outliers
  filter(!(ID %in% outlier_IDs))

fileName_path   <- "census_pct.csv"

census_match_cat_count <- suppressMessages(read_csv(
  str_c(input_file_path, fileName_path)
)) %>% 
  mutate(n_census = round(nrow(original_input)*(pct_census/100), 0))

census_match_pct_wide <- suppressMessages(read_csv(
  str_c(input_file_path, fileName_path)
)) %>%
  select(-var) %>%
  pivot_wider(names_from = cat,
              values_from = pct_census)

var_order_census_match %>%
  map(
    ~ census_match_cat_count %>%
      filter(var == all_of(.x)) %>%
      select(-var, -pct_census) %>%
      rename(!!.x := cat, Freq = n_census)
  ) %>%
  setNames(str_c(var_order_census_match, "_census")) %>%
  list2env(envir = .GlobalEnv) %>% 
  invisible(.)

unweighted_survey_object <- svydesign(ids = ~1, 
                                      data = original_input, 
                                      weights = NULL)

rake_original_input <- rake(design = unweighted_survey_object,
                              sample.margins = list(~gender, ~educ, ~ethnic, ~region),
                              population.margins = list(gender_census, educ_census, 
                                                        ethnic_census, region_census))

input_demo_wts <- bind_cols(
  rake_original_input[["variables"]],  
  data.frame(rake_original_input[["prob"]]), 
  data.frame(demo_wt = weights(rake_original_input))
) %>% 
  rename(samp_prob = rake_original_input...prob...) %>% 
  mutate(ratio = samp_prob / demo_wt) %>% 
  select(ID:region, samp_prob, demo_wt, ratio, everything()) %>% 
  arrange(desc(samp_prob))

rm(list = ls(pattern = "object|rake"))

unweighted_output <- input_demo_wts %>% 
  select(-c(samp_prob, ratio)) %>%
  rename_with(~ str_c("i", str_pad(
    as.character(1:last_item), 2, side = "left", pad = "0"), "_uw"), 
    i01:!!sym(str_c("i", last_item))) %>%
  mutate(
    TOT_raw_unweight = rowSums(.[grep("*_uw", names(.))]
  )) %>%
  relocate(TOT_raw_unweight, .after = demo_wt)

write_csv(unweighted_output,
          here(
            str_c(
              output_file_path,
              file_name,
              "unweighted-data-for-analysis.csv"
            )
          ),
          na = "")

weighted_output <- original_input %>%
  left_join(unweighted_output[c("ID", "demo_wt")], by = "ID") %>%
  rename_with(~ str_c("i", str_pad(
    as.character(1:last_item), 2, side = "left", pad = "0"
  ), "_w"),
  i01:!!sym(str_c("i", last_item))) %>%
  mutate(across(c(i01_w:!!sym(str_c("i", last_item, "_w"))),
                ~ . * demo_wt)) %>%
  mutate(
    TOT_raw_weight = rowSums(.[grep("*_w$", names(.))]
    )) %>%
  relocate(demo_wt, TOT_raw_weight, .before = i01_w)

write_csv(weighted_output,
          here(
            str_c(
              output_file_path,
              file_name,
              "weighted-data-for-analysis.csv"
            )
          ),
          na = "")

demo_weight_by_crossing_input <- weighted_output %>%
  group_by(demo_wt) %>%
  summarize(
    gender = first(gender),
    educ = first(educ),
    ethnic = first(ethnic),
    n_input = n(),
    region = first(region)
  ) %>%
  relocate(c(demo_wt, n_input), .after = region) %>%
  arrange(
    match(gender, cat_order),
    match(educ, cat_order),
    match(ethnic, cat_order),
    match(region, cat_order)
  )

write_csv(demo_weight_by_crossing_input,
          here(
            str_c(
              output_file_path,
              file_name,
              "weights-per-demo-crossing-input.csv"
            )
          ),
          na = "")

demo_crossings_all <- expand_grid(
  gender = gender,
  educ = educ,
  ethnic = ethnic,
  region = region
)

demo_weight_by_crossing_all <- demo_crossings_all %>% 
  left_join(demo_weight_by_crossing_input, by = c("gender", "educ", "ethnic", "region")) %>% 
  bind_cols(
    census_match_pct_wide
  ) %>% 
  mutate(
    pct_census_gender = case_when(
      gender == "male" ~ male,
      TRUE ~ female
    ), 
    pct_census_educ = case_when(
      educ == "no_HS" ~ no_HS,
      educ == "HS_grad" ~ HS_grad,
      educ == "some_college" ~ some_college,
      TRUE ~ BA_plus
    ), 
    pct_census_ethnic = case_when(
      ethnic == "hispanic" ~ hispanic,
      ethnic == "asian" ~ asian,
      ethnic == "black" ~ black,
      ethnic == "white" ~ white,
      TRUE ~ other
    ), 
    pct_census_region = case_when(
      region == "northeast" ~ northeast,
      region == "south" ~ south,
      region == "midwest" ~ midwest,
      TRUE ~ west
    ), 
    across(
      c(
        pct_census_gender,
        pct_census_educ,
        pct_census_ethnic,
        pct_census_region,
      ),
      ~
        . * .01
    ),
    n_census = round(
      (pct_census_gender * pct_census_educ * 
         pct_census_ethnic * pct_census_region) *
        nrow(original_input), 
      2
    )
  ) %>% 
  select(gender:n_input, n_census)

write_csv(demo_weight_by_crossing_all,
          here(
            str_c(
              output_file_path,
              file_name,
              "weights-per-demo-crossing-all.csv"
            )
          ),
          na = "")

weighted_sum_scores <- original_input %>%
  mutate(ORF_noNeg = ORF + 100) %>%
  select(-ORF,-(i01:!!sym(str_c("i", last_item)))) %>%
  left_join(demo_weight_by_crossing_input,
            by = c("gender", "educ", "ethnic", "region")) %>%
  relocate(agestrat:gradestrat, .before = gender) %>%
  mutate(
    sege_sum_w = sege_sum,
    rlne_sum_w = rlne_sum,
    rhme_sum_w = rhme_sum,
    snwe_sum_w = snwe_sum,
    lswe_sum_w = lswe_sum,
    lske_sum_w = lske_sum,
    ORF_noNeg_w = ORF_noNeg
  ) %>%
  mutate(across(c(sege_sum_w:ORF_noNeg_w),
                ~
                  case_when(
                    is.na(noweight) ~ round(. * demo_wt, 0),
                    TRUE ~ .
                    )
                )
         ) %>%
  select(ID:region, demo_wt, 
         sege_sum, sege_sum_w,
         rlne_sum, rlne_sum_w,
         rhme_sum, rhme_sum_w,
         snwe_sum, snwe_sum_w,
         lswe_sum, lswe_sum_w,
         lske_sum, lske_sum_w,
         ORF_noNeg, ORF_noNeg_w
  )

# write weighted_sum_scores twice, because it is both output from weighting, and
# input to norming.
write_csv(weighted_sum_scores,
          here(
            str_c(
              input_file_path,
              file_name,
              "weighted-sum-scores.csv"
            )
          ),
          na = "")
write_csv(weighted_sum_scores,
          here(
            str_c(
              output_file_path,
              file_name,
              "weighted-sum-scores.csv"
            )
          ),
          na = "")

# PROOF OF CONCEPT

cat_count_comp <-
  var_order_census_match %>%
  map_df(
    ~
      original_input %>%
      group_by(across(all_of(.x))) %>%
      summarize(n_input = n()) %>%
      rename(cat = all_of(.x)) %>%
      mutate(var = all_of(.x),
             pct_input = round((n_input / nrow(original_input)) * 100, 1)) %>%
      relocate(var, .before = cat)
  ) %>%
  arrange(match(cat, cat_order)) %>%
  bind_cols(census_match_cat_count[c("n_census", "pct_census")]) %>%
  mutate(pct_diff = pct_input - pct_census)

knitr::kable(cat_count_comp %>%
               mutate(across(var,
                             ~ case_when(
                               lag(.x) == .x ~ "",
                               T ~ .x
                             ))),
             caption = "Table 1: Comparison of input sample percentage to census target")

knitr::kable(
  weighted_output %>%
    filter(
      gender == "female" &
        educ == "no_HS" &
        ethnic == "white" &
        region == "northeast"
    ) %>%
    select(-noweight, -(sege_sum:gradestrat), -(i01_w:!!sym(
      str_c("i", last_item, "_w")
    )), -TOT_raw_weight) %>%
    sample_n(1),
  digits = 2,
  caption = "Table 2: Demographic multiplier from under-sampled cell"
)

knitr::kable(
  weighted_output %>%
    filter(
      gender == "female" &
        educ == "BA_plus" &
        ethnic == "white" &
        region == "northeast"
    ) %>%
    select(-noweight, -(sege_sum:gradestrat), -(i01_w:!!sym(
      str_c("i", last_item, "_w")
    )), -TOT_raw_weight) %>%
    sample_n(1),
  digits = 2,
  caption = "Table 3: Demographic mupltiplier from accurately sampled cell"
)

knitr::kable(
  tail(input_demo_wts) %>%
    select(-noweight, -(sege_sum:gradestrat), -(i01:!!sym(
      str_c("i", last_item)
    )),-ratio),
  digits = 2,
  caption = "Table 4: Cases from under-sampled cells"
)

knitr::kable(
  filter(input_demo_wts, between(samp_prob, .98, 1.02)) %>%
    select(-noweight, -(sege_sum:gradestrat), -(i01:!!sym(
      str_c("i", last_item)
    )),-ratio),
  digits = 2,
  caption = "Table 5: Cases from accurately sampled cells"
)

knitr::kable(
  head(input_demo_wts) %>%
    select(-noweight, -(sege_sum:gradestrat), -(i01:!!sym(
      str_c("i", last_item)
    )),-ratio),
  digits = 2,
  caption = "Table 6: Cases from over-sampled cells"
)

ggplot(input_demo_wts, aes(demo_wt, samp_prob)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(x=1, y=1, color='purple', size = 3) + 
  scale_x_continuous(breaks = seq(0, 8, .5), minor_breaks = seq(0, 8, .1)) +
  scale_y_continuous(breaks = seq(0, 2.5, .5),
                     minor_breaks = seq(0, 2.5, .1)) +
  xlab("demographic weighting multiplier") +
  ylab("sampling probability") +
  annotate(
    "text",
    x = 1.5,
    y = 1.5,
    label = "Oversampled relative to census: sampling probability > 1",
    color = "red",
    hjust = 0
  ) +
  annotate(
    "text",
    x = 1.5,
    y = 1.4,
    label = "Undersampled relative to census: sampling probability < 1",
    color = "darkgreen",
    hjust = 0
  ) +
annotate(
  "text",
  x = 1.1,
  y = 1.1,
  label = "samp prob = weight = 1: input cell pct matches census pct",
  color = "purple",
  hjust = 0
) 

unweighted_TOT_sum <- var_order_census_match %>%
  map_df(
    ~
      unweighted_output %>%
      group_by(across(all_of(.x))) %>%
      summarize(TOT_sum_input = sum(TOT_raw_unweight)) %>% 
      rename(cat = all_of(.x)) %>%
      mutate(var = all_of(.x)) %>%
      relocate(var, .before = cat)
  ) %>% 
  arrange(match(cat, cat_order))

weighted_TOT_sum <- var_order_census_match %>%
  map_df(
    ~
      weighted_output %>%
      group_by(across(all_of(.x))) %>%
      summarize(TOT_sum_weighted = round(sum(TOT_raw_weight))) %>% 
      rename(cat = all_of(.x)) %>%
      mutate(var = all_of(.x)) %>%
      relocate(var, .before = cat)
  ) %>% 
  arrange(match(cat, cat_order))

list_comp <- list(census_match_cat_count[c("var", "cat", "n_census")], 
                  cat_count_comp[c("var", "cat", "n_input")],
                  weighted_TOT_sum, unweighted_TOT_sum)

TOT_sum_cat_count_comp <- list_comp %>%
  reduce(left_join, by = c("var", "cat")) %>%
  mutate(n_diff = n_input - n_census,
         sum_diff = TOT_sum_input - TOT_sum_weighted) %>%
  relocate(var,
           cat,
           n_input,
           n_census,
           TOT_sum_input,
           TOT_sum_weighted,
           n_diff,
           sum_diff) %>%
  mutate(cat = factor(cat, levels = cat))

knitr::kable(TOT_sum_cat_count_comp %>%
               mutate(across(var,
                             ~ case_when(
                               lag(.x) == .x ~ "",
                               T ~ .x
                             ))),
             caption = "Table 7: Comparison of unweighted and weighted total scores")

plot_data <- TOT_sum_cat_count_comp %>%
  mutate(across(var,
                ~ runner::fill_run(.)))

ggplot(plot_data, aes(n_diff, sum_diff)) +
  geom_point(aes(shape = var, color = cat), size = 3) +
  facet_wrap( ~ var) +
  xlab("n_diff") +
  ylab("sum_diff") +
  guides(col = guide_legend(nrow = 11)) +
  geom_smooth(
    method = 'lm',
    se = F,
    formula = y ~ x,
    size = .3
  ) +
  ggpmisc::stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..rr.label.., sep = '*plain(\',\')~')),
    rr.digits = 5,
    parse = TRUE
  )

  


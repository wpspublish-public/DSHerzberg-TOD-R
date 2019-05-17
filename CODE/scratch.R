# frm <- c("TOD", "TOD_E")
# map(
#   frm,
#   ~
#     eval(as.name(paste0("demos_", .x))) %>% ungroup() %>% select(agestrat, gender)  %>%
#     count(agestrat, gender) %>%
#     add_row(agestrat = NA_character_, gender = "Less_than_HS", n = NA_integer_) %>% 
#     add_row(agestrat = NA_character_, gender = "HS_degree", n = NA_integer_) %>% 
#     spread(gender, n, fill = 0) %>%
#     filter(!is.na(agestrat)) %>% 
#     select(agestrat, Male, Female) %>%
#     rename(Male_actual = Male, Female_actual = Female) %>%
#     assign(paste0("gender_input_", .x), ., envir = .GlobalEnv)
# )
# 
# 
# frm <- c("TOD", "TOD_E")
map(
  frm,
  ~
    list(
      paste0("static_columns_TOD", .x),
      paste0("gender_input_TOD", .x),
      paste0("PEL_input_TOD", .x),
      paste0("ethnic_input_TOD", .x),
      paste0("region_input_TOD", .x)
    ) %>%
    reduce(left_join, by = "agestrat") %>%
    mutate_if(is.numeric , replace_na, replace = 0) %>%
    mutate(
      Male_needed = (target_n * Male_census_pct) - Male_actual,
      Female_needed = (target_n * Female_census_pct) - Female_actual,
      Less_than_HS_needed = (target_n * Less_than_HS_census_pct) - Less_than_HS_actual,
      HS_degree_needed = (target_n * HS_degree_census_pct) - HS_degree_actual,
      Some_college_needed = (target_n * Some_college_census_pct) - Some_college_actual,
      BA_plus_needed = (target_n * BA_plus_census_pct) - BA_plus_actual,
      Hispanic_needed = (target_n * Hispanic_census_pct) - Hispanic_actual,
      Asian_needed = (target_n * Asian_census_pct) - Asian_actual,
      Black_needed = (target_n * Black_census_pct) - Black_actual,
      White_needed = (target_n * White_census_pct) - White_actual,
      Other_needed = (target_n * Other_census_pct) - Other_actual,
      Northeast_needed = (target_n * Northeast_census_pct) - Northeast_actual,
      South_needed = (target_n * South_census_pct) - South_actual,
      Midwest_needed = (target_n * Midwest_census_pct) - Midwest_actual,
      West_needed = (target_n * West_census_pct) - West_actual
    ) %>%
    select(final_output_cols) %>%
    # `mutate_at` applies funs over columns designated by
    # `vars(contains("_needed"))`, i.e., only the columns specifying cases still
    # needed for a given demographic category, in this case it finds any negative
    # value in any cell, and recodes it to 0 using `case_when`, when it finds
    # non-negative values (the `TRUE` argument - equivalent to ELSE),  it doesn't
    # change the value: `.x` is a token for any cell value, `as.double` ensures
    # output of `TRUE` is same data type as input.
    mutate_at(vars(contains("_needed")),
              ~ case_when(.x < 0 ~ 0,
                          TRUE ~ as.double(.x))) %>%
    # In this `mutate`, `pmax` is used to get the maximum of a set of values,
    # because the elements being evaluated are of different lengths, and using
    # `max` would return incorrect values.
    mutate(total_usable_cases = target_n - pmax((Male_needed + Female_needed),
                                                (
                                                  Less_than_HS_needed + HS_degree_needed + Some_college_needed + BA_plus_needed
                                                ),
                                                (
                                                  Hispanic_needed + Asian_needed + Black_needed + White_needed + Other_needed
                                                ),
                                                (Northeast_needed + South_needed + Midwest_needed + West_needed)
    )) %>%
    # save the rounding to the last step, so calculations can occur on unrounded
    # numbers, use `mutate_at` with `vars` and `funs` args to provide correct
    # arguments within pipe.
    mutate_at(vars(contains("_needed"), total_usable_cases), funs(round(., 0))) %>%
    assign(paste0("demo_tracking_output_", .x), ., envir = .GlobalEnv)
  
  # Write output appended with date
  write_csv(paste0("demo_tracking_output_", .x), here(
    paste0(
      "DATA/OUTPUT/",
      .x,
      "TOD_demo_tracking_output_",
      format(Sys.Date(), "%Y-%m-%d"),
      ".csv"
    )
  ))
)

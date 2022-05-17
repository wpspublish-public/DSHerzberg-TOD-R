print_lookups_at_start <- age_test_cols_at[1:10] %>% 
  map(
    ~
      .x %>% 
      reduce(left_join, by = c("perc", "ss")) %>%
      rename_with(~ output_test_names_sre1, contains("-"))
  ) %>% 
  set_names(age_strat_start)


print_lookups_at_both <- age_test_cols_at[11:16] %>% 
  map(
    ~
      .x %>% 
      reduce(left_join, by = c("perc", "ss")) %>%
      rename_with(~ output_test_names_both, contains("-"))
  ) %>% 
  set_names(age_strat_both)

print_lookups_at_end <- age_test_cols_at[17:20] %>% 
  map(
    ~
      .x %>% 
      reduce(left_join, by = c("perc", "ss")) %>%
      rename_with(~ output_test_names_sre2, contains("-"))
  ) %>% 
  set_names(age_strat_end)

print_lookups_at <- c(print_lookups_at_start, print_lookups_at_both, print_lookups_at_end)


write_xlsx(print_lookups_at,
           here(
             str_c(
               output_file_path, tod_form, "-print-lookup-tables-test-", norm_type, ".xlsx"
             ))
)

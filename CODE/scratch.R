fileName_path   <- "census_pct.csv"

temp1 <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
))) %>% 
  mutate(
    cat_gender = cat,
    cat_educ = cat,
    cat_ethnic = cat,
    cat_region = cat
  ) %>%
  select(-var, -cat)


temp2 <- demo_weight_by_crossing %>% 
  left_join(
    temp1, 
    by = c(
      "gender" = "cat_gender"
      )
  )


# "educ" = "cat_educ", 
# "ethnic" = "cat_ethnic", 
# "region" = "cat_region"

temp3 <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
))) %>% 
  select(-var) %>% 
  pivot_wider(
    names_from = cat,
    values_from = pct_census
    
  )

temp4 <- 
  bind_cols(
    demo_weight_by_crossing,
    temp3
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

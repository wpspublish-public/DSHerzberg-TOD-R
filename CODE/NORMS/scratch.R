temp1 <- read_csv(
  here(
    str_c(
      input_file_path, combined_score_to_norm_file_name
    )
  )
)%>% 
  mutate(
    across(
      c(DOB, admin_date),
      ~
        mdy(.x)
    ),
    age = (DOB %--% admin_date) / years (1)
  ) %>% 
  filter(!is.na(pflsum2))



sort(unique(temp1$age))

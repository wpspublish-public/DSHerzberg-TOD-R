frm <- c("TOD", "TOD_E")
map(
  frm,
  ~
    test <- demos_TOD %>% ungroup() %>% select(agestrat, gender)  %>% filter(gender != "Other") %>% 
    count(agestrat, gender) %>%
    add_row(agestrat = NA_character_, gender = "Male", n = NA_integer_) %>%
    add_row(agestrat = NA_character_, gender = "Female", n = NA_integer_) %>%
    spread(gender, n, fill = 0) %>%
    filter(!is.na(agestrat)) %>%
    select(agestrat, Male, Female) %>%
    rename(Male_actual = Male, Female_actual = Female) %>%
    assign('gender_input_TOD', ., envir = .GlobalEnv)
)

test_PEL <- demos_TOD %>% ungroup() %>% select(agestrat, PEL)  %>%
  count(agestrat, PEL) %>%
  # add_row(agestrat = NA_character_, PEL = "Less_than_HS", n = NA_integer_) %>% 
  # add_row(agestrat = NA_character_, PEL = "HS_degree", n = NA_integer_) %>% 
  # add_row(agestrat = NA_character_, PEL = "Some_college", n = NA_integer_) %>% 
  # add_row(agestrat = NA_character_, PEL = "BA_plus", n = NA_integer_) %>% 
  spread(PEL, n, fill = 0) %>%
  filter(!is.na(agestrat)) %>% 
  select(agestrat, Less_than_HS, HS_degree, Some_college, BA_plus) %>%
  rename(
    Less_than_HS_actual = Less_than_HS,
    HS_degree_actual = HS_degree,
    Some_college_actual = Some_college,
    BA_plus_actual = BA_plus) %>% 
  assign('PEL_input_TOD', ., envir = .GlobalEnv)

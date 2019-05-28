frm <- c("TOD", "TOD_E")
map(
  frm,
  ~
    test <- demos_TOD %>% ungroup() %>% select(agestrat, gender)  %>%
    count(agestrat, gender) %>%
    add_row(agestrat = NA_character_, gender = "Less_than_HS", n = NA_integer_) %>%
    add_row(agestrat = NA_character_, gender = "HS_degree", n = NA_integer_) %>%
    spread(gender, n, fill = 0) %>%
    filter(!is.na(agestrat)) %>%
    select(agestrat, Male, Female) %>%
    rename(Male_actual = Male, Female_actual = Female) %>%
    assign(paste0("gender_input_", .x), ., envir = .GlobalEnv)
)

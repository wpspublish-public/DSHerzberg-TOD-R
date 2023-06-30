write_csv(index_composites_lookups, 
  here(
    "OUTPUT-FILES/OES-INPUT-TABLES/TOD-C/TOD-C-OES-index-composite-lookup-table.csv"
  ),
  na = ""
)


mutate(test = case_when(
  str_detect(.x, "-age") ~str_sub(.x, 1,-5),
  str_detect(.x, "-adult") ~str_sub(.x, 1,-7),
  TRUE ~ NA_character_
)) %>% 


age_lookups <- bind_rows(
  map(
  test_age_stems,
  ~
    suppressMessages(read_csv(here(
      str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-C/", .x, "-lookup.csv")
    ))) %>%
    pivot_longer(
      cols = -raw,
      names_to = "age_grade",
      values_to = "ss"
    ) 
))
  


temp1 <- map(
  test_age_stems,
  ~
    suppressMessages(read_csv(here(
      str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-C/", .x, "-lookup.csv")
    ))) %>%
    pivot_longer(
      cols = -raw,
      names_to = "age_grade",
      values_to = "ss"
    ) %>%
    mutate(test = str_sub(.x, 1,-5)) %>%
    select(test, age_grade, raw, ss) %>%
    arrange(test,
            match(age_grade, age_grade_order), raw)
) %>%
  bind_rows()

temp2 <- map(
  test_age_adult_stems,
  ~
    suppressMessages(read_csv(here(
      str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-C/", .x, "-lookup.csv")
    ))) %>%
    pivot_longer(
      cols = -raw,
      names_to = "age_grade",
      values_to = "ss"
    ) %>%
    mutate(test = str_sub(.x, 1,-7)) %>%
    select(test, age_grade, raw, ss) %>%
    arrange(test,
            match(age_grade, age_grade_order), raw)
) %>%
  bind_rows()

temp4 <- bind_rows(map(
  test_age_stems,
  ~
    suppressMessages(read_csv(here(
      str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-C/", .x, "-lookup.csv")
    ))) %>%
    pivot_longer(
      cols = -raw,
      names_to = "age_grade",
      values_to = "ss"
    ) %>%
    mutate(test = str_sub(.x, 1,-5)) %>%
    select(test, age_grade, raw, ss) %>%
    arrange(test,
            match(age_grade, age_grade_order), raw)
) %>%
  bind_rows(), 
map(
  test_age_adult_stems,
  ~
    suppressMessages(read_csv(here(
      str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-C/", .x, "-lookup.csv")
    ))) %>%
    pivot_longer(
      cols = -raw,
      names_to = "age_grade",
      values_to = "ss"
    ) %>%
    mutate(test = str_sub(.x, 1,-7)) %>%
    select(test, age_grade, raw, ss) %>%
    arrange(test,
            match(age_grade, age_grade_order), raw)
) %>%
  bind_rows()
)


age_lookups <- bind_rows(bind_rows(
  map(
    test_age_stems,
    ~
      suppressMessages(read_csv(here(
        str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-C/", .x, "-lookup.csv")
      ))) %>%
      pivot_longer(
        cols = -raw,
        names_to = "age_grade",
        values_to = "ss"
      ) %>%
      mutate(test = str_sub(.x, 1, -5)) %>%
      select(test, age_grade, raw, ss) %>%
      arrange(test,
              match(age_grade, age_grade_order), raw)
  )
),
bind_rows(
  map(
    test_age_adult_stems,
    ~
      suppressMessages(read_csv(here(
        str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-C/", .x, "-lookup.csv")
      ))) %>%
      pivot_longer(
        cols = -raw,
        names_to = "age_grade",
        values_to = "ss"
      ) %>%
      mutate(test = str_sub(.x, 1, -7)) %>%
      select(test, age_grade, raw, ss) %>%
      arrange(test,
              match(age_grade, age_grade_order), raw)
  )
)) %>% 


age_lookups <- map(
  test_age_stems,
  ~
    suppressMessages(read_csv(here(
      str_c("INPUT-FILES/OES-INPUT-TABLES/TOD-C/", .x, "-lookup.csv")
    ))) %>%
    pivot_longer(
      cols = -raw,
      names_to = "age_grade",
      values_to = "ss"
    ) %>%
    mutate(test = str_sub(.x, 1,-5)) %>%
    select(test, age_grade, raw, ss) %>%
    arrange(test,
            match(age_grade, age_grade_order), raw)
) %>%
  bind_rows() %>%
  

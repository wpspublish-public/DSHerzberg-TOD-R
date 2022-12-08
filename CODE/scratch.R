lookup_v3 <- map2(
  form_file_name,
  form,
  ~
    suppressMessages(read_csv(here(
      str_c("INPUT-FILES/OES-INPUT-TABLES/", .x, ".csv")
    ))) %>%
    mutate(version = .y) %>%
    pivot_longer(
      cols = ends_with("_t"),
      names_to = "norm_rater",
      values_to = "t_score"
    ) %>% relocate(c(version, norm_rater), .before = "raw") %>%
    arrange(norm_rater)
) %>% 
  bind_rows(.) %>%
  mutate(
    across(norm_rater,
           ~
             case_when(
               version == "TODC-child" & norm_rater == "parent_t" ~ "child-parent",
               version == "TODC-child" & norm_rater == "self_t" ~ "child-self",
               version == "TODC-child" & norm_rater == "teacher_t" ~ "child-teacher",
               version == "TODC-adult" & norm_rater == "self_t" ~ "adult-self",
               version == "TODE" & norm_rater == "parent_t" ~ "child-parent",
               version == "TODE" & norm_rater == "teacher_t" ~ "child-teacher",
               TRUE ~ NA_character_
             )
    )
  )


  
  
  ########### ALL BELOW WORKS
  
  todc_child <- suppressMessages(
  read_csv(
    here(
      "INPUT-FILES/OES-INPUT-TABLES/TODC-child-rating-scale-raw-to-t-score.csv"
    ))) %>% 
  mutate(version = "TOD-C") %>% 
  rename(
    child_self = self_t,
    child_parent = parent_t,
    child_teacher = teacher_t
  ) %>% 
  pivot_longer(
    cols = starts_with("child"),
    names_to = "norm_rater",
    values_to = "t_score"
  ) %>% relocate(c(version, norm_rater), .before = "raw") %>% 
  arrange(norm_rater)

todc_adult <- suppressMessages(
  read_csv(
    here(
      "INPUT-FILES/OES-INPUT-TABLES/TODC-adult-rating-scale-raw-to-t-score.csv"
    ))) %>% 
  mutate(version = "TOD-C") %>% 
  rename(
    adult_self = self_t
  ) %>% 
  pivot_longer(
    cols = starts_with("adult"),
    names_to = "norm_rater",
    values_to = "t_score"
  ) %>% relocate(c(version, norm_rater), .before = "raw") %>% 
  arrange(norm_rater)

tode <- suppressMessages(
  read_csv(
    here(
      "INPUT-FILES/OES-INPUT-TABLES/TODE-rating-scale-raw-to-t-score.csv"
    ))) %>% 
  mutate(version = "TOD-E") %>% 
  rename(
    child_parent = parent_t,
    child_teacher = teacher_t
  ) %>% 
  pivot_longer(
    cols = starts_with("child"),
    names_to = "norm_rater",
    values_to = "t_score"
  ) %>% relocate(c(version, norm_rater), .before = "raw") %>% 
  arrange(norm_rater)

lookup <- bind_rows(
  todc_child,
  todc_adult,
  tode
)

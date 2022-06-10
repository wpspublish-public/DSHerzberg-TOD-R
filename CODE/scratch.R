all_lookup_basic1 <- map(
  c("TODEparent_tot", "TODEteacher_tot"), 
  ~
    output_ntScore_perCase %>% 
    group_by(!!sym(.x)) %>% 
    summarize(!!sym(str_c(.x, "_nt")) := min(!!sym(str_c(.x, "_nt"))))
)
    
    
all_lookup_basic <- map(
  c("TODEparent_tot", "TODEteacher_tot"), 
  ~
    output_ntScore_perCase %>% 
    group_by(!!sym(.x)) %>% 
    summarize(!!sym(str_c(.x, "_nt")) := min(!!sym(str_c(.x, "_nt")))) %>% 
    complete(!!sym(.x) := all_raw_range) %>% 
    drop_na(!!sym(.x)) %>% 
    fill(!!sym(str_c(.x, "_nt")), .direction = "downup") %>% 
    rename(raw = !!sym(.x))
) %>%
  reduce(left_join,
         by = 'raw') %>% 
  rename(
    parent_t = TODEparent_tot_nt,
    teacher_t = TODEteacher_tot_nt
  ) %>% 
  mutate(
    across(
      parent_t,
      ~
        case_when(
          raw %in% c(27:108) ~ .x,
          TRUE ~ NA_integer_)
    ), 
    across(
      teacher_t,
      ~
        case_when(
          raw %in% c(26:104) ~ .x,
          TRUE ~ NA_integer_)
    ), 
  )


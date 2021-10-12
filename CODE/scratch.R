age5 <- data.frame(
  raw = c(5, 4, 3, 2, 1), 
  ss = c(120, 110, 100, 90, 80)
)
age6 <- data.frame(
  raw = c(5, 4, 3, 2, 1), 
  ss = c(117, 107, 97, 87, 77)
)
age7 <- data.frame(
  raw = c(5, 4, 3, 2, 1), 
  ss = c(115, 105, 95, 85, 75)
)

lookup_list <- lst(age5, age6, age7)


ss_range <- 90:110

lookup_combo <- map(
  ss_range,
  ~
    getNormCurve(.x, model, step = .5) %>% 
    mutate(
      across(
        raw,
        ~
          round(., 0)
      )
    ) %>% 
    select(age, raw) %>% 
    rename(!!sym(str_c("ss", .x)) := raw) 
) %>% 
  reduce(
  left_join,
  by = "age"
) %>% 
  pivot_longer(-age, names_to = "ss", values_to = "raw") 

ggplot(lookup_combo, aes(age, raw)) +
  geom_line(
    aes(
      color = ss
    )
  )


norms_list_temp <- rawTable(
  c(5.25, 5.75, 6.25, 6.75, 7.25, 7.75, 8.25), 
  model, 
  step = 1, 
  minNorm = 40, 
  maxNorm = 130, 
  minRaw = 1, 
  maxRaw = 30,
  pretty = FALSE
) %>% 
  set_names(tab_names) %>% 
  map( 
    ~
      select(.x, raw, norm) %>% 
      summarize(raw = raw,
                ss = round(norm, 0))
  )

# Write assignments by coder into tabbed, xlsx workbook. To create named tabs,
# supply writexl::write_xlsx() with a named list of dfs for each tab, tab names
# will be the names of the list elements
write_xlsx(norms_list_temp,
           here(str_c(
             "temp-raw-ss-lookup.xlsx"
           )))


age_contin <- suppressMessages(read_csv(here(
  str_c(input_file_path, "TODE_8.27.21_fornorms.csv")
)))


temp1 <- age_contin %>% filter(ID %in% c(234032, 234033, 234034, 234035, 262027))


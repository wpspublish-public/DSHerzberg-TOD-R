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


urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish/DSHerzberg-TOD-R/master/INPUT-FILES/NORMS/TODE_8.27.21_fornorms/"
input_file_name <- "input-TOD-cNORM-reprex1.csv"

input <- suppressMessages(read_csv(url(
  paste0(urlRemote_path, github_path, input_file_name)
)))

model <- cnorm(raw = input$raw, group = input$group, k = 4, terms = 4, scale = "IQ")

tab_names <- c(5.00, 5.67, 6.00, 6.33, 6.67, 7.00, 7.50, 8.00)

norms_combo_table <- rawTable(
  c(5.00, 5.67, 6.00, 6.33, 6.67, 7.00, 7.50, 8.00),
  model,
  step = 1,
  minNorm = 40,
  maxNorm = 160,
  minRaw = 1,
  maxRaw = 26,
  pretty = FALSE
) %>%
  map(~
        select(.x, raw, norm) %>%
        summarize(raw = raw,
                  ss = round(norm, 0))) %>%
  reduce(left_join,
         by = "raw") %>% 
  set_names("raw", tab_names) %>% 
  pivot_longer(-raw, names_to = "agestrat", values_to = "ss") %>% 
  group_by(raw) %>% 
  mutate(
    reversal = case_when(
      lag(ss) < ss ~ 1
    )
  )

reversal_report <- norms_combo_table %>% 
  filter(reversal == 1) %>% 
  select(raw, agestrat)



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


sink(h)
i <- 1:10
outer(i, i, "*")
sink()

capture.output(
  summary(model),
  file = here(
    str_c(output_file_path, input_file_stem, "-model-summ.txt")  )
)

max_raw <- data.frame(test = "lswe") %>%
  mutate(
    max_raw = case_when(
      test == "snwe" ~ 32,
      test == "sege" ~ 120,
      test == "lswe" ~ 38,
      test == "rlne" ~ 120,
      test == "rhme" ~ 30,
      test == "lske" ~ 33,
      test == "snwe" ~ 32,
      test == "ORF_noNeg" ~ 163
    )
  ) %>%
  pull(max_raw)

max_raw <- temp2$max_raw

# lske = 33
# orf = each grade is given a different passage to read within 1 minute and 
# it is unlikely anyone would read the whole thing with zero errors, 
# but the total max for each passage is: Kinder 114, 1st grade fall = 119, 
# 1st grade spring = 112, 2nd grade = 163


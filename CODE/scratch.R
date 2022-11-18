scale_readin <- function(x) {
  # express the directory path to the input file as a string.
  here(
    paste0("INPUT-FILES/OES-INPUT-TABLES/DP4-EXAMPLE/", x, ".xlsx")) %>%
    # assign("path", ., envir = .GlobalEnv)
  
  
  # input file is multi-tabbed .xlsx. Tabs contain lookup tables for each
  # agestrat. read input file into a df, stacking tabs on top of one another, and
  # creating a new column "agestrat" to identify the origin tab of each set of rows.
  # path %>% 
    excel_sheets() %>%
    set_names() %>%
    map_df(read_excel,
           path = .,
           .id = "agestrat") %>% 
    # recode agestrat so that it will sort properly
    mutate(agestrat = str_sub(agestrat, 4) %>% 
             str_pad(3, side = "left", "0"))
}

scale_lookup_pre <- scale_file_name %>% 
  map(scale_readin) %>% 
  setNames(form) %>% 
  bind_rows(.id = "form")


test <- map_df(
  paths,
  ~
  read_excel(
               path = path,
               .id = "agestrat"))

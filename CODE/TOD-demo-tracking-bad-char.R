# TOD: two-level demographic tracking

suppressMessages(library(here))
suppressMessages(suppressWarnings(library(data.table)))
suppressMessages(suppressWarnings(library(tidyverse)))

# Read in static file of demos per case from old survey closed out on
# 2019-05-22, compute agestrat
old_demos_input <-
  suppressMessages(suppressWarnings(read_csv(here(
    'TOD-ANNIKA/INPUT-FILES/TOD-old-demo-survey-2019-05-22.csv'
  )))) %>% filter(!is.na(.[1])) %>% select(
    10,
    11,
    12,
    14,
    15,
    16,
    17,
    18,
    19
  )
# strip all attributes and garbage from input table.
attributes(old_demos_input) <- NULL
names(old_demos_input) <- c("IDnum", "form", "zip", "ageyear", "grade", "gender", "PEL", "hispanic", "ethnic")
old_demos_input_2019_05_22 <- old_demos_input %>% enframe() %>% unnest() %>% unstack(value ~ name) %>% 
  select(IDnum, form, zip, ageyear, grade, gender, PEL, hispanic, ethnic) %>% 
  mutate_at(vars(IDnum, zip, ageyear), as.integer) %>% 
  mutate(
    agestrat = case_when(
      ageyear == 5 ~ "05",
      ageyear == 6 ~ "06",
      ageyear == 7 ~ "07",
      ageyear == 8 ~ "08",
      ageyear == 9 ~ "09",
      ageyear >= 10 & ageyear <= 24 ~ as.character(ageyear),
      ageyear >= 25 & ageyear <= 40 ~ "2540",
      ageyear >= 41 & ageyear <= 50 ~ "4150",
      ageyear >= 51 & ageyear <= 60 ~ "5160",
      ageyear >= 61 & ageyear <= 70 ~ "6170",
      ageyear >= 71 & ageyear <= 80 ~ "7180",
      ageyear >= 81 & ageyear <= 90 ~ "8190",
      TRUE ~ NA_character_
    ),
    form1 = case_when(
      form == "TOD-S + TOD-E" ~ "TOD_E",
      form == "TOD-S + TOD" ~ "TOD",
      TRUE ~ NA_character_
    ),
    PEL1 = case_when(
      PEL == "Less than HS" ~ "Less_than_HS",
      PEL == "High School" ~ "HS_degree",
      PEL == "Some College (including Associate's Degree)" ~ "Some_college",
      PEL == "Bachelors Degree (or higher)" ~ "BA_plus",
      TRUE ~ NA_character_
    ),
    ethnic1 = case_when(hispanic == "Yes" ~ "Hispanic",
                        ethnic == "White or Caucasian" & hispanic == "No" ~ "White",
                        ethnic == "Black or African American" & hispanic == "No" ~ "Black",
                        ethnic == "Asian or Asian American" & hispanic == "No" ~ "Asian",
                        (ethnic == "Other / Multiracial" | ethnic == "American Indian or Alaska Native" | 
                           ethnic == "Native Hawaiian or other Pacific Islander") & hispanic == "No" ~ "Other",
                        TRUE ~ NA_character_),
    region = case_when(
      inrange(zip, 1000, 8999) | inrange(zip, 10000, 19699) ~ "Northeast",
      inrange(zip, 900, 999) |
        inrange(zip,
                19700,
                33999) |
        inrange(zip,
                34100,
                42799) |
        inrange(zip, 70000, 79999) |
        inrange(zip, 88500, 88599) ~ "South",
      inrange(zip, 43000, 58999) |
        inrange(zip, 60000, 69999) ~ "Midwest",
      inrange(zip, 59000, 59999) |
        inrange(zip,
                80000,
                88499) |
        inrange(zip,
                88900,
                96199) |
        inrange(zip, 96700, 96899) |
        inrange(zip, 97000, 99999) ~ "West",
      TRUE ~ NA_character_
    )
  ) %>% select(IDnum, form1, ageyear, agestrat, gender, PEL1, ethnic1, region) %>% rename(form = form1, ethnic = ethnic1, PEL = PEL1)

# Read in current demos per case from current survey, compute agestrat
demos_input_bad_char <-
  suppressMessages(suppressWarnings(read_csv(here(
    'TOD-ANNIKA/INPUT-FILES/TOD-demos-current-input.csv'
  )))) %>% 
  filter(!is.na(.[1])) %>%
  select(
    10,
    12,
    13,
    15,
    23,
    16,
    22,
    20,
    21
  )
# give PEL column a manageable name
colnames(demos_input_bad_char)[7] <- c("PEL")
# recode HS_grad PEL to strip out garbage character that comes in from Survey Monkey input.
demos_input_interim <- demos_input_bad_char %>% 
  mutate_at(
    vars(PEL), 
    ~ case_when(
      .x == "Highˇschool graduate/GED" ~ "HS_grad", 
      TRUE ~ .x)
  )
# write .csv with garbage character removed
write_csv(demos_input_interim, 
          here(
            'TOD-ANNIKA/INPUT-FILES/demos_input_interim.csv'
          )
)
# read this interim file back in as demos_input (coercing all vars to char to
# avoid errors in unnest())
demos_input <- suppressMessages(suppressWarnings(read_csv(
  'TOD-ANNIKA/INPUT-FILES/demos_input_interim.csv',
  skip = 14,
  local = locale(encoding = "latin1")
  ))) %>% mutate_all(as.character)
# strip all attributes and garbage from input table.
attributes(demos_input) <- NULL
names(demos_input) <- c("IDnum", "form", "zip", "ageyear", "grade", "gender", "PEL", "hispanic", "ethnic")
current_demos_input <- demos_input %>% enframe() %>% unnest() %>% unstack(value ~ name) %>% 
  select(IDnum, form, zip, ageyear, grade, gender, PEL, hispanic, ethnic) %>% 
  mutate_at(vars(IDnum, zip, ageyear), as.integer) %>% 
  mutate(
    agestrat = case_when(
      ageyear == 5 ~ "05",
      ageyear == 6 ~ "06",
      ageyear == 7 ~ "07",
      ageyear == 8 ~ "08",
      ageyear == 9 ~ "09",
      ageyear >= 10 & ageyear <= 24 ~ as.character(ageyear),
      ageyear >= 25 & ageyear <= 40 ~ "2540",
      ageyear >= 41 & ageyear <= 50 ~ "4150",
      ageyear >= 51 & ageyear <= 60 ~ "5160",
      ageyear >= 61 & ageyear <= 70 ~ "6170",
      ageyear >= 71 & ageyear <= 80 ~ "7180",
      ageyear >= 81 & ageyear <= 90 ~ "8190",
      TRUE ~ NA_character_
    ),
    form1 = case_when(
      form == "TOD-S + TOD-E" ~ "TOD_E",
      form == "TOD-S + TOD" ~ "TOD",
      TRUE ~ NA_character_
    ),
    PEL1 = case_when(
      PEL == "Did not complete high school" ~ "Less_than_HS",
      PEL == "HS_grad" ~ "HS_degree",
      # is.na(PEL) ~ "HS_degree",
      PEL == "Some College or associate's degree" ~ "Some_college",
      PEL == "Bachelors degree or higher" ~ "BA_plus",
      TRUE ~ NA_character_
    ),
    ethnic1 = case_when(hispanic == "Yes" ~ "Hispanic",
                        ethnic == "White" & hispanic == "No" ~ "White",
                        ethnic == "Black/African American" & hispanic == "No" ~ "Black",
                        ethnic == "Asian" & hispanic == "No" ~ "Asian",
                        (ethnic == "Other / Multiracial" | ethnic == "American Indian/Alaska Native" | 
                           ethnic == "Native Hawaiian/Pacific Islander") & hispanic == "No" ~ "Other",
                        TRUE ~ NA_character_),
    region = case_when(
      inrange(zip, 1000, 8999) | inrange(zip, 10000, 19699) ~ "Northeast",
      inrange(zip, 900, 999) |
        inrange(zip,
                19700,
                33999) |
        inrange(zip,
                34100,
                42799) |
        inrange(zip, 70000, 79999) |
        inrange(zip, 88500, 88599) ~ "South",
      inrange(zip, 43000, 58999) |
        inrange(zip, 60000, 69999) ~ "Midwest",
      inrange(zip, 59000, 59999) |
        inrange(zip,
                80000,
                88499) |
        inrange(zip,
                88900,
                96199) |
        inrange(zip, 96700, 96899) |
        inrange(zip, 97000, 99999) ~ "West",
      TRUE ~ NA_character_
    )
  ) %>% select(IDnum, form1, ageyear, agestrat, gender, PEL1, ethnic1, region) %>% 
  rename(form = form1, ethnic = ethnic1, PEL = PEL1) %>% 
  filter(ageyear >= 5)

# Join old survey static table with new survey dynamic table, sort by ageyear, group by agestrat
All_demos <- bind_rows(current_demos_input, old_demos_input_2019_05_22) %>%
  arrange(agestrat, ageyear, IDnum) %>% group_by(agestrat)

# Separate demos by form.
demos_TOD <- All_demos %>% filter(form == "TOD")
demos_TOD_E <- All_demos %>% filter(form == "TOD_E")

# Initialize table of static columns.
static_columns_TOD <-
  bind_cols(
    tibble(
      agestrat = c(
        "05",
        "06",
        "07",
        "08",
        "09",
        "10",
        "11",
        "12",
        "13",
        "14",
        "15",
        "16",
        "17",
        "18",
        "19",
        "20",
        "21",
        "22",
        "23",
        "24",
        "2540",
        "4150",
        "5160",
        "6170",
        "7180",
        "8190"
      )
    ),
    fread(here("DATA/STATIC_COLUMNS/TOD_Target_n_by_age.csv"), select = c("target_n")),
    (
      fread(
        here("DATA/STATIC_COLUMNS/Age_x_gender_TOD_final.csv"),
        select = c("Male", "Female")
      ) %>%
        rename(Male_census_pct = Male, Female_census_pct = Female)
    ),
    (
      fread(
        here("DATA/STATIC_COLUMNS/Age_x_PEL_TOD_final.csv"),
        select = c("Less_than_HS", "HS_Degree", "Some_College", "BA_or_Higher")
      ) %>%
        rename(
          Less_than_HS_census_pct = Less_than_HS,
          HS_degree_census_pct = HS_Degree,
          Some_college_census_pct = Some_College,
          BA_plus_census_pct = BA_or_Higher
        )
    ),
    (
      fread(
        here("DATA/STATIC_COLUMNS/Age_x_race_TOD_final.csv"),
        select = c("White", "Black", "Asian", "Other", "Hispanic")
      ) %>%
        rename(
          White_census_pct = White,
          Black_census_pct = Black,
          Asian_census_pct = Asian,
          Other_census_pct = Other,
          Hispanic_census_pct = Hispanic
        ) %>%
        select(
          Hispanic_census_pct,
          Asian_census_pct,
          Black_census_pct,
          White_census_pct,
          Other_census_pct
        )
    ),
    (
      fread(
        here("DATA/STATIC_COLUMNS/Age_x_region_TOD_final.csv"),
        select = c("Northeast", "Midwest", "South", "West")
      ) %>%
        rename(
          Northeast_census_pct = Northeast,
          Midwest_census_pct = Midwest,
          South_census_pct = South,
          West_census_pct = West
        ) 
    )
  )

# Create TOD_E static columns by filtering, recoding TOD columns.
static_columns_TOD_E <- static_columns_TOD %>% filter(agestrat %in% c("05", "06", "07", "08", "09")) %>% 
  mutate_at(
    vars(target_n), list(~case_when(
      agestrat == "05" ~ 225,
      agestrat == "06" ~ 225,
      agestrat == "07" ~ 225,
      agestrat == "08" ~ 225,
      agestrat == "09" ~ 10,
      TRUE ~ NA_real_
    )))


# Individual demo input tables
frm <- c("TOD", "TOD_E")
map(
  frm,
  ~
    eval(as.name(paste0("demos_", .x))) %>% ungroup() %>% select(agestrat, gender) %>% filter(gender != "Other") %>%
    count(agestrat, gender) %>%
    add_row(agestrat = NA_character_, gender = "Male", n = NA_integer_) %>% 
    add_row(agestrat = NA_character_, gender = "Female", n = NA_integer_) %>% 
    spread(gender, n, fill = 0) %>%
    filter(!is.na(agestrat)) %>% 
    select(agestrat, Male, Female) %>%
    rename(Male_actual = Male, Female_actual = Female) %>%
    assign(paste0("gender_input_", .x), ., envir = .GlobalEnv)
)

map(
  frm,
  ~
    eval(as.name(paste0("demos_", .x))) %>% ungroup() %>% select(agestrat, PEL)  %>%
    count(agestrat, PEL) %>%
    add_row(agestrat = NA_character_, PEL = "Less_than_HS", n = NA_integer_) %>% 
    add_row(agestrat = NA_character_, PEL = "HS_degree", n = NA_integer_) %>% 
    add_row(agestrat = NA_character_, PEL = "Some_college", n = NA_integer_) %>% 
    add_row(agestrat = NA_character_, PEL = "BA_plus", n = NA_integer_) %>% 
    spread(PEL, n, fill = 0) %>%
    filter(!is.na(agestrat)) %>% 
    select(agestrat, Less_than_HS, HS_degree, Some_college, BA_plus) %>%
    rename(
      Less_than_HS_actual = Less_than_HS,
      HS_degree_actual = HS_degree,
      Some_college_actual = Some_college,
      BA_plus_actual = BA_plus) %>% 
    assign(paste0("PEL_input_", .x), ., envir = .GlobalEnv)
)

map(
  frm,
  ~
    eval(as.name(paste0("demos_", .x))) %>% ungroup() %>% select(agestrat, ethnic)  %>%
    count(agestrat, ethnic) %>%
    add_row(agestrat = NA_character_, ethnic = "Hispanic", n = NA_integer_) %>% 
    add_row(agestrat = NA_character_, ethnic = "Asian", n = NA_integer_) %>% 
    add_row(agestrat = NA_character_, ethnic = "Black", n = NA_integer_) %>% 
    add_row(agestrat = NA_character_, ethnic = "White", n = NA_integer_) %>% 
    add_row(agestrat = NA_character_, ethnic = "Other", n = NA_integer_) %>% 
    spread(ethnic, n, fill = 0) %>%
    filter(!is.na(agestrat)) %>% 
    select(agestrat, Hispanic, Asian, Black, White, Other) %>%
    rename(
      Hispanic_actual = Hispanic,
      Asian_actual = Asian,
      Black_actual = Black,
      White_actual = White,
      Other_actual = Other) %>% 
    assign(paste0("ethnic_input_", .x), ., envir = .GlobalEnv)
)

map(
  frm,
  ~
    eval(as.name(paste0("demos_", .x))) %>% ungroup() %>% select(agestrat, region)  %>%
    count(agestrat, region) %>%
    add_row(agestrat = NA_character_, region = "Northeast", n = NA_integer_) %>% 
    add_row(agestrat = NA_character_, region = "South", n = NA_integer_) %>% 
    add_row(agestrat = NA_character_, region = "Midwest", n = NA_integer_) %>% 
    add_row(agestrat = NA_character_, region = "West", n = NA_integer_) %>% 
    spread(region, n, fill = 0) %>%
    filter(!is.na(agestrat)) %>% 
    select(agestrat, Northeast, South, Midwest, West) %>%
    rename(
      Northeast_actual = Northeast,
      South_actual = South,
      Midwest_actual = Midwest,
      West_actual = West) %>% 
    assign(paste0("region_input_", .x), ., envir = .GlobalEnv)
)

# Initialize char vec of final output columns in final order.
final_output_cols <- c( "agestrat", "target_n", "Male_census_pct", "Male_actual", 
                        "Male_needed", "Female_census_pct", "Female_actual", "Female_needed", 
                        "Less_than_HS_census_pct", "Less_than_HS_actual", "Less_than_HS_needed", 
                        "HS_degree_census_pct", "HS_degree_actual", "HS_degree_needed", "Some_college_census_pct", 
                        "Some_college_actual", "Some_college_needed", "BA_plus_census_pct", "BA_plus_actual", 
                        "BA_plus_needed", "Hispanic_census_pct", "Hispanic_actual", "Hispanic_needed", 
                        "Asian_census_pct", "Asian_actual", "Asian_needed", "Black_census_pct", "Black_actual", 
                        "Black_needed", "White_census_pct", "White_actual", "White_needed", 
                        "Other_census_pct", "Other_actual", "Other_needed", 
                        "Northeast_census_pct", "Northeast_actual", "Northeast_needed", "South_census_pct", 
                        "South_actual", "South_needed", "Midwest_census_pct", "Midwest_actual", 
                        "Midwest_needed", "West_census_pct", "West_actual", "West_needed")

# Final output table joins static columns with actual counts from each demo cat,
# then calculates still needed count for each cat. 
map(
  frm,
  ~
    list(
      eval(as.name(paste0("static_columns_", .x))),
      eval(as.name(paste0("gender_input_", .x))),
      eval(as.name(paste0("PEL_input_", .x))),
      eval(as.name(paste0("ethnic_input_", .x))),
      eval(as.name(paste0("region_input_", .x)))
    ) %>%
    reduce(left_join, by = "agestrat") %>%
    mutate_if(is.numeric , replace_na, replace = 0) %>%
    mutate(
      Male_needed = (target_n * Male_census_pct) - Male_actual,
      Female_needed = (target_n * Female_census_pct) - Female_actual,
      Less_than_HS_needed = (target_n * Less_than_HS_census_pct) - Less_than_HS_actual,
      HS_degree_needed = (target_n * HS_degree_census_pct) - HS_degree_actual,
      Some_college_needed = (target_n * Some_college_census_pct) - Some_college_actual,
      BA_plus_needed = (target_n * BA_plus_census_pct) - BA_plus_actual,
      Hispanic_needed = (target_n * Hispanic_census_pct) - Hispanic_actual,
      Asian_needed = (target_n * Asian_census_pct) - Asian_actual,
      Black_needed = (target_n * Black_census_pct) - Black_actual,
      White_needed = (target_n * White_census_pct) - White_actual,
      Other_needed = (target_n * Other_census_pct) - Other_actual,
      Northeast_needed = (target_n * Northeast_census_pct) - Northeast_actual,
      South_needed = (target_n * South_census_pct) - South_actual,
      Midwest_needed = (target_n * Midwest_census_pct) - Midwest_actual,
      West_needed = (target_n * West_census_pct) - West_actual
    ) %>%
    select(final_output_cols) %>%
    mutate_at(vars(contains("_needed")),
              ~ case_when(.x < 0 ~ 0,
                          TRUE ~ as.double(.x))) %>%
    mutate(total_usable_cases = target_n - pmax((Male_needed + Female_needed),
                                                (
                                                  Less_than_HS_needed + HS_degree_needed + Some_college_needed + BA_plus_needed
                                                ),
                                                (
                                                  Hispanic_needed + Asian_needed + Black_needed + White_needed + Other_needed
                                                ),
                                                (Northeast_needed + South_needed + Midwest_needed + West_needed)
    )) %>%
    mutate_at(vars(contains("_needed"), total_usable_cases), list(~round(., 0))) %>%
    assign(paste0("demo_tracking_output_", .x), ., envir = .GlobalEnv))

# Write output appended with date
map(
  frm,
  ~
    write_csv(eval(as.name(paste0("demo_tracking_output_", .x))), here(
      paste0(
        "TOD-ANNIKA/OUTPUT-FILES/",
        .x,
        "_demo_tracking_output_",
        format(Sys.Date(), "%Y-%m-%d"),
        ".csv"
      )
    ))
)
rm(list = ls())


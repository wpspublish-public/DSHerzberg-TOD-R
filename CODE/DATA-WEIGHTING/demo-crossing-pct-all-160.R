suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(here))

input_file_path <- "INPUT-FILES/NORMS/TODE_8.27.21_fornorms/"
fileName_path   <- "census_pct.csv"

census_pct_by_cat <- suppressMessages(read_csv(
  str_c(input_file_path, fileName_path)
)) 

gender <- c("male", "female")
educ <- c("no_HS", "HS_grad", "some_college", "BA_plus")
ethnic <- c("hispanic", "asian", "black", "white", "other")
region <- c("northeast", "south", "midwest", "west")
gender_pct <- census_pct_by_cat %>% filter(var == "gender") %>% pull(pct_census)
educ_pct <- census_pct_by_cat %>% filter(var == "educ") %>% pull(pct_census)
ethnic_pct <- census_pct_by_cat %>% filter(var == "ethnic") %>% pull(pct_census)
region_pct <- census_pct_by_cat %>% filter(var == "region") %>% pull(pct_census)

names_all <- expand_grid(
  gender = gender,
  educ = educ,
  ethnic = ethnic,
  region = region
)

pct_all <- expand_grid(
  gender_pct = gender_pct,
  educ_pct = educ_pct,
  ethnic_pct = ethnic_pct,
  region_pct = region_pct
) %>%
  # details on this instance of mutate(). The input "_pct" variables are scaled
  # as conventional percentages (e.g., 62.1%). First, we transform these values
  # into proportions, by dividing by 100. This allows us to find the cell
  # proportion for each cell in the total grid of cells produced by crossing the
  # categories of the four demographic vars (e.g., male x hs_grad x black x
  # west). All of these multiplied out proportions sum to ~ 1. However, to use
  # the "weights" argument in cNORM, the values of the vector used as the
  # weights col need to be > 1 (per wolfgang lenhard). Therefore, we multiply by
  # 100 to get these cell proportions back into the metric of a conventional
  # percentages. Now these percentages sum to ~100 across all demographic
  # crossings.
mutate(across(everything(),
              ~
                . / 100),
       cell_pct = (gender_pct * educ_pct * ethnic_pct * region_pct) * 100)

sum_cell_pct <- sum(pct_all$cell_pct)

demo_crossing_pct_all_160 <- bind_cols(
  names_all,
  pct_all
) %>% 
  select(gender:region, cell_pct) %>% 
  write_csv(
    here(
      str_c(
        input_file_path, "demo_crossing_pct_all_160.csv"
      )
    )
  )



library(psych)
suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))


outlier_IDs <- c(326013, 234011, 235029, 262026, 
                 305022, 326018, 231001, 231007, 338006)
scores <- c("sege_sum", "rlne_sum", "rhme_sum", "snwe_sum",
            "lswe_sum", "lske_sum", "ORF_noNeg")


# input_file_path <- "OUTPUT-FILES/NORMS/"
combined_input_file_name <- "TODE_8.27.21_fornorms.csv"
input_file_path <- "INPUT-FILES/NORMS/TODE_8.27.21_fornorms/"


input_orig <- suppressMessages(read_csv(
  str_c(input_file_path, combined_input_file_name)
)) %>% rename(
  gender = Gender,
  educ = SES,
  ethnic = Ethnicity,
) %>% 
  mutate(
    agestrat2 = case_when(
      agestrat %in% c(1, 2) ~ "5:0-5:11",
      agestrat %in% c(3, 4, 5) ~ "6:0-6:11",
      agestrat %in% c(6, 7, 8) ~ "7:0-9:3"
    ),
    across(agestrat, ~ case_when(
      . == 1 ~ "5:0-5:7",
      . == 2 ~ "5:8-5:11",
      . == 3 ~ "6:0-6:3",
      . == 4 ~ "6:4-6:7",
      . == 5 ~ "6:8-6:11",
      . == 6 ~ "7:0-7:5",
      . == 7 ~ "7:6-7:11",
      . == 8 ~ "8:0-9:3")),
    across(gradestrat, ~ case_when(
      . == 1 ~ "K_Fall",
      . == 2 ~ "K_Spring",
      . == 3 ~ "1_Fall",
      . == 4 ~ "1_Spring",
      . == 5 ~ "2_Fall",
      . == 6 ~ "2_Spring")),
    across(gender, ~ case_when(
      . == 1 ~ "male",
      . == 2 ~ "female")),
    across(educ, ~ case_when(
      . == 1 ~ "no_HS",
      . == 2 ~ "HS_grad",
      . == 3 ~ "some_college",
      . == 4 ~ "BA_plus")),
    across(ethnic, ~ case_when(
      . == 1 ~ "asian",
      . == 2 ~ "black",
      . == 3 ~ "white",
      . %in% c(4, 5, 6) ~ "other",
      . == 7 ~ "hispanic")),
    across(region, ~ case_when(
      . == 1 ~ "northeast",
      . == 2 ~ "midwest",
      . == 3 ~ "south",
      . == 4 ~ "west"))
  ) %>% 
  relocate(agestrat2, .after = "agestrat") %>% 
  # Get rid of zero values for "sum" scores by adding 1 to all
  mutate(
    across(
      c(sege_sum:lske_sum),
      ~
        .x + 1
    )
  ) %>% 
  # remove outliers
  filter(!(ID %in% outlier_IDs)) %>% 
  mutate(ORF_noNeg = ORF + 100)

score_means_sds_agestrat <- input_orig %>% 
  select(all_of(scores)) %>%
  describeBy(group = input_orig$agestrat, mat = TRUE, digits = 2) %>% 
  rownames_to_column(var = "scale") %>% 
  arrange(group1) %>% 
  select(group1, scale, n, mean, sd)

score_means_sds_agestrat_sege <- score_means_sds_agestrat %>% 
  filter(str_detect(scale, "sege"))

score_means_sds_agestrat2 <- input_orig %>% 
  select(all_of(scores)) %>%
  describeBy(group = input_orig$agestrat2, mat = TRUE, digits = 2) %>% 
  rownames_to_column(var = "scale") %>% 
  arrange(group1) %>% 
  select(group1, scale, n, mean, sd)

score_means_sds_agestrat2_sege <- score_means_sds_agestrat2 %>% 
  filter(str_detect(scale, "sege"))



freq1 <- as.data.frame(table(input_orig$demo_wt)) %>%
  mutate(demo_wt = as.numeric(as.character(Var1))) %>% 
  select(demo_wt, Freq)

freq2 <- as.data.frame(table(input$normValue))

mean(freq1$demo_wt)
sd(freq1$demo_wt)
mean_plus_2sd <- round(mean(freq1$demo_wt) + (2 * sd(freq1$demo_wt)), 2)
mean_minus_2sd <- round(mean(freq1$demo_wt) - (2 * sd(freq1$demo_wt)), 2)

plot(freq1$demo_wt, freq1$Freq, type = "h")

outliers <- input_orig %>% 
  filter(!between(demo_wt, .5, 1.5))

score_means_sds_all <- input_orig %>% 
  select(all_of(scores)) %>%
  describe(fast = T) %>%
  rownames_to_column(var = "scale")



plotNormCurves(
  model,
  normList = c(80, 90, 100, 110, 120)
)
plotDerivative(model)


ggplot(data = input, aes(raw)) +
  geom_histogram(col = "red",
  fill = "blue",
  alpha = .2)

outlier <- input_orig %>% filter(ID == 234011)

# outlier is id# 234011

write_csv(input, here("input-TOD-cNORM-reprex1.csv"))

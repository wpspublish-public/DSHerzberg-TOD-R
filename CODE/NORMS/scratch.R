library(psych)

# input_file_path <- "OUTPUT-FILES/NORMS/"
combined_input_file_name <- "TODE_8.27.21_fornorms-weighted-sum-scores.csv"
input_file_path <- "INPUT-FILES/NORMS/TODE_8.27.21_fornorms/"


input_orig <- suppressMessages(read_csv(here(str_c(
 input_file_path, combined_input_file_name
))))

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

score_means_sds_agestrat <- input_orig %>% 
  select(all_of(scores)) %>%
  describeBy(group = input_orig$agestrat, mat = TRUE, digits = 2) %>% 
  rownames_to_column(var = "scale") %>% 
  arrange(group1) %>% 
  select(group1, scale, n, mean, sd)


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

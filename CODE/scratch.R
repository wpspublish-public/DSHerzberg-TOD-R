a <- round(rnorm(500, mean = 50, sd = 10), 0)
b <- round(rnorm(500, mean = 60, sd = 11), 0)
data <- list(a, b)

norms <- map(
  data,
  ~
    bestNormalize(.x) %>%
    pluck("chosen_transform") %>% 
    class() %>% 
    pluck(1)
)

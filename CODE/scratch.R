set.seed(12345)
a <- rnorm(500, mean = 50, sd = 10)
set.seed(12345)
b <- rnorm(500, mean = 50, sd = 10)
identical(a, b)

library(tidyverse)

means <- c(40, 50, 60)
sds <- c(9, 10, 11)

set.seed(12345)
data1 <- map2(
  means,
  sds,
  ~
    rnorm(500, mean = .x, sd = .y)
    )


As a followup, here is the most concise way to solve my original problem:

```
library(tidyverse)

means <- c(40, 50, 60)
sds <- c(9, 10, 11)

data <- map2(
  means,
  sds,
  ~ {
    set.seed(12345)
    rnorm(500, mean = .x, sd = .y)
  }
)
```

This code returns identical results each time it is run.


means <- c(40, 50, 60)
sds <- c(9, 10, 11)

myfun <- function(means, sds){
  set.seed(12345) # set it before each call
  ret <- rnorm(500, mean = means, sd = sds)
  return(ret)
}

data <- purrr::map2(means,
                    sds,
                    ~ myfun(.x, .y))


norm_fun <- function(x){
  set.seed(12345)
  ret <- bestNormalize(x) %>%
    pluck("chosen_transform") %>% 
    class() %>% 
    pluck(1)
  return(ret)
}

norm_model_per_score_chosen_transform3 <- map(
  norm_input_per_score,
  ~
    norm_fun(.x)
) %>% 
  set_names(score_names)
    


set.seed(12345)
norm_model_per_score_chosen_transform <- map(
  norm_input_per_score,
  ~ {
    set.seed(12345)
    bestNormalize(.x) %>%
    pluck("chosen_transform") %>% 
    class() %>% 
    pluck(1)
    }
) %>% 
  set_names(score_names)




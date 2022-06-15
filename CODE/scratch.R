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



temp1 <- flatten(list(30:60, rep(list(20:40), 3)))
temp2 <- flatten(list(30:60, 20:40, 20:40, 20:40))
identical(temp1, temp2)


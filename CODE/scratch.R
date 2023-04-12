temp1 <- index_composites_lookups %>% 
  filter(score == "epa")

range(temp1$raw)

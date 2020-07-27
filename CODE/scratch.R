ColNums_NotAllMissing <- function(df){ # helper function
  as.vector(which(colSums(is.na(df)) != nrow(df)))
}

test <- miss_recode %>%
  select(ColNums_NotAllMissing(.))

test1 <- miss_recode %>%
  select(across(
    c(ID, recode_cols1, recode_cols2, recode_cols3),
    ~ as.vector(which(colSums(is.na(.)) != nrow(.)))
  ))
    
test2 <- miss_recode %>%
  select(as.vector(which(colSums(is.na(.)) != nrow(.)))
  )

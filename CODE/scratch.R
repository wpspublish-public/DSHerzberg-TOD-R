all_0orNA_rows <- input_orig %>% filter(if_all(names(input_orig)[-1], ~(is.na(.) | . == 0)))

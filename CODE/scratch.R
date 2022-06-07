temp <- c(str_c("i", sprintf("%02d", 1:44)))

temp <- input_uw %>% 
  filter(between(age, 11.5, 12))

range(temp$age)

table(temp$iws_tot)

temp2 <- input_uw %>% 
  filter(between(age, 10.5, 11))

range(temp2$age)

table(temp2$iws_tot)


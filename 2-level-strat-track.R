# TOD: two-level demographic tracking

suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
library(reshape2) # RESHAPE DATA FROM WIDE TO TALL
library(broom) # TIDY MODEL OUTPUTS
library(moderndive) # USER-FRIENDLY LINEAR MODELING, REGRESSION AND CORRELATION TOOLS.
library(magrittr) # PIPE OPERATORS
# note use of `suppressWarnings` to silence chatter during interactive session
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(ggpmisc)) # EXTENSIONS TO ggplot2: ADD EQUATIONS AND FIT STATISTICS TO FITTED LINE PLOTS
library(ggrepel) # MORE ggplot2 EXTENSIONS


TOD_demos <- suppressMessages(read_csv(here('DATA/TOD-demos_input1.csv'))) %>% arrange(Ageyear) %>% 
  mutate(Agestrat = case_when(
    Ageyear <=24 ~ as.character(Ageyear),
    Ageyear >=25 & Ageyear <=40 ~ "25-40",
    Ageyear >=41 & Ageyear <=50 ~ "41-50",
    Ageyear >=51 & Ageyear <=60 ~ "51-60",
    Ageyear >=61 & Ageyear <=70 ~ "61-70",
    Ageyear >=71 & Ageyear <=80 ~ "71-80",
    Ageyear >=81 & Ageyear <=90 ~ "81-90",
    TRUE ~ NA_character_
  )) %>% group_by(Agestrat)
  
  



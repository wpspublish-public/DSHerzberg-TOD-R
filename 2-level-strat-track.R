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


TOD_demos <- suppressMessages(read_csv(here('DATA/TOD-demos_input1.csv'))) %>% arrange(ageyear) %>% 
  mutate(agestrat = case_when(
    ageyear <=24 ~ as.character(ageyear),
    ageyear >=25 & ageyear <=40 ~ "25-40",
    ageyear >=41 & ageyear <=50 ~ "41-50",
    ageyear >=51 & ageyear <=60 ~ "51-60",
    ageyear >=61 & ageyear <=70 ~ "61-70",
    ageyear >=71 & ageyear <=80 ~ "71-80",
    ageyear >=81 & ageyear <=90 ~ "81-90",
    TRUE ~ NA_character_
  )) %>% mutate_at(vars(agestrat), as.factor) %>% group_by(agestrat)


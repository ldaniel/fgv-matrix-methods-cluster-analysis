# loading required libraries --------------------------------------------------
library(dplyr)

# load data

df <- readRDS('data/raw/skillset.rds')

combinations <- t(combn(unique(df$skill), 2, simplify = TRUE)) %>% as_tibble
combinations <- distinct(combinations, V1, V2)




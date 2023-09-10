library(tidyverse)
library(readr)
library(readxl)
library(magrittr)

# Load dataset
df <- read_excel("data/1_Sep13/Tybur_data.xlsx")
nm <- read_excel("data/1_Sep13/nation_match.xlsx")

numeric <- df %>% group_by(nation) %>% summarise(n = n())

ij <- inner_join(nm, numeric, by = "n")

df %<>% inner_join(ij %>% dplyr::select(-n), by = "nation")

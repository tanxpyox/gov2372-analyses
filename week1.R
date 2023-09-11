library(tidyverse)
library(readr)
library(readxl)
library(magrittr)

# Load dataset
df <- read_excel("data/1_Sep13/Tybur_data.xlsx")
nm <- read_excel("data/1_Sep13/nation_match.xlsx")

# Join country names
numeric <- df %>% group_by(nation) %>% summarise(n = n())
ij <- inner_join(nm, numeric, by = "n")
df %<>% inner_join(ij %>% dplyr::select(-n), by = "nation")

# New DVs
df %<>% mutate(
  sd_total = sd01 + sd02 + sd03 + sd04,
  trad_total = trad1 + trad2 + trad3 + trad4 + trad5 + trad6,
  DS_total = DS1 + DS2 + DS3 + DS4 + DS5 + DS6 + DS7
)

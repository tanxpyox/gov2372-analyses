library(tidyverse)
library(readr)
library(readxl)
library(magrittr)

# Load dataset
df <- read_excel("data/1_Sep13/Tybur_data.xlsx")
nm <- read_excel("data/1_Sep13/nation_match.xlsx")

theme_set(theme_bw())

# Join country names
numeric <- df %>% group_by(nation) %>% summarise(n = n())
ij <- inner_join(nm, numeric, by = "n")
df %<>% inner_join(ij %>% dplyr::select(-n), by = "nation")

# Aggregate DVs
df %<>% mutate(
  sd_total = sd01r + sd02 + sd03r + sd04,
  trad_total = trad1 + trad2 + trad3r + trad4 + trad5r + trad6r,
  DS_total = DS1 + DS2 + DS3 + DS4 + DS5 + DS6 + DS7
)

# Check if aggregation was correct
cor(df$DS_total, df$DS, use="complete.obs")
cor(df$trad_total, df$Traditionalim, use="complete.obs")
cor(df$sd_total, df$SDO, use="complete.obs")



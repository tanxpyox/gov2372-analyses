library(tidyverse)
library(readr)
library(readxl)
library(magrittr)
library(interplot)

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

df %<>% mutate(
  pdeaths_per1k = WHO2012infectiousparasiticdiseasedeathsper1Kpopn,
  parasite = Parasite_PCA,
  trad = Traditionalim,
  sd = SDO,
  ds = DS
)

country <- df %>% group_by(nation) %>% summarise(
  nation_name = nation_name[1],
  nation_abbrev = nation_abbrev[1],
  ds_average = mean(DS),
  trad_average = mean(Traditionalim),
  sdo_average = mean(SDO),
  pdeaths_per1k = pdeaths_per1k[1],
  parasite = Parasite_PCA[1],
  zoonotic = ZoonoticParasitePrevalence[1],
  nonzoonotic = NonzoonoticParasitePrevalence[1]
)

r1 <- lm(ds ~ pdeaths_per1k * parasite, data = df) %>% summary
# r2 <- lm(trad ~ pdeaths_per1k * parasite, data = df) %>% summary
# r3 <- lm(trad ~ pdeaths_per1k * ds * parasite, data = df) %>% summary

# plot_model(r, type = "pred", terms = c("pdeaths_per1k", "parasite"))
interplot(r1, var1 = "pdeaths_per1k", var2 = "pdeaths_per1k")

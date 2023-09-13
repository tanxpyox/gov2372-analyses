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
numeric$weights = mean(numeric$n)/numeric$n
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
  ds = DS,
  ismale = sex == 1
)

country <- df %>% group_by(nation) %>% summarise(
  n = n(),
  nation_name = nation_name[1],
  nation_abbrev = nation_abbrev[1],
  ds_average = mean(DS),
  trad_average = mean(Traditionalim),
  sdo_average = mean(SDO),
  pdeaths_per1k = pdeaths_per1k[1],
  parasite = Parasite_PCA[1],
  zoonotic = ZoonoticParasitePrevalence[1],
  nonzoonotic = NonzoonoticParasitePrevalence[1],
  ismale = mean(ismale),
  age = mean(age),
  gdp_pc_ppp = gdp_pc_ppp[1]
)


country %<>% arrange(parasite)

top5_parasite <- country$nation[1:5]
top10_parasite <- country$nation[1:10]
# r1i <- lm(ds ~ pdeaths_per1k + parasite + ismale + age + log(gdp_pc_ppp), data = df)
# r2i <- lm(ds ~ pdeaths_per1k * parasite + ismale + age + log(gdp_pc_ppp), data = df)
# r3i <- lm(trad ~ pdeaths_per1k + parasite + ismale + age, data = df)
# r5i <- lm(trad ~ pdeaths_per1k + parasite + ismale + age + log(gdp_pc_ppp) + factor(nation), data = df)
# r4i <- lm(trad ~ pdeaths_per1k * parasite + ismale + age, data = df)
# r3 <- lm(trad ~ pdeaths_per1k * ds * parasite, data = df)
# 
# library(stargazer)
# stargazer(r3i, r4i, out = "out/week1/reg_indv.html", type = 'html')
# 
# r1n <- lm(ds_average ~ pdeaths_per1k + parasite + ismale + age + log(gdp_pc_ppp), data = country)
# r2n <- lm(ds_average ~ pdeaths_per1k * parasite + ismale + age + log(gdp_pc_ppp), data = country)
# r3n <- lm(trad_average ~ pdeaths_per1k + parasite + ismale + age + log(gdp_pc_ppp), data = country)
# r4n <- lm(trad_average ~ pdeaths_per1k * parasite + ismale + age + log(gdp_pc_ppp), data = country)
# 
# stargazer(r3n, r4n, out = "out/week1/reg_nation.html", type = 'html')
# 
# interplot(r2, var1 = "parasite", var2 = "pdeaths_per1k") +
#   ggtitle("Marginal Effect of deaths by parasitic disease per 1000 population")+
#   labs(x = "dy/dx(deaths by parasite per 1k pop)", x = "Parasite")+
#   geom_hline(aes(yintercept = 0), linetype = "dashed") +
#   theme(
#     panel.grid = element_blank()
#   )

# Sampling Bias
# country %<>% arrange(-n)
# p1 <- ggplot(country, aes(gdp_pc_ppp, n)) +
#   geom_point() +
#   labs(x = "GDP p.c. ppp",
#        y = "Sample size per nation") +
#   geom_smooth(method = "lm") + theme(axis.text.x = element_blank(),
#                                      axis.ticks.x = element_blank())
# 
# p2 <- ggplot(country, aes(age, n)) +
#   geom_point() +
#   labs(x = "Age",
#        y = "Sample size per nation") +
#   geom_smooth(method = "lm") + theme(axis.text.x = element_blank(),
#                                    axis.ticks.x = element_blank(),
#                                    axis.title.y = element_blank(),
#                                    axis.ticks.y = element_blank(),
#                                    axis.text.y = element_blank())
# 
# p3 <- ggplot(country, aes(ismale, n)) +
#   geom_point() +
#   labs(x = "Prop. of Male",
#        y = "Sample size per nation") +
#   geom_smooth(method = "lm") + theme(axis.text.x = element_blank(),
#                                   axis.ticks.x = element_blank(),
#                                   axis.title.y = element_blank(),
#                                   axis.ticks.y = element_blank(),
#                                   axis.text.y = element_blank())
# 
# library(patchwork)
# 
# (p1 | p2 | p3)  + plot_layout(guides = "collect") +plot_annotation(title = "Sampling Biases", tag_levels = 'A')

# Regressions

r2 <- lm(trad ~ parasite * age + ismale , data = df)
r3 <- lm(trad ~ parasite * age + ismale , data = df[df$nation %in% top5_parasite,])
r4 <- lm(trad ~ parasite * age + ismale, data = df[df$nation %in% top10_parasite,])

stargazer(r2, r3, r4, out = "out/week1/regs.html", type = "html")

library(margins)

# cplot(r1, x = "age", dx = "parasite", what = "effect")

p1 <- interplot(r4, var1 = "parasite", var2 = "age") +
  labs(x = "Age", y = "dy/dx(parasite)", title = "dy/dx(parasite) w.r.t. age (Model 2)") + geom_hline(yintercept = 0, linetype = "dashed")

p2 <- interplot(r4, var1 = "parasite", var2 = "age") +
  labs(x = "Age", y = "dy/dx(parasite)", title = "dy/dx(parasite) w.r.t. age  (Model 3)") + geom_hline(yintercept = 0, linetype = "dashed")
  
p1
ggsave("out/week1/marginplot.png", width = 6, height = 4)

# r1 <- lm(trad ~ parasite * age + ismale, data = df[nation %in% ])
# 
# r2 <- lm(trad ~ parasite * pdeaths_per1k + ismale + age + log(gdp_pc_ppp), data = df)
# r3 <- lm(trad ~ parasite + ismale + age, data = df, weights = weights)
# r4 <- lm(trad ~ parasite * pdeaths_per1k + ismale + age + log(gdp_pc_ppp), data = df, weights = weights)



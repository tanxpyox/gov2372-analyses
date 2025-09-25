#----------------------------------------------
# Replication Materials for Orr Fowler Huber 2023 AJPS
# Figure A4
# Table A7
#----------------------------------------------


#----------------------------------------------
# Load Packages
#----------------------------------------------

#install.packages("dplyr")
#install.packages("estimatr")
#install.packages("ggplot2")

library(dplyr)
library(estimatr)
library(ggplot2)


#----------------------------------------------
# Load data
#----------------------------------------------

dat1 <- read.csv("study1_public.csv", as.is = TRUE) 
dat2 <- read.csv("study2_public.csv", as.is = TRUE)
dat3 <- read.csv("study3_public.csv", as.is = TRUE)


#----------------------------------------------
# Study 1 data processing from Orr Huber 2020 (code not modified)
#----------------------------------------------

# Submitted Surveys: 1996
study1_wave1N <- nrow(dat1)
study1_wave1N

# Wave 2 submitted surveys: 1717
dat1 <- dat1[!is.na(dat1$state1),]
nrow(dat1)

nrow(dat1)/study1_wave1N # Panel retention reported in Table 1

# Excluded due to missing, or third party partisanship: 60

table(dat1$pre_pidl, useNA = "always") # Sizes reported in Table 1

dat1 <- dat1[!is.na(dat1$pre_pidl),]

# Excluded Independents: 211
dat1 <- dat1[dat1$pre_pidl %in% c("D", "R"),]

# Excluded due to missing policy preferences: 7
sum(apply(dat1[, c("ABORTPRE_ABSELFSTD", "IMMIG_CHILD", "FEDSPEND_FSWELF")], 
          1, function(x) any(is.na(x))))

dat1 <- dat1[!apply(dat1[, c("ABORTPRE_ABSELFSTD", "IMMIG_CHILD", "FEDSPEND_FSWELF")], 
                    1, function(x) any(is.na(x))),]

# Excluded due to stated policy preferences not used in vignettes (e.g, \other"): 207
table(dat1$ABORTPRE_ABSELFSTD %in% c(1, 2, 4))

dat1 <- dat1[dat1$ABORTPRE_ABSELFSTD %in% c(1, 2, 4),]

# Surveys included in main analysis: 1232
nrow(dat1)

#----------------------------------------------
# Study 2 data processing from Orr Huber 2020 (code not modified)
#----------------------------------------------

# Wave 1 Submitted Surveys: 1624 (1595 with codes to link waves)
# Wave 2 submitted surveys: 942 (934 with responses entered and codes also found in wave 1)

sum(dat2$study == "panel")/1595 # Panel retention reported in Table 1

# Singe-wave submitted surveys: 802
# Only participants who completed both waves (with information necessary to link records) are included here.
table(dat2$study)

# Excluded due to missing, or third party partisanship: 108
# Excluded Independents: 188

table(dat2$pidl, useNA = "always")
dat2 <- dat2[dat2$pidl %in% c("D", "R"),]

# Excluded due to missing policy preferences: 2
sum(apply(dat2[, c("abort", "adopt", "gun", "imm", "welfare")], 1, 
          function(x) any(is.na(x))))

dat2 <- dat2[!apply(dat2[, c("abort", "adopt", "gun", "imm", "welfare")], 1, function(x) any(is.na(x))),]


# Excluded due to stated policy preferences not used in vignettes (e.g, \other"): 56
table(dat2$abort)
dat2 <- dat2[dat2$abort != "other",]

# Surveys included in main analysis: 1382
nrow(dat2)


#----------------------------------------------
# Study 3 data processing from Orr Huber 2020 (code not modified)
#----------------------------------------------

# Submitted surveys: 5001
nrow(dat3) + 10 # 10 minors were removed 

# Excluded due to age restriction: 10
# To protect minors, these responses are excluded from the replication file

# Excluded due to missing, or third party partisanship: 0
table(dat3$dem_pid7, useNA = "always")
table(sign(dat3$dem_pid7), useNA = "always")

# Excluded Independents: 1011
dat3 <- dat3[dat3$dem_pid7 != 0,]

# Excluded due to missing policy preferences: 3
sum(apply(dat3[, c("abort", "adopt", "gun", "imm", "welfare")], 1, function(x) any(is.na(x))))

dat3 <- dat3[!apply(dat3[, c("abort", "adopt", "gun", "imm", "welfare")], 1, function(x) any(is.na(x))),]

# Excluded due to stated policy preferences not used in vignettes (e.g, \other"): 102
table(dat3$abort)

dat3 <- dat3[dat3$abort != "other",]

# Surveys included in main analysis: 3873
nrow(dat3)

# Across studies, what percept were dropped?
# Independents and third party affiliates
(211 + 60 + 188 + 108 + 1011)/(1717 + 1737 + 4991)

# Missing or "other" policy
(7 + 207 + 2 + 56 + 3 + 102)/(1717 + 1737 + 4991)

#----------------------------------------------
# Recode study 1 treatments and outcomes from Orr Huber 2020 (code not modified)
#----------------------------------------------

# Define functions which generate indicators of match between randomized 
# vignette trait and respondent trait.

make_copar_indicator1 <- function(dat, vignett_pid = "", respondent_pid = "") {
  I_copar <- rep(0, nrow(dat))
  for(i in 1:nrow(dat)) {
    if(!dat[i, vignett_pid] %in% c("a Republican", "a Democrat")) {I_copar[i] <- NA; next}
    if(dat[i, respondent_pid] == "R" & dat[i, vignett_pid] == "a Republican") I_copar[i] <- 1
    if(dat[i, respondent_pid] == "D" & dat[i, vignett_pid] == "a Democrat") I_copar[i] <- 1
  }
  return(I_copar)
}

dat1$I_FT1copar <- make_copar_indicator1(dat = dat1, vignett_pid = "FTpid1", respondent_pid = "pre_pidl")
dat1$I_FT2copar <- make_copar_indicator1(dat = dat1, vignett_pid = "FTpid2", respondent_pid = "pre_pidl")
dat1$I_FT3copar <- make_copar_indicator1(dat = dat1, vignett_pid = "FTpid3", respondent_pid = "pre_pidl")

make_copol_indicator1 <- function(dat, vignett_pol = "", respondent_abort = "", 
                                  respondent_imm = "", respondent_welfare = "") {
  I_copol <- rep(0, nrow(dat))
  for(i in 1:nrow(dat)) {
    if(dat[i, respondent_abort] == 4 & dat[i, vignett_pol] == "by law, a woman should always be able to obtain an abortion as a matter of personal choice") I_copol[i] <- 1
    if(dat[i, respondent_abort] == 2 & dat[i, vignett_pol] == "the law should permit abortion only in case of rape, incest, or when the woman's life is in danger") I_copol[i] <- 1
    if(dat[i, respondent_abort] == 1 & dat[i, vignett_pol] == "by law, abortion should never be permitted") I_copol[i] <- 1
    if(dat[i, respondent_imm] == 2 & dat[i, vignett_pol] == "immigrants who were brought to the U.S. illegally as children should be allowed to live and work in the U.S") I_copol[i] <- 1
    if(dat[i, respondent_imm] == 1 & dat[i, vignett_pol] == "immigrants who were brought to the U.S. illegally as children should be sent back where they came from") I_copol[i] <- 1
    if(dat[i, respondent_welfare] == 1 & dat[i, vignett_pol] == "federal spending on welfare programs should be increased") I_copol[i] <- 1
    if(dat[i, respondent_welfare] == 3 & dat[i, vignett_pol] == "federal spending on welfare programs should be maintained") I_copol[i] <- 1
    if(dat[i, respondent_welfare] == 2 & dat[i, vignett_pol] == "federal spending on welfare programs should be decreased") I_copol[i] <- 1
  }
  return(I_copol)
}


dat1$I_FT1copol <- make_copol_indicator1(dat = dat1, vignett_pol = "FTpolicy1", respondent_abort = "ABORTPRE_ABSELFSTD",
                                         respondent_imm = "IMMIG_CHILD", respondent_welfare = "FEDSPEND_FSWELF")
dat1$I_FT2copol <- make_copol_indicator1(dat = dat1, vignett_pol = "FTpolicy2", respondent_abort = "ABORTPRE_ABSELFSTD",
                                         respondent_imm = "IMMIG_CHILD", respondent_welfare = "FEDSPEND_FSWELF")
dat1$I_FT3copol <- make_copol_indicator1(dat = dat1, vignett_pol = "FTpolicy3", respondent_abort = "ABORTPRE_ABSELFSTD",
                                         respondent_imm = "IMMIG_CHILD", respondent_welfare = "FEDSPEND_FSWELF")

# Responses in each treatment arm were saved in separate variables. Combine these into
# a single set of outcomes (FT_1st, FT_2nd, and FT_3rd) with a treatment indicator (treat_pa).

dat1$treat_pa <- dat1$THERMSD_pid.1
dat1$treat_pa[dat1$treat_pa == 1] <- "pid"
dat1$treat_pa[dat1$THERMSD_policy.1 == 1] <- "policy"
dat1$treat_pa[dat1$THERMSD_pid_policy.1 == 1] <- "pid_policy"

dat1$FT_1st <- dat1$THERMSD_pid.2_1
dat1$FT_1st[is.na(dat1$FT_1st)] <- dat1$THERMSD_policy.2_1[is.na(dat1$FT_1st)]
dat1$FT_1st[is.na(dat1$FT_1st)] <- dat1$THERMSD_pid_policy.2_1[is.na(dat1$FT_1st)]
dat1$FT_2nd <- dat1$THERMSD_pid.3_1
dat1$FT_2nd[is.na(dat1$FT_2nd)] <- dat1$THERMSD_policy.3_1[is.na(dat1$FT_2nd)]
dat1$FT_2nd[is.na(dat1$FT_2nd)] <- dat1$THERMSD_pid_policy.3_1[is.na(dat1$FT_2nd)]
dat1$FT_3rd <- dat1$THERMSD_pid.4_1
dat1$FT_3rd[is.na(dat1$FT_3rd)] <- dat1$THERMSD_policy.4_1[is.na(dat1$FT_3rd)]
dat1$FT_3rd[is.na(dat1$FT_3rd)] <- dat1$THERMSD_pid_policy.4_1[is.na(dat1$FT_3rd)]

# Generate a variable to indicate which policy area was randomly assigned in each vignette

dat1$FTpolicy_area1 <- ""
dat1$FTpolicy_area1[grepl("abortion", dat1$FTpolicy1)] <- "abort"
dat1$FTpolicy_area1[grepl("immigrants", dat1$FTpolicy1)] <- "imm"
dat1$FTpolicy_area1[grepl("welfare", dat1$FTpolicy1)] <- "welfare"
dat1$FTpolicy_area1[dat1$treat_pa == "pid"] <- "none"

dat1$FTpolicy_area2 <- ""
dat1$FTpolicy_area2[grepl("abortion", dat1$FTpolicy2)] <- "abort"
dat1$FTpolicy_area2[grepl("immigrants", dat1$FTpolicy2)] <- "imm"
dat1$FTpolicy_area2[grepl("welfare", dat1$FTpolicy2)] <- "welfare"
dat1$FTpolicy_area2[dat1$treat_pa == "pid"] <- "none"

dat1$FTpolicy_area3 <- ""
dat1$FTpolicy_area3[grepl("abortion", dat1$FTpolicy3)] <- "abort"
dat1$FTpolicy_area3[grepl("immigrants", dat1$FTpolicy3)] <- "imm"
dat1$FTpolicy_area3[grepl("welfare", dat1$FTpolicy3)] <- "welfare"
dat1$FTpolicy_area3[dat1$treat_pa == "pid"] <- "none"

# In the dataframe dat1, each row represents a respondent. Each respondent 
# evaluated three vignettes. Generate a "long" data structure such that
# each row represents a vignette evaluation.

dat1_stacked <- data.frame(FT = c(dat1$FT_1st, dat1$FT_2nd, dat1$FT_3rd),
                           treat_pa = rep(dat1$treat_pa, 3),
                           I_FT_copar = c(dat1$I_FT1copar, dat1$I_FT2copar, dat1$I_FT3copar),
                           I_FT_copol = c(dat1$I_FT1copol, dat1$I_FT2copol, dat1$I_FT3copol),
                           policy_area = c(dat1$FTpolicy_area1, dat1$FTpolicy_area2, dat1$FTpolicy_area3),
                           ID = rep(1:nrow(dat1), 3))

#----------------------------------------------
# Recode study 2 treatments and outcomes from Orr Huber 2020 (code not modified)
#----------------------------------------------

# Generate indicators of high valence fiance traits

low_val <- c("is considerably overweight", "is divorced", "occasionally smokes cigarettes",
             "occasionally smokes marijuana")
high_val <- c("doesn't smoke", "exercises regularly", "has never been married before")
dat2$Mextra1_val <- NA
dat2$Mextra1_val[dat2$Mextra1 %in% low_val] <- 0
dat2$Mextra1_val[dat2$Mextra1 %in% high_val] <- 1
dat2$Mextra2_val <- NA
dat2$Mextra2_val[dat2$Mextra2 %in% low_val] <- 0
dat2$Mextra2_val[dat2$Mextra2 %in% high_val] <- 1
dat2$Mextra3_val <- NA
dat2$Mextra3_val[dat2$Mextra3 %in% low_val] <- 0
dat2$Mextra3_val[dat2$Mextra3 %in% high_val] <- 1
dat2$Mextra4_val <- NA
dat2$Mextra4_val[dat2$Mextra4 %in% low_val] <- 0
dat2$Mextra4_val[dat2$Mextra4 %in% high_val] <- 1
dat2$Mextra5_val <- NA
dat2$Mextra5_val[dat2$Mextra5 %in% low_val] <- 0
dat2$Mextra5_val[dat2$Mextra5 %in% high_val] <- 1
dat2$Mextra6_val <- NA
dat2$Mextra6_val[dat2$Mextra6 %in% low_val] <- 0
dat2$Mextra6_val[dat2$Mextra6 %in% high_val] <- 1

dat2$M4nosmokes <- as.numeric(grepl("doesn't", dat2$Mextra4))
dat2$M4exercise <- as.numeric(grepl("exercises", dat2$Mextra4))
dat2$M4nodivorce <- as.numeric(grepl("married", dat2$Mextra4))

# Generate indicators of match between randomized vignette trait and respondent trait.

dat2$M4covac <- rep(0, nrow(dat2))
dat2$M4cospank <- rep(0, nrow(dat2))
for(i in 1:nrow(dat2)) {
  if(is.na(dat2[i, "spank"])) {dat2$M4covac[i] <- NA; next}
  if(is.na(dat2[i, "vaccinate"])) {dat2$M4covac[i] <- NA; next}
  if(dat2[i, "spank"] %in% c("Somewhat disagree", "Strongly disagree") & dat2[i, "Mchild4"] == "doesn't think children should be spanked") dat2$M4cospank[i] <- 1
  if(dat2[i, "spank"] %in% c("Somewhat agree", "Strongly agree") & dat2[i, "Mchild4"] == "believes children sometimes need to be spanked") dat2$M4cospank[i] <- 1
  if(dat2[i, "vaccinate"] %in% c("Somewhat agree", "Strongly agree") & dat2[i, "Mchild4"] == "believes all children should be vaccinated before starting public school") dat2$M4covac[i] <- 1
  if(dat2[i, "vaccinate"] %in% c("Somewhat disagree", "Strongly disagree") & dat2[i, "Mchild4"] == "believes parents should decide whether or not to vaccinate children before they go to public school") dat2$M4covac[i] <- 1
  if(dat2[i, "spank"] == "Neither agree nor disagree") dat2$M4cospank[i] <- NA
  if(dat2[i, "vaccinate"] == "Neither agree nor disagree") dat2$M4covac[i] <- NA
}

make_copar_indicator2 <- function(dat, vignett_pid = "", respondent_pid = "") {
  I_copar <- rep(0, nrow(dat))
  for(i in 1:nrow(dat)) {
    if(dat[i, respondent_pid] == "R" & dat[i, vignett_pid] == "R") I_copar[i] <- 1
    if(dat[i, respondent_pid] == "D" & dat[i, vignett_pid] == "D") I_copar[i] <- 1
    if(!dat[i, vignett_pid] %in% c("R", "D")) I_copar[i] <- NA
  }
  return(I_copar)
}

dat2$I_FT1copar <- make_copar_indicator2(dat = dat2, vignett_pid = "FTpid1", respondent_pid = "pidl")
dat2$I_FT4copar <- make_copar_indicator2(dat = dat2, vignett_pid = "FTpid4", respondent_pid = "pidl")
dat2$I_FT5copar <- make_copar_indicator2(dat = dat2, vignett_pid = "FTpid5", respondent_pid = "pidl")
dat2$I_FT6copar <- make_copar_indicator2(dat = dat2, vignett_pid = "FTpid6", respondent_pid = "pidl")
dat2$I_FT8copar <- make_copar_indicator2(dat = dat2, vignett_pid = "FTpid8", respondent_pid = "pidl")
dat2$I_FT9copar <- make_copar_indicator2(dat = dat2, vignett_pid = "FTpid9", respondent_pid = "pidl")
dat2$I_M1copar <- make_copar_indicator2(dat = dat2, vignett_pid = "Mpid1", respondent_pid = "pidl")
dat2$I_M4copar <- make_copar_indicator2(dat = dat2, vignett_pid = "Mpid4", respondent_pid = "pidl")
dat2$I_M5copar <- make_copar_indicator2(dat = dat2, vignett_pid = "Mpid5", respondent_pid = "pidl")
dat2$I_M6copar <- make_copar_indicator2(dat = dat2, vignett_pid = "Mpid6", respondent_pid = "pidl")
dat2$I_M7copar <- make_copar_indicator2(dat = dat2, vignett_pid = "Mpid7", respondent_pid = "pidl")

make_coethnic_indicator <- function(dat, vignett_race = "", res_white = "", res_black = "",
                                    res_asian = "", res_hispanic = "") {
  I_coethnic <- rep(0, nrow(dat))
  for(i in 1:nrow(dat)) {
    if(dat[i, res_white] == 1 & grepl("White", dat[i, vignett_race])) I_coethnic[i] <- 1
    if(dat[i, res_black] == 1 & grepl("Black", dat[i, vignett_race])) I_coethnic[i] <- 1
    if(dat[i, res_asian] == 1 & grepl("Asian", dat[i, vignett_race])) I_coethnic[i] <- 1
    if(dat[i, res_hispanic] == 1 & grepl("Hispanic", dat[i, vignett_race])) I_coethnic[i] <- 1
    if(is.na(dat[i, res_white]) & is.na(dat[i, res_black]) & 
       is.na(dat[i, res_asian]) & is.na(dat[i, res_hispanic])) I_coethnic[i] <- NA
    if(dat[i, res_white] == 0 & dat[i, res_black] == 0 &
       dat[i, res_asian] == 0 & dat[i, res_hispanic] == 0) I_coethnic[i] <- NA
  }
  return(I_coethnic)
}

dat2$I_FT3coethnic <- make_coethnic_indicator(dat = dat2, vignett_race = "FTrace3", 
                                              res_white = "white", res_black = "black",
                                              res_asian = "asian", res_hispanic = "hispanic")
dat2$I_FT4coethnic <- make_coethnic_indicator(dat = dat2, vignett_race = "FTrace4", 
                                              res_white = "white", res_black = "black",
                                              res_asian = "asian", res_hispanic = "hispanic")
dat2$I_FT6coethnic <- make_coethnic_indicator(dat = dat2, vignett_race = "FTrace6", 
                                              res_white = "white", res_black = "black",
                                              res_asian = "asian", res_hispanic = "hispanic")
dat2$I_FT7coethnic <- make_coethnic_indicator(dat = dat2, vignett_race = "FTrace7", 
                                              res_white = "white", res_black = "black",
                                              res_asian = "asian", res_hispanic = "hispanic")
dat2$I_FT9coethnic <- make_coethnic_indicator(dat = dat2, vignett_race = "FTrace9", 
                                              res_white = "white", res_black = "black",
                                              res_asian = "asian", res_hispanic = "hispanic")
dat2$I_M3coethnic <- make_coethnic_indicator(dat = dat2, vignett_race = "Mrace3", 
                                             res_white = "white", res_black = "black",
                                             res_asian = "asian", res_hispanic = "hispanic")
dat2$I_M4coethnic <- make_coethnic_indicator(dat = dat2, vignett_race = "Mrace4", 
                                             res_white = "white", res_black = "black",
                                             res_asian = "asian", res_hispanic = "hispanic")
dat2$I_M6coethnic <- make_coethnic_indicator(dat = dat2, vignett_race = "Mrace6", 
                                             res_white = "white", res_black = "black",
                                             res_asian = "asian", res_hispanic = "hispanic")

make_corel_indicator <- function(dat, vignett_rel = "", respondent_rel = "") {
  I_corel <- rep(0, nrow(dat))
  for(i in 1:nrow(dat)) {
    if(is.na(dat[i, respondent_rel])){I_corel[i] <- NA; next}
    if(dat[i, respondent_rel] == "Atheist" & dat[i, vignett_rel] == "an Atheist") I_corel[i] <- 1
    if(dat[i, respondent_rel] == "Roman Catholic" & dat[i, vignett_rel] == "Catholic") I_corel[i] <- 1
    if(dat[i, respondent_rel] == "Jewish" & dat[i, vignett_rel] == "Jewish") I_corel[i] <- 1
    if(dat[i, respondent_rel] == "Protestant" & dat[i, vignett_rel] == "Protestant") I_corel[i] <- 1
    if(dat[i, respondent_rel] == "Nothing in particular" & dat[i, vignett_rel] == "not particularly religious") I_corel[i] <- 1
    if(dat[i, respondent_rel] == "Agnostic" & dat[i, vignett_rel] == "not particularly religious") I_corel[i] <- 1
    if(is.na(dat[i, respondent_rel])) I_corel[i] <- NA
    if(dat[i, respondent_rel] == "Muslim") I_corel[i] <- NA
    if(dat[i, respondent_rel] == "Hindu") I_corel[i] <- NA
    if(dat[i, respondent_rel] == "Mormon") I_corel[i] <- NA
    if(dat[i, respondent_rel] == "Eastern or Greek Orthodox") I_corel[i] <- NA
    if(dat[i, respondent_rel] == "Buddhist") I_corel[i] <- NA
  }
  return(I_corel)
}
dat2$I_FT3corel <- make_corel_indicator(dat = dat2, vignett_rel = "FTrel3", respondent_rel = "rel")
dat2$I_FT4corel <- make_corel_indicator(dat = dat2, vignett_rel = "FTrel4", respondent_rel = "rel")
dat2$I_FT6corel <- make_corel_indicator(dat = dat2, vignett_rel = "FTrel6", respondent_rel = "rel")
dat2$I_FT7corel <- make_corel_indicator(dat = dat2, vignett_rel = "FTrel7", respondent_rel = "rel")
dat2$I_FT9corel <- make_corel_indicator(dat = dat2, vignett_rel = "FTrel9", respondent_rel = "rel")
dat2$I_M3corel <- make_corel_indicator(dat = dat2, vignett_rel = "Mrel3", respondent_rel = "rel")
dat2$I_M4corel <- make_corel_indicator(dat = dat2, vignett_rel = "Mrel4", respondent_rel = "rel")
dat2$I_M6corel <- make_corel_indicator(dat = dat2, vignett_rel = "Mrel6", respondent_rel = "rel")

make_copol_indicator2 <- function(dat, vignett_pol = "", respondent_abort = "", respondent_adopt = "", 
                                  respondent_gun = "", respondent_imm = "", respondent_welfare = "") {
  I_copol <- rep(0, nrow(dat))
  for(i in 1:nrow(dat)) {
    if(dat[i, respondent_abort] == "always" & dat[i, vignett_pol] == "a woman should always be able to obtain an abortion as a personal choice") I_copol[i] <- 1
    if(dat[i, respondent_abort] == "sometimes" & dat[i, vignett_pol] == "abortion should only be permitted in cases of rape, incest, or when the woman's life is in danger") I_copol[i] <- 1
    if(dat[i, respondent_abort] == "never" & dat[i, vignett_pol] == "abortion should never be permitted") I_copol[i] <- 1
    if(dat[i, respondent_adopt] == "yes" & dat[i, vignett_pol] == "same sex couples should be permitted to adopt children") I_copol[i] <- 1
    if(dat[i, respondent_adopt] == "no" & dat[i, vignett_pol] == "same sex couples should not be permitted to adopt children") I_copol[i] <- 1
    if(dat[i, respondent_gun] == "difficult" & grepl("the federal government should make it more", dat[i, vignett_pol])) I_copol[i] <- 1
    if(dat[i, respondent_gun] == "same" & dat[i, vignett_pol] == "the federal government should should keep rules for buying guns about the same as they are now") I_copol[i] <- 1
    if(dat[i, respondent_gun] == "easier" & dat[i, vignett_pol] == "the federal government should make it easier for people to buy a gun") I_copol[i] <- 1
    if(dat[i, respondent_imm] == "citizen" & dat[i, vignett_pol] == "DREAMers who were brought to the U.S. illegally as children should be allowed to live and work in the U.S. and to eventually become citizens") I_copol[i] <- 1
    if(dat[i, respondent_imm] == "stay" & dat[i, vignett_pol] == "DREAMers who were brought to the U.S. illegally as children should be allowed to live and work in the U.S. but only under strict regulation") I_copol[i] <- 1
    if(dat[i, respondent_imm] == "deport" & dat[i, vignett_pol] == "DREAMers who were brought to the U.S. illegally as children should be sent back where they came from") I_copol[i] <- 1
    if(dat[i, respondent_welfare] == "inc" & dat[i, vignett_pol] == "federal spending on welfare programs should be increased") I_copol[i] <- 1
    if(dat[i, respondent_welfare] == "same" & dat[i, vignett_pol] == "federal spending on welfare programs should be kept the same") I_copol[i] <- 1
    if(dat[i, respondent_welfare] == "dec" & dat[i, vignett_pol] == "federal spending on welfare programs should be decreased") I_copol[i] <- 1
    if(dat[i, respondent_abort] == "other") I_copol[i] <- NA
  }
  return(I_copol)
}

dat2$I_FT2copol <- make_copol_indicator2(dat = dat2, vignett_pol = "FTpolicy2", respondent_abort = "abort",
                                         respondent_adopt = "adopt", respondent_gun = "gun", 
                                         respondent_imm = "imm", respondent_welfare = "welfare")
dat2$I_FT4copol <- make_copol_indicator2(dat = dat2, vignett_pol = "FTpolicy4", respondent_abort = "abort",
                                         respondent_adopt = "adopt", respondent_gun = "gun", 
                                         respondent_imm = "imm", respondent_welfare = "welfare")
dat2$I_FT5copol <- make_copol_indicator2(dat = dat2, vignett_pol = "FTpolicy5", respondent_abort = "abort",
                                         respondent_adopt = "adopt", respondent_gun = "gun", 
                                         respondent_imm = "imm", respondent_welfare = "welfare")
dat2$I_FT7copol <- make_copol_indicator2(dat = dat2, vignett_pol = "FTpolicy7", respondent_abort = "abort",
                                         respondent_adopt = "adopt", respondent_gun = "gun", 
                                         respondent_imm = "imm", respondent_welfare = "welfare")
dat2$I_FT9copol <- make_copol_indicator2(dat = dat2, vignett_pol = "FTpolicy9", respondent_abort = "abort",
                                         respondent_adopt = "adopt", respondent_gun = "gun", 
                                         respondent_imm = "imm", respondent_welfare = "welfare")
dat2$I_M2copol <- make_copol_indicator2(dat = dat2, vignett_pol = "Mpolicy2", respondent_abort = "abort",
                                        respondent_adopt = "adopt", respondent_gun = "gun", 
                                        respondent_imm = "imm", respondent_welfare = "welfare")
dat2$I_M4copol <- make_copol_indicator2(dat = dat2, vignett_pol = "Mpolicy4", respondent_abort = "abort",
                                        respondent_adopt = "adopt", respondent_gun = "gun", 
                                        respondent_imm = "imm", respondent_welfare = "welfare")
dat2$I_M5copol <- make_copol_indicator2(dat = dat2, vignett_pol = "Mpolicy5", respondent_abort = "abort",
                                        respondent_adopt = "adopt", respondent_gun = "gun", 
                                        respondent_imm = "imm", respondent_welfare = "welfare")

make_cochild_indicator <- function(dat, vignett_child = "", respondent_spank = "", respondent_vac = "") {
  I_cochild <- rep(0, nrow(dat))
  for(i in 1:nrow(dat)) {
    if(is.na(dat[i, respondent_spank])){I_cochild[i] <- NA; next}
    if(is.na(dat[i, respondent_vac])){I_cochild[i] <- NA; next}
    if(dat[i, respondent_spank] %in% c("Somewhat disagree", "Strongly disagree") & dat[i, vignett_child] == "doesn't think children should be spanked") I_cochild[i] <- 1
    if(dat[i, respondent_spank] %in% c("Somewhat agree", "Strongly agree") & dat[i, vignett_child] == "believes children sometimes need to be spanked") I_cochild[i] <- 1
    if(dat[i, respondent_vac] %in% c("Somewhat agree", "Strongly agree") & dat[i, vignett_child] == "believes all children should be vaccinated before starting public school") I_cochild[i] <- 1
    if(dat[i, respondent_vac] %in% c("Somewhat disagree", "Strongly disagree") & dat[i, vignett_child] == "believes parents should decide whether or not to vaccinate children before they go to public school") I_cochild[i] <- 1
    if(dat[i, respondent_spank] == "Neither agree nor disagree") I_cochild[i] <- NA
    if(dat[i, respondent_vac] == "Neither agree nor disagree") I_cochild[i] <- NA
  }
  return(I_cochild)
}

dat2$I_M1cochild <- make_cochild_indicator(dat = dat2, vignett_child = "Mchild1", 
                                           respondent_spank = "spank", respondent_vac = "vaccinate")
dat2$I_M2cochild <- make_cochild_indicator(dat = dat2, vignett_child = "Mchild2", 
                                           respondent_spank = "spank", respondent_vac = "vaccinate")
dat2$I_M3cochild <- make_cochild_indicator(dat = dat2, vignett_child = "Mchild3", 
                                           respondent_spank = "spank", respondent_vac = "vaccinate")
dat2$I_M4cochild <- make_cochild_indicator(dat = dat2, vignett_child = "Mchild4", 
                                           respondent_spank = "spank", respondent_vac = "vaccinate")
dat2$I_M5cochild <- make_cochild_indicator(dat = dat2, vignett_child = "Mchild5", 
                                           respondent_spank = "spank", respondent_vac = "vaccinate")
dat2$I_M6cochild <- make_cochild_indicator(dat = dat2, vignett_child = "Mchild6", 
                                           respondent_spank = "spank", respondent_vac = "vaccinate")

# In the dataframe dat2, each row represents a respondent. Each respondent 
# evaluated multiple vignettes. Generate a "long" data structure such that
# each row represents a vignette evaluation in the pid, policy, or pid + policy 
# treatment groups.

dat2_stacked <- data.frame(FT = c(dat2$FT1_1, dat2$FT8_1, dat2$FT2_1, dat2$FT5_1),
                           policy_area = "none",
                           policy = c(rep("NA", length(c(dat2$FT1_1, dat2$FT8_1))), dat2$FTpolicy2, dat2$FTpolicy5),
                           I_FT_copar = c(dat2$I_FT1copar, dat2$I_FT8copar, rep(NA, length(dat2$FT2_1)), dat2$I_FT5copar),
                           I_FT_copol = c(rep(NA, length(c(dat2$FT1_1, dat2$FT8_1))), dat2$I_FT2copol, dat2$I_FT5copol),
                           treat_pa = factor(c(rep("pid", length(c(dat2$FT1_1, dat2$FT8_1))),
                                               rep("policy", length(dat2$FT2_1)),
                                               rep("pid_policy", length(dat2$FT5_1))),
                                             levels = c("pid", "policy", "pid_policy")),
                           ID = rep(1:nrow(dat2), 4))

dat2_stacked[,c(2,3)] <- lapply(dat2_stacked[,c(2,3)], as.character)
dat2_stacked$policy_area[grepl("abortion", as.character(dat2_stacked$policy))] <- "abort"
dat2_stacked$policy_area[grepl("adopt", dat2_stacked$policy)] <- "adopt"
dat2_stacked$policy_area[grepl("gun", dat2_stacked$policy)] <- "gun"
dat2_stacked$policy_area[grepl("DREAMers", dat2_stacked$policy)] <- "imm"
dat2_stacked$policy_area[grepl("welfare", dat2_stacked$policy)] <- "welfare"

#----------------------------------------------
# Recode study 3 treatments and outcomes from Orr Huber 2020 (code not modified)
#----------------------------------------------

# Generate indicators of match between randomized vignette trait and respondent trait.

make_copar_indicator3 <- function(dat, vignett_pid = "", respondent_pid = "") {
  I_copar <- rep(0, nrow(dat))
  for(i in 1:nrow(dat)) {
    if(dat[i, respondent_pid] > 0 & dat[i, vignett_pid] == "a Republican") I_copar[i] <- 1
    if(dat[i, respondent_pid] < 0 & dat[i, vignett_pid] == "a Democrat") I_copar[i] <- 1
    if(!dat[i, vignett_pid] %in% c("a Republican", "a Democrat")) I_copar[i] <- NA
  }
  return(I_copar)
}

dat3$I_FT_copar <- make_copar_indicator3(dat = dat3, vignett_pid = "pid", respondent_pid = "dem_pid7")

make_copol_indicator3 <- function(dat, vignett_pol = "", respondent_abort = "", respondent_adopt = "", 
                                  respondent_gun = "", respondent_imm = "", respondent_welfare = "") {
  I_copol <- rep(0, nrow(dat))
  for(i in 1:nrow(dat)) {
    if(dat[i, respondent_abort] == "always" & dat[i, vignett_pol] == "a woman should always be able to obtain an abortion as a personal choice") I_copol[i] <- 1
    if(dat[i, respondent_abort] == "sometimes" & dat[i, vignett_pol] == "abortion should only be permitted in case of rape, incest, or when the woman's life is in danger") I_copol[i] <- 1
    if(dat[i, respondent_abort] == "never" & dat[i, vignett_pol] == "abortion should never be permitted") I_copol[i] <- 1
    if(dat[i, respondent_adopt] == "yes" & dat[i, vignett_pol] == "same sex couples should be allowed to adopt children") I_copol[i] <- 1
    if(dat[i, respondent_adopt] == "no" & dat[i, vignett_pol] == "same sex couples should not be allowed to adopt children") I_copol[i] <- 1
    if(dat[i, respondent_gun] == "difficult" & grepl("the federal government should make it more", dat[i, vignett_pol])) I_copol[i] <- 1
    if(dat[i, respondent_gun] == "same" & dat[i, vignett_pol] == "the federal government should should keep rules for buying guns about the same as they are now") I_copol[i] <- 1
    if(dat[i, respondent_gun] == "easier" & dat[i, vignett_pol] == "the federal government should make it easier for people to buy a gun") I_copol[i] <- 1
    if(dat[i, respondent_imm] == "citizen" & dat[i, vignett_pol] == "immigrants brought to the U.S. illegally as children should be allowed to live and work in the U.S. and eventually become citizens") I_copol[i] <- 1
    if(dat[i, respondent_imm] == "stay" & dat[i, vignett_pol] == "immigrants brought to the U.S. illegally as children should be allowed to live and work in the U.S. under strict regulation") I_copol[i] <- 1
    if(dat[i, respondent_imm] == "deport" & dat[i, vignett_pol] == "immigrants brought to the U.S. illegally as children should be sent back to where they came from") I_copol[i] <- 1
    if(dat[i, respondent_welfare] == "inc" & dat[i, vignett_pol] == "federal spending on welfare programs should be increased") I_copol[i] <- 1
    if(dat[i, respondent_welfare] == "same" & dat[i, vignett_pol] == "federal spending on welfare programs should be maintained") I_copol[i] <- 1
    if(dat[i, respondent_welfare] == "dec" & dat[i, vignett_pol] == "federal spending on welfare programs should be decreased") I_copol[i] <- 1
  }
  return(I_copol)
}

dat3$I_FT_copol <- make_copol_indicator3(dat = dat3, vignett_pol = "policy", 
                                         respondent_abort = "abort", respondent_adopt = "adopt",
                                         respondent_gun = "gun",respondent_imm = "imm", 
                                         respondent_welfare = "welfare")

# Responses in each treatment arm were saved in separate variables. Combine these into
# a single outcome (FT)

dat3$FT <- dat3$FT_pid
dat3$FT[is.na(dat3$FT)] <- dat3$FT_pol[is.na(dat3$FT)]
dat3$FT[is.na(dat3$FT)] <- dat3$FT_both[is.na(dat3$FT)]

dat3$policy_area_viewed <- dat3$policy_area
dat3$policy_area_viewed[dat3$treat_pa == "pid"] <- "none"

dat3$dem_pid3 <- sign(dat3$dem_pid7)



#----------------------------------------------
# New Analysis: Extract all partisan vignettes with a policy position
#----------------------------------------------

# Variables needed across studies: vigette_pid, policy_area, policy, (vigette characteristics)
#                                  respondent_pid, abort, adopt, gun, imm, welfare, (respondent characteristics)
#                                  I_FT_copar, I_FT_copol, (vignette x respondent characteristics)
#                                  FT, vignette_num, respondent_id, study (outcomes and process)

# Study 1: one row per pid_policy vignette evaluation
dat1_pid_policy <- data.frame(treat_pa = "pid_policy",
                              vignette_pid = c(dat1$FTpid1[dat1$treat_pa %in% "pid_policy"], dat1$FTpid2[dat1$treat_pa %in% "pid_policy"], dat1$FTpid3[dat1$treat_pa %in% "pid_policy"]),
                              policy_area = c(dat1$FTpolicy_area1[dat1$treat_pa %in% "pid_policy"], dat1$FTpolicy_area2[dat1$treat_pa %in% "pid_policy"], dat1$FTpolicy_area3[dat1$treat_pa %in% "pid_policy"]),
                              policy = c(dat1$FTpolicy1[dat1$treat_pa %in% "pid_policy"], dat1$FTpolicy2[dat1$treat_pa %in% "pid_policy"], dat1$FTpolicy3[dat1$treat_pa %in% "pid_policy"]),
                              respondent_pid = rep(dat1$pre_pidl[dat1$treat_pa %in% "pid_policy"], 3),
                              abort = rep(dat1$ABORTPRE_ABSELFSTD[dat1$treat_pa %in% "pid_policy"], 3), 
                              adopt = NA, gun = NA, 
                              imm = rep(dat1$IMMIG_CHILD[dat1$treat_pa %in% "pid_policy"], 3),
                              welfare = rep(dat1$FEDSPEND_FSWELF[dat1$treat_pa %in% "pid_policy"], 3), 
                              copar = c(dat1$I_FT1copar[dat1$treat_pa %in% "pid_policy"], dat1$I_FT2copar[dat1$treat_pa %in% "pid_policy"], dat1$I_FT3copar[dat1$treat_pa %in% "pid_policy"]),
                              copol = c(dat1$I_FT1copol[dat1$treat_pa %in% "pid_policy"], dat1$I_FT2copol[dat1$treat_pa %in% "pid_policy"], dat1$I_FT3copol[dat1$treat_pa %in% "pid_policy"]),
                              FT = c(dat1$FT_1st[dat1$treat_pa %in% "pid_policy"], dat1$FT_2nd[dat1$treat_pa %in% "pid_policy"], dat1$FT_3rd[dat1$treat_pa %in% "pid_policy"]), 
                              vignette_num = rep(c(1, 2, 3), each = sum(dat1$treat_pa %in% "pid_policy")),
                              ID = rep(1:sum(dat1$treat_pa %in% "pid_policy"), 3) + 1000,
                              study = c("study1"))

dat1_pid_policy$abort <- dplyr::recode(dat1_pid_policy$abort, "1" = "never", "2" = "sometimes", "4" = "always")
dat1_pid_policy$imm <- dplyr::recode(dat1_pid_policy$imm, "1" = "deport", "2" = "stay")
dat1_pid_policy$welfare <- dplyr::recode(dat1_pid_policy$welfare, "1" = "inc", "2" = "dec", "3" = "same")
dat1_pid_policy$vignette_pid <- dplyr::recode(dat1_pid_policy$vignette_pid, "a Democrat" = "D", "a Republican" = "R", 
                                              "a political Independent" = "I", "not interested in politics" = "none")

# Study 2: one row per pid_policy vignette evaluation
dat2_pid_policy <- data.frame(treat_pa = "pid_policy",
                              vignette_pid = dat2$FTpid5,
                              policy_area = NA,
                              policy = dat2$FTpolicy5,
                              respondent_pid = dat2$pidl,
                              abort = dat2$abort, adopt = dat2$adopt, gun = dat2$gun, imm = dat2$imm, welfare = dat2$welfare, 
                              copar = dat2$I_FT5copar,
                              copol = dat2$I_FT5copol,
                              FT = dat2$FT5_1, 
                              vignette_num = 1, # of included vignettes
                              ID = 1:nrow(dat2) + 2000,
                              study = c("study2"))

dat2_pid_policy$policy_area[grepl("abortion", as.character(dat2_pid_policy$policy))] <- "abort"
dat2_pid_policy$policy_area[grepl("adopt", dat2_pid_policy$policy)] <- "adopt"
dat2_pid_policy$policy_area[grepl("gun", dat2_pid_policy$policy)] <- "gun"
dat2_pid_policy$policy_area[grepl("DREAMers", dat2_pid_policy$policy)] <- "imm"
dat2_pid_policy$policy_area[grepl("welfare", dat2_pid_policy$policy)] <- "welfare"


# Study 3: one row per pid_policy vignette evaluation
dat3_pid_policy <- data.frame(treat_pa = "pid_policy",
                              vignette_pid = dat3$pid[dat3$treat_pa %in% "pid_policy"],
                              policy_area = dat3$policy_area[dat3$treat_pa %in% "pid_policy"],
                              policy = dat3$policy[dat3$treat_pa %in% "pid_policy"],
                              respondent_pid = dat3$dem_pid3[dat3$treat_pa %in% "pid_policy"],
                              abort = dat3$abort[dat3$treat_pa %in% "pid_policy"], adopt = dat3$adopt[dat3$treat_pa %in% "pid_policy"], 
                              gun = dat3$gun[dat3$treat_pa %in% "pid_policy"], imm = dat3$imm[dat3$treat_pa %in% "pid_policy"], 
                              welfare = dat3$welfare[dat3$treat_pa %in% "pid_policy"], 
                              copar = dat3$I_FT_copar[dat3$treat_pa %in% "pid_policy"],
                              copol = dat3$I_FT_copol[dat3$treat_pa %in% "pid_policy"],
                              FT = dat3$FT[dat3$treat_pa %in% "pid_policy"], 
                              vignette_num = 1, # of included vignettes
                              ID = 1:sum(dat3$treat_pa %in% "pid_policy") + 4000,
                              study = c("study3"))

dat3_pid_policy$vignette_pid <- dplyr::recode(dat3_pid_policy$vignette_pid, 
                                              "a Democrat" = "D", "a Republican" = "R", "a political Independent" = "I")

dat3_pid_policy$respondent_pid <- dplyr::recode(dat3_pid_policy$respondent_pid, 
                                                "-1" = "D", "1" = "R")

# Merge into one data frame
dat_pid_policy <- rbind(dat1_pid_policy, dat2_pid_policy, dat3_pid_policy)

# Keep partisan vignette profiles
dat_pid_policy <- dat_pid_policy[dat_pid_policy$vignette_pid %in% c("D", "R"),]


#----------------------------------------------
# New Analysis: Code loyalty status for each vignette
#----------------------------------------------

dat_pid_policy$loyal <- NA

for(i in 1:nrow(dat_pid_policy)) {
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("should always be able to obtain an abortion", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 1
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("rape, incest, or when", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 0 
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("abortion should never be permitted", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 0
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("same sex couples should be", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 1
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("same sex couples should not be", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 0
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("the federal government should make it more", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 1
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("keep rules for buying guns about the same", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- NA
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("easier for people to buy a gun", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 0
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("become citizens", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 1
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("immigrants who were brought to the U.S. illegally as children should be allowed to live and work in the U.S", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- NA
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("under strict regulation", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- NA # Note: also excluded from preference measure - should be excluded here
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("illegally as children should be sent back", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 0
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("welfare programs should be increased", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 1
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("welfare programs should be kept the same", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- NA
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("welfare programs should be maintained", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- NA
  if(dat_pid_policy$vignette_pid[i] == "D" & grepl("welfare programs should be decreased", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 0
  
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("should always be able to obtain an abortion", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 0
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("rape, incest, or when", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 1 
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("abortion should never be permitted", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 1
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("same sex couples should be", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 0
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("same sex couples should not be", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 1
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("the federal government should make it more", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 0
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("keep rules for buying guns about the same", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- NA
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("easier for people to buy a gun", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 1
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("become citizens", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 0
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("immigrants who were brought to the U.S. illegally as children should be allowed to live and work in the U.S", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- NA
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("under strict regulation", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- NA # Note: also excluded from preference measure - should be excluded here
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("illegally as children should be sent back", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 1
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("welfare programs should be increased", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 0
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("welfare programs should be kept the same", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- NA
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("welfare programs should be maintained", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- NA
  if(dat_pid_policy$vignette_pid[i] == "R" & grepl("welfare programs should be decreased", dat_pid_policy$policy[i])) dat_pid_policy$loyal[i] <- 1
}

# Check coding: 
# table(dat_pid_policy$loyal, dat_pid_policy$vignette_pid, dat_pid_policy$policy, useNA = "ifany")


#----------------------------------------------
# New Analysis: Count party-consistent views for each respondent
#----------------------------------------------

# Note that there are multiple reasonable approaches to this coding.
# This coding is consistent with loyalty measure above, which excludes most moderate 
# positions, but codes abortion restrictions with exceptions as R aligned policies

dat_pid_policy$party_consistent_view <- NA

for(i in 1:nrow(dat_pid_policy)) {
  if(dat_pid_policy[i, "study"] == "study1"){
    if(dat_pid_policy[i, "respondent_pid"] == "D") dat_pid_policy$party_consistent_view[i] <- sum(dat_pid_policy$abort[i] == "always", dat_pid_policy$welfare[i] == "inc")/2 
    if(dat_pid_policy[i, "respondent_pid"] == "R") dat_pid_policy$party_consistent_view[i] <- sum(dat_pid_policy$abort[i] %in% c("sometimes", "never"), dat_pid_policy$welfare[i] == "dec")/2
  }
  if(dat_pid_policy[i, "study"] %in% c("study2", "study3")){
    if(dat_pid_policy[i, "respondent_pid"] == "D"){
      dat_pid_policy$party_consistent_view[i] <- sum(dat_pid_policy$abort[i] == "always", dat_pid_policy$adopt[i] == "yes", dat_pid_policy$gun[i] == "difficult", 
                                                     dat_pid_policy$imm[i] == "citizen", dat_pid_policy$welfare[i] == "inc")/5
    } 
    if(dat_pid_policy[i, "respondent_pid"] == "R"){
      dat_pid_policy$party_consistent_view[i] <- sum(dat_pid_policy$abort[i] %in% c("sometimes", "never"), dat_pid_policy$adopt[i] == "no", dat_pid_policy$gun[i] == "easier", 
                                                     dat_pid_policy$imm[i] == "deport", dat_pid_policy$welfare[i] == "dec")/5
    }                                                                                               
  }
}

#----------------------------------------------
# New Analysis: Replicate main result from Dias Lelkes re-analysis
# Table A7 (Policy Agreement versus Party Loyalty – Orr and Huber 2020)
#----------------------------------------------

# Vignettes with conflicting agreement and loyalty
dat_pid_policy_conflict <- dat_pid_policy[(dat_pid_policy$copar == 0 & dat_pid_policy$loyal == 0 & dat_pid_policy$copol == 0) |
                                            (dat_pid_policy$copar == 0 & dat_pid_policy$loyal == 1 & dat_pid_policy$copol == 1) |
                                            (dat_pid_policy$copar == 1 & dat_pid_policy$loyal == 0 & dat_pid_policy$copol == 1) |
                                            (dat_pid_policy$copar == 1 & dat_pid_policy$loyal == 1 & dat_pid_policy$copol == 0),]

dat_pid_policy_conflict <- dat_pid_policy_conflict[!is.na(dat_pid_policy_conflict$loyal),]

# Averages and sample sizes for Table A7
group_by(dat_pid_policy_conflict, copar, loyal, copol) %>% 
  summarize(ft = round(mean(FT, na.rm = T), 2),
            ft_se = round(sd(FT, na.rm = T)/sqrt(n()), 2),
            n = n())

# Differences for Table A7
lm_robust(FT ~ loyal, data = dat_pid_policy_conflict[dat_pid_policy_conflict$copar == 1,], clusters = ID)
lm_robust(FT ~ loyal, data = dat_pid_policy_conflict[dat_pid_policy_conflict$copar == 0,], clusters = ID)


#----------------------------------------------
# New Analysis: Replicate main result from Dias Lelkes re-analysis
# Figure A4 (Policy Agreement versus Party Loyalty by Respondent Party Alignment)
#----------------------------------------------

# Add labels for plot
dat_pid_policy_conflict$Copartisan <- recode(dat_pid_policy_conflict$copar, "1" = "Copartisan", "0" = "Outpartisan")

dat_pid_policy_conflict$Label_loyal <- NA_character_
dat_pid_policy_conflict$Label_loyal[dat_pid_policy_conflict$loyal == 1] <- "Loyal" 
dat_pid_policy_conflict$Label_loyal[dat_pid_policy_conflict$loyal == 0] <- "Disloyal" 
dat_pid_policy_conflict$Label_loyal <- factor(dat_pid_policy_conflict$Label_loyal, levels = c("Loyal", "Disloyal"))

dat_pid_policy_conflict$Label_agree <- NA_character_
dat_pid_policy_conflict$Label_agree[dat_pid_policy_conflict$copol == 0] <- "Disagree" 
dat_pid_policy_conflict$Label_agree[dat_pid_policy_conflict$copol == 1] <- "Agree" 
dat_pid_policy_conflict$Label_agree <- factor(dat_pid_policy_conflict$Label_agree, levels = c("Agree", "Disagree"))


# Figure A4
set.seed(100)
ggplot(data = dat_pid_policy_conflict[dat_pid_policy_conflict$party_consistent_view < 1,], 
       aes(x = party_consistent_view)) +
  geom_rug(aes(y = -1), alpha = 0.05, position = "jitter", size = 2) +
  geom_smooth(aes(y = FT, linetype = Label_agree, fill = Label_loyal), method = "lm", color = "black") +
  ylim(0, 100) +
  labs(linetype = "Agreement", fill = "Loyalty", x = "% Respondent policy views consistent with respondent party", y = "Warmth") +
  scale_linetype_manual(values = c(1, 3)) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  facet_wrap(~ Copartisan) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(linetype = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(linetype = 0)))


# 3.68% of respondents had fully consistent views (excluded from this analysis)
mean(dat_pid_policy_conflict$party_consistent_view == 1)

# Average consistency level of 39.6%
mean(dat_pid_policy_conflict$party_consistent_view)

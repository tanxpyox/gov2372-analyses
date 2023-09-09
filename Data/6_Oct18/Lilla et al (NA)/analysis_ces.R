#----------------------------------------------
# Replication Materials for Orr Fowler Huber 2023 AJPS
# Table A8
#----------------------------------------------


#----------------------------------------------
# Load Packages
#----------------------------------------------

#install.packages("dplyr")

library(dplyr)


#----------------------------------------------
# Load data
#----------------------------------------------

ces <- read.csv("data_ces.csv")


#----------------------------------------------
# Data processing 
#----------------------------------------------

ces <- rename(ces,
              "imm_respondent" = "CC20_331e",
              "epa_respondent" = "CC20_333c",
              "pol_imm_rep" = "YAL410_rule",
              "pol_epa_rep" = "YAL413_rule",
              "pol_imm_dem" = "YAL422_rule",
              "pol_epa_dem" = "YAL425_rule")

ces$pid3_lean <- NA
ces$pid3_lean[grepl("Democrat", ces$pid7)] <- "D"
ces$pid3_lean[grepl("Independent", ces$pid7)] <- "I"
ces$pid3_lean[grepl("Republican", ces$pid7)] <- "R"

ces$imm_respondentLC <- recode(ces$imm_respondent, .default = NA_character_,  
                                "Support" = "Con", "Oppose" = "Lib")
ces$epa_respondentLC <- recode(ces$epa_respondent, .default = NA_character_,  
                                "Support" = "Lib", "Oppose" = "Con")

ces$imm_align <- NA
ces$imm_align[ces$imm_respondentLC == "Lib"& ces$pid3_lean == "D"] <- 1
ces$imm_align[ces$imm_respondentLC == "Lib"& ces$pid3_lean == "R"] <- 0
ces$imm_align[ces$imm_respondentLC == "Con"& ces$pid3_lean == "D"] <- 0
ces$imm_align[ces$imm_respondentLC == "Con"& ces$pid3_lean == "R"] <- 1

ces$epa_align <- NA
ces$epa_align[ces$epa_respondentLC == "Lib"& ces$pid3_lean == "D"] <- 1
ces$epa_align[ces$epa_respondentLC == "Lib"& ces$pid3_lean == "R"] <- 0
ces$epa_align[ces$epa_respondentLC == "Con"& ces$pid3_lean == "D"] <- 0
ces$epa_align[ces$epa_respondentLC == "Con"& ces$pid3_lean == "R"] <- 1


#----------------------------------------------
# Table A8 (Awareness of alignment in 2020 CES)
#----------------------------------------------

group_by(ces[!is.na(ces$imm_align), ], imm_align) %>% 
  summarise(N = n(), 
            imm_gap = round(mean(pol_imm_rep > pol_imm_dem, na.rm = TRUE),2))

group_by(ces[!is.na(ces$epa_align), ], epa_align) %>% 
  summarise(N = n(), 
            epa_gap = round(mean(pol_epa_rep < pol_epa_dem, na.rm = TRUE),2))


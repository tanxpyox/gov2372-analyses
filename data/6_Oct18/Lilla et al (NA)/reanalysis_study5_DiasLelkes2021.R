#----------------------------------------------
# Replication Materials for Orr Fowler Huber 2023 AJPS
# Table A1, A11
#----------------------------------------------


#----------------------------------------------
# Load Packages
#----------------------------------------------

#install.packages("rio")
#install.packages("estimatr")
#install.packages("dplyr")
#install.packages("lme4")
#install.packages("mediation")

library(rio)
library(estimatr)
library(dplyr)
library(lme4)
library(mediation)


#----------------------------------------------
# Load data
#----------------------------------------------

study5_data <- import('study5_data.RData') 


#----------------------------------------------
# Table A1: Average Treatment Effects of Timing Conditions on Perceived Preferences
#----------------------------------------------

# Column 1
tidy(lm_robust(inParty_agree ~ time_condition, 
               data = study5_data[study5_data$info_condition == "Party" & study5_data$match_party == "Yes",]))

nobs(lm(inParty_agree ~ time_condition, 
        data = study5_data[study5_data$info_condition == "Party" & study5_data$match_party == "Yes",]))

# Column 2
tidy(lm_robust(inParty_agree ~ time_condition, 
               data = study5_data[study5_data$info_condition == "Party" & study5_data$match_party == "No",]))

nobs(lm(inParty_agree ~ time_condition, 
        data = study5_data[study5_data$info_condition == "Party" & study5_data$match_party == "No",]))


#----------------------------------------------
# Table A11: Mediation of Warmth Effects
#----------------------------------------------

# The following code is copied directly from Dias and Lelkes 2021
# Lines 34 to 55 of "study5_analysis.R" 
# https://doi.org/10.7910/DVN/JHJJW0

## Filtering data
mediate_df <- study5_data %>% 
  subset(time_condition != 'Time Pressure') %>%
  dplyr::select(therm, match_party, policy_type, inParty_agree, vignette_num, id) %>%
  na.omit()

## Creating models for the mediation analysis
mediate_dv_model <- lmer(therm ~ match_party*policy_type + inParty_agree + as.factor(vignette_num) + (1|id), 
                         data = mediate_df)
mediate_dv_model %>% summary()

mediate_mediator_model <- lmer(inParty_agree ~ match_party*policy_type + as.factor(vignette_num) + (1|id), 
                               data = mediate_df)
mediate_mediator_model %>% summary()

## Running the mediation analysis (Warning: This takes a while)
set.seed(12620)

mediate_result <- mediate(mediate_mediator_model, mediate_dv_model, 
                          treat = 'policy_type',
                          mediator = 'inParty_agree')
mediate_result %>% summary()


# The following code is new analysis from Orr Fowler Huber 2023
# The only modification is in which treatment groups are compared

set.seed(12620)

mediate_result_new <- mediate(mediate_mediator_model, mediate_dv_model, 
                              treat = 'policy_type', 
                              mediator = 'inParty_agree',
                              treat.value = "Unbranded", control.value = "Disloyal")
mediate_result_new %>% summary()



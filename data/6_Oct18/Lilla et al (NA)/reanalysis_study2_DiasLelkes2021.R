#----------------------------------------------
# Replication Materials for Orr Fowler Huber 2023 AJPS
# Table 1, A5, A6
#----------------------------------------------


#----------------------------------------------
# Load Packages
#----------------------------------------------

#install.packages("dplyr")

library(dplyr)


#----------------------------------------------
# Load data
#----------------------------------------------

load('study2_data.RData')


#----------------------------------------------
# Data processing
#----------------------------------------------

datDL <- study2_data[study2_data$info_condition %in% c("Party + Policy", "Policy + Party"),]

datDL_conflict <- datDL[(datDL$match_party == "No" & datDL$policy_type == "Disloyal" & datDL$match_policy == "No") |
                          (datDL$match_party == "No" & datDL$policy_type == "Loyal" & datDL$match_policy == "Yes") |
                          (datDL$match_party == "Yes" & datDL$policy_type == "Disloyal" & datDL$match_policy == "Yes") |
                          (datDL$match_party == "Yes" & datDL$policy_type == "Loyal" & datDL$match_policy == "No"),]

datDL_noconflict <- datDL[(datDL$match_party == "Yes" & datDL$policy_type == "Disloyal" & datDL$match_policy == "No") |
                            (datDL$match_party == "Yes" & datDL$policy_type == "Loyal" & datDL$match_policy == "Yes") |
                            (datDL$match_party == "No" & datDL$policy_type == "Disloyal" & datDL$match_policy == "Yes") |
                            (datDL$match_party == "No" & datDL$policy_type == "Loyal" & datDL$match_policy == "No"),]

#----------------------------------------------
# Table 1: Policy Agreement versus Party Loyalty
#----------------------------------------------

# Averages and SEs by condition

group_by(datDL_conflict[datDL_conflict$policy_type != "Unbranded",], 
         match_party, policy_type, match_policy) %>% 
  summarize(ft = round(mean(therm, na.rm = T), 2),
            ftse = round(sd(therm, na.rm = T)/sqrt(n()), 2),
            marry = round(mean(as.numeric(child_marry, na.rm = T)), 2),
            marryse = round(sd(as.numeric(child_marry, na.rm = T))/sqrt(n()), 2),
            neighbor = round(mean(as.numeric(be_neighbor, na.rm = T)), 2),
            neighborse = round(sd(as.numeric(be_neighbor, na.rm = T))/sqrt(n()), 2),
            n = n()) %>%
  as.data.frame()

# Differences across conditions

summary(lm(therm ~ policy_type, data = datDL_conflict[datDL_conflict$match_party == "Yes",]))
summary(lm(as.numeric(child_marry) ~ policy_type, data = datDL_conflict[datDL_conflict$match_party == "Yes",]))
summary(lm(as.numeric(be_neighbor) ~ policy_type, data = datDL_conflict[datDL_conflict$match_party == "Yes",]))

summary(lm(therm ~ policy_type, data = datDL_conflict[datDL_conflict$match_party == "No",]))
summary(lm(as.numeric(child_marry) ~ policy_type, data = datDL_conflict[datDL_conflict$match_party == "No",]))
summary(lm(as.numeric(be_neighbor) ~ policy_type, data = datDL_conflict[datDL_conflict$match_party == "No",]))


#----------------------------------------------
# Table A5: Aligned Policy Agreement and Party Loyalty
#----------------------------------------------

# Averages and SEs by condition

group_by(datDL_noconflict[datDL_noconflict$policy_type != "Unbranded",], 
         match_party, policy_type, match_policy) %>% 
  summarize(ft = round(mean(therm, na.rm = T), 2),
            ftse = round(sd(therm, na.rm = T)/sqrt(n()), 2),
            marry = round(mean(as.numeric(child_marry, na.rm = T)), 2),
            marryse = round(sd(as.numeric(child_marry, na.rm = T))/sqrt(n()), 2),
            neighbor = round(mean(as.numeric(be_neighbor, na.rm = T)), 2),
            neighborse = round(sd(as.numeric(be_neighbor, na.rm = T))/sqrt(n()), 2),
            n = n()) %>%
  as.data.frame()

# Differences across conditions

summary(lm(therm ~ policy_type, data = datDL_noconflict[datDL_noconflict$match_party == "Yes",]))
summary(lm(as.numeric(child_marry) ~ policy_type, data = datDL_noconflict[datDL_noconflict$match_party == "Yes",]))
summary(lm(as.numeric(be_neighbor) ~ policy_type, data = datDL_noconflict[datDL_noconflict$match_party == "Yes",]))

summary(lm(therm ~ policy_type, data = datDL_noconflict[datDL_noconflict$match_party == "No",]))
summary(lm(as.numeric(child_marry) ~ policy_type, data = datDL_noconflict[datDL_noconflict$match_party == "No",]))
summary(lm(as.numeric(be_neighbor) ~ policy_type, data = datDL_noconflict[datDL_noconflict$match_party == "No",]))


#----------------------------------------------
# "Back of the envelope" party vs loyalty analysis
# Main text following Table 1
#----------------------------------------------

solve(rbind(c(1, 1),   # A + L = 43.66
            c(1, -1)), # A - L = 15.62
      c(43.66, 15.62))


#----------------------------------------------
# Table A6: Policy Agreement versus Party Loyalty Among Strong Partisans
#----------------------------------------------

# Averages and SEs by condition

group_by(datDL_conflict[datDL_conflict$policy_type != "Unbranded" & grepl("Strong", datDL_conflict$pid6),], 
         match_party, policy_type, match_policy) %>% 
  summarize(ft = round(mean(therm, na.rm = T), 2),
            ftse = round(sd(therm, na.rm = T)/sqrt(n()), 2),
            marry = round(mean(as.numeric(child_marry, na.rm = T)), 2),
            marryse = round(sd(as.numeric(child_marry, na.rm = T))/sqrt(n()), 2),
            neighbor = round(mean(as.numeric(be_neighbor, na.rm = T)), 2),
            neighborse = round(sd(as.numeric(be_neighbor, na.rm = T))/sqrt(n()), 2),
            n = n()) %>%
  as.data.frame()

# Differences across conditions

summary(lm(therm ~ policy_type, data = datDL_conflict[datDL_conflict$match_party == "Yes" & grepl("Strong", datDL_conflict$pid6),]))
summary(lm(as.numeric(child_marry) ~ policy_type, data = datDL_conflict[datDL_conflict$match_party == "Yes" & grepl("Strong", datDL_conflict$pid6),]))
summary(lm(as.numeric(be_neighbor) ~ policy_type, data = datDL_conflict[datDL_conflict$match_party == "Yes" & grepl("Strong", datDL_conflict$pid6),]))

summary(lm(therm ~ policy_type, data = datDL_conflict[datDL_conflict$match_party == "No" & grepl("Strong", datDL_conflict$pid6),]))
summary(lm(as.numeric(child_marry) ~ policy_type, data = datDL_conflict[datDL_conflict$match_party == "No" & grepl("Strong", datDL_conflict$pid6),]))
summary(lm(as.numeric(be_neighbor) ~ policy_type, data = datDL_conflict[datDL_conflict$match_party == "No" & grepl("Strong", datDL_conflict$pid6),]))


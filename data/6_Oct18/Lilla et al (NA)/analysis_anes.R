#----------------------------------------------
# Replication Materials for Orr Fowler Huber 2023 AJPS
# Table A10 (A9) 
#----------------------------------------------


#----------------------------------------------
# Load Packages
#----------------------------------------------

#install.packages("dplyr")
#install.packages("estimatr")

library(dplyr)
library(estimatr)


#----------------------------------------------
# Load data
#----------------------------------------------

anes_raw <- read.csv("anes_timeseries_2020_csv_20220210.csv", as.is = TRUE)


#----------------------------------------------
# Data processing for ANES
#----------------------------------------------

# Select variables

anes <- anes_raw[, c("V201231x", # Pre party id summary
                     "V201337", # Pre importance of abortion issue
                     "V201336", # Pre abortion
                     "V201342x", # Pre pleased with supreme court reducing access
                     "V202333", # Post importance of issue of climate change
                     "V202336x", # Post favor regulation of business with high emissions linked to climate change
                     "V202338", # Post importance of gun access
                     "V202337", # Post gun access
                     "V202341x", # Post background checks
                     "V202344x", # Post assault rifle ban
                     "V202347x")] # Post assault rifle mandatory buy back

# Rename variables

anes <- dplyr::rename(anes, 
                      "pid" = "V201231x",
                      "abort_allow_import_raw" = "V201337",
                      "abort_allow" = "V201336",
                      "abort_court" = "V201342x",
                      "climate_import_raw" = "V202333" ,
                      "climate_regulate" = "V202336x",
                      "gun_access_import_raw" = "V202338",
                      "gun_access" = "V202337",
                      "gun_background" = "V202341x",
                      "gun_ban" = "V202344x",
                      "gun_buyback" = "V202347x")

# Recode new variables

# Two level pid

anes$pid2 <- recode(anes$pid, .default = NA_character_,
                    "1" = "D", "2" = "D", "3" = "D", "5" = "R", "6" = "R", "7" = "R")


# Importance

anes$gun_access_import <- recode(anes$gun_access_import_raw, .default = NA_real_,
                                 "1" = 1, "2" = 0.75, "3" = 0.5, "4" = 0.25, "5" = 0)

anes$abort_allow_import <- recode(anes$abort_allow_import_raw, .default = NA_real_,
                                  "5" = 1, "4" = 0.75, "3" = 0.5, "2" = 0.25, "1" = 0)

anes$climate_import <- recode(anes$climate_import_raw, .default = NA_real_,
                              "5" = 1, "4" = 0.75, "3" = 0.5, "2" = 0.25, "1" = 0)

anes$mean_import <- rowMeans(anes[, c("abort_allow_import", "gun_access_import", "climate_import")])


# Pid / policy alignment (Coding rules from Table A9)

# Note: To test robustness to excluding midpoints, comment out all lines that 
# assign 0.5 alignment scores

# Note: Abortion positions can be coded in multiple ways
# Never + rape/incest/life = R, always = D, clear need = NA, other = NA
anes$abort_allow_align <- NA
anes$abort_allow_align[anes$pid2 == "D" & anes$abort_allow == "4"] <- 1
anes$abort_allow_align[anes$pid2 == "R" & anes$abort_allow == "4"] <- 0
anes$abort_allow_align[anes$pid2 == "D" & anes$abort_allow %in% c("1", "2")] <- 0
anes$abort_allow_align[anes$pid2 == "R" & anes$abort_allow %in% c("1", "2")] <- 1
anes$abort_allow_align[anes$abort_allow == "3"] <- 0.5
# Check coding: 
# table(anes$abort_allow, anes$pid2, anes$abort_allow_align)

anes$abort_court_align <- NA
anes$abort_court_align[anes$pid2 == "D" & anes$abort_court %in% c("5", "6", "7")] <- 1
anes$abort_court_align[anes$pid2 == "R" & anes$abort_court %in% c("5", "6", "7")] <- 0
anes$abort_court_align[anes$pid2 == "D" & anes$abort_court %in% c("1", "2", "3")] <- 0
anes$abort_court_align[anes$pid2 == "R" & anes$abort_court %in% c("1", "2", "3")] <- 1
anes$abort_court_align[anes$abort_court == "4"] <- 0.5
# Check coding: 
# table(anes$abort_court, anes$pid2, anes$abort_court_align)

anes$climate_regulate_align <- NA
anes$climate_regulate_align[anes$pid2 == "D" & anes$climate_regulate %in% c("5", "6", "7")] <- 0
anes$climate_regulate_align[anes$pid2 == "R" & anes$climate_regulate %in% c("5", "6", "7")] <- 1
anes$climate_regulate_align[anes$pid2 == "D" & anes$climate_regulate %in% c("1", "2", "3")] <- 1
anes$climate_regulate_align[anes$pid2 == "R" & anes$climate_regulate %in% c("1", "2", "3")] <- 0
anes$climate_regulate_align[anes$climate_regulate == "4"] <- 0.5
# Check coding: 
# table(anes$climate_regulate, anes$pid2, anes$climate_regulate_align)

anes$gun_access_align <- NA
anes$gun_access_align[anes$pid2 == "D" & anes$gun_access %in% c("2", "3")] <- 0
anes$gun_access_align[anes$pid2 == "R" & anes$gun_access %in% c("2", "3")] <- 1
anes$gun_access_align[anes$pid2 == "D" & anes$gun_access %in% c("1")] <- 1
anes$gun_access_align[anes$pid2 == "R" & anes$gun_access %in% c("1")] <- 0
# Check coding: 
# table(anes$gun_access, anes$pid2, anes$gun_access_align)

anes$gun_background_align <- NA
anes$gun_background_align[anes$pid2 == "D" & anes$gun_background %in% c("5", "6", "7")] <- 0
anes$gun_background_align[anes$pid2 == "R" & anes$gun_background %in% c("5", "6", "7")] <- 1
anes$gun_background_align[anes$pid2 == "D" & anes$gun_background %in% c("1", "2", "3")] <- 1
anes$gun_background_align[anes$pid2 == "R" & anes$gun_background %in% c("1", "2", "3")] <- 0
anes$gun_background_align[anes$gun_background == "4"] <- 0.5
# Check coding: 
# table(anes$gun_background, anes$pid2, anes$gun_background_align)

anes$gun_ban_align <- NA
anes$gun_ban_align[anes$pid2 == "D" & anes$gun_ban %in% c("5", "6", "7")] <- 0
anes$gun_ban_align[anes$pid2 == "R" & anes$gun_ban %in% c("5", "6", "7")] <- 1
anes$gun_ban_align[anes$pid2 == "D" & anes$gun_ban %in% c("1", "2", "3")] <- 1
anes$gun_ban_align[anes$pid2 == "R" & anes$gun_ban %in% c("1", "2", "3")] <- 0
anes$gun_ban_align[anes$gun_ban == "4"] <- 0.5
# Check coding: 
# table(anes$gun_ban, anes$pid2, anes$gun_ban_align)

anes$gun_buyback_align <- NA
anes$gun_buyback_align[anes$pid2 == "D" & anes$gun_buyback %in% c("5", "6", "7")] <- 0
anes$gun_buyback_align[anes$pid2 == "R" & anes$gun_buyback %in% c("5", "6", "7")] <- 1
anes$gun_buyback_align[anes$pid2 == "D" & anes$gun_buyback %in% c("1", "2", "3")] <- 1
anes$gun_buyback_align[anes$pid2 == "R" & anes$gun_buyback %in% c("1", "2", "3")] <- 0
anes$gun_buyback_align[anes$gun_buyback == "4"] <- 0.5
# Check coding: 
# table(anes$gun_ban, anes$pid2, anes$gun_ban_align)

anes$mean_align <- rowMeans(anes[, grepl("align", names(anes))]) # If not using midpoint, remove NAs


#----------------------------------------------
# Table A10 (Issue Importance and Alignment in 2020 ANES)
#----------------------------------------------

# Column 1: Alignment / importance association

round(tidy(lm_robust(abort_allow_align ~ abort_allow_import, data = anes))[2, 2:3], 2)
round(tidy(lm_robust(abort_court_align ~ abort_allow_import, data = anes))[2, 2:3], 2)
round(tidy(lm_robust(gun_access_align ~ gun_access_import, data = anes))[2, 2:3], 2)
round(tidy(lm_robust(gun_background_align ~ gun_access_import, data = anes))[2, 2:3], 2)
round(tidy(lm_robust(gun_ban_align ~ gun_access_import, data = anes))[2, 2:3], 2)
round(tidy(lm_robust(gun_buyback_align ~ gun_access_import, data = anes))[2, 2:3], 2)
round(tidy(lm_robust(climate_regulate_align ~ climate_import, data = anes))[2, 2:3], 2)

# Column 2: Relative alignment / relative importance association

round(tidy(lm_robust(abort_allow_align - mean_align ~ I(abort_allow_import - mean_import), data = anes))[2, 2:3], 2)
round(tidy(lm_robust(abort_court_align - mean_align ~ I(abort_allow_import - mean_import), data = anes))[2, 2:3], 2)
round(tidy(lm_robust(gun_access_align - mean_align ~ I(gun_access_import - mean_import), data = anes))[2, 2:3], 2)
round(tidy(lm_robust(gun_background_align - mean_align ~ I(gun_access_import - mean_import), data = anes))[2, 2:3], 2)
round(tidy(lm_robust(gun_ban_align - mean_align ~ I(gun_access_import - mean_import), data = anes))[2, 2:3], 2)
round(tidy(lm_robust(gun_buyback_align - mean_align ~ I(gun_access_import - mean_import), data = anes))[2, 2:3], 2)
round(tidy(lm_robust(climate_regulate_align - mean_align ~ I(climate_import - mean_import), data = anes))[2, 2:3], 2)

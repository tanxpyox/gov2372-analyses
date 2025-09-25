#----------------------------------------------
# Replication Materials for Orr Fowler Huber 2023 AJPS
# Figure 1, 2, A1, A2, A3
# Table A2, A3, A4
#----------------------------------------------


#----------------------------------------------
# Load Packages
#----------------------------------------------

# install.packages("dplyr")
# install.packages("estimatr")
# install.packages("reshape2")
# install.packages("ggplot2")

library(dplyr)
library(estimatr)
library(reshape2)
library(ggplot2)


#----------------------------------------------
# Load data
#----------------------------------------------

dat <- read.csv("data_lucid.csv", as.is = TRUE)

# Extract study 1 participants and variables 
dat1 <- dat[dat$treat_vignette2 != "" & dat$respondent_pid %in% c("Dem", "Rep"), 
            c("infer_engaged2", "infer_agree2", "infer_ideo2", "vignette_warmth_1", 
              "treat_vignette2", "vignette2", "vignette_pid2", "respondent_pid",
              "gender", "age", "FL_149_DO_vignette2_infer")]

# Extract study 1 participants and variables 
dat2 <- dat[dat$treat_vignette != "",
            c("infer_dem", "infer_rep", "infer_engaged", "infer_agree", 
              "infer_black", "infer_religious", "infer_abort", "infer_welfare", 
              "infer_pol", "treat_vignette", "vignette", "vignette_pid", "respondent_pid",
              "treat_vignette_loyal", "rand_policy_branded", "rand_policy_unbranded")]

#----------------------------------------------
# Data processing for experiment 1
# Inferences based on party
#----------------------------------------------

dat1$infer_engaged2 <- recode(dat1$infer_engaged2, .default = NA_real_,
                              "A great deal" = 3, "Somewhat" = 2,
                              "A little bit" = 1, "None at all" = 0)/3

dat1$infer_agree2 <- recode(dat1$infer_agree2, .default = NA_real_,
                            "Always agree" = 4, "Usually agree" = 3, "Sometimes agree, sometimes disagree" = 2, 
                            "Usually disagree" = 1, "Always disagree" = 0)/4

dat1$infer_ideo2 <- recode(dat1$infer_ideo2, .default = NA_real_,
                           "Very liberal" = 0, "Somewhat liberal" = 1, "Moderate" = 2, 
                           "Somewhat conservative" = 3, "Very conservative" = 4)/4

dat1$infer_ideo_consistent <- NA
dat1$infer_ideo_consistent[dat1$vignette_pid2 == "Republican"] <- dat1$infer_ideo2[dat1$vignette_pid2 == "Republican"]
dat1$infer_ideo_consistent[dat1$vignette_pid2 == "Democrat"] <- 1 - dat1$infer_ideo2[dat1$vignette_pid2 == "Democrat"]

#----------------------------------------------
# Table A2 (Sample Demographics and Balance)
#----------------------------------------------

# Partisanship by treatment group
table(dat1$treat_vignette2, dat1$respondent_pid)
round(table(dat1$treat_vignette2, dat1$respondent_pid)/rowSums(table(dat1$treat_vignette2, dat1$respondent_pid)), 2)

# Gender by treatment group
table(dat1$treat_vignette2, dat1$gender)
round(table(dat1$treat_vignette2, dat1$gender)/rowSums(table(dat1$treat_vignette2, dat1$gender)), 2)

# Age by treatment group
group_by(dat1, treat_vignette2) %>% 
  summarise(age = median(age, na.rm = T))

# Total sample size (partisans only)
table(dat1$treat_vignette2)


#----------------------------------------------
# Table A3 (Average Treatment Effects on All Outcomes Measured)
#----------------------------------------------

lm_ft <- tidy(lm_robust(vignette_warmth_1 ~ treat_vignette2, data = dat1))
lm_ag <- tidy(lm_robust(infer_agree2 ~ treat_vignette2, data = dat1))
lm_en <- tidy(lm_robust(infer_engaged2 ~ treat_vignette2, data = dat1))
lm_id <- tidy(lm_robust(infer_ideo_consistent ~ treat_vignette2, data = dat1))

# Coefficients and SEs
apply(lm_ft[c(1, 5, 2, 3, 4), c(2, 3)], 2, round, 2)
apply(lm_ag[c(1, 5, 2, 3, 4), c(2, 3)], 2, round, 3)
apply(lm_en[c(1, 5, 2, 3, 4), c(2, 3)], 2, round, 3)
apply(lm_id[c(1, 5, 2, 3, 4), c(2, 3)], 2, round, 3)

# Sample size
nobs(lm_robust(vignette_warmth_1 ~ treat_vignette2, data = dat1))
nobs(lm_robust(infer_agree2 ~ treat_vignette2, data = dat1))
nobs(lm_robust(infer_engaged2 ~ treat_vignette2, data = dat1))
nobs(lm_robust(infer_ideo_consistent ~ treat_vignette2, data = dat1))


#----------------------------------------------
# Figure 1 (Effects of Treatment Conditions on Subjects’ Perceptions)
#----------------------------------------------

coeff_figure1 <- data.frame(beta = c(lm_ft[-1, 2]/101, lm_ag[-1, 2]),
                            se = c(lm_ft[-1, 3]/101, lm_ag[-1, 3]),
                            Treatment = rep(factor(c("Strong", "Strong + Extreme Active", "Strong + Moderate Inactive", "Weak"), 
                                                   levels = c("Strong + Moderate Inactive", "Strong + Extreme Active", "Strong", "Weak")), 2),
                            Outcome = rep(c("Warmth", "Agree"), each = 4))

png("Figure1.png", width = 7, height = 4.5, res = 200, units = "in")
ggplot(coeff_figure1, aes(y = beta, x = Treatment, color = Outcome)) +
  geom_hline(yintercept = 0, color = "darkgrey", linetype = "dashed") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = beta - 1.96*se, ymax = beta + 1.96*se), 
                width = 0.01, position = position_dodge(0.2)) +
  labs(y = "ATE relative to generic out-partisan") +
  ylim(-0.21, 0.1) +
  coord_flip() +
  scale_color_grey(start = 0.3, end = 0.7,
                   guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"))
dev.off()

#----------------------------------------------
# Display order effects 
#----------------------------------------------

tidy(lm_robust(vignette_warmth_1 ~ FL_149_DO_vignette2_infer, data = dat1))

tidy(lm_robust(vignette_warmth_1 ~ treat_vignette2*FL_149_DO_vignette2_infer, data = dat1))


#----------------------------------------------
# Data processing for experiment 2
# Dias and Lelkes Replication, inferences based on party and policy
#----------------------------------------------

# All "infer_" variables indicate guesses made about the person described in the vignette - outcomes scaled 0 to 1
dat2$infer_dem <- recode(dat2$infer_dem, .default = NA_real_,
                        "Extremely likely" = 6, "Moderately likely" = 5, "Slightly likely" = 4, 
                        "Neither likely nor unlikely" = 3, "Slightly unlikely" = 2,
                        "Moderately unlikely" = 1, "Extremely unlikely" = 0)/6

dat2$infer_rep <- recode(dat2$infer_rep, .default = NA_real_,
                        "Extremely likely" = 6, "Moderately likely" = 5, "Slightly likely" = 4, 
                        "Neither likely nor unlikely" = 3, "Slightly unlikely" = 2,
                        "Moderately unlikely" = 1, "Extremely unlikely" = 0)/6

dat2$infer_black <- recode(dat2$infer_black, .default = NA_real_,
                          "Extremely likely" = 6, "Moderately likely" = 5, "Slightly likely" = 4, 
                          "Neither likely nor unlikely" = 3, "Slightly unlikely" = 2,
                          "Moderately unlikely" = 1, "Extremely unlikely" = 0)/6

dat2$infer_religious <- recode(dat2$infer_religious, .default = NA_real_,
                              "Extremely likely" = 6, "Moderately likely" = 5, "Slightly likely" = 4, 
                              "Neither likely nor unlikely" = 3, "Slightly unlikely" = 2,
                              "Moderately unlikely" = 1, "Extremely unlikely" = 0)/6

dat2$infer_abort <- recode(dat2$infer_abort, .default = NA_real_,
                          "Extremely likely" = 6, "Moderately likely" = 5, "Slightly likely" = 4, 
                          "Neither likely nor unlikely" = 3, "Slightly unlikely" = 2,
                          "Moderately unlikely" = 1, "Extremely unlikely" = 0)/6

dat2$infer_welfare <- recode(dat2$infer_welfare, .default = NA_real_,
                            "Extremely likely" = 6, "Moderately likely" = 5, "Slightly likely" = 4, 
                            "Neither likely nor unlikely" = 3, "Slightly unlikely" = 2,
                            "Moderately unlikely" = 1, "Extremely unlikely" = 0)/6

dat2$infer_pol <- recode(dat2$infer_pol, .default = NA_real_,
                        "Extremely likely" = 6, "Moderately likely" = 5, "Slightly likely" = 4, 
                        "Neither likely nor unlikely" = 3, "Slightly unlikely" = 2,
                        "Moderately unlikely" = 1, "Extremely unlikely" = 0)/6

dat2$infer_engaged <- recode(dat2$infer_engaged, .default = NA_real_,
                            "A great deal" = 3, "Somewhat" = 2,
                            "A little bit" = 1, "None at all" = 0)/3

dat2$infer_agree <- recode(dat2$infer_agree, .default = NA_real_,
                          "Always agree" = 4, "Usually agree" = 3, "Sometimes agree, sometimes disagree" = 2, 
                          "Usually disagree" = 1, "Always disagree" = 0)/4

# Agreement reverse coded for respondents who are Republicans, NA for Independents
dat2$infer_agree_Dresp <- NA
dat2$infer_agree_Dresp[dat2$respondent_pid %in% "Dem"] <- dat2$infer_agree[dat2$respondent_pid %in% "Dem"]
dat2$infer_agree_Dresp[dat2$respondent_pid %in% "Rep"] <- 1 - dat2$infer_agree[dat2$respondent_pid %in% "Rep"]


# Which policy position appeared in each vignette
dat2$vignette_policy <- NA
dat2$vignette_policy[dat2$treat_vignette == "pid_branded" & grepl("not", dat2$rand_policy_branded)] <- "Adopt no"
dat2$vignette_policy[dat2$treat_vignette == "pid_branded" & dat2$rand_policy_branded == "same sex couples should be allowed to adopt children."] <- "Adopt yes"
dat2$vignette_policy[dat2$treat_vignette == "pid_branded" & grepl("easier", dat2$rand_policy_branded)] <- "Gun easier"
dat2$vignette_policy[dat2$treat_vignette == "pid_branded" & grepl("difficult", dat2$rand_policy_branded)] <- "Gun harder"
dat2$vignette_policy[dat2$treat_vignette == "pid_branded" & grepl("back", dat2$rand_policy_branded)] <- "Immigration deport"
dat2$vignette_policy[dat2$treat_vignette == "pid_branded" & grepl("citizens", dat2$rand_policy_branded)] <- "Immigration stay"
dat2$vignette_policy[dat2$treat_vignette == "pid_unbranded" & grepl("voluntarily", dat2$rand_policy_unbranded)] <- "Data sell"
dat2$vignette_policy[dat2$treat_vignette == "pid_unbranded" & grepl("selling", dat2$rand_policy_unbranded)] <- "Data protect"
dat2$vignette_policy[dat2$treat_vignette == "pid_unbranded" & grepl("danger", dat2$rand_policy_unbranded)] <-  "License require"
dat2$vignette_policy[dat2$treat_vignette == "pid_unbranded" & grepl("quality", dat2$rand_policy_unbranded)] <- "License unnecessary"
dat2$vignette_policy[dat2$treat_vignette == "pid_unbranded" & grepl("land if", dat2$rand_policy_unbranded)] <- "Eminent domain support"
dat2$vignette_policy[dat2$treat_vignette == "pid_unbranded" & grepl("never", dat2$rand_policy_unbranded)] <-   "Eminent domain oppose"
dat2$vignette_policy[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("voluntarily", dat2$rand_policy_unbranded)] <- "Data sell"
dat2$vignette_policy[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("selling", dat2$rand_policy_unbranded)] <- "Data protect"
dat2$vignette_policy[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("danger", dat2$rand_policy_unbranded)] <-  "License require"
dat2$vignette_policy[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("quality", dat2$rand_policy_unbranded)] <- "License unnecessary"
dat2$vignette_policy[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("land if", dat2$rand_policy_unbranded)] <- "Eminent domain support"
dat2$vignette_policy[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("never", dat2$rand_policy_unbranded)] <-   "Eminent domain oppose"

# Did respondent correctly identify the vignette's party?
dat2$correct_pid <- NA
dat2$correct_pid[dat2$vignette_pid == "Democrat"] <- c(dat2$infer_dem == 1)[dat2$vignette_pid == "Democrat"]
dat2$correct_pid[dat2$vignette_pid == "Republican"] <- c(dat2$infer_rep == 1)[dat2$vignette_pid == "Republican"]

# Did respondent correctly identify the vignette's policy position?
dat2$correct_pol <- NA
dat2$correct_pol[dat2$vignette_policy %in% "Adopt no"] <- c(dat2$infer_pol == 0)[dat2$vignette_policy %in% "Adopt no"]
dat2$correct_pol[dat2$vignette_policy %in% "Adopt yes"] <- c(dat2$infer_pol == 1)[dat2$vignette_policy %in% "Adopt yes"]
dat2$correct_pol[dat2$vignette_policy %in% "Gun easier"] <- c(dat2$infer_pol == 0)[dat2$vignette_policy %in% "Gun easier"]
dat2$correct_pol[dat2$vignette_policy %in% "Gun harder"] <- c(dat2$infer_pol == 1)[dat2$vignette_policy %in% "Gun harder"]
dat2$correct_pol[dat2$vignette_policy %in% "Immigration deport"] <- c(dat2$infer_pol == 1)[dat2$vignette_policy %in% "Immigration deport"]
dat2$correct_pol[dat2$vignette_policy %in% "Immigration stay"] <- c(dat2$infer_pol == 0)[dat2$vignette_policy %in% "Immigration stay"]
dat2$correct_pol[dat2$vignette_policy %in% "Data protect"] <- c(dat2$infer_pol == 1)[dat2$vignette_policy %in% "Data protect"]
dat2$correct_pol[dat2$vignette_policy %in% "Data sell"] <- c(dat2$infer_pol == 0)[dat2$vignette_policy %in% "Data sell"]
dat2$correct_pol[dat2$vignette_policy %in% "License require"] <- c(dat2$infer_pol == 1)[dat2$vignette_policy %in% "License require"]
dat2$correct_pol[dat2$vignette_policy %in% "License unnecessary"] <- c(dat2$infer_pol == 0)[dat2$vignette_policy %in% "License unnecessary"]
dat2$correct_pol[dat2$vignette_policy %in% "Eminent domain support"] <- c(dat2$infer_pol == 0)[dat2$vignette_policy %in% "Eminent domain support"]
dat2$correct_pol[dat2$vignette_policy %in% "Eminent domain oppose"] <- c(dat2$infer_pol == 1)[dat2$vignette_policy %in% "Eminent domain oppose"]


# Coding treatments

# Is the vignette policy conservative, liberal, or unbranded?
dat2$branded_ideo <- NA
dat2$branded_ideo[dat2$treat_vignette == "pid_branded" & 
                   grepl("not|easier|back", dat2$rand_policy_branded)] <- "Con"
dat2$branded_ideo[dat2$treat_vignette == "pid_branded" & 
                   grepl("difficult|eventually", dat2$rand_policy_branded)] <- "Lib"
dat2$branded_ideo[dat2$treat_vignette == "pid_branded" & 
                   dat2$rand_policy_branded == "same sex couples should be allowed to adopt children."] <- "Lib"
dat2$branded_ideo[dat2$treat_vignette == "pid_unbranded"] <- "Unbranded"

# Is the unbranded vignette policy a big-government or small-government policy?
dat2$unbranded_ideo <- NA
dat2$unbranded_ideo[dat2$treat_vignette == "pid_unbranded" & 
                     grepl("voluntarily|even|requiring", dat2$rand_policy_unbranded)] <- "Small gov"
dat2$unbranded_ideo[dat2$treat_vignette == "pid_unbranded" & 
                     grepl("prohibited|land if|danger", dat2$rand_policy_unbranded)] <- "Big gov"

# Is the vignette policy conservative (including small-gov) or liberal (including big-gov)?
dat2$policy_ideo <- dat2$branded_ideo
dat2$policy_ideo[dat2$unbranded_ideo == "Big gov"] <- "Lib"
dat2$policy_ideo[dat2$unbranded_ideo == "Small gov"] <- "Con"

# Remove "label" treatment status unless the respondent actually got a labeled policy
dat2$treat_vignette_loyal[dat2$treat_vignette != "pid_unbranded_labeled"] <- NA

# Combine vignette pid with policy ideology or label
dat2$treat_pid_ideo <- paste(dat2$vignette_pid, dat2$branded_ideo, dat2$treat_vignette_loyal)
dat2$treat_pid_ideo <- gsub("NA", "", dat2$treat_pid_ideo)
dat2$treat_pid_ideo <- trimws(gsub("  ", " ", dat2$treat_pid_ideo))
dat2$treat_pid_ideo <- factor(dat2$treat_pid_ideo, levels = c("Democrat Lib", "Democrat Like", "Democrat", "Democrat Unbranded",
                                                             "Democrat Unlike", "Democrat Con", "Republican Lib",
                                                             "Republican Unlike", "Republican", "Republican Unbranded", "Republican Like", "Republican Con"))

# Does the label imply a liberal or conservative leaning?
dat2$treat_label_ideo <- NA
dat2$treat_label_ideo[dat2$treat_pid_ideo %in% c("Republican Unlike", "Democrat Like")] <- "Lib"
dat2$treat_label_ideo[dat2$treat_pid_ideo %in% c("Democrat Unlike", "Republican Like")] <- "Con"

# Consistency between party and policy information
dat2$treat_ideo <- as.character(recode(dat2$treat_pid_ideo, 
                                       "Democrat Lib" = "Consistent", 
                                       "Democrat Like" = "Labeled consistent", 
                                       "Democrat" = "None",
                                       "Democrat Unbranded" = "Unbranded",
                                       "Democrat Unlike" = "Labeled inconsistent", 
                                       "Democrat Con" = "Inconsistent",
                                       "Republican Lib" = "Inconsistent",
                                       "Republican Unlike" = "Labeled inconsistent", 
                                       "Republican" = "None", 
                                       "Republican Unbranded" = "Unbranded",
                                       "Republican Like" = "Labeled consistent", 
                                       "Republican Con" = "Consistent"))
dat2$treat_ideo <- factor(dat2$treat_ideo, levels = c("None", "Consistent", "Labeled consistent", "Inconsistent", "Labeled inconsistent", "Unbranded"))


#----------------------------------------------
# Figure 2 (Effects of Branded and Unbranded Policy Positions on Other Perceptions)
#----------------------------------------------

# Models underlying figures 2 and A2 
pol_abo_lib <- tidy(lm_robust(I(1 - infer_abort) ~ policy_ideo, data = dat2[dat2$treat_vignette == "pid_branded",]))
pol_abo_lablib <- tidy(lm_robust(I(1 - infer_abort) ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled",]))
pol_abo_biggov <- tidy(lm_robust(I(1 - infer_abort) ~ policy_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded",]))
pol_abo_gun <- tidy(lm_robust(I(1 - infer_abort) ~ relevel(factor(vignette_policy), ref = "Gun easier"), data = dat2[dat2$treat_vignette == "pid_branded"& grepl("Gun", dat2$vignette_policy),]))
pol_abo_imm <- tidy(lm_robust(I(1 - infer_abort) ~ vignette_policy, data = dat2[dat2$treat_vignette == "pid_branded" & grepl("Immigration", dat2$vignette_policy),]))
pol_abo_ado <- tidy(lm_robust(I(1 - infer_abort) ~ relevel(factor(vignette_policy), ref = "Adopt no"), data = dat2[dat2$treat_vignette == "pid_branded" & grepl("Adopt", dat2$vignette_policy),]))
pol_abo_datlab <- tidy(lm_robust(I(1 - infer_abort) ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("Data", dat2$vignette_policy),]))
pol_abo_emilab <- tidy(lm_robust(I(1 - infer_abort) ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("Eminent", dat2$vignette_policy),]))
pol_abo_liclab <- tidy(lm_robust(I(1 - infer_abort) ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("License", dat2$vignette_policy),]))
pol_abo_dat <- tidy(lm_robust(I(1 - infer_abort) ~ relevel(factor(vignette_policy), ref = "Data sell"), data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("Data", dat2$vignette_policy),]))
pol_abo_emi <- tidy(lm_robust(I(1 - infer_abort) ~ vignette_policy, data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("Eminent", dat2$vignette_policy),]))
pol_abo_lic <- tidy(lm_robust(I(1 - infer_abort) ~ relevel(factor(vignette_policy), ref = "License unnecessary"), data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("License", dat2$vignette_policy),]))

pol_wel_lib <- tidy(lm_robust(infer_welfare ~ policy_ideo, data = dat2[dat2$treat_vignette == "pid_branded",]))
pol_wel_lablib <- tidy(lm_robust(infer_welfare ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled",]))
pol_wel_biggov <- tidy(lm_robust(infer_welfare ~ policy_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded",]))
pol_wel_gun <- tidy(lm_robust(infer_welfare ~ relevel(factor(vignette_policy), ref = "Gun easier"), data = dat2[dat2$treat_vignette == "pid_branded"& grepl("Gun", dat2$vignette_policy),]))
pol_wel_imm <- tidy(lm_robust(infer_welfare ~ vignette_policy, data = dat2[dat2$treat_vignette == "pid_branded" & grepl("Immigration", dat2$vignette_policy),]))
pol_wel_ado <- tidy(lm_robust(infer_welfare ~ relevel(factor(vignette_policy), ref = "Adopt no"), data = dat2[dat2$treat_vignette == "pid_branded" & grepl("Adopt", dat2$vignette_policy),]))
pol_wel_datlab <- tidy(lm_robust(infer_welfare ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("Data", dat2$vignette_policy),]))
pol_wel_emilab <- tidy(lm_robust(infer_welfare ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("Eminent", dat2$vignette_policy),]))
pol_wel_liclab <- tidy(lm_robust(infer_welfare ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("License", dat2$vignette_policy),]))
pol_wel_dat <- tidy(lm_robust(infer_welfare ~ relevel(factor(vignette_policy), ref = "Data sell"), data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("Data", dat2$vignette_policy),]))
pol_wel_emi <- tidy(lm_robust(infer_welfare ~ vignette_policy, data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("Eminent", dat2$vignette_policy),]))
pol_wel_lic <- tidy(lm_robust(infer_welfare ~ relevel(factor(vignette_policy), ref = "License unnecessary"), data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("License", dat2$vignette_policy),]))

pol_bla_lib <- tidy(lm_robust(infer_black ~ policy_ideo, data = dat2[dat2$treat_vignette == "pid_branded",]))
pol_bla_lablib <- tidy(lm_robust(infer_black ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled",]))
pol_bla_biggov <- tidy(lm_robust(infer_black ~ policy_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded",]))
pol_bla_gun <- tidy(lm_robust(infer_black ~ relevel(factor(vignette_policy), ref = "Gun easier"), data = dat2[dat2$treat_vignette == "pid_branded"& grepl("Gun", dat2$vignette_policy),]))
pol_bla_imm <- tidy(lm_robust(infer_black ~ vignette_policy, data = dat2[dat2$treat_vignette == "pid_branded" & grepl("Immigration", dat2$vignette_policy),]))
pol_bla_ado <- tidy(lm_robust(infer_black ~ relevel(factor(vignette_policy), ref = "Adopt no"), data = dat2[dat2$treat_vignette == "pid_branded" & grepl("Adopt", dat2$vignette_policy),]))
pol_bla_datlab <- tidy(lm_robust(infer_black ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("Data", dat2$vignette_policy),]))
pol_bla_emilab <- tidy(lm_robust(infer_black ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("Eminent", dat2$vignette_policy),]))
pol_bla_liclab <- tidy(lm_robust(infer_black ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("License", dat2$vignette_policy),]))
pol_bla_dat <- tidy(lm_robust(infer_black ~ relevel(factor(vignette_policy), ref = "Data sell"), data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("Data", dat2$vignette_policy),]))
pol_bla_emi <- tidy(lm_robust(infer_black ~ vignette_policy, data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("Eminent", dat2$vignette_policy),]))
pol_bla_lic <- tidy(lm_robust(infer_black ~ relevel(factor(vignette_policy), ref = "License unnecessary"), data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("License", dat2$vignette_policy),]))

pol_rel_lib <- tidy(lm_robust(I(1 - infer_religious) ~ policy_ideo, data = dat2[dat2$treat_vignette == "pid_branded",]))
pol_rel_lablib <- tidy(lm_robust(I(1 - infer_religious) ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled",]))
pol_rel_biggov <- tidy(lm_robust(I(1 - infer_religious) ~ policy_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded",]))
pol_rel_gun <- tidy(lm_robust(I(1 - infer_religious) ~ relevel(factor(vignette_policy), ref = "Gun easier"), data = dat2[dat2$treat_vignette == "pid_branded"& grepl("Gun", dat2$vignette_policy),]))
pol_rel_imm <- tidy(lm_robust(I(1 - infer_religious) ~ vignette_policy, data = dat2[dat2$treat_vignette == "pid_branded" & grepl("Immigration", dat2$vignette_policy),]))
pol_rel_ado <- tidy(lm_robust(I(1 - infer_religious) ~ relevel(factor(vignette_policy), ref = "Adopt no"), data = dat2[dat2$treat_vignette == "pid_branded" & grepl("Adopt", dat2$vignette_policy),]))
pol_rel_datlab <- tidy(lm_robust(I(1 - infer_religious) ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("Data", dat2$vignette_policy),]))
pol_rel_emilab <- tidy(lm_robust(I(1 - infer_religious) ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("Eminent", dat2$vignette_policy),]))
pol_rel_liclab <- tidy(lm_robust(I(1 - infer_religious) ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("License", dat2$vignette_policy),]))
pol_rel_dat <- tidy(lm_robust(I(1 - infer_religious) ~ relevel(factor(vignette_policy), ref = "Data sell"), data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("Data", dat2$vignette_policy),]))
pol_rel_emi <- tidy(lm_robust(I(1 - infer_religious) ~ vignette_policy, data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("Eminent", dat2$vignette_policy),]))
pol_rel_lic <- tidy(lm_robust(I(1 - infer_religious) ~ relevel(factor(vignette_policy), ref = "License unnecessary"), data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("License", dat2$vignette_policy),]))

pol_agr_lib <- tidy(lm_robust(infer_agree_Dresp ~ policy_ideo, data = dat2[dat2$treat_vignette == "pid_branded",]))
pol_agr_lablib <- tidy(lm_robust(infer_agree_Dresp ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled",]))
pol_agr_biggov <- tidy(lm_robust(infer_agree_Dresp ~ policy_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded",]))
pol_agr_gun <- tidy(lm_robust(infer_agree_Dresp ~ relevel(factor(vignette_policy), ref = "Gun easier"), data = dat2[dat2$treat_vignette == "pid_branded"& grepl("Gun", dat2$vignette_policy),]))
pol_agr_imm <- tidy(lm_robust(infer_agree_Dresp ~ vignette_policy, data = dat2[dat2$treat_vignette == "pid_branded" & grepl("Immigration", dat2$vignette_policy),]))
pol_agr_ado <- tidy(lm_robust(infer_agree_Dresp ~ relevel(factor(vignette_policy), ref = "Adopt no"), data = dat2[dat2$treat_vignette == "pid_branded" & grepl("Adopt", dat2$vignette_policy),]))
pol_agr_datlab <- tidy(lm_robust(infer_agree_Dresp ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("Data", dat2$vignette_policy),]))
pol_agr_emilab <- tidy(lm_robust(infer_agree_Dresp ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("Eminent", dat2$vignette_policy),]))
pol_agr_liclab <- tidy(lm_robust(infer_agree_Dresp ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("License", dat2$vignette_policy),]))
pol_agr_dat <- tidy(lm_robust(infer_agree_Dresp ~ relevel(factor(vignette_policy), ref = "Data sell"), data = dat2[dat2$treat_vignette == "pid_unbranded"& grepl("Data", dat2$vignette_policy),]))
pol_agr_emi <- tidy(lm_robust(infer_agree_Dresp ~ vignette_policy, data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("Eminent", dat2$vignette_policy),]))
pol_agr_lic <- tidy(lm_robust(infer_agree_Dresp ~ relevel(factor(vignette_policy), ref = "License unnecessary"), data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("License", dat2$vignette_policy),]))

pol_dem_lib <- tidy(lm_robust(infer_dem ~ policy_ideo, data = dat2[dat2$treat_vignette == "pid_branded",]))
pol_dem_lablib <- tidy(lm_robust(infer_dem ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled",]))
pol_dem_biggov <- tidy(lm_robust(infer_dem ~ policy_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded",]))
pol_dem_gun <- tidy(lm_robust(infer_dem ~ relevel(factor(vignette_policy), ref = "Gun easier"), data = dat2[dat2$treat_vignette == "pid_branded"& grepl("Gun", dat2$vignette_policy),]))
pol_dem_imm <- tidy(lm_robust(infer_dem ~ vignette_policy, data = dat2[dat2$treat_vignette == "pid_branded" & grepl("Immigration", dat2$vignette_policy),]))
pol_dem_ado <- tidy(lm_robust(infer_dem ~ relevel(factor(vignette_policy), ref = "Adopt no"), data = dat2[dat2$treat_vignette == "pid_branded" & grepl("Adopt", dat2$vignette_policy),]))
pol_dem_datlab <- tidy(lm_robust(infer_dem ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("Data", dat2$vignette_policy),]))
pol_dem_emilab <- tidy(lm_robust(infer_dem ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("Eminent", dat2$vignette_policy),]))
pol_dem_liclab <- tidy(lm_robust(infer_dem ~ treat_label_ideo, data = dat2[dat2$treat_vignette == "pid_unbranded_labeled" & grepl("License", dat2$vignette_policy),]))
pol_dem_dat <- tidy(lm_robust(infer_dem ~ relevel(factor(vignette_policy), ref = "Data sell"), data = dat2[dat2$treat_vignette == "pid_unbranded"& grepl("Data", dat2$vignette_policy),]))
pol_dem_emi <- tidy(lm_robust(infer_dem ~ vignette_policy, data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("Eminent", dat2$vignette_policy),]))
pol_dem_lic <- tidy(lm_robust(infer_dem ~ relevel(factor(vignette_policy), ref = "License unnecessary"), data = dat2[dat2$treat_vignette == "pid_unbranded" & grepl("License", dat2$vignette_policy),]))

png("Figure2.png", width = 8, height = 4.5, res = 200, units = "in")
data.frame(estimate = c(pol_abo_lib[2, 2], pol_abo_lablib[2, 2], pol_abo_biggov[2, 2],
                        pol_wel_lib[2, 2], pol_wel_lablib[2, 2], pol_wel_biggov[2, 2],
                        pol_agr_lib[2, 2], pol_agr_lablib[2, 2], pol_agr_biggov[2, 2],
                        pol_bla_lib[2, 2], pol_bla_lablib[2, 2], pol_bla_biggov[2, 2],
                        pol_rel_lib[2, 2], pol_rel_lablib[2, 2], pol_rel_biggov[2, 2],
                        pol_dem_lib[2, 2], pol_dem_lablib[2, 2], pol_dem_biggov[2, 2]),
           SE = c(pol_abo_lib[2, 3], pol_abo_lablib[2, 3], pol_abo_biggov[2, 3],
                  pol_wel_lib[2, 3], pol_wel_lablib[2, 3], pol_wel_biggov[2, 3],
                  pol_agr_lib[2, 3], pol_agr_lablib[2, 3], pol_agr_biggov[2, 3],
                  pol_bla_lib[2, 3], pol_bla_lablib[2, 3], pol_bla_biggov[2, 3],
                  pol_rel_lib[2, 3], pol_rel_lablib[2, 3], pol_rel_biggov[2, 3],
                  pol_dem_lib[2, 3], pol_dem_lablib[2, 3], pol_dem_biggov[2, 3]),
           trait = factor(rep(c("Allow Abortion", "Increase Welfare", "General Policy (Dis)agreement", "Black", "Not Religious", "Democrat"), each = 3),
                          levels = c("Allow Abortion", "Increase Welfare", "General Policy (Dis)agreement", "Black", "Not Religious", "Democrat")),
           Policy = factor(rep(c("Naturally branded liberal\n(vs naturally branded conservative)", 
                                 "Artificially branded liberal\n(vs artificially branded conservative)", 
                                 "Unbranded big-government\n(vs unbranded small-government)"), 6),
                           levels = c("Unbranded big-government\n(vs unbranded small-government)", 
                                      "Artificially branded liberal\n(vs artificially branded conservative)", 
                                      "Naturally branded liberal\n(vs naturally branded conservative)"))) %>% 
  ggplot(aes(x = Policy, y = estimate)) +
  geom_point() +
  facet_wrap(vars(trait)) +
  geom_errorbar(aes(ymin = estimate - 1.96*SE, ymax = estimate + 1.96*SE), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = "Effect of vignette policy on perceptions in the presence of known partisanship", x = "") +
  coord_flip() + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"))
dev.off()

#----------------------------------------------
# Figure A1 (Effect of Partisanship on Other Perceptions)
#----------------------------------------------

pid_abo_nop <- tidy(lm_robust(I(1 - infer_abort) ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid",]))
pid_abo_bra <- tidy(lm_robust(I(1 - infer_abort) ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_branded",]))
pid_abo_lab <- tidy(lm_robust(I(1 - infer_abort) ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_unbranded_labeled",]))
pid_abo_unb <- tidy(lm_robust(I(1 - infer_abort) ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_unbranded",]))

pid_wel_nop <- tidy(lm_robust(infer_welfare ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid",]))
pid_wel_bra <- tidy(lm_robust(infer_welfare ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_branded",]))
pid_wel_lab <- tidy(lm_robust(infer_welfare ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_unbranded_labeled",]))
pid_wel_unb <- tidy(lm_robust(infer_welfare ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_unbranded",]))

pid_bla_nop <- tidy(lm_robust(infer_black ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid",]))
pid_bla_bra <- tidy(lm_robust(infer_black ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_branded",]))
pid_bla_lab <- tidy(lm_robust(infer_black ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_unbranded_labeled",]))
pid_bla_unb <- tidy(lm_robust(infer_black ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_unbranded",]))

pid_rel_nop <- tidy(lm_robust(I(1 - infer_religious) ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid",]))
pid_rel_bra <- tidy(lm_robust(I(1 - infer_religious) ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_branded",]))
pid_rel_lab <- tidy(lm_robust(I(1 - infer_religious) ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_unbranded_labeled",]))
pid_rel_unb <- tidy(lm_robust(I(1 - infer_religious) ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_unbranded",]))

pid_agr_nop <- tidy(lm_robust(infer_agree_Dresp ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid",]))
pid_agr_bra <- tidy(lm_robust(infer_agree_Dresp ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_branded",]))
pid_agr_lab <- tidy(lm_robust(infer_agree_Dresp ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_unbranded_labeled",]))
pid_agr_unb <- tidy(lm_robust(infer_agree_Dresp ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_unbranded",]))

pid_dem_nop <- tidy(lm_robust(infer_dem ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid",]))
pid_dem_bra <- tidy(lm_robust(infer_dem ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_branded",]))
pid_dem_lab <- tidy(lm_robust(infer_dem ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_unbranded_labeled",]))
pid_dem_unb <- tidy(lm_robust(infer_dem ~ relevel(factor(vignette_pid), ref = "Republican"), data = dat2[dat2$treat_vignette == "pid_unbranded",]))

data.frame(estimate = c(pid_abo_nop[2, 2], pid_abo_bra[2, 2], pid_abo_lab[2, 2], pid_abo_unb[2, 2],
                        pid_wel_nop[2, 2], pid_wel_bra[2, 2], pid_wel_lab[2, 2], pid_wel_unb[2, 2],
                        pid_agr_nop[2, 2], pid_agr_bra[2, 2], pid_agr_lab[2, 2], pid_agr_unb[2, 2],
                        pid_bla_nop[2, 2], pid_bla_bra[2, 2], pid_bla_lab[2, 2], pid_bla_unb[2, 2],
                        pid_rel_nop[2, 2], pid_rel_bra[2, 2], pid_rel_lab[2, 2], pid_rel_unb[2, 2]),
SE = c(pid_abo_nop[2, 3], pid_abo_bra[2, 3], pid_abo_lab[2, 3], pid_abo_unb[2, 3],
       pid_wel_nop[2, 3], pid_wel_bra[2, 3], pid_wel_lab[2, 3], pid_wel_unb[2, 3],
       pid_agr_nop[2, 3], pid_agr_bra[2, 3], pid_agr_lab[2, 3], pid_agr_unb[2, 3],
       pid_bla_nop[2, 3], pid_bla_bra[2, 3], pid_bla_lab[2, 3], pid_bla_unb[2, 3],
       pid_rel_nop[2, 3], pid_rel_bra[2, 3], pid_rel_lab[2, 3], pid_rel_unb[2, 3]),
trait = factor(rep(c("Allow abortion", "Increase welfare", "General policy (dis)agreement", "Black", "Not Religious"), each = 4),
               levels = c("Allow abortion", "Increase welfare", "General policy (dis)agreement", "Black", "Not Religious")),
Policy = factor(rep(c("No policy", "Naturally branded policy", "Artificially branded policy", "Unbranded policy"), 5),
                levels = c("Unbranded policy", 
                           "Artificially branded policy", 
                           "Naturally branded policy", "No policy"))) %>% 
  ggplot(aes(x = Policy, y = estimate)) +
  geom_point() +
  facet_wrap(vars(trait)) +
  geom_errorbar(aes(ymin = estimate - 1.96*SE, ymax = estimate + 1.96*SE), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = "Effect of vignette partisanship on perceptions in the presence (or absence) of policy information", x = "") +
  coord_flip() + 
  theme_bw()


#----------------------------------------------
# Figure A2 (Effects of Specific Policy Positions on Other Perceptions)
#----------------------------------------------

data.frame(estimate = c(pol_abo_lib[2, 2], pol_abo_lablib[2, 2], pol_abo_biggov[2, 2], pol_abo_gun[2, 2], pol_abo_imm[2, 2], pol_abo_ado[2, 2], pol_abo_datlab[2, 2], pol_abo_emilab[2, 2], pol_dem_liclab[2, 2], pol_abo_dat[2, 2], pol_abo_emi[2, 2], pol_dem_lic[2, 2],
                        pol_wel_lib[2, 2], pol_wel_lablib[2, 2], pol_wel_biggov[2, 2], pol_wel_gun[2, 2], pol_wel_imm[2, 2], pol_wel_ado[2, 2], pol_wel_datlab[2, 2], pol_wel_emilab[2, 2], pol_dem_liclab[2, 2], pol_wel_dat[2, 2], pol_wel_emi[2, 2], pol_dem_lic[2, 2],
                        pol_agr_lib[2, 2], pol_agr_lablib[2, 2], pol_agr_biggov[2, 2], pol_agr_gun[2, 2], pol_agr_imm[2, 2], pol_agr_ado[2, 2], pol_agr_datlab[2, 2], pol_agr_emilab[2, 2], pol_dem_liclab[2, 2], pol_agr_dat[2, 2], pol_agr_emi[2, 2], pol_dem_lic[2, 2],
                        pol_bla_lib[2, 2], pol_bla_lablib[2, 2], pol_bla_biggov[2, 2], pol_bla_gun[2, 2], pol_bla_imm[2, 2], pol_bla_ado[2, 2], pol_bla_datlab[2, 2], pol_bla_emilab[2, 2], pol_dem_liclab[2, 2], pol_bla_dat[2, 2], pol_bla_emi[2, 2], pol_dem_lic[2, 2],
                        pol_rel_lib[2, 2], pol_rel_lablib[2, 2], pol_rel_biggov[2, 2], pol_rel_gun[2, 2], pol_rel_imm[2, 2], pol_rel_ado[2, 2], pol_rel_datlab[2, 2], pol_rel_emilab[2, 2], pol_dem_liclab[2, 2], pol_rel_dat[2, 2], pol_rel_emi[2, 2], pol_dem_lic[2, 2],
                        pol_dem_lib[2, 2], pol_dem_lablib[2, 2], pol_dem_biggov[2, 2], pol_dem_gun[2, 2], pol_dem_imm[2, 2], pol_dem_ado[2, 2], pol_dem_datlab[2, 2], pol_dem_emilab[2, 2], pol_dem_liclab[2, 2], pol_dem_dat[2, 2], pol_dem_emi[2, 2], pol_dem_lic[2, 2]),
           SE = c(pol_abo_lib[2, 3], pol_abo_lablib[2, 3], pol_abo_biggov[2, 3], pol_abo_gun[2, 3], pol_abo_imm[2, 3], pol_abo_ado[2, 3], pol_abo_datlab[2, 3], pol_abo_emilab[2, 3], pol_dem_liclab[2, 3], pol_abo_dat[2, 3], pol_abo_emi[2, 3], pol_dem_lic[2, 3],
                  pol_wel_lib[2, 3], pol_wel_lablib[2, 3], pol_wel_biggov[2, 3], pol_wel_gun[2, 3], pol_wel_imm[2, 3], pol_wel_ado[2, 3], pol_wel_datlab[2, 3], pol_wel_emilab[2, 3], pol_dem_liclab[2, 3], pol_wel_dat[2, 3], pol_wel_emi[2, 3], pol_dem_lic[2, 3],
                  pol_agr_lib[2, 3], pol_agr_lablib[2, 3], pol_agr_biggov[2, 3], pol_agr_gun[2, 3], pol_agr_imm[2, 3], pol_agr_ado[2, 3], pol_agr_datlab[2, 3], pol_agr_emilab[2, 3], pol_dem_liclab[2, 3], pol_agr_dat[2, 3], pol_agr_emi[2, 3], pol_dem_lic[2, 3],
                  pol_bla_lib[2, 3], pol_bla_lablib[2, 3], pol_bla_biggov[2, 3], pol_bla_gun[2, 3], pol_bla_imm[2, 3], pol_bla_ado[2, 3], pol_bla_datlab[2, 3], pol_bla_emilab[2, 3], pol_dem_liclab[2, 3], pol_bla_dat[2, 3], pol_bla_emi[2, 3], pol_dem_lic[2, 3],
                  pol_rel_lib[2, 3], pol_rel_lablib[2, 3], pol_rel_biggov[2, 3], pol_rel_gun[2, 3], pol_rel_imm[2, 3], pol_rel_ado[2, 3], pol_rel_datlab[2, 3], pol_rel_emilab[2, 3], pol_dem_liclab[2, 3], pol_rel_dat[2, 3], pol_rel_emi[2, 3], pol_dem_lic[2, 3],
                  pol_dem_lib[2, 3], pol_dem_lablib[2, 3], pol_dem_biggov[2, 3], pol_dem_gun[2, 3], pol_dem_imm[2, 3], pol_dem_ado[2, 3], pol_dem_datlab[2, 3], pol_dem_emilab[2, 3], pol_dem_liclab[2, 3], pol_dem_dat[2, 3], pol_dem_emi[2, 3], pol_dem_lic[2, 3]),
           trait = factor(rep(c("Allow abortion", "Increase welfare", "General policy (dis)agreement", "Black", "Not Religious", "Democrat"), each = 12),
                          levels = c("Allow abortion", "Increase welfare", "General policy (dis)agreement", "Black", "Not Religious", "Democrat")),
           Policy = factor(rep(c("Naturally branded liberal\n(vs naturally branded\nconservative)", 
                                 "Artificially branded liberal\n(vs artificially branded\nconservative)", 
                                 "Unbranded big-gov\n(vs unbranded small-gov)",
                                 rep("Naturally branded liberal\n(vs naturally branded\nconservative)", 3),
                                 rep("Artificially branded liberal\n(vs artificially branded\nconservative)", 3),
                                 rep("Unbranded big-gov\n(vs unbranded small-gov)", 3)), 6),
                           levels = c("Unbranded big-gov\n(vs unbranded small-gov)", 
                                      "Artificially branded liberal\n(vs artificially branded\nconservative)", 
                                      "Naturally branded liberal\n(vs naturally branded\nconservative)")),
           Topic = factor(rep(c("Merged", "Merged", "Merged", "Gun control", "Immigration", "Same sex\nadoption",
                                "Don't sell data", "Allow eminent\ndomain", "Require license", "Don't sell data", "Allow eminent\ndomain", "Require license"), 6),
                          levels = c("Don't sell data", "Allow eminent\ndomain", "Require license", "Gun control", "Immigration", "Same sex\nadoption", "Merged"))) %>% 
  ggplot(aes(x = Policy, y = estimate, color = Topic)) +
  geom_point(position = position_dodge(width = .5)) +
  facet_wrap(vars(trait)) +
  geom_errorbar(aes(ymin = estimate - 1.96*SE, ymax = estimate + 1.96*SE), width = 0.1, 
                position = position_dodge(width = .5)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_color_manual(values = c(rgb(1,.9,0,0.4), rgb(1,.7,0,0.4), rgb(1,.5,0,0.4),
                                rgb(.4,.9,1,0.4), rgb(.3,.7,1,0.4), rgb(.5,.5,1,0.4), "black"),
                     guide = guide_legend(reverse = TRUE),
                     name = "Policy Area") +
  labs(y = "Effect of vignette policy on perceptions in the presence of known partisanship", x = "") +
  coord_flip() + 
  theme_bw()

#----------------------------------------------
# Figure A3 (Average perceptions by treatment condition)
#----------------------------------------------

pid_ideo_means <- group_by(dat2[!is.na(dat2$treat_pid_ideo),], treat_pid_ideo) %>%
  summarize(Democrat_mean = mean(infer_dem, na.rm = T),
            Democrat_SE = sd(infer_dem, na.rm = T)/sqrt(sum(!is.na(infer_dem))),
            Republican_mean = mean(infer_rep, na.rm = T),
            Republican_SE = sd(infer_rep, na.rm = T)/sqrt(sum(!is.na(infer_rep))),
            Abortion_mean = mean(infer_abort, na.rm = T),
            Abortion_SE= sd(infer_abort, na.rm = T)/sqrt(sum(!is.na(infer_abort))),
            Welfare_mean = mean(infer_welfare, na.rm = T),
            Welfare_SE= sd(infer_welfare, na.rm = T)/sqrt(sum(!is.na(infer_welfare))),
            Black_mean = mean(infer_black, na.rm = T),
            Black_SE= sd(infer_black, na.rm = T)/sqrt(sum(!is.na(infer_black))),
            Religious_mean = mean(infer_religious, na.rm = T),
            Religious_SE= sd(infer_religious, na.rm = T)/sqrt(sum(!is.na(infer_religious))),
            Engaged_mean = mean(infer_engaged, na.rm = T),
            Engaged_SE= sd(infer_engaged, na.rm = T)/sqrt(sum(!is.na(infer_engaged))),
            Agree_mean = mean(infer_agree_Dresp, na.rm = T),
            Agree_SE= sd(infer_agree_Dresp, na.rm = T)/sqrt(sum(!is.na(infer_agree_Dresp))))

pid_ideo_means_long <- cbind(melt(pid_ideo_means, id.vars = "treat_pid_ideo", 
                                  measure.vars = names(pid_ideo_means)[grep("mean", names(pid_ideo_means))],
                                  value.name = "mean", variable.name = "area"),
                             SE = melt(pid_ideo_means, id.vars = "treat_pid_ideo", 
                                       measure.vars = names(pid_ideo_means)[grep("SE", names(pid_ideo_means))],
                                       value.name = "SE")$SE)
pid_ideo_means_long$area <- gsub("_mean", "", pid_ideo_means_long$area)
pid_ideo_means_long$area <- factor(pid_ideo_means_long$area, levels = unique(pid_ideo_means_long$area))


ggplot(pid_ideo_means_long[!grepl("Unbranded", pid_ideo_means_long$treat_pid_ideo),],
       aes(x = treat_pid_ideo, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - 1.96*SE, 
                    ymax = mean + 1.96*SE), width = 0.5) +
  facet_wrap(vars(area), ncol = 2, scales = "free_y") +
  labs(x = "Vignette", y = "Average estimated likelihood") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#----------------------------------------------
# Table A4 (% Not Reporting “extremely likely” for Stated Partisanship and Policy Positions)
#----------------------------------------------

group_by(dat2, treat_ideo) %>%
  summarize(reject_pid = round(1 - mean(correct_pid, na.rm = T), 2),
            reject_pol = round(1 - mean(correct_pol, na.rm = T), 2))


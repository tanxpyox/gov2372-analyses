# Medicaid_2010_05_average effects.R
# created 2010 05 17 from Medicaid_2008_11_average_effects.R
#
# Analyze data from the May 2010 Medicaid experiment, which 
# manipulates party cues, policy information, and extremity 
# of policy changes.  
#
# This experiment has two policy conditions (liberal,
# conservative), two "extremity" conditions (large change, 
# small change), and two cue conditions (none,
# "Democrats oppose").  This file just calculates average 
# effects and bootstrapped standard errors.
#
# Most of the functions here calculate -unweighted-
# average absolute differences.  This seems appropriate.
# Why let the reported average absolute differences be 
# affected by chance differences in (say) the number
# of Republicans in the sample vs. the number of
# Democrats?  In any case, the differences between 
# unweighted and weighted AADs are very small in the 
# cases that I've examined.  
#
# This file relies on code from other files and should  
# be sourced from other files rather than run directly.
#

library(boot)

average.effects <- function(Dem.subset.var=Dem, Rep.subset.var=Rep, subset.var=NULL, iterations=1000, ...) {
  # This function returns three sets of results.  The first set of
  # results is "overall".  It is produced for all subjects in the 
  # data.  The second is for all subjects in Dem.subset.var.  This 
  # is typically Democrats, but it could be low-education subjects,
  # women, etc.  The third set of results if for all subjects in 
  # Rep.subset.var.
  #     There is an important qualification: all three subsets can 
  # be restricted by subset.var.  For example, if subset.var=female,
  # all three sets of results are for women only.  [2010 06 26]
  
  #data <- data[data$educ.low, ] # calculate overall effects only for low-ed subjects


  ##########################################
  # ALIASES
  # Note that I am using the pre-treatment 
  # versions of Dem and Rep here, not the 
  # post-treatment versions.  [2009 06 04]
  ##########################################
  tr         <- data$article.type
  large      <- pol.size.large
  small      <- pol.size.small 
  
  
  
  #########################################################
  # ABSOLUTE AVERAGES OF EFFECTS [2008 02 29]
  #########################################################
  # These are unweighted averages of absolute differences,
  # e.g., of the absolute differences between (Demlib_int
  # and Demlib_nocue), (Demcon_int and Demcon_nocue),
  # (Replib_int and Replib_nocue), and
  # (Repcon_int and Repcon_nocue).  I do not want to weight
  # the different components of the average by the number
  # of subjects in each cued condition, because the number
  # of people in each condition is immaterial.  (And in any
  # case, it was determined by random assignment, not by
  # any real-world condition.)
  #
  #library(plotrix)
  #tapply(unclass(att), list(tr, Dem),        mean, na.rm=T) # all 12 group means: (Dem/Rep)*(6 experimental conditions)
  #tapply(unclass(att), list(tr, Dem),   std.error, na.rm=T) # all 12 group SEs: (Dem/Rep)*(6 experimental conditions)
  
  
  
  #########################################################
  # SET UP DATA FRAME FOR BOOTSTRAPPING
  #########################################################
  if ( is.null(subset.var)) { 
    boot.data  <- data.frame(att=unclass(att), tr, expand, reduce, large, small, Dem, Rep, Demopp, nocue)
    bootD.data <- boot.data[Dem.subset.var,]
    bootR.data <- boot.data[Rep.subset.var,]   
  } else { 
    boot.data  <- data.frame(att=unclass(att), tr, expand, reduce, large, small, Dem, Rep, Demopp, nocue)[subset.var, ]
    bootD.data <- data.frame(att=unclass(att), tr, expand, reduce, large, small, Dem, Rep, Demopp, nocue)[Dem.subset.var & subset.var, ]
    bootR.data <- data.frame(att=unclass(att), tr, expand, reduce, large, small, Dem, Rep, Demopp, nocue)[Rep.subset.var & subset.var, ]
  }
    
  
  
  #########################################################
  # MEAN AND BOOTSTRAPPED CI FOR AVERAGE ABSOLUTE EFFECTS:
  # "DEMOCRATS OPPOSE" VS. NO CUE [2010 05 17]
  #########################################################
  # Demoppose vs. no-cue, all subjects [2010 05 17]
  boot.effect.Demoppose <- function(data, indices) { # bootstrapped SE for the -unweighted- mean
    data  <- data[indices, ]
    if (sum(data$Dem, na.rm=TRUE)>0) {
      if (sum(data$large, na.rm=TRUE)>0) { 
        diff1 <- mean(data$att[data$Demopp & data$expand & data$Dem & data$large], na.rm=TRUE) - mean(data$att[data$nocue & data$expand & data$Dem & data$large], na.rm=TRUE)
        diff2 <- mean(data$att[data$Demopp & data$reduce & data$Dem & data$large], na.rm=TRUE) - mean(data$att[data$nocue & data$reduce & data$Dem & data$large], na.rm=TRUE)
      }
      if (sum(data$small, na.rm=TRUE)>0) {
        diff3 <- mean(data$att[data$Demopp & data$expand & data$Dem & data$small], na.rm=TRUE) - mean(data$att[data$nocue & data$expand & data$Dem & data$small], na.rm=TRUE)
        diff4 <- mean(data$att[data$Demopp & data$reduce & data$Dem & data$small], na.rm=TRUE) - mean(data$att[data$nocue & data$reduce & data$Dem & data$small], na.rm=TRUE)
      }
    }
    if (sum(data$Rep, na.rm=TRUE)>0) {
      if (sum(data$large, na.rm=TRUE)>0) {
        diff5 <- mean(data$att[data$Demopp & data$expand & data$Rep & data$large], na.rm=TRUE) - mean(data$att[data$nocue & data$expand & data$Rep & data$large], na.rm=TRUE)
        diff6 <- mean(data$att[data$Demopp & data$reduce & data$Rep & data$large], na.rm=TRUE) - mean(data$att[data$nocue & data$reduce & data$Rep & data$large], na.rm=TRUE)
      }
      if (sum(data$small, na.rm=TRUE)>0) {
        diff7 <- mean(data$att[data$Demopp & data$expand & data$Rep & data$small], na.rm=TRUE) - mean(data$att[data$nocue & data$expand & data$Rep & data$small], na.rm=TRUE)
        diff8 <- mean(data$att[data$Demopp & data$reduce & data$Rep & data$small], na.rm=TRUE) - mean(data$att[data$nocue & data$reduce & data$Rep & data$small], na.rm=TRUE)
      }
    }  
    diff.names <- ls(pat="^diff[[:digit:]]$")
    for (i in 1:length(diff.names)) { diff.names[i] <- get(diff.names[i]) } # replace variable names with their values
    mean(abs(as.numeric(diff.names)))
  }
  bootresult.Demoppose                 <- boot(data=boot.data, statistic=boot.effect.Demoppose, R=iterations) # mean 
  bootresult.Demoppose.ci              <- boot.ci(bootresult.Demoppose, type=c("norm","basic", "perc")) # percentile-based CI 
  bootresult.Demoppose.summary         <- as.numeric(c(bootresult.Demoppose[1], bootresult.Demoppose.ci$percent[4:5]))
  
  bootresult.Demoppose.Demonly         <- boot(data=bootD.data, statistic=boot.effect.Demoppose, R=iterations)  # mean 
  bootresult.Demoppose.Demonly.ci      <- boot.ci(bootresult.Demoppose.Demonly, type=c("norm","basic", "perc")) # percentile-based CI 
  bootresult.Demoppose.Demonly.summary <- as.numeric(c(bootresult.Demoppose.Demonly[1], bootresult.Demoppose.Demonly.ci$percent[4:5]))
  
  bootresult.Demoppose.Reponly         <- boot(data=bootR.data, statistic=boot.effect.Demoppose, R=iterations)  # mean 
  bootresult.Demoppose.Reponly.ci      <- boot.ci(bootresult.Demoppose.Reponly, type=c("norm","basic", "perc")) # percentile-based CI 
  bootresult.Demoppose.Reponly.summary <- as.numeric(c(bootresult.Demoppose.Reponly[1], bootresult.Demoppose.Reponly.ci$percent[4:5]))
  
  
  
  #########################################################
  # MEAN AND BOOTSTRAPPED CI FOR AVERAGE ABSOLUTE EFFECTS:
  # "DEMOCRATS OPPOSE" VS. NO CUE, MULTIPLIED BY 1.55 TO 
  # APPROXIMATE "DEMOCRATS OPPOSE" VS. "DEMOCRATS SUPPORT" 
  # CUES [2010 05 19]
  #########################################################
  # Demoppose vs. no-cue, all subjects [2010 05 19]
  boot.effect.Demoppose.155 <- function(data, indices) { # bootstrapped SE for the -unweighted- mean
    data  <- data[indices, ]
    if (sum(data$Dem, na.rm=TRUE)>0) {
      if (sum(data$large, na.rm=TRUE)>0) { 
        diff1 <- mean(data$att[data$Demopp & data$expand & data$Dem & data$large], na.rm=TRUE) - mean(data$att[data$nocue & data$expand & data$Dem & data$large], na.rm=TRUE)
        diff2 <- mean(data$att[data$Demopp & data$reduce & data$Dem & data$large], na.rm=TRUE) - mean(data$att[data$nocue & data$reduce & data$Dem & data$large], na.rm=TRUE)
      }
      if (sum(data$small, na.rm=TRUE)>0) {
        diff3 <- mean(data$att[data$Demopp & data$expand & data$Dem & data$small], na.rm=TRUE) - mean(data$att[data$nocue & data$expand & data$Dem & data$small], na.rm=TRUE)
        diff4 <- mean(data$att[data$Demopp & data$reduce & data$Dem & data$small], na.rm=TRUE) - mean(data$att[data$nocue & data$reduce & data$Dem & data$small], na.rm=TRUE)
      }
    }
    if (sum(data$Rep, na.rm=TRUE)>0) {
      if (sum(data$large, na.rm=TRUE)>0) {
        diff5 <- mean(data$att[data$Demopp & data$expand & data$Rep & data$large], na.rm=TRUE) - mean(data$att[data$nocue & data$expand & data$Rep & data$large], na.rm=TRUE)
        diff6 <- mean(data$att[data$Demopp & data$reduce & data$Rep & data$large], na.rm=TRUE) - mean(data$att[data$nocue & data$reduce & data$Rep & data$large], na.rm=TRUE)
      }
      if (sum(data$small, na.rm=TRUE)>0) {
        diff7 <- mean(data$att[data$Demopp & data$expand & data$Rep & data$small], na.rm=TRUE) - mean(data$att[data$nocue & data$expand & data$Rep & data$small], na.rm=TRUE)
        diff8 <- mean(data$att[data$Demopp & data$reduce & data$Rep & data$small], na.rm=TRUE) - mean(data$att[data$nocue & data$reduce & data$Rep & data$small], na.rm=TRUE)
      }
    }  
    diff.names <- ls(pat="^diff[[:digit:]]$")
    for (i in 1:length(diff.names)) { diff.names[i] <- get(diff.names[i]) } # replace variable names with their values
    diff.names <- 1.55*as.numeric(diff.names)
    mean(abs(as.numeric(diff.names)))
  }
  bootresult.Demoppose.155                 <- boot(data=boot.data, statistic=boot.effect.Demoppose.155, R=iterations)  # mean 
  bootresult.Demoppose.155.ci              <- boot.ci(bootresult.Demoppose.155, type=c("norm","basic", "perc"))            # percentile-based CI 
  bootresult.Demoppose.155.summary         <- as.numeric(c(bootresult.Demoppose.155[1], bootresult.Demoppose.155.ci$percent[4:5]))
  
  bootresult.Demoppose.155.Demonly         <- boot(data=bootD.data, statistic=boot.effect.Demoppose.155, R=iterations) # mean 
  bootresult.Demoppose.155.Demonly.ci      <- boot.ci(bootresult.Demoppose.155.Demonly, type=c("norm","basic", "perc"))    # percentile-based CI 
  bootresult.Demoppose.155.Demonly.summary <- as.numeric(c(bootresult.Demoppose.155.Demonly[1], bootresult.Demoppose.155.Demonly.ci$percent[4:5]))
  
  bootresult.Demoppose.155.Reponly         <- boot(data=bootR.data, statistic=boot.effect.Demoppose.155, R=iterations) # mean 
  bootresult.Demoppose.155.Reponly.ci      <- boot.ci(bootresult.Demoppose.155.Reponly, type=c("norm","basic", "perc"))    # percentile-based CI 
  bootresult.Demoppose.155.Reponly.summary <- as.numeric(c(bootresult.Demoppose.155.Reponly[1], bootresult.Demoppose.155.Reponly.ci$percent[4:5]))
  
  
  
  #########################################################
  # MEAN AND BOOTSTRAPPED CI FOR AVERAGE ABSOLUTE EFFECTS:
  # DEMOCRATIC PID VS. REPUBLICAN PID [2010 05 17]
  #########################################################
  boot.effect.PID <- function(data, indices) { # bootstrapped SE for the -unweighted- mean
    data  <- data[indices, ]
    if (sum(data$large, na.rm=TRUE)>0) {
      diff1 <- mean(data$att[data$Demopp & data$expand & data$Dem & data$large], na.rm=TRUE) - mean(data$att[data$Demopp & data$expand & data$Rep & data$large], na.rm=TRUE)
      diff2 <- mean(data$att[data$Demopp & data$reduce & data$Dem & data$large], na.rm=TRUE) - mean(data$att[data$Demopp & data$reduce & data$Rep & data$large], na.rm=TRUE)
      diff3 <- mean(data$att[data$nocue  & data$expand & data$Dem & data$large], na.rm=TRUE) - mean(data$att[data$nocue  & data$expand & data$Rep & data$large], na.rm=TRUE)
      diff4 <- mean(data$att[data$nocue  & data$reduce & data$Dem & data$large], na.rm=TRUE) - mean(data$att[data$nocue  & data$reduce & data$Rep & data$large], na.rm=TRUE)
    }
    if (sum(data$small, na.rm=TRUE)>0) {
      diff5 <- mean(data$att[data$Demopp & data$expand & data$Dem & data$small], na.rm=TRUE) - mean(data$att[data$Demopp & data$expand & data$Rep & data$small], na.rm=TRUE)
      diff6 <- mean(data$att[data$Demopp & data$reduce & data$Dem & data$small], na.rm=TRUE) - mean(data$att[data$Demopp & data$reduce & data$Rep & data$small], na.rm=TRUE)
      diff7 <- mean(data$att[data$nocue  & data$expand & data$Dem & data$small], na.rm=TRUE) - mean(data$att[data$nocue  & data$expand & data$Rep & data$small], na.rm=TRUE)
      diff8 <- mean(data$att[data$nocue  & data$reduce & data$Dem & data$small], na.rm=TRUE) - mean(data$att[data$nocue  & data$reduce & data$Rep & data$small], na.rm=TRUE)
    }
    diff.names <- ls(pat="^diff[[:digit:]]$")
    for (i in 1:length(diff.names)) { diff.names[i] <- get(diff.names[i]) } # replace variable names with their values
    mean(abs(as.numeric(diff.names)))  
  }
  bootresult.PID <- boot(data=boot.data, statistic=boot.effect.PID, R=iterations)    # mean 
  bootresult.PID.ci <- boot.ci(bootresult.PID, type=c("norm","basic", "perc")) # percentile-based CI 
  bootresult.PID.summary <- as.numeric(c(bootresult.PID[1], bootresult.PID.ci$percent[4:5]))

  
  
  #########################################################
  # MEAN AND BOOTSTRAPPED CI FOR AVERAGE ABSOLUTE EFFECTS:
  # LIBERAL VS. CONSERVATIVE POLICY CHANGES 
  #########################################################
  boot.effect.policy <- function(data, indices) { # bootstrapped SE for the -unweighted- mean
    data  <- data[indices, ]
    if (sum(data$Dem, na.rm=TRUE)>0) {     
      if (sum(data$large, na.rm=TRUE)>0) {
        diff1 <- mean(data$att[data$Demopp & data$expand & data$Dem & data$large], na.rm=TRUE) - mean(data$att[data$Demopp & data$reduce & data$Dem & data$large], na.rm=TRUE)
        diff2 <- mean(data$att[data$nocue  & data$expand & data$Dem & data$large], na.rm=TRUE) - mean(data$att[data$nocue  & data$reduce & data$Dem & data$large], na.rm=TRUE)
      }
      if (sum(data$small, na.rm=TRUE)>0) {
        diff3 <- mean(data$att[data$Demopp & data$expand & data$Dem & data$small], na.rm=TRUE) - mean(data$att[data$Demopp & data$reduce & data$Dem & data$small], na.rm=TRUE)
        diff4 <- mean(data$att[data$nocue  & data$expand & data$Dem & data$small], na.rm=TRUE) - mean(data$att[data$nocue  & data$reduce & data$Dem & data$small], na.rm=TRUE)
      }  
    }
    if (sum(data$Rep, na.rm=TRUE)>0) {
      if (sum(data$large, na.rm=TRUE)>0) {
        diff5 <- mean(data$att[data$Demopp & data$expand & data$Rep & data$large], na.rm=TRUE) - mean(data$att[data$Demopp & data$reduce & data$Rep & data$large], na.rm=TRUE)
        diff6 <- mean(data$att[data$nocue  & data$expand & data$Rep & data$large], na.rm=TRUE) - mean(data$att[data$nocue  & data$reduce & data$Rep & data$large], na.rm=TRUE)
      }
      if (sum(data$small, na.rm=TRUE)>0) {
        diff7 <- mean(data$att[data$Demopp & data$expand & data$Rep & data$small], na.rm=TRUE) - mean(data$att[data$Demopp & data$reduce & data$Rep & data$small], na.rm=TRUE)
        diff8 <- mean(data$att[data$nocue  & data$expand & data$Rep & data$small], na.rm=TRUE) - mean(data$att[data$nocue  & data$reduce & data$Rep & data$small], na.rm=TRUE)
      }  
    }
    diff.names <- ls(pat="^diff[[:digit:]]$")
    for (i in 1:length(diff.names)) { diff.names[i] <- get(diff.names[i]) } # replace variable names with their values
    mean(abs(as.numeric(diff.names)))  
  }
  bootresult.policy                 <- boot(data=boot.data, statistic=boot.effect.policy, R=iterations)   # mean 
  bootresult.policy.ci              <- boot.ci(bootresult.policy, type=c("norm","basic", "perc"))         # percentile-based CI 
  bootresult.policy.summary         <- as.numeric(c(bootresult.policy[1], bootresult.policy.ci$percent[4:5]))
  
  bootresult.policy.Demonly         <- boot(data=bootD.data, statistic=boot.effect.policy, R=iterations)  # mean 
  bootresult.policy.Demonly.ci      <- boot.ci(bootresult.policy.Demonly, type=c("norm","basic", "perc")) # percentile-based CI 
  bootresult.policy.Demonly.summary <- as.numeric(c(bootresult.policy.Demonly[1], bootresult.policy.Demonly.ci$percent[4:5]))
  
  bootresult.policy.Reponly         <- boot(data=bootR.data, statistic=boot.effect.policy, R=iterations)  # mean 
  bootresult.policy.Reponly.ci      <- boot.ci(bootresult.policy.Reponly, type=c("norm","basic", "perc")) # percentile-based CI 
  bootresult.policy.Reponly.summary <- as.numeric(c(bootresult.policy.Reponly[1], bootresult.policy.Reponly.ci$percent[4:5]))
  
  
  
  ###########################
  # CREATE LIST TO RETURN
  ###########################
  results <- rbind(bootresult.Demoppose.summary,         bootresult.Demoppose.155.summary,         bootresult.PID.summary, bootresult.policy.summary,
                   bootresult.Demoppose.Demonly.summary, bootresult.Demoppose.155.Demonly.summary, c(-100, -100, -100),    bootresult.policy.Demonly.summary,
                   bootresult.Demoppose.Reponly.summary, bootresult.Demoppose.155.Reponly.summary, c(-100, -100, -100),    bootresult.policy.Reponly.summary)
}



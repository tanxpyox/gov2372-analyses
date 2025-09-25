# Medicaid_2008_11_average effects.R
#
load('Experiment_1.RData')



##########################################
# ALIASES
# Note that I am using the pre-treatment 
# versions of Dem and Rep here, not the 
# post-treatment versions.  [2009 06 04]
##########################################
Dem          <- data$PID.pre=="Dem"
Rep          <- data$PID.pre=="Rep"
tr         <- data$article.type
tr.con     <- gregexpr('con', tr)>-1
tr.lib     <- !tr.con
tr.nocue   <- tr%in%c("con_nocue", "lib_nocue")
Demsupp    <- gregexpr('Demsupp', tr)>-1
Demopp     <- gregexpr('Demopp', tr)>-1
att        <- data$initial.like
educ.low   <- data$educ.low # 2009 06 22: bad to create a copy like this.
                            # I'm doing it to ease the feeding of data to 
                            # the bootstrapping routine, but it's a klugey
                            # thing to do.
age.low <- data$age<=30 & data$age.legit
age.hi  <- data$age> 55 & data$age.legit



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
library(boot)
library(plotrix)
tapply(unclass(att), list(tr, Dem),        mean, na.rm=TRUE) # all 12 group means: (Dem/Rep)*(6 experimental conditions)
tapply(unclass(att), list(tr, Dem),   std.error, na.rm=TRUE) # all 12 group SEs: (Dem/Rep)*(6 experimental conditions)



#########################################################
# SET UP DATA FRAME FOR BOOTSTRAPPING
#########################################################
boot.data <- data.frame(att, tr, tr.lib, tr.con, tr.nocue, Dem, Rep, Demsupp, 
                        Demopp, educ.low, age.low, age.hi)
boot.data$att <- unclass(att)



#########################################################
# MEAN AND BOOTSTRAPPED CI FOR AVERAGE ABSOLUTE EFFECTS 
# [2008 04 17]
#########################################################

# Demsupport vs. no-cue, all subjects
boot.effect.Demsupport <- function(data, indices) { 
  data  <- data[indices, ]
  diff1 <- mean(data$att[data$Demsupp & data$tr.lib & data$Dem], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.lib & data$Dem], na.rm=TRUE)
  diff2 <- mean(data$att[data$Demsupp & data$tr.con & data$Dem], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & data$Dem], na.rm=TRUE)
  diff3 <- mean(data$att[data$Demsupp & data$tr.lib & data$Rep], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.lib & data$Rep], na.rm=TRUE)
  diff4 <- mean(data$att[data$Demsupp & data$tr.con & data$Rep], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & data$Rep], na.rm=TRUE)
  mean(c(abs(diff1), abs(diff2), abs(diff3), abs(diff4))) # unweighted mean
  #tmpweights <- with(data, c(sum(tr.lib[Dem], tr.nocue[Dem]), sum(tr.con[Dem], tr.nocue[Dem]), sum(tr.lib[Rep], tr.nocue[Rep]), sum(tr.con[Rep], tr.nocue[Rep])))  
  #weighted.mean(c(abs(diff1), abs(diff2), abs(diff3), abs(diff4)), w=tmpweights)
}
bootresult.Demsupport <- boot(data=boot.data, statistic=boot.effect.Demsupport, R=1000)    # mean
bootresult.Demsupport.ci <- boot.ci(bootresult.Demsupport, type=c("norm","basic", "perc")) # CI
bootresult.Demsupport.summary <- as.numeric(c(bootresult.Demsupport[1], bootresult.Demsupport.ci$percent[4], bootresult.Demsupport.ci$percent[5]))

# Demsupport vs. no-cue, Dem subjects only
boot.effect.Demsupport.Demsonly <- function(data, indices) { 
  data  <- data[indices, ]
  diff1 <- mean(data$att[data$Demsupp & data$tr.lib & data$Dem], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.lib & data$Dem], na.rm=TRUE)
  diff2 <- mean(data$att[data$Demsupp & data$tr.con & data$Dem], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & data$Dem], na.rm=TRUE)
  mean(c(abs(diff1), abs(diff2)))
}
bootresult.Demsupport.Demsonly <- boot(data=boot.data, statistic=boot.effect.Demsupport.Demsonly, R=1000) 
bootresult.Demsupport.Demsonly.ci <- boot.ci(bootresult.Demsupport.Demsonly, type=c("norm","basic", "perc"))                              
bootresult.Demsupport.Demsonly.summary <- as.numeric(c(bootresult.Demsupport.Demsonly[1], bootresult.Demsupport.Demsonly.ci$percent[4], bootresult.Demsupport.Demsonly.ci$percent[5]))

# Demsupport vs. no-cue, GOP subjects only
boot.effect.Demsupport.GOPonly <- function(data, indices) { 
  data  <- data[indices, ]
  diff1 <- mean(data$att[data$Demsupp & data$tr.lib & data$Rep], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.lib & data$Rep], na.rm=TRUE)
  diff2 <- mean(data$att[data$Demsupp & data$tr.con & data$Rep], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & data$Rep], na.rm=TRUE)
  mean(c(abs(diff1), abs(diff2)))
}
bootresult.Demsupport.GOPonly <- boot(data=boot.data, statistic=boot.effect.Demsupport.GOPonly, R=1000) 
bootresult.Demsupport.GOPonly.ci <- boot.ci(bootresult.Demsupport.GOPonly, type=c("norm","basic", "perc"))                              
bootresult.Demsupport.GOPonly.summary <- as.numeric(c(bootresult.Demsupport.GOPonly[1], bootresult.Demsupport.GOPonly.ci$percent[4], bootresult.Demsupport.GOPonly.ci$percent[5]))

# Demsupport vs. no-cue, low-education subjects only [2009 06 22]
boot.effect.Demsupport.lowedonly <- function(data, indices) { 
    data  <- data[indices, ]
    diff1 <- mean(data$att[data$Demsupp & data$tr.lib & data$educ.low], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.lib & data$educ.low], na.rm=TRUE)
    diff2 <- mean(data$att[data$Demsupp & data$tr.con & data$educ.low], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & data$educ.low], na.rm=TRUE)
    mean(c(abs(diff1), abs(diff2)))
}
bootresult.Demsupport.lowedonly <- boot(data=boot.data, statistic=boot.effect.Demsupport.lowedonly, R=1000) 
bootresult.Demsupport.lowedonly.ci <- boot.ci(bootresult.Demsupport.lowedonly, type=c("norm","basic", "perc"))                              
bootresult.Demsupport.lowedonly.summary <- as.numeric(c(bootresult.Demsupport.lowedonly[1], bootresult.Demsupport.lowedonly.ci$percent[4], bootresult.Demsupport.lowedonly.ci$percent[5]))

# Demsupport vs. no-cue, high-education subjects only [2009 06 22]
boot.effect.Demsupport.highedonly <- function(data, indices) { 
    data  <- data[indices, ]
    diff1 <- mean(data$att[data$Demsupp & data$tr.lib & !data$educ.low], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.lib & !data$educ.low], na.rm=TRUE)
    diff2 <- mean(data$att[data$Demsupp & data$tr.con & !data$educ.low], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & !data$educ.low], na.rm=TRUE)
    mean(c(abs(diff1), abs(diff2)))
}
bootresult.Demsupport.highedonly <- boot(data=boot.data, statistic=boot.effect.Demsupport.highedonly, R=1000) 
bootresult.Demsupport.highedonly.ci <- boot.ci(bootresult.Demsupport.highedonly, type=c("norm","basic", "perc"))                              
bootresult.Demsupport.highedonly.summary <- as.numeric(c(bootresult.Demsupport.highedonly[1], bootresult.Demsupport.highedonly.ci$percent[4], bootresult.Demsupport.highedonly.ci$percent[5]))


# Demoppose vs. no-cue, all subjects [2008 04 17]
boot.effect.Demoppose <- function(data, indices) { # bootstrapped SE for the -unweighted- mean
  data  <- data[indices, ]
  diff1 <- mean(data$att[data$Demopp & data$tr.lib & data$Dem], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.lib & data$Dem], na.rm=TRUE)
  diff2 <- mean(data$att[data$Demopp & data$tr.con & data$Dem], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & data$Dem], na.rm=TRUE)
  diff3 <- mean(data$att[data$Demopp & data$tr.lib & data$Rep], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.lib & data$Rep], na.rm=TRUE)
  diff4 <- mean(data$att[data$Demopp & data$tr.con & data$Rep], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & data$Rep], na.rm=TRUE)
  mean(c(abs(diff1), abs(diff2), abs(diff3), abs(diff4)))
}
bootresult.Demoppose <- boot(data=boot.data, statistic=boot.effect.Demoppose, R=1000)    # mean 
bootresult.Demoppose.ci <- boot.ci(bootresult.Demoppose, type=c("norm","basic", "perc")) # percentile-based CI 
bootresult.Demoppose.summary <- as.numeric(c(bootresult.Demoppose[1], bootresult.Demoppose.ci$percent[4], bootresult.Demoppose.ci$percent[5]))

# Demoppose vs. no-cue, Dem subjects only 
boot.effect.Demoppose.Demsonly <- function(data, indices) { 
  data  <- data[indices, ]
  diff1 <- mean(data$att[data$Demopp & data$tr.lib & data$Dem], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.lib & data$Dem], na.rm=TRUE)
  diff2 <- mean(data$att[data$Demopp & data$tr.con & data$Dem], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & data$Dem], na.rm=TRUE)
  mean(c(abs(diff1), abs(diff2)))
}
bootresult.Demoppose.Demsonly <- boot(data=boot.data, statistic=boot.effect.Demoppose.Demsonly, R=1000) 
bootresult.Demoppose.Demsonly.ci <- boot.ci(bootresult.Demoppose.Demsonly, type=c("norm","basic", "perc"))                              
bootresult.Demoppose.Demsonly.summary <- as.numeric(c(bootresult.Demoppose.Demsonly[1], bootresult.Demoppose.Demsonly.ci$percent[4], bootresult.Demoppose.Demsonly.ci$percent[5]))

# Demoppose vs. no-cue, GOP subjects only 
boot.effect.Demoppose.GOPonly <- function(data, indices) { 
  data  <- data[indices, ]
  diff1 <- mean(data$att[data$Demopp & data$tr.lib & data$Rep], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.lib & data$Rep], na.rm=TRUE)
  diff2 <- mean(data$att[data$Demopp & data$tr.con & data$Rep], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & data$Rep], na.rm=TRUE)
  mean(c(abs(diff1), abs(diff2)))
}
bootresult.Demoppose.GOPonly <- boot(data=boot.data, statistic=boot.effect.Demoppose.GOPonly, R=1000) 
bootresult.Demoppose.GOPonly.ci <- boot.ci(bootresult.Demoppose.GOPonly, type=c("norm","basic", "perc"))                              
bootresult.Demoppose.GOPonly.summary <- as.numeric(c(bootresult.Demoppose.GOPonly[1], bootresult.Demoppose.GOPonly.ci$percent[4], bootresult.Demoppose.GOPonly.ci$percent[5]))

# Demoppose vs. no-cue, low-education subjects only [2009 06 22] 
boot.effect.Demoppose.lowedonly <- function(data, indices) { 
    data  <- data[indices, ]
    diff1 <- mean(data$att[data$Demopp & data$tr.lib & data$educ.low], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.lib & data$educ.low], na.rm=TRUE)
    diff2 <- mean(data$att[data$Demopp & data$tr.con & data$educ.low], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & data$educ.low], na.rm=TRUE)
    mean(c(abs(diff1), abs(diff2)))
}
bootresult.Demoppose.lowedonly <- boot(data=boot.data, statistic=boot.effect.Demoppose.lowedonly, R=1000) 
bootresult.Demoppose.lowedonly.ci <- boot.ci(bootresult.Demoppose.lowedonly, type=c("norm","basic", "perc"))                              
bootresult.Demoppose.lowedonly.summary <- as.numeric(c(bootresult.Demoppose.lowedonly[1], bootresult.Demoppose.lowedonly.ci$percent[4], bootresult.Demoppose.lowedonly.ci$percent[5]))

# Demoppose vs. no-cue, high-education subjects only [2009 06 22] 
boot.effect.Demoppose.highedonly <- function(data, indices) { 
    data  <- data[indices, ]
    diff1 <- mean(data$att[data$Demopp & data$tr.lib & !data$educ.low], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.lib & !data$educ.low], na.rm=TRUE)
    diff2 <- mean(data$att[data$Demopp & data$tr.con & !data$educ.low], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & !data$educ.low], na.rm=TRUE)
    mean(c(abs(diff1), abs(diff2)))
}
bootresult.Demoppose.highedonly <- boot(data=boot.data, statistic=boot.effect.Demoppose.highedonly, R=1000) 
bootresult.Demoppose.highedonly.ci <- boot.ci(bootresult.Demoppose.highedonly, type=c("norm","basic", "perc"))                              
bootresult.Demoppose.highedonly.summary <- as.numeric(c(bootresult.Demoppose.highedonly[1], bootresult.Demoppose.highedonly.ci$percent[4], bootresult.Demoppose.highedonly.ci$percent[5]))


# Demsupport vs. Demoppose cues, all subjects [2008 04 17]
boot.effect.Demsuppoppose <- function(data, indices) { # bootstrapped SE for the -unweighted- mean
  data  <- data[indices, ]
  diff1 <- mean(data$att[data$Demopp & data$tr.lib & data$Dem], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.lib & data$Dem], na.rm=TRUE)
  diff2 <- mean(data$att[data$Demopp & data$tr.con & data$Dem], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.con & data$Dem], na.rm=TRUE)
  diff3 <- mean(data$att[data$Demopp & data$tr.lib & data$Rep], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.lib & data$Rep], na.rm=TRUE)
  diff4 <- mean(data$att[data$Demopp & data$tr.con & data$Rep], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.con & data$Rep], na.rm=TRUE)
  mean(c(abs(diff1), abs(diff2), abs(diff3), abs(diff4)))
}
bootresult.Demsuppoppose    <- boot(data=boot.data, statistic=boot.effect.Demsuppoppose, R=1000)  
bootresult.Demsuppoppose.ci <- boot.ci(bootresult.Demsuppoppose, type=c("norm","basic", "perc"))                                  
bootresult.Demsuppoppose.summary <- as.numeric(c(bootresult.Demsuppoppose[1], bootresult.Demsuppoppose.ci$percent[4], bootresult.Demsuppoppose.ci$percent[5]))

# Demsupport vs. Demoppose cues, Dem subjects only 
boot.effect.Demsuppoppose.Demsonly <- function(data, indices) { 
  data  <- data[indices, ]
  diff1 <- mean(data$att[data$Demopp & data$tr.lib & data$Dem], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.lib & data$Dem], na.rm=TRUE)
  diff2 <- mean(data$att[data$Demopp & data$tr.con & data$Dem], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.con & data$Dem], na.rm=TRUE)
  mean(c(abs(diff1), abs(diff2)))
}
bootresult.Demsuppoppose.Demsonly <- boot(data=boot.data, statistic=boot.effect.Demsuppoppose.Demsonly, R=1000) 
bootresult.Demsuppoppose.Demsonly.ci <- boot.ci(bootresult.Demsuppoppose.Demsonly, type=c("norm","basic", "perc"))                              
bootresult.Demsuppoppose.Demsonly.summary <- as.numeric(c(bootresult.Demsuppoppose.Demsonly[1], bootresult.Demsuppoppose.Demsonly.ci$percent[4], bootresult.Demsuppoppose.Demsonly.ci$percent[5]))

# Demsupport vs. Demoppose cues, GOP subjects only
boot.effect.Demsuppoppose.GOPonly <- function(data, indices) { 
  data  <- data[indices, ]
  diff1 <- mean(data$att[data$Demopp & data$tr.lib & data$Rep], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.lib & data$Rep], na.rm=TRUE)
  diff2 <- mean(data$att[data$Demopp & data$tr.con & data$Rep], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.con & data$Rep], na.rm=TRUE)
  mean(c(abs(diff1), abs(diff2)))
}
bootresult.Demsuppoppose.GOPonly <- boot(data=boot.data, statistic=boot.effect.Demsuppoppose.GOPonly, R=1000) 
bootresult.Demsuppoppose.GOPonly.ci <- boot.ci(bootresult.Demsuppoppose.GOPonly, type=c("norm","basic", "perc"))                              
bootresult.Demsuppoppose.GOPonly.summary <- as.numeric(c(bootresult.Demsuppoppose.GOPonly[1], bootresult.Demsuppoppose.GOPonly.ci$percent[4], bootresult.Demsuppoppose.GOPonly.ci$percent[5]))


# Demsupport vs. Demoppose cues, low-education subjects only [2009 06 22] 
boot.effect.Demsuppoppose.lowedonly <- function(data, indices) { 
    data  <- data[indices, ]
    diff1 <- mean(data$att[data$Demopp & data$tr.lib & data$educ.low], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.lib & data$educ.low], na.rm=TRUE)
    diff2 <- mean(data$att[data$Demopp & data$tr.con & data$educ.low], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.con & data$educ.low], na.rm=TRUE)
    mean(c(abs(diff1), abs(diff2)))
}
bootresult.Demsuppoppose.lowedonly         <- boot(data=boot.data, statistic=boot.effect.Demsuppoppose.lowedonly, R=1000) 
bootresult.Demsuppoppose.lowedonly.ci      <- boot.ci(bootresult.Demsuppoppose.lowedonly, type=c("norm","basic", "perc"))                              
bootresult.Demsuppoppose.lowedonly.summary <- as.numeric(c(bootresult.Demsuppoppose.lowedonly[1], bootresult.Demsuppoppose.lowedonly.ci$percent[4], bootresult.Demsuppoppose.lowedonly.ci$percent[5]))


# Demsupport vs. Demoppose cues, high-education subjects only [2009 06 22] 
boot.effect.Demsuppoppose.highedonly <- function(data, indices) { 
    data  <- data[indices, ]
    diff1 <- mean(data$att[data$Demopp & data$tr.lib & !data$educ.low], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.lib & !data$educ.low], na.rm=TRUE)
    diff2 <- mean(data$att[data$Demopp & data$tr.con & !data$educ.low], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.con & !data$educ.low], na.rm=TRUE)
    mean(c(abs(diff1), abs(diff2)))
}
bootresult.Demsuppoppose.highedonly         <- boot(data=boot.data, statistic=boot.effect.Demsuppoppose.highedonly, R=1000) 
bootresult.Demsuppoppose.highedonly.ci      <- boot.ci(bootresult.Demsuppoppose.highedonly, type=c("norm","basic", "perc"))                              
bootresult.Demsuppoppose.highedonly.summary <- as.numeric(c(bootresult.Demsuppoppose.highedonly[1], bootresult.Demsuppoppose.highedonly.ci$percent[4], bootresult.Demsuppoppose.highedonly.ci$percent[5]))

# Demsupport vs. Demoppose cues, low-age subjects only [2011 03 03] 
boot.effect.Demsuppoppose.lowageonly <- function(data, indices) { 
  data  <- data[indices, ]
  diff1 <- mean(data$att[data$Demopp & data$tr.lib & data$age.low], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.lib & data$age.low], na.rm=TRUE)
  diff2 <- mean(data$att[data$Demopp & data$tr.con & data$age.low], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.con & data$age.low], na.rm=TRUE)
  mean(c(abs(diff1), abs(diff2)))
}
bootresult.Demsuppoppose.lowageonly         <- boot(data=boot.data, statistic=boot.effect.Demsuppoppose.lowageonly, R=1000) 
bootresult.Demsuppoppose.lowageonly.ci      <- boot.ci(bootresult.Demsuppoppose.lowageonly, type=c("norm","basic", "perc"))                              
bootresult.Demsuppoppose.lowageonly.summary <- as.numeric(c(bootresult.Demsuppoppose.lowageonly[1], bootresult.Demsuppoppose.lowageonly.ci$percent[4], bootresult.Demsuppoppose.lowageonly.ci$percent[5]))

# Demsupport vs. Demoppose cues, high-age subjects only [2011 03 03] 
boot.effect.Demsuppoppose.highageonly <- function(data, indices) { 
  data  <- data[indices, ]
  diff1 <- mean(data$att[data$Demopp & data$tr.lib & data$age.hi], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.lib & data$age.hi], na.rm=TRUE)
  diff2 <- mean(data$att[data$Demopp & data$tr.con & data$age.hi], na.rm=TRUE) - mean(data$att[data$Demsupp & data$tr.con & data$age.hi], na.rm=TRUE)
  mean(c(abs(diff1), abs(diff2)))
}
bootresult.Demsuppoppose.highageonly         <- boot(data=boot.data, statistic=boot.effect.Demsuppoppose.highageonly, R=1000) 
bootresult.Demsuppoppose.highageonly.ci      <- boot.ci(bootresult.Demsuppoppose.highageonly, type=c("norm","basic", "perc"))                              
bootresult.Demsuppoppose.highageonly.summary <- as.numeric(c(bootresult.Demsuppoppose.highageonly[1], bootresult.Demsuppoppose.highageonly.ci$percent[4], bootresult.Demsuppoppose.highageonly.ci$percent[5]))


# Democratic PID vs. Republican PID
boot.effect.PID <- function(data, indices) { # bootstrapped SE for the -unweighted- mean
  data     <- data[indices, ]
  Demmeans <- tapply(data$att[ data$Dem], data$tr[ data$Dem], mean, na.rm=T)
  Repmeans <- tapply(data$att[!data$Dem], data$tr[!data$Dem], mean, na.rm=T)
  mean(abs(Demmeans-Repmeans))
}
bootresult.PID <- boot(data=boot.data, statistic=boot.effect.PID, R=1000) 
  tmpDem <- tapply(unclass(att[Dem]), tr[Dem], meanNA) 
  tmpRep <- tapply(unclass(att[Rep]), tr[Rep], meanNA)
  weighted.mean(x=abs(tmpDem-tmpRep), w=table(tr)) # weighted mean "by hand" = .96
bootresult.PID.ci      <- boot.ci(bootresult.PID, type=c("norm","basic", "perc"))  # percentile-based CI
bootresult.PID.summary <- as.numeric(c(bootresult.PID[1], bootresult.PID.ci$percent[4], bootresult.PID.ci$percent[5]))

# Democratic PID vs. Republican PID, low-education subjects only [2009 06 22] 
boot.effect.PID.lowedonly <- function(data, indices) { # bootstrapped SE for the -unweighted- mean
    data     <- data[indices, ]
    Demmeans <- tapply(data$att[ data$Dem & data$educ.low], data$tr[ data$Dem & data$educ.low], mean, na.rm=T)
    Repmeans <- tapply(data$att[!data$Dem & data$educ.low], data$tr[!data$Dem & data$educ.low], mean, na.rm=T)
    mean(abs(Demmeans-Repmeans))
}
bootresult.PID.lowedonly <- boot(data=boot.data, statistic=boot.effect.PID.lowedonly, R=1000) 
bootresult.PID.ci.lowedonly      <- boot.ci(bootresult.PID.lowedonly, type=c("norm","basic", "perc"))  # percentile-based CI
bootresult.PID.summary.lowedonly <- as.numeric(c(bootresult.PID.lowedonly[1], bootresult.PID.ci.lowedonly$percent[4], bootresult.PID.ci.lowedonly$percent[5]))

# Democratic PID vs. Republican PID, high-education subjects only [2009 06 22] 
boot.effect.PID.highedonly <- function(data, indices) { # bootstrapped SE for the -unweighted- mean
    data     <- data[indices, ]
    Demmeans <- tapply(data$att[ data$Dem & !data$educ.low], data$tr[ data$Dem & !data$educ.low], mean, na.rm=T)
    Repmeans <- tapply(data$att[!data$Dem & !data$educ.low], data$tr[!data$Dem & !data$educ.low], mean, na.rm=T)
    mean(abs(Demmeans-Repmeans))
}
bootresult.PID.highedonly <- boot(data=boot.data, statistic=boot.effect.PID.highedonly, R=1000) 
bootresult.PID.ci.highedonly      <- boot.ci(bootresult.PID.highedonly, type=c("norm","basic", "perc"))  # percentile-based CI
bootresult.PID.summary.highedonly <- as.numeric(c(bootresult.PID.highedonly[1], bootresult.PID.ci.highedonly$percent[4], bootresult.PID.ci.highedonly$percent[5]))


# Liberal vs. conservative policy changes, all subjects [2008 02 29]
boot.effect.polinfo <- function(data, indices) { # bootstrapped SE for the -unweighted- mean
  data     <- data[indices, ]
  diff1 <- mean(data$att[data$Demopp   & data$tr.lib & data$Dem], na.rm=TRUE) - mean(data$att[data$Demopp   & data$tr.con & data$Dem], na.rm=TRUE)
  diff2 <- mean(data$att[data$Demsupp  & data$tr.lib & data$Dem], na.rm=TRUE) - mean(data$att[data$Demsupp  & data$tr.con & data$Dem], na.rm=TRUE)
  diff3 <- mean(data$att[data$tr.nocue & data$tr.lib & data$Dem], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & data$Dem], na.rm=TRUE)
  diff4 <- mean(data$att[data$Demopp   & data$tr.lib & data$Rep], na.rm=TRUE) - mean(data$att[data$Demopp   & data$tr.con & data$Rep], na.rm=TRUE)
  diff5 <- mean(data$att[data$Demsupp  & data$tr.lib & data$Rep], na.rm=TRUE) - mean(data$att[data$Demsupp  & data$tr.con & data$Rep], na.rm=TRUE)
  diff6 <- mean(data$att[data$tr.nocue & data$tr.lib & data$Rep], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & data$Rep], na.rm=TRUE)
  mean(c(abs(diff1), abs(diff2), abs(diff3), abs(diff4), abs(diff5), abs(diff6)))
}
bootresult.polinfo <- boot(data=boot.data, statistic=boot.effect.polinfo, R=1000) 
  tmpDemdiffs <- tmpDem[4:6]-tmpDem[1:3] # differences for Dems between "conservative" and "liberal" conditions (no cue, intuitive cue, counterint. cue)
  tmpRepdiffs <- tmpRep[1:3]-tmpRep[4:6]
  mean(abs(c(tmpDemdiffs,tmpRepdiffs)))  # mean "by hand" is 1.68
  tmpweightsD <- table(tr[Dem])[1:3] + table(tr[Rep])[4:6]
  tmpweightsR <- table(tr[Rep])[1:3] + table(tr[Rep])[4:6]
  weighted.mean(abs(c(tmpDemdiffs,tmpRepdiffs)), w=c(tmpweightsD, tmpweightsR)) # weighted mean "by hand" is 1.71
bootresult.polinfo.ci      <- boot.ci(bootresult.polinfo, type=c("norm","basic", "perc"))               
bootresult.polinfo.summary <- as.numeric(c(bootresult.polinfo[1], bootresult.polinfo.ci$percent[4], bootresult.polinfo.ci$percent[5]))

# Liberal vs. conservative policy changes, Dem subjects only
boot.effect.polinfo.Demsonly <- function(data, indices) { 
  data     <- data[indices, ]
  diff1 <- mean(data$att[data$Demopp   & data$tr.lib & data$Dem], na.rm=TRUE) - mean(data$att[data$Demopp   & data$tr.con & data$Dem], na.rm=TRUE)
  diff2 <- mean(data$att[data$Demsupp  & data$tr.lib & data$Dem], na.rm=TRUE) - mean(data$att[data$Demsupp  & data$tr.con & data$Dem], na.rm=TRUE)
  diff3 <- mean(data$att[data$tr.nocue & data$tr.lib & data$Dem], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & data$Dem], na.rm=TRUE)
  mean(c(abs(diff1), abs(diff2), abs(diff3)))
}
bootresult.polinfo.Demsonly         <- boot(data=boot.data, statistic=boot.effect.polinfo.Demsonly, R=1000) 
bootresult.polinfo.Demsonly.ci      <- boot.ci(bootresult.polinfo.Demsonly, type=c("norm","basic", "perc"))
bootresult.polinfo.Demsonly.summary <- as.numeric(c(bootresult.polinfo.Demsonly[1], bootresult.polinfo.Demsonly.ci$percent[4], bootresult.polinfo.Demsonly.ci$percent[5]))

# Liberal vs. conservative policy changes, GOP subjects only
boot.effect.polinfo.GOPonly <- function(data, indices) { 
  data     <- data[indices, ]
  diff4 <- mean(data$att[data$Demopp   & data$tr.lib & data$Rep], na.rm=TRUE) - mean(data$att[data$Demopp   & data$tr.con & data$Rep], na.rm=TRUE)
  diff5 <- mean(data$att[data$Demsupp  & data$tr.lib & data$Rep], na.rm=TRUE) - mean(data$att[data$Demsupp  & data$tr.con & data$Rep], na.rm=TRUE)
  diff6 <- mean(data$att[data$tr.nocue & data$tr.lib & data$Rep], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & data$Rep], na.rm=TRUE)
  mean(c(abs(diff4), abs(diff5), abs(diff6)))
}
bootresult.polinfo.GOPonly         <- boot(data=boot.data, statistic=boot.effect.polinfo.GOPonly, R=1000) 
bootresult.polinfo.GOPonly.ci      <- boot.ci(bootresult.polinfo.GOPonly, type=c("norm","basic", "perc"))
bootresult.polinfo.GOPonly.summary <- as.numeric(c(bootresult.polinfo.GOPonly[1], bootresult.polinfo.GOPonly.ci$percent[4], bootresult.polinfo.GOPonly.ci$percent[5]))

# Liberal vs. conservative policy changes, low-education subjects only [2009 06 22]
boot.effect.polinfo.lowedonly <- function(data, indices) { 
    data     <- data[indices, ]
    diff1 <- mean(data$att[data$Demopp   & data$tr.lib & data$educ.low], na.rm=TRUE) - mean(data$att[data$Demopp   & data$tr.con & data$educ.low], na.rm=TRUE)
    diff2 <- mean(data$att[data$Demsupp  & data$tr.lib & data$educ.low], na.rm=TRUE) - mean(data$att[data$Demsupp  & data$tr.con & data$educ.low], na.rm=TRUE)
    diff3 <- mean(data$att[data$tr.nocue & data$tr.lib & data$educ.low], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & data$educ.low], na.rm=TRUE)
    mean(c(abs(diff1), abs(diff2), abs(diff3)))
}
bootresult.polinfo.lowedonly         <- boot(data=boot.data, statistic=boot.effect.polinfo.lowedonly, R=1000) 
bootresult.polinfo.lowedonly.ci      <- boot.ci(bootresult.polinfo.lowedonly, type=c("norm","basic", "perc"))
bootresult.polinfo.lowedonly.summary <- as.numeric(c(bootresult.polinfo.lowedonly[1], bootresult.polinfo.lowedonly.ci$percent[4], bootresult.polinfo.lowedonly.ci$percent[5]))

# Liberal vs. conservative policy changes, high-education subjects only [2009 06 22]
boot.effect.polinfo.highedonly <- function(data, indices) { 
    data     <- data[indices, ]
    diff1 <- mean(data$att[data$Demopp   & data$tr.lib & !data$educ.low], na.rm=TRUE) - mean(data$att[data$Demopp   & data$tr.con & !data$educ.low], na.rm=TRUE)
    diff2 <- mean(data$att[data$Demsupp  & data$tr.lib & !data$educ.low], na.rm=TRUE) - mean(data$att[data$Demsupp  & data$tr.con & !data$educ.low], na.rm=TRUE)
    diff3 <- mean(data$att[data$tr.nocue & data$tr.lib & !data$educ.low], na.rm=TRUE) - mean(data$att[data$tr.nocue & data$tr.con & !data$educ.low], na.rm=TRUE)
    mean(c(abs(diff1), abs(diff2), abs(diff3)))
}
bootresult.polinfo.highedonly         <- boot(data=boot.data, statistic=boot.effect.polinfo.highedonly, R=1000) 
bootresult.polinfo.highedonly.ci      <- boot.ci(bootresult.polinfo.highedonly, type=c("norm","basic", "perc"))
bootresult.polinfo.highedonly.summary <- as.numeric(c(bootresult.polinfo.highedonly[1], bootresult.polinfo.highedonly.ci$percent[4], bootresult.polinfo.highedonly.ci$percent[5]))




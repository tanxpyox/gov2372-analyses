# tables_Experiment_2.R
# created 2011 10 30

# Creates all data tables related to Experiment 2 in 
#
#    Bullock, John G.  2011.  "Elite Influence on Public Opinion in an 
#    Informed Electorate."  American Political Science Review 105 (3): 496-515.

rm(list=ls())
library(arm)   # for display(), a prettier summary()
library(MASS)  # provides ordered probit
library(pscl)  # goodness of fit for GLMs (with pR2)
load('Experiment_2.RData')



#############################################################################
# TABLE 3: NEED FOR COGNITION MODERATES THE EFFECTS OF POLICY IN EXPERIMENT 1
#############################################################################
lmT3.Dem <- lm(unclass(att) ~ (Demopp + expand + pol.size.large)*ncog.score, subset=Dem)
lmT3.Rep <- update(lmT3.Dem, subset=Rep)
arm::display(lmT3.Dem)
arm::display(lmT3.Rep)



#############################################################################
# TABLE A8: NEED-FOR-COGNITION ANALYSES WITH HIGHER-ORDER INTERACTIONS
# (EXPERIMENT 2)
#############################################################################
lmTA8.Dem <- update(lmT3.Dem, . ~ Demopp * expand * pol.size.large * ncog.score)
lmTA8.Rep <- update(lmT3.Rep, . ~ Demopp * expand * pol.size.large * ncog.score)
arm::display(lmTA8.Dem)
arm::display(lmTA8.Rep)



###############################################################################
# TABLE A9: DEPTH OF PROCESSING MODERATES REPUBLICAN PREFERENCES (EXPERIMENT 2)
###############################################################################
correct <- apply(memory[, c('cutoff', 'policy.direction', 'number.recipients')], 1, sum)
cog.hi <- correct>=quantile(correct, 2/3, na.rm=TRUE) & 
          data$time.on.article>=quantile(data$time.on.article, 2/3, na.rm=TRUE) &
          thoughts.averaged>=quantile(thoughts.averaged, 2/3, na.rm=TRUE)
cog.lo <- correct<=quantile(correct, 1/3, na.rm=TRUE) & 
          data$time.on.article<=quantile(data$time.on.article, 1/3, na.rm=TRUE) &
          thoughts.averaged<=quantile(thoughts.averaged, 1/3, na.rm=TRUE)
lm.cog.1.Rep <- update(lmT3.Rep, . ~ (cog.hi + cog.lo)*(Demopp + expand + pol.size.large))
lm.cog.2.Rep <- update(lmT3.Rep, . ~ . + (cog.hi + cog.lo)*(Demopp + expand + pol.size.large))
lm.cog.3.Rep <- update(lmT3.Rep, . ~ . + (cog.hi + cog.lo)*(Demopp + expand + pol.size.large + ncog.score))
display(lm.cog.1.Rep)
display(lm.cog.2.Rep)
display(lm.cog.3.Rep)

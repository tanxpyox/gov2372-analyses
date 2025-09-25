# tables_Experiment_1.R
# created 2011 10 18

# Creates all data tables related to Experiment 1 in 
#
#    Bullock, John G.  2011.  "Elite Influence on Public Opinion in an 
#    Informed Electorate."  American Political Science Review 105 (3): 496-515.

rm(list=ls())
library(arm)   # for display(), a prettier summary()
library(MASS)  # provides ordered probit
library(pscl)  # goodness of fit for GLMs (with pR2)
load('Experiment_1.RData')



#############################################################################
# SET UP ALIASES
#############################################################################
tr         <- data$article.type
tr.con     <- regexpr('con', tr)>-1
tr.lib     <- !tr.con
tr.nocue   <- tr%in%c("con_nocue", "lib_nocue")
Demsupp    <- data$Demsupp   
Demopp     <- data$Demopp  
att        <- data$initial.like



#############################################################################
# TABLE 2: NEED FOR COGNITION MODERATES THE EFFECTS OF POLICY IN EXPERIMENT 1
#############################################################################
lmT2.Dem <- lm(unclass(att) ~ (Demsupp + Demopp + tr.lib)*ncog.score, subset=Dem)      
display(lmT2.Dem)
display(update(lmT2.Dem, subset=Rep))                                                



#############################################################################
# TABLE A3: RANDOMIZATION CHECKS FOR EXPERIMENT 1
#############################################################################
ed.tri <- car::recode(unclass(data$education), '1:3="low"; 4:6="med"; 7:hi="high"', as.factor=TRUE)
tmp <- data.frame(data$Demsupp, data$Demopp, gregexpr('lib', data$article.type)>-1, data$female, data$age, data$education, ed.tri, data$PID.pre, data$state, data$region)
colnames(tmp) <- c('Demsupp', 'Demopp', 'tr.lib', 'female', 'age', 'education', 'ed.tri', 'PID.pre', 'state', 'region')
rand.check.summary <- function(model) {
  N        <- length(model$fitted.values) 
  chi.sq <- model$null.deviance - model$deviance
  if   (class(model)[1]=='polr') { df <- model$n - model$df.residual }
  else                           { df <- model$df.null - model$df.residual + 1} # for models created with glm()
  p        <- 1-pchisq(chi.sq, df)
  llh      <- model$deviance / -2 
  llhNull  <- model$null.deviance / -2 
  
  r2ML     <- 1 - exp(-chi.sq/N)
  r2ML.max <- 1 - exp(llhNull * 2/N)
  r2CU <- r2ML / r2ML.max
  
  c(logLik(model), chi.sq, p, r2CU, N)
}

lib.randcheck.glma         <- glm(tr.lib  ~ I(PID.pre=='Dem') + female + age + ed.tri + region + Demsupp + Demopp, family=binomial(link=logit), data=tmp[data$age.legit, ])
Demsupp.randcheck.glmb     <- glm(Demsupp ~                     female + age + ed.tri + region + tr.lib, family=binomial(link=logit), data=tmp[data$age.legit & (tmp$Demsupp | tr.nocue), ])
Demopp.randcheck.glmb      <- glm(Demopp  ~                     female + age + ed.tri + region + tr.lib, family=binomial(link=logit), data=tmp[data$age.legit & (tmp$Demopp | tr.nocue), ])
lib.randcheck.glmb         <- glm(tr.lib  ~                     female + age + ed.tri + region + Demsupp + Demopp, family=binomial(link=logit), data=tmp[data$age.legit, ])
Demsupp.randcheck.glma.Dem <- glm(Demsupp ~                     female + age + ed.tri + region + tr.lib, family=binomial(link=logit), data=tmp[data$PID.pre=='Dem' & data$age.legit & (tmp$Demsupp | tr.nocue), ])
Demopp.randcheck.glma.Dem  <- glm(Demopp  ~                     female + age + ed.tri + region + tr.lib, family=binomial(link=logit), data=tmp[data$PID.pre=='Dem' & data$age.legit & (tmp$Demopp | tr.nocue), ])
lib.randcheck.glma.Dem     <- glm(tr.lib  ~                     female + age + ed.tri + region + Demsupp + Demopp, family=binomial(link=logit), data=tmp[data$PID.pre=='Dem' & data$age.legit, ])
Demsupp.randcheck.glma.Rep <- glm(Demsupp ~                     female + age + ed.tri + region + tr.lib, family=binomial(link=logit), data=tmp[data$PID.pre=='Rep' & data$age.legit & (tmp$Demsupp | tr.nocue), ])
Demopp.randcheck.glma.Rep  <- glm(Demopp  ~                     female + age + ed.tri + region + tr.lib, family=binomial(link=logit), data=tmp[data$PID.pre=='Rep' & data$age.legit & (tmp$Demopp | tr.nocue), ])
lib.randcheck.glma.Rep     <- glm(tr.lib  ~                     female + age + ed.tri + region + Demsupp + Demopp, family=binomial(link=logit), data=tmp[data$PID.pre=='Rep' & data$age.legit, ])

model.list.ab <- list(Demsupp.randcheck.glmb, 
                       Demopp.randcheck.glmb,
                       lib.randcheck.glmb,
                       Demsupp.randcheck.glma.Dem,
                       Demopp.randcheck.glma.Dem,
                       lib.randcheck.glma.Dem,
                       Demsupp.randcheck.glma.Rep,
                       Demopp.randcheck.glma.Rep,
                       lib.randcheck.glma.Rep)
for (i in model.list.ab) { print(summary(i)) }
                   
# CALCULATE PSEUDO-R^2 AND OTHER SUMMARY STATISTICS                   
results <- vector("list", length(model.list.ab))
for (i in 1:length(model.list.ab)) { results[[i]] <- rand.check.summary(model.list.ab[[i]]) }
for (i in 1:length(results[[1]])) { 
  tmp <- sapply(results, function(x) { x[[i]] } )
  if (i==1) { tmp <- round(tmp) } else { tmp <- round(tmp, 3) }
  print(paste('& ', tmp, collapse=' & '))
}



#############################################################################
# TABLE A4: NEED-FOR-COGNITION ANALYSES WITH HIGHER-ORDER INTERACTIONS
#############################################################################
lmTA4a.Dem <- lm(unclass(att) ~ (Demsupp + Demopp + tr.lib)*ncog.score + Demsupp:tr.lib + Demopp:tr.lib, subset=Dem)
display(lmTA4a.Dem)
display(update(lmTA4a.Dem, . ~ (Demsupp + Demopp + tr.lib + Demsupp:tr.lib + Demopp:tr.lib)*ncog.score))
display(update(lmTA4a.Dem, subset=Rep))                                                          
display(update(lmTA4a.Dem, . ~ (Demsupp + Demopp + tr.lib + Demsupp:tr.lib + Demopp:tr.lib)*ncog.score, subset=Rep))

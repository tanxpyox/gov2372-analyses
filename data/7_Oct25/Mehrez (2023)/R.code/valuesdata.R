

###############R Script ###################################################
#### Paper: When Right is Left: Values and Voting Behavior in Tunisia######
##Author: Ameni Mehrez 
##University: Central European University 
##Date: 05/18/2023
#Email Address: Mehrez_Ameni@phd.ceu.edu
##############################################

############################### CODEBOOK #############################
##Values Variables: 

#Q11 Whether or not someone's action showed love for his or her country
#Q12 Whether or not someone showed a lack of respect for authority
#Q18 Whether or not someone conformed to the traditions of society
#Q29 I am proud of my country's history
#Q30 Respect for authority is something all children need to learn
#Q36 Men and women each have different roles to play in society
#Q41 If I were a soldier and disagreed with my commanding officer's orders, I would obey anyway because that is my duty
#Q25 Whether or not private property was respected
#Q26 Whether or not everyone was free to do as they wanted
#Q10 Whether or not some people were treated differently from others
#Q21 Whether or not someone was denied his or her rights
#Q16 Whether or not someone acted unfairly
#Q39 I think it's morally wrong that rich children inherit a lot of money while poor children inherit nothing
#Q34 Justice is the most important requirement for a society

######Socio-demographic variables###########
#Income: What is your net household income
#Gender: female/male
#Prayer: How often do you pray? 
#SI: Secular-Islamist self-identification question
#Milieu: Urban vs. rural
#age1: age of respondents
#yearbirth: year of birth of the respondents
#Age: age groupings from 1 to 5

######Voting variables####################
#Q51: for whom did you vote in the parliamentary elections?
#Q51other: Other (option not mentioned in the list)
#Q53for whom did you vote in the presidential elections?
#Q53other: Other (option not mentioned in the list)


#########################################################################
##load packages
library(jtools)
library(car)
library(dplyr)
library(foreign)
library(psych)
library(effects)
library(tidyr)
library(base)
library(psych)
library(ggplot2)
library(tidyr)
library(nnet) #for multinomial function 
library(mlogit) #another way to run multinomial models in R
library(stargazer)
library(patchwork)
library(rockchalk)

##Upload dataset
value <- read.csv(choose.files(), header = T)


##Recode voting variables
##Parliamentary vote choice Q51 and Q51other
##Merge the two into one column
##This takes values from other and inserts them to parliamentary vote dataset
df1 <-ifelse(value$Q51==9,value$Q51other,value$Q51)
df1 <- as.data.frame(df1)


##DO the same for presidential vote choice
##Variables are Q53 and Q53other
tf1 <-ifelse(value$Q53==13, value$Q53other,value$Q53)
tf1 <- as.data.frame(tf1)


##NAs are the respondents who did not vote
###And 98, 97 and 99 are the ones who answered by "Don't Know" or "refuse to answer"

##Combine both votes
cc <- cbind(df1, tf1)

###Parties' code in the dataset:
##SO Errahma party = 10
##I dont remember = 11
# An independent list = 12
# Nidaa Tounes =13
# Front popular = 14
# Nahnou laha list of safi said =15
#List tomorrow is better =16
# PDL =17
# AMAL party = 18
# Albadil = 19
# Bani watani party = 20
# The social democratic party = 21
# One hand list = 22
# Tayar lmahaba party =23
# Republican people's union party = 24
#Another tunisia is possible = 25
# hope and independent list = 26
# successful sidi bouzid list = 27

####NEW VOTING SCHEME for Parliamentary vote choice####################
##Category 1 = Islamists
## Category 2 = Center-left
## Category 3 = Secular-Nationalists
## Category 4 = Social Democrats
## Category 5 = Independents
## Category 6 = Did not vote
## Category 7 = DK/refuse/blank vote
table(cc$df1)
vot1 <- as.data.frame(cc)
vot1 <- car::recode(vot1$df1, "1=1; 4=1; 10=1; 23=1; 
                   2=2; 3=4;6=4;14=4;21=4;
                   5=3;7=3;8=3;13=3;17=3;18=3;20=3;24=3;
                   12=5; 15=5; 16=5; 22=5; 25=5; 26=5; 27=5;
                   NA= 6; 19=6;
                   11=7; 96=7; 98=7; 99=7")

vote1 <- as.data.frame(vot1)
table(vote1$vot1)


##Coding scheme for each presidential candidates 
##Moncef marzouki =1
# Mehdi Jomaa = 2
#safi said = 3
#Mohammed Abbou = 4
#hamma Hammami = 5
#Youssed Chahed = 6
#Seifeddine makhlouf = 7
#Abdelkarim zbidi = 8
#Abir Mousi = 9
#Kais Saied = 10
#Abdelfatteh Mourou = 11
#Nabil Karoui = 12
#Other 13
#Blank vote = 96
#DK = 98 
#refuse = 99
###Lotfi Mrayhi =14
##Hechmi Hamdi = 15
## Safi Said = 16
## Said Aydi = 17
## Hamadi Jebali = 18



##ReCoding Presidential candidates
#category 1 = Right-wing
#category 2 = Center-left
#category 3 = secular-nationalist
#category 4 = social democrats
#category 5 = Independents
#category 6 = did not vote
#category 7 = DK/ refuse to answer/ Blank
vot4 <- as.data.frame(cc)
table(vot4$tf1)
vot4 <- car::recode(cc$tf1, "7=1; 11=1; 15=1; 18=1; 2=1; 
                   12=2;
                   1=3;6=3;9=3;17=3;; 14=3;
                   5=4;4=4;
                   10=5; 3=5; 16=5; 8=5;
                   96=7; 98=7; 99=7")

vote4$vot4 <- car::recode(vote4$vot4, "NA=6")
vote4 <- as.data.frame(vot4)
table(vote4$vot4)


##Add new variables to the main dataset "value"
value1 <- cbind(value, vote1, vote4)


###########Exploratory Factor Analysis ##########

##EFA was performed both in R and Mplus, Results are very similar
##The paper reports the MPlus results as they use complete observations 
##and the method for estimating the model with missing values is Maximum Likelihood 
##(The problem in R with listwise or pairwise deletion is the assumption
##that the data has to be completely missing at random - MCAR - and this
##assumption is very difficult to satisfy) For this reason, 
##I go with ML estimation in MPlus. It is often discouraged to do listwise 
##deletion as it produces biased estimates. (See Marsh, 1998; Wothke, 1993)


###Check EFA
EFA1 <- psych::fa(value1[,1:14], nfactors=2, rotate="oblimin")
EFA1 


###Keep only those who voted with the following answer options: 1,2,3,4,5
#(remove 6 -did not vote- and 7-refuse/DK-  as answer choices)
votparl <- value1[!(value1$vot1 == 6 | value1$vot1 == 7 ),]
table(votparl$vot1)
describe(votparl[,15:20])

##Presidential votes (1,2,3,4,5 categories only)
#(remove 6 -did not vote- and 7-refuse/DK-  as answer choices)
votpresi <- value1[!(value1$vot4 == 6 | value1$vot4 == 7 ),]
table(votpresi$vot4)
describe(votpresi[,15:20])

###Take the mean out the three authority-nationalism items
votparl$auth1 <- rowMeans(votparl[ , c("Q29", "Q30","Q41")], na.rm=TRUE)
votpresi$auth1 <- rowMeans(votpresi[ , c("Q29", "Q30","Q41")], na.rm=TRUE)

###Take the mean out the five liberty-justice items
votparl$lib1 <- rowMeans(votparl[ , c("Q25","Q26","Q10","Q21", "Q16")], na.rm=TRUE)
votpresi$lib1 <- rowMeans(votpresi[ , c("Q25","Q26","Q10","Q21", "Q16")], na.rm=TRUE)



###Multinomial logit - parliamentary vote
dt1_1 <- multinom(vot1 ~ SI + Income + Age + Milieu +
                    Prayer + Gender + auth1 + lib1, data = votparl,
                  reflevel = 1)
summary(dt1_1)
stargazer(dt1_1, type= "latex")
exp(coef(dt1_1))


###Run Multinomial logit model - Presidential vote
dt1_4 <- multinom(vot4 ~ SI + Income + Age + Milieu +
                    Prayer + Gender + auth1 + lib1, data = votpresi,
                  reflevel = 1)
summary(dt1_4)
stargazer(dt1_4, type= "latex")


#####IIA Check ############################################
###########################################################
#############################################

###Checking multinomial assumptions
######Testing IIA assumption #######
##### Parliamentary vote######
d_mlogit <- dfidx(votparl, choice = "vot1", shape = "wide")

out1 <- mlogit(vot1  ~ 0 | SI + Income + Age + Milieu +
                 Prayer + Gender + auth1 + lib1, data = d_mlogit,
               reflevel = 1)

out2 <- mlogit(vot1  ~ 0 | SI + Income + Age + Milieu +
                 Prayer + Gender + auth1 + lib1, data = d_mlogit,
               reflevel = 2)

out3 <- mlogit(vot1  ~ 0 | SI + Income + Age + Milieu +
                 Prayer + Gender + auth1 + lib1, data = d_mlogit,
               reflevel = 3)

out4 <- mlogit(vot1  ~ 0 | SI + Income + Age + Milieu +
                 Prayer + Gender + auth1 + lib1, data = d_mlogit,
               reflevel = 4)

out1_1 <- mlogit(vot1  ~ 0 |  SI + Income + Age + Milieu +
                   Prayer + Gender + auth1 + lib1, data = d_mlogit,
                 alt.subset = c("2", "3", "4", "5"))

out1_2 <- mlogit(vot1  ~ 0 |  SI + Income + Age + Milieu +
                   Prayer + Gender + auth1 + lib1, data = d_mlogit,
                 alt.subset = c("1", "3", "4", "5"))


out1_3 <- mlogit(vot1  ~ 0 |  SI + Income + Age + Milieu +
                   Prayer + Gender + auth1 + lib1, data = d_mlogit,
                 alt.subset = c("1", "2", "4", "5"))

out1_4 <- mlogit(vot1  ~ 0 |  SI + Income + Age + Milieu +
                   Prayer + Gender + auth1 + lib1, data = d_mlogit,
                 alt.subset = c("1", "2", "3", "5"))

out1_5 <- mlogit(vot1  ~ 0 |  SI + Income + Age + Milieu +
                   Prayer + Gender + auth1 + lib1, data = d_mlogit,
                 alt.subset = c("1", "2", "3", "4"))

hmftest(out2, out1_1)
hmftest(out1, out1_2)
hmftest(out1, out1_3)
hmftest(out1, out1_4)
hmftest(out1, out1_5)

### ---> All P-values are above .05 (=1) therefore we can say 
##that the IIA assumption is not violated. IIA checks whether an 
## individual's choice depends on the characteristics of the 
##alternative choices. 


######DO IIA for presidential vote##########
d_mlogit1 <- dfidx(votpresi, choice = "vot4", shape = "wide")

fit1 <- mlogit(vot4  ~ 0 | SI + Income + Age + Milieu +
                 Prayer + Gender + auth1 + lib1, data = d_mlogit1,
               reflevel = 1)


fit2 <- mlogit(vot4  ~ 0 | SI + Income + Age + Milieu +
                 Prayer + Gender + auth1 + lib1, data = d_mlogit1,
               reflevel = 2)

out1_a <- mlogit(vot4  ~ 0 |  SI + Income + Age + Milieu +
                   Prayer + Gender + auth1 + lib1, data = d_mlogit1,
                 alt.subset = c("2", "3", "4","5"))

out1_b <- mlogit(vot4  ~ 0 |  SI + Income + Age + Milieu +
                   Prayer + Gender + auth1 + lib1, data = d_mlogit1,
                 alt.subset = c("1", "3", "4","5"))


out1_c <- mlogit(vot4  ~ 0 |  SI + Income + Age + Milieu +
                   Prayer + Gender + auth1 + lib1, data = d_mlogit1,
                 alt.subset = c("1", "2", "4","5"))

out1_d <- mlogit(vot4  ~ 0 |  SI + Income + Age + Milieu +
                   Prayer + Gender + auth1 + lib1, data = d_mlogit1,
                 alt.subset = c("1", "2", "3","5"))

out1_e <- mlogit(vot4  ~ 0 |  SI + Income + Age + Milieu +
                   Prayer + Gender + auth1 + lib1, data = d_mlogit1,
                 alt.subset = c("1", "2", "3","4"))

hmftest(fit2, out1_a)
hmftest(fit1, out1_b)
hmftest(fit1, out1_c)
hmftest(fit1, out1_d)
hmftest(fit1, out1_e)


####################################### PLotting ########################
###################Parliamentary election vote choice ##################

## Liberty-justice values ##########
dat85 <- ggeffects::ggeffect(model = dt1_1, terms = "lib1[all]", ci.lvl = .95)

##Islamist party
part1 <- dat85[1:35,]
F1 <- ggplot(data = part1 ) +
  geom_line(aes(x, y = predicted), color = "black") +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high), fill = "blue", alpha= 0.3, size = 1.5) + theme_bw() +
  xlab("Liberty-justice values") + ylab("Islamist")+ ylim(0, 0.8)
F1

##For center-left
part2 <- dat85[36:70,]
F2 <- ggplot(data = part2 ) +
  geom_line(aes(x, y = predicted), color = "black") +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high), fill = "red", alpha= 0.3, size = 1.5) + theme_bw() +
  xlab("Liberty-justice values") + ylab("Center-left") + ylim(0, 0.8)
F2


###For secular-nationalists
part3 <- dat85[71:105,]
F3 <- ggplot(data = part3 ) +
  geom_line(aes(x, y = predicted), color = "black") +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high), fill = "gray1", alpha= 0.3, size = 1.5) + theme_bw() +
  xlab("Liberty-justice values") + ylab("Secular-nationalist") + ylim(0, 0.8)
F3

###social democrats
part4 <- dat85[106:140,]
F4 <- ggplot(data = part4 ) +
  geom_line(aes(x, y = predicted), color = "black") +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high), fill = "darkgoldenrod3", alpha= 0.3, size = 1.5) + theme_bw() +
  xlab("Liberty-justice values") + ylab("Social Democrats") + ylim(0, 0.8)
F4

##Independents
part5 <- dat85[141:175,]
F5 <- ggplot(data = part5 ) +
  geom_line(aes(x, y = predicted), color = "black") +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high), fill = "chocolate4", alpha= 0.3, size = 1.5) + theme_bw() +
  xlab("Liberty-justice values") + ylab("Independent") + ylim(0, 0.8)
F5

F1 + F2 + F3 + F4 + F5 + plot_layout(ncol=2, nrow = 3)+ 
  plot_annotation(title ="Predicted Probability to vote for:" , theme = theme(plot.title = element_text(size = 13)))

F1 + F2 + plot_layout(ncol=2)
F3 + F4 + plot_layout(ncol=2)

##try the two plots together
###########Islamists vs. Center-left########
T1 <- ggplot(data = part1 ) +
  geom_line(data = part1, aes(x, y = predicted), color = "black") +
  geom_ribbon(data = part1,aes(x, ymin = conf.low, ymax = conf.high), fill = "blue", alpha= 0.3, size = 1.5) + theme_bw() +
  
  geom_line(data = part2,aes(x, y = predicted), color = "black", linetype = "dashed") +
  geom_ribbon(data = part2,aes(x, ymin = conf.low, ymax = conf.high), fill = "red", alpha= 0.3, size = 1.5) + theme_bw() +
  xlab("Liberty-justice values") + ylab("Predicted Probability") +
  annotate("text", x = 1.5, y =0.75, label = "center-left", color = "red")+
  annotate("text", x = 1.5, y =0.05, label = "Islamists", color = "blue")

T1

########Center-left vs. Secular-nationalists#################
T1_1 <- ggplot(data = part2 ) +
  geom_line(data = part2, aes(x, y = predicted), color = "black") +
  geom_ribbon(data = part2,aes(x, ymin = conf.low, ymax = conf.high), fill = "red", alpha= 0.3, size = 1.5) + theme_bw() +
  
  geom_line(data = part3,aes(x, y = predicted), color = "black", linetype = "dashed") +
  geom_ribbon(data = part3,aes(x, ymin = conf.low, ymax = conf.high), fill = "gray2", alpha= 0.3, size = 1.5) + theme_bw() +
  xlab("Liberty-justice values") + ylab("Predicted Probability") +
  annotate("text", x = 1.8, y =0.21, label = "Secular-nationalists", color = "gray1")+
  annotate("text", x = 1.5, y =0.72, label = "Center-left", color = "red")

T1_1

###########Center-left vs. Social-democrats ####################
T1_2 <- ggplot(data = part1 ) +
  geom_line(data = part1, aes(x, y = predicted), color = "black") +
  geom_ribbon(data = part1,aes(x, ymin = conf.low, ymax = conf.high), fill = "blue", alpha= 0.3, size = 1.5) + theme_bw() +
  
  geom_line(data = part4,aes(x, y = predicted), color = "black", linetype = "dashed") +
  geom_ribbon(data = part4,aes(x, ymin = conf.low, ymax = conf.high), fill = "darkgoldenrod3", alpha= 0.3, size = 1.5) + theme_bw() +
  xlab("Liberty-justice values") + ylab("Predicted Probability") +
  annotate("text", x = 1.8, y =0.55, label = "Social democrat", color = "darkgoldenrod3")+
  annotate("text", x = 1.5, y =0.05, label = "Islamist", color = "blue")

T1_2

#############################################################
###########Authority values ### parliamentary vote#######

dat95 <- ggeffects::ggeffect(model = dt1_1, terms = "auth1[all]", ci.lvl = .95)

###Islamists ##
par1 <- dat95[1:13,]
Y1 <- ggplot(data = par1 ) +
  geom_line(aes(x, y = predicted), color = "black") +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high), fill = "blue", alpha= 0.3, size = 1.5) + theme_bw() +
  xlab("Authority values") + ylab("Islamist")+ ylim(0,0.9)
Y1

##For center-left
par2 <- dat95[14:26,]
Y2 <- ggplot(data = par2 ) +
  geom_line(aes(x, y = predicted), color = "black") +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high), fill = "red", alpha= 0.3, size = 1.5) + theme_bw() +
  xlab("Authority values") + ylab("Center-Left")+ ylim(0, 0.9)
Y2

###For secular-nationalists
par3 <- dat95[27:39,]
Y3 <- ggplot(data = par3 ) +
  geom_line(aes(x, y = predicted), color = "black") +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high), fill = "gray1", alpha= 0.3, size = 1.5) + theme_bw() +
  xlab("Authority values") + ylab("Secular Nationalist")+ylim(0, 0.9)
Y3

###social democrats
par4 <- dat95[40:52,]
Y4 <- ggplot(data = par4 ) +
  geom_line(aes(x, y = predicted), color = "black") +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high), fill = "darkgoldenrod3", alpha= 0.3, size = 1.5) + theme_bw() +
  xlab("Authority values") + ylab("Social Democrat")+ylim(0,0.9)
Y4

##Independents
par5 <- dat95[53:65,]
Y5 <- ggplot(data = par5 ) +
  geom_line(aes(x, y = predicted), color = "black") +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high), fill = "chocolate3", alpha= 0.3, size = 1.5) + theme_bw() +
  xlab("Authority values") + ylab("Independent")+ylim(0,0.9)
Y5

Y1 + Y2 + Y3 + Y4 + Y5 + plot_layout(ncol=2, nrow = 3)+
  plot_annotation(title ="Parliamentary Vote: Predicted Probability to vote for:" , theme = theme(plot.title = element_text(size = 13)))


##Together
##Center-left vs. Islamists
Y1_1 <- ggplot(data = par1 ) +
  geom_line(data = par1, aes(x, y = predicted), color = "black") +
  geom_ribbon(data = par1, aes(x, ymin = conf.low, ymax = conf.high), fill = "blue", alpha= 0.3, size = 1.5) + theme_bw() +
  
  geom_line(data = par2,aes(x, y = predicted), color = "black", linetype = "dashed") +
  geom_ribbon(data = par2,aes(x, ymin = conf.low, ymax = conf.high), fill = "red", alpha= 0.3, size = 1.5) + theme_bw() +
  xlab("Auhority values") + ylab("Predicted Probability") +
  annotate("text", x = 1.5, y =0.21, label = "center-left", color = "red")+
  annotate("text", x = 1.5, y =0.85, label = "Islamist", color = "blue")
Y1_1



##############################Predicted Probabilities #################
#######################################################################
#################################Parliamentary vote ###################

###Predicted Probabilities all outcome values across the range of
##Prayer (1to 5)
set.seed(1234)
simb <- mvrnorm(n = 1000, mu = coef(out1), Sigma = vcov(out1))
simb2 <- simb[, seq(1, 36, 4)] # simulated coefficients for center-left (vs islamists)
simb3 <- simb[, seq(2, 37, 4)] # simulated coefficients for secular-nationalist (vs islamists)
simb4 <- simb[, seq(3, 38, 4)] # for social democrats (vs. Islamists)
simb5 <- simb[, seq(4, 39, 4)] # for independents (vs. Islamists)


# Matrix to store results
## Step 1: Preparation
out_mat1 <- matrix(NA, nrow = 5, ncol = 15) 
colnames(out_mat1) <- paste0(rep(c("Mean", "Lower", "Upper"), times =5),
                             rep(1:5, each = 3))

# Compute the predicted probs
for (i in 1:5){ # for  each  value  of prayer...
  xb2 <- (simb2[, 1] # intercept
          
          + simb2[, 2] * mean(votparl$SI, na.rm = T) # lr
          + simb2[, 3] * mean(votparl$Income, na.rm = T) # income
          + simb2[, 4] * mean(votparl$Age, na.rm = T) # gender
          + simb2[, 5] * mean(votparl$Milieu, na.rm = T) #Milieu
          + simb2[, 6] * i #prayer
          + simb2[, 7] * 0 #gender
          + simb2[, 8] * mean(votparl$auth1, na.rm = T) #authority values
          + simb2[, 9]* mean(votparl$lib1, na.rm = T) #liberty-justice values
  )
  xb3 <- (simb3[, 1] # intercept
          + simb3[, 2] * mean(votparl$SI, na.rm = T) # lr
          + simb3[, 3] * mean(votparl$Income, na.rm = T) # income
          + simb3[, 4] * mean(votparl$Age, na.rm = T) # gender
          + simb3[, 5] * mean(votparl$Milieu, na.rm = T) #Milieu
          + simb3[, 6] * i #prayer
          + simb3[, 7] * 0 #gender
          + simb3[, 8] * mean(votparl$auth1, na.rm = T)
          + simb3[, 9]* mean(votparl$lib1, na.rm = T)
  )
  
  xb4 <- (simb4[, 1] # intercept
          + simb4[, 2] * mean(votparl$SI, na.rm = T) # lr
          + simb4[, 3] * mean(votparl$Income, na.rm = T) # income
          + simb4[, 4] * mean(votparl$Age, na.rm = T) # gender
          + simb4[, 5] * mean(votparl$Milieu, na.rm = T) #Milieu
          + simb4[, 6] * i #prayer
          + simb4[, 7] * 0 #gender
          + simb4[, 8] * mean(votparl$auth1, na.rm = T)
          + simb4[, 9]* mean(votparl$lib1, na.rm = T)
  )
  xb5 <- (simb5[, 1] # intercept
          
          + simb5[, 2] * mean(votparl$SI, na.rm = T) # lr
          + simb5[, 3] * mean(votparl$Income, na.rm = T) # income
          + simb5[, 4] * mean(votparl$Age, na.rm = T) # gender
          + simb5[, 5] * mean(votparl$Milieu, na.rm = T) #Milieu
          + simb5[, 6] * i #prayer
          + simb5[, 7] * 0 #gender
          + simb5[, 8] * mean(votparl$auth1, na.rm = T) #authority values
          + simb5[, 9]* mean(votparl$lib1, na.rm = T) #liberty-justice values
  )
  
  denominator <- (1 + exp(xb2) + exp(xb3) + exp(xb4)+ exp(xb5))
  # store the simulation summaries
  out_mat_1<- matrix(NA, nrow = 1000, ncol = 5)
  out_mat_1[,1] <- 1/ denominator
  out_mat_1[,2] <- exp(xb2)/ denominator
  out_mat_1[,3] <- exp(xb3)/ denominator
  out_mat_1[,4] <- exp(xb4)/ denominator
  out_mat_1[,5] <- exp(xb5)/ denominator
  
  out_mat1[i, c(1,4,7,10,13)] <- colMeans(out_mat_1)
  CI <- apply(out_mat_1, 2 ,quantile, probs = c(0.025, 0.975))
  out_mat1[i, 2:3] <- CI[,1]
  out_mat1[i, 5:6] <- CI[,2]
  out_mat1[i, 8:9] <- CI[,3]
  out_mat1[i, 11:12] <- CI[,4]
  out_mat1[i, 14:15] <- CI[,5]
}


out_mat1 <- as.data.frame(out_mat1)
out_mat1$Prayer <- c(1:5)

##Plot out
B1 <- ggplot(data = out_mat1) + 
  geom_pointrange(aes(x = Prayer, y= Mean1, ymin =Lower1, ymax= Upper1),
                  shape = 18, color = "cyan4") +
  xlab("Prayer") + ylab("Predicted Probability to vote Islamists") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.9) +  theme_bw()
B1


B2 <- ggplot(data = out_mat1) + 
  geom_pointrange(aes(x = Prayer, y= Mean2, ymin =Lower2, ymax= Upper2),
                  shape = 17, color = "red") +
  xlab("Prayer") + ylab("Predicted Probability to vote Center-Left") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday")) +
  ylim(0, 0.9) +  theme_bw()
B2


B3 <- ggplot(data = out_mat1) + 
  geom_pointrange(aes(x = Prayer, y= Mean3, ymin =Lower3, ymax= Upper3),
                  shape = 16, color = "gray2") +
  xlab("Prayer") + ylab("Predicted Probability to vote Secular-Nationalists") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.9) +  theme_bw()
B3


B4 <- ggplot(data = out_mat1) + 
  geom_pointrange(aes(x = Prayer, y= Mean4, ymin =Lower4, ymax= Upper4),
                  shape = 15, color = "goldenrod3") +
  xlab("Prayer") + ylab("Predicted Probability to vote Social Democrats") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.9) +  theme_bw()
B4

B5 <- ggplot(data = out_mat1) + 
  geom_pointrange(aes(x = Prayer, y= Mean5, ymin =Lower5, ymax= Upper5),
                  shape = 10, color = "chocolate3") +
  xlab("Prayer") + ylab("Predicted Probability to vote Independent") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.9) +  theme_bw()
B5

##Plot all five together
B1 + B2 + B3 + B4 + B5 + plot_layout(ncol=3, nrow = 2)
B1 + B2 + plot_layout(ncol = 2)
B3 + B4 + plot_layout(ncol = 2)

##Plot out all five plots together
Bb1 <- ggplot(data = out_mat1) + 
  geom_pointrange(aes(x = Prayer, y= Mean1, ymin =Lower1, ymax= Upper1),
                  shape = 18, color = "cyan4") +
  xlab("Prayer") + ylab("Islamists") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.9) +  theme_bw()


Bb2 <- ggplot(data = out_mat1) + 
  geom_pointrange(aes(x = Prayer, y= Mean2, ymin =Lower2, ymax= Upper2),
                  shape = 17, color = "red") +
  xlab("Prayer") + ylab("Center-Left") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday")) +
  ylim(0, 0.9) +  theme_bw()


Bb3 <- ggplot(data = out_mat1) + 
  geom_pointrange(aes(x = Prayer, y= Mean3, ymin =Lower3, ymax= Upper3),
                  shape = 16, color = "gray2") +
  xlab("Prayer") + ylab("Secular-Nationalists") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.9) +  theme_bw()


Bb4 <- ggplot(data = out_mat1) + 
  geom_pointrange(aes(x = Prayer, y= Mean4, ymin =Lower4, ymax= Upper4),
                  shape = 15, color = "goldenrod3") +
  xlab("Prayer") + ylab("Social Democrats") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.9) +  theme_bw()

Bb5 <- ggplot(data = out_mat1) + 
  geom_pointrange(aes(x = Prayer, y= Mean5, ymin =Lower5, ymax= Upper5),
                  shape = 10, color = "chocolate3") +
  xlab("Prayer") + ylab("Independent") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.9) +  theme_bw()

Bb1 + Bb2 + Bb3 + Bb4 + Bb5 + plot_layout(ncol=2, nrow = 3)+
  plot_annotation(title ="Parliamentary Vote: Predicted Probability to vote for:" , theme = theme(plot.title = element_text(size = 13)))

#############################DO the same with presidential vote choice###
###Presidential vote#####################################################


###Predicted Probabilities all outcome values across the range of
##Prayer (1to 5)
set.seed(1234)
simb <- mvrnorm(n = 1000, mu = coef(fit1), Sigma = vcov(fit1))
simb2 <- simb[, seq(1, 36, 4)] # simulated coefficients for center-left (vs islamists)
simb3 <- simb[, seq(2, 37, 4)] # simulated coefficients for secular-nationalist (vs islamists)
simb4 <- simb[, seq(3, 38, 4)] # for social democrats (vs. Islamists)
simb5 <- simb[, seq(4, 39, 4)] # for independents (vs. Islamists)


# Matrix to store results
## Step 1: Preparation
out_mat2 <- matrix(NA, nrow = 5, ncol = 15) 
colnames(out_mat2) <- paste0(rep(c("Mean", "Lower", "Upper"), times =5),
                             rep(1:5, each = 3))

# Compute the predicted probs
for (i in 1:5){ # for  each  value  of prayer...
  xb2 <- (simb2[, 1] # intercept
          
          + simb2[, 2] * mean(votpresi$SI, na.rm = T) # lr
          + simb2[, 3] * mean(votpresi$Income, na.rm = T) # income
          + simb2[, 4] * mean(votpresi$Age, na.rm = T) # gender
          + simb2[, 5] * mean(votpresi$Milieu, na.rm = T) #Milieu
          + simb2[, 6] * i #prayer
          + simb2[, 7] * 0 #gender
          + simb2[, 8] * mean(votpresi$auth1, na.rm = T) #authority values
          + simb2[, 9]* mean(votpresi$lib1, na.rm = T) #liberty-justice values
  )
  xb3 <- (simb3[, 1] # intercept
          + simb3[, 2] * mean(votpresi$SI, na.rm = T) # lr
          + simb3[, 3] * mean(votpresi$Income, na.rm = T) # income
          + simb3[, 4] * mean(votpresi$Age, na.rm = T) # gender
          + simb3[, 5] * mean(votpresi$Milieu, na.rm = T) #Milieu
          + simb3[, 6] * i #prayer
          + simb3[, 7] * 0 #gender
          + simb3[, 8] * mean(votpresi$auth1, na.rm = T)
          + simb3[, 9]* mean(votpresi$lib1, na.rm = T)
  )
  
  xb4 <- (simb4[, 1] # intercept
          + simb4[, 2] * mean(votpresi$SI, na.rm = T) # lr
          + simb4[, 3] * mean(votpresi$Income, na.rm = T) # income
          + simb4[, 4] * mean(votpresi$Age, na.rm = T) # gender
          + simb4[, 5] * mean(votpresi$Milieu, na.rm = T) #Milieu
          + simb4[, 6] * i #prayer
          + simb4[, 7] * 0 #gender
          + simb4[, 8] * mean(votpresi$auth1, na.rm = T)
          + simb4[, 9]* mean(votpresi$lib1, na.rm = T)
  )
  xb5 <- (simb5[, 1] # intercept
          
          + simb5[, 2] * mean(votpresi$SI, na.rm = T) # lr
          + simb5[, 3] * mean(votpresi$Income, na.rm = T) # income
          + simb5[, 4] * mean(votpresi$Age, na.rm = T) # gender
          + simb5[, 5] * mean(votpresi$Milieu, na.rm = T) #Milieu
          + simb5[, 6] * i #prayer
          + simb5[, 7] * 0 #gender
          + simb5[, 8] * mean(votpresi$auth1, na.rm = T) #authority values
          + simb5[, 9]* mean(votpresi$lib1, na.rm = T) #liberty-justice values
  )
  
  denominator <- (1 + exp(xb2) + exp(xb3) + exp(xb4)+ exp(xb5))
  # store the simulation summaries
  out_mat_2<- matrix(NA, nrow = 1000, ncol = 5)
  out_mat_2[,1] <- 1/ denominator
  out_mat_2[,2] <- exp(xb2)/ denominator
  out_mat_2[,3] <- exp(xb3)/ denominator
  out_mat_2[,4] <- exp(xb4)/ denominator
  out_mat_2[,5] <- exp(xb5)/ denominator
  
  out_mat2[i, c(1,4,7,10,13)] <- colMeans(out_mat_2)
  CI <- apply(out_mat_2, 2 ,quantile, probs = c(0.025, 0.975))
  out_mat2[i, 2:3] <- CI[,1]
  out_mat2[i, 5:6] <- CI[,2]
  out_mat2[i, 8:9] <- CI[,3]
  out_mat2[i, 11:12] <- CI[,4]
  out_mat2[i, 14:15] <- CI[,5]
}


out_mat2 <- as.data.frame(out_mat2)
out_mat2$Prayer <- c(1:5)

##Plot out
D1 <- ggplot(data = out_mat2) + 
  geom_pointrange(aes(x = Prayer, y= Mean1, ymin = Lower1, ymax= Upper1),
                  shape = 18, color = "cyan4") +
  xlab("Prayer") + ylab("Predicted Probability to vote Right-wing") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.8) +  theme_bw()
D1

D2 <- ggplot(data = out_mat2) + 
  geom_pointrange(aes(x = Prayer, y= Mean2, ymin =Lower2, ymax= Upper2),
                  shape = 17, color = "red") +
  xlab("Prayer") + ylab("Predicted Probability to vote Center-Left") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.8) +  theme_bw()
D2


D3 <- ggplot(data = out_mat2) + 
  geom_pointrange(aes(x = Prayer, y= Mean3, ymin =Lower3, ymax= Upper3),
                  shape = 16, color = "gray2") +
  xlab("Prayer") + ylab("Predicted Probability to vote Secular-nationalist") + 
  ggtitle("") +scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.8) +  theme_bw()
D3


D4 <- ggplot(data = out_mat2) + 
  geom_pointrange(aes(x = Prayer, y= Mean4, ymin =Lower4, ymax= Upper4),
                  shape = 15, color = "goldenrod3") +
  xlab("Prayer") + ylab("Predicted Probability to vote Social-democrats") + 
  ggtitle("") +scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.8) +  theme_bw()
D4


D5 <- ggplot(data = out_mat2) + 
  geom_pointrange(aes(x = Prayer, y= Mean5, ymin =Lower5, ymax= Upper5),
                  shape = 15, color = "chocolate3") +
  xlab("Prayer") + ylab("Predicted Probability to vote Independent") + 
  ggtitle("") +scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.8) +  theme_bw()
D5

###Plot two together####
D1 + D2 + plot_layout(ncol = 2)
D5 + D3 + plot_layout(ncol = 2)


D2 + D2 + D3 + D4 + plot_layout(ncol=4)


########Plot out all five plots together#####
Dd1 <- ggplot(data = out_mat2) + 
  geom_pointrange(aes(x = Prayer, y= Mean1, ymin =Lower1, ymax= Upper1),
                  shape = 18, color = "cyan4") +
  xlab("Prayer") + ylab("Right-Wing") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.9) +  theme_bw()
Dd1

Dd2 <- ggplot(data = out_mat2) + 
  geom_pointrange(aes(x = Prayer, y= Mean2, ymin =Lower2, ymax= Upper2),
                  shape = 17, color = "red") +
  xlab("Prayer") + ylab("Center-Left") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday")) +
  ylim(0, 0.9) +  theme_bw()


Dd3 <- ggplot(data = out_mat2) + 
  geom_pointrange(aes(x = Prayer, y= Mean3, ymin =Lower3, ymax= Upper3),
                  shape = 16, color = "gray2") +
  xlab("Prayer") + ylab("Secular-Nationalists") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.9) +  theme_bw()


Dd4 <- ggplot(data = out_mat2) + 
  geom_pointrange(aes(x = Prayer, y= Mean4, ymin =Lower4, ymax= Upper4),
                  shape = 15, color = "goldenrod3") +
  xlab("Prayer") + ylab("Social Democrats") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.9) +  theme_bw()

Dd5 <- ggplot(data = out_mat2) + 
  geom_pointrange(aes(x = Prayer, y= Mean5, ymin =Lower5, ymax= Upper5),
                  shape = 10, color = "chocolate3") +
  xlab("Prayer") + ylab("Independent") + 
  ggtitle("") + scale_x_continuous(breaks = c(1:5), labels= c("never", "2","3","4","everyday"))+
  ylim(0, 0.9) +  theme_bw()

Dd1 + Dd2 + Dd3 + Dd4 + Dd5 + plot_layout(ncol=2, nrow = 3)+
  plot_annotation(title ="Presidential Vote: Predicted Probability to vote for:" , theme = theme(plot.title = element_text(size = 13)))


###################################Robustness Checks ###################
########################################################################
########################################################################

###Robustness checks ### table 3 Appendix C #######
##Do it with full model including voters and non-voters
##FUll sample to compare results - parliamentary

###Take the mean out the three authority-nationalism items
value1$auth1 <- rowMeans(value1[ , c("Q29", "Q30","Q41")], na.rm=TRUE)

###Take the mean out the five liberty-justice items
value1$lib1 <- rowMeans(value1[ , c("Q25","Q26","Q10","Q21", "Q16")], na.rm=TRUE)

##Parliamentary model
full1 <- multinom(vot1 ~   SI + Income + Age + Milieu +
                   Prayer +  auth1 + lib1, data = value1,
                 reflevel = 1)

stargazer(full1, type = "latex")

##Presidential model #########table 4 Appendix C #######
full2 <- multinom(vot4 ~   SI + Income + Age + Milieu +
                    Prayer +  auth1 + lib1, data = value1,
                  reflevel = 1)

stargazer(full2, type = "latex")

#### Robustness Checks #### table 5 Appendix D ########
###Now do it with binomial logit ##Parliamentary vote choice
##Islamists against everyone else (Islamists = 1 and all rest = 0)
votparl$Isl <- car::recode(votparl$vot1, "1=1; 2=0; 3=0; 4=0; 5=0")

##center left against everyone else(Center-left = 1 and all rest = 0)
votparl$CL <- car::recode(votparl$vot1, "1=0; 2=1; 3=0;4=0; 5=0")

##secular-nationalists against everyone else(secular-nationalist = 1 and all rest = 0)
votparl$SN <- car::recode(votparl$vot1, "1=0; 2=0; 3=1;4=0; 5=0")

##Social-democrats against everyone else(social-democrats = 1 and all rest = 0)
votparl$SD <- car::recode(votparl$vot1, "1=0; 2=0; 3=0;4=1; 5=0")

##Independents against everyone else(independent= 1 and all rest = 0)
votparl$Ind <- car::recode(votparl$vot1, "1=0; 2=0; 3=0;4=0; 5=1")


dt_CL <- glm(CL ~ SI + Income + Age + Milieu +
               Prayer + Gender + auth1 + lib1, 
             data =votparl, family = "binomial")

dt_Isl <- glm(Isl ~ SI + Income + Age + Milieu +
                Prayer + Gender + auth1 + lib1, 
              data = votparl, family = "binomial")

dt_SN <- glm(SN ~ SI + Income + Age + Milieu +
               Prayer + Gender + auth1 + lib1, 
             data = votparl, family = "binomial")

dt_SD <- glm(SD ~ SI + Income + Age + Milieu +
               Prayer + Gender + auth1 + lib1, 
             data = votparl, family = "binomial")

dt_Ind <- glm(Ind ~ SI + Income + Age + Milieu +
                Prayer + Gender + auth1 + lib1, 
              data = votparl, family = "binomial")

stargazer(dt_Isl, dt_CL, dt_SN, dt_SD, dt_Ind, type = "latex")

###Now do it with binomial logit ###Presidential vote ###Table 6 Appendix D
##Islamists against everyone else (Islamists = 1 and all rest = 0)
votpresi$Right <- car::recode(votpresi$vot4, "1=1; 2=0; 3=0; 4=0; 5=0")

##center left against everyone else(Center-left = 1 and all rest = 0)
votpresi$CL <- car::recode(votpresi$vot4, "1=0; 2=1; 3=0;4=0; 5=0")

##secular-nationalists against everyone else(secular-nationalist = 1 and all rest = 0)
votpresi$SN <- car::recode(votpresi$vot4, "1=0; 2=0; 3=1;4=0; 5=0")

##Social-democrats against everyone else(social-democrats = 1 and all rest = 0)
votpresi$SD <- car::recode(votpresi$vot4, "1=0; 2=0; 3=0;4=1; 5=0")

##Independents against everyone else(independent= 1 and all rest = 0)
votpresi$Ind <- car::recode(votpresi$vot4, "1=0; 2=0; 3=0;4=0; 5=1")


dt_CL2 <- glm(CL ~ SI + Income + Age + Milieu +
                Prayer + Gender + auth1 + lib1, data = votpresi, family = "binomial")

dt_Isl2 <- glm(Right ~ SI + Income + Age + Milieu +
                 Prayer + Gender + auth1 + lib1, data = votpresi, family = "binomial")

dt_SN2 <- glm(SN ~ SI + Income + Age + Milieu +
                Prayer + Gender + auth1 + lib1, data = votpresi, family = "binomial")

dt_SD2 <- glm(SD ~ SI + Income + Age + Milieu +
                Prayer + Gender + auth1 + lib1, data = votpresi, family = "binomial")

dt_Ind2 <- glm(Ind ~ SI + Income + Age + Milieu +
                 Prayer + Gender + auth1 + lib1, data = votpresi, family = "binomial")

stargazer(dt_Isl2, dt_CL2, dt_SN2, dt_SD2, dt_Ind2, type = "latex")



###########################################################
##Robustness checks ####### Tables 7,8,9,10
##Estimating the models by including the secular-Islamist dimension and value-dimensions separately

###Table 7 ## parliamentary vote choice excluding value-dimensions
tab7 <- multinom(vot1 ~ SI + Income + Age + Milieu +
                      Prayer + Gender, data = votparl,
                    reflevel = 1)

summary(tab7)
stargazer(tab7, type = "latex")


##Table 8### parliamentary vote choice excluding SI variable
tab8 <- multinom(vot1 ~ Income + Age + Milieu +
                   Prayer + Gender + auth1 + lib1, data = votparl,
                 reflevel = 1)

summary(tab8)
stargazer(tab8, type = "latex")

###Table 9 ## presidential vote choice excluding value-dimensions
tab9 <- multinom(vot4 ~ SI + Income + Age + Milieu +
                   Prayer + Gender, data = votpresi,
                 reflevel = 1)

summary(tab9)
stargazer(tab9, type = "latex")

##Table 10### presidential vote choice excluding SI variable
tab10 <- multinom(vot4 ~ Income + Age + Milieu +
                   Prayer + Gender + auth1 + lib1, data = votpresi,
                 reflevel = 1)

summary(tab10)
stargazer(tab10, type = "latex")



#########################################################
##Robustness checks ####### Tables 11, 12 Appendix F
####Run the same multinomial regression but with different party classification ###
##less fine-grained classification #######
###Here I group all leftist parties together#########

##change classification, center-left, secular-nationalists and social democrats = LEFT
votparl$vot1_1 <- car::recode(votparl$vot1, "1=1; 2=2;3=3;4=3;5=4")

votparl$vot1_1 <- car::recode(votparl$vot1, "1=1; 2=2;3=2;4=2;5=3")

dt1_2_2 <- multinom(vot1_1 ~ SI + Income + Age + Milieu +
                      Prayer + Gender + auth1 + lib1, data = votparl,
                    reflevel = 1)
summary(dt1_2_2)
stargazer(dt1_2_2, type= "latex")

###change classification for presidential vote choice
votpresi$vot4_1 <- car::recode(votpresi$vot4, "1=1; 2=2;3=2;4=2;5=3")

dt1_4_2 <- multinom(vot4_1 ~ SI + Income + Age + Milieu +
                      Prayer + Gender + auth1 + lib1, data = votpresi,
                    reflevel = 1)
summary(dt1_4_2)
stargazer(dt1_4_2, type= "latex")



########################################################################
###Robustness checks ############### Tables 13, 14 Appendix F
##############Remove independent categories and check results ###############

#(remove 4, 5 and 6 as answer choices)
votparl2 <- votparl[!(votparl$vot1 == 5 |votparl$vot1 == 6 | votparl$vot1 == 7 ),]
table(votparl2$vot1)

##Presidential votes (1,2,3,4 categories only)
votpresi2 <- votpresi[!(votpresi$vot4 == 5 | votpresi$vot4 == 6 | votpresi$vot4 == 7 ),]
table(votpresi2$vot4)


##Re-run model - parliamentary vote
dt1_2_3 <- multinom(vot1 ~ SI + Income + Age + Milieu +
                      Prayer + Gender + auth1 + lib1, data = votparl2,
                    reflevel = 1)
summary(dt1_2_3)
stargazer(dt1_2_3, type= "latex")

##Re-run model - presidential vote
dt1_4_3 <- multinom(vot4 ~ SI + Income + Age + Milieu +
                      Prayer + Gender + auth1 + lib1, data = votpresi2,
                    reflevel = 1)
summary(dt1_4_3)
stargazer(dt1_4_3, type= "latex")


################################################
###############Robustness Checks################
################################################
#Finally, a latent variable analysis has been conducted as a final robustness check!
#To the best of my knowledge, latent analysis (or Structural Equation Model using 
#categorical endogenous variable) is not supported in R when the dependent variable 
#is unordered categorical. For this reason, I conducted the latent analysis using 
#MPlus (version 7), all results are still robust. The only estimate that differs from 
#the multinomial logit model is the authority-nationalist value coefficient that is 
#significant only at 0.1 alpha level for predicting vote for 
#secular-nationalists vs. right-wing candidates in the presidential election vote choice. 
#This difference is due to the different ways value scores are computed in the two approaches. 
#In multinomial regression, the average across items pertaining to each dimension is computed 
#whereas in the latent analysis, a weighted average is computed. Existing studies recommend 
#scholars to use average scores if the study is exploratory and the scale has not been 
#validated yet, which is the case in this study 
#(See Hair et al,. 2006; Tabeachinck and Fidell, 2001). 
#Materials and code from MPlus can be shared upon request.


####################################################################
###############END #####################################################


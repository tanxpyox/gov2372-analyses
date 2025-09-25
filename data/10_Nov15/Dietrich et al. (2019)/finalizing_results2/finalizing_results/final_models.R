#################################################################
# Description: Replicates Table 1 in Dietrich, Enos, Sen (2018) #
# Author: Bryce J. Dietrich                                     #
# Affiliation: University of Iowa                               #
# Date: 5/24/2018                                               #
# Email: brycedietrich@gmail.com                                #
# R Version: 3.4.3 (2017-11-30)                                 #
# Platform: x86_64-apple-darwin15.6.0 (64-bit)                  #
# Computer: MacBook Pro (Retina, 13-inch, Early 2013)           #
# Processor: 3 GHz Intel Core i7                                #
# OS: macOS Sierra 10.12.6                                      #
# Data: justice_results.tab                                     #
# Packages: lme4_1.1-15                                         #
#           Matrix_1.2-12                                       #
#           stargazer_5.2                                       #
# Output: table_1.html                                          #
# Run Time: 26.18765 secs                                       #
#################################################################

require(lme4)
require(stargazer)

setwd('/Users/brycedietrich/Dropbox/oral_arguments_base/replication/finalizing_results/')

#load justice_results
sc<-read.table("justice_results.tab",header=TRUE,as.is=TRUE,sep="\t")

#intercept only (Table 1, Model 1)
mod0<-glmer(petitioner_vote~1+(1|justiceName),data=sc,family=binomial)
pred_mod0<-sum(diag(table(ifelse(predict(mod0,type="response")>.50,1,0),sc[names(residuals(mod0)),"petitioner_vote"])))/length(residuals(mod0))

#pitch only (Table 1, Model 2)
mod1<-glmer(petitioner_vote~pitch_diff+(1|justiceName),data=sc,family=binomial)
pred_mod1<-sum(diag(table(ifelse(predict(mod1,type="response")>.50,1,0),sc[names(residuals(mod1)),"petitioner_vote"])))/length(residuals(mod1))

#dal model (Table 1, Model 3)
sc$petitioner_pos_words<-sc$petitioner_dal_pos
sc$petitioner_neg_words<-sc$petitioner_dal_neg
sc$respondent_pos_words<-sc$respondent_dal_pos
sc$respondent_neg_words<-sc$respondent_dal_neg

mod2<-glmer(petitioner_vote~pitch_diff+I((petitioner_neg_words/petitioner_wc)-(respondent_neg_words/respondent_wc))+I((petitioner_pos_words/petitioner_wc)-(respondent_pos_words/respondent_wc))+I(petitioner_count-respondent_count)+lagged_ideology+conservative_lc+I(lagged_ideology*conservative_lc)+sgpetac+sgrespac+petac+respac+petNumStat+respNumStat+(1|justiceName),data=sc,family=binomial,nAGQ=2)
pred_mod2<-sum(diag(table(ifelse(predict(mod2,type="response")>.50,1,0),sc[names(residuals(mod2)),"petitioner_vote"])))/length(residuals(mod2))
#the model does not converge unless the number of points per axis for evaluating the adaptive Gauss-Hermite approximation to the log-likelihood is increased from 0. Coefficients and prediction rate are essentiall the same regardless of nAGQ used. More specifically, max coefficient change is around 10^-04

#harvard model (Table 1, Model 4)
sc$petitioner_pos_words<-sc$petitioner_harvard_pos
sc$petitioner_neg_words<-sc$petitioner_harvard_neg
sc$respondent_pos_words<-sc$respondent_harvard_pos
sc$respondent_neg_words<-sc$respondent_harvard_neg

mod3<-glmer(petitioner_vote~pitch_diff+I((petitioner_neg_words/petitioner_wc)-(respondent_neg_words/respondent_wc))+I((petitioner_pos_words/petitioner_wc)-(respondent_pos_words/respondent_wc))+I(petitioner_count-respondent_count)+lagged_ideology+conservative_lc+I(lagged_ideology*conservative_lc)+sgpetac+sgrespac+petac+respac+petNumStat+respNumStat+(1|justiceName),data=sc,family=binomial)
pred_mod3<-sum(diag(table(ifelse(predict(mod3,type="response")>.50,1,0),sc[names(residuals(mod3)),"petitioner_vote"])))/length(residuals(mod3))

#liwc model (Table 1, Model 5)
sc$petitioner_pos_words<-sc$petitioner_liwc_pos
sc$petitioner_neg_words<-sc$petitioner_liwc_neg
sc$respondent_pos_words<-sc$respondent_liwc_pos
sc$respondent_neg_words<-sc$respondent_liwc_neg

mod4<-glmer(petitioner_vote~pitch_diff+I((petitioner_neg_words/petitioner_wc)-(respondent_neg_words/respondent_wc))+I((petitioner_pos_words/petitioner_wc)-(respondent_pos_words/respondent_wc))+I(petitioner_count-respondent_count)+lagged_ideology+conservative_lc+I(lagged_ideology*conservative_lc)+sgpetac+sgrespac+petac+respac+petNumStat+respNumStat+(1|justiceName),data=sc,family=binomial,nAGQ=2)
pred_mod4<-sum(diag(table(ifelse(predict(mod4,type="response")>.50,1,0),sc[names(residuals(mod4)),"petitioner_vote"])))/length(residuals(mod4))
#the model does not converge unless the number of points per axis for evaluating the adaptive Gauss-Hermite approximation to the log-likelihood is increased from 0. Coefficients and prediction rate are essentiall the same regardless of nAGQ used. More specifically, max coefficent change is around 10^-04

stargazer(mod0,mod1,mod2,mod3,mod4,type='html',out='table_1.html',intercept.bottom = FALSE, intercept.top = TRUE, omit.stat = c('bic'), dep.var.labels.include = FALSE, dep.var.caption = "", column.labels = c('intercept only','no controls','dal','harvard','liwc'))
********************************************************************************
********************************************************************************
****
**** File 3 of the replication archive for:
****   Osmundsen, Mathias, David J. Hendry, Lasse Laustsen, Kevin B. Smith, and
****     Michael Bang Petersen. 2020. "The Psychophysiology of Political
****     Ideology: Replications, Reanalysis, and Recommendations." Journal of
****     Politics XX(X): XXX-XXX.
****
**** Note: This file reads in the data an performs recodes for the online
**** picture rating survey.
****
********************************************************************************
********************************************************************************

* uncomment the following line and the closing line to create a log file for the
*   current run
*log using Logs/03_OsmundsenEtAl-PsychophysiologyIdeology-JOP-Log.txt, replace

* change the working directory to the location of this .do file (if necessary)
* cd

* read in raw data file
clear
use Data/OsmundsenEtAl-RatingStudy-JOP-Original.dta, clear

**** recode variables

** Strong emotional reactions - image categories
egen threat_strong_rec = rmean(Q3_1 Q6_1 Q7_1 Q8_1 Q13_1 Q18_1)
alpha Q3_1 Q6_1 Q7_1 Q8_1 Q13_1 Q18_1, asis std
egen disgust_strong_rec = rmean(Q1_1 Q12_1 Q16_1 Q20_1 Q21_1 Q24_1)
alpha Q1_1 Q12_1 Q16_1 Q20_1 Q21_1 Q24_2, asis std
egen positive_strong_rec = rmean(Q4_1 Q5_1 Q10_1 Q17_1 Q22_1 Q23_1)
alpha Q4_1 Q5_1 Q10_1 Q17_1 Q22_1 Q23_1, asis std
egen neutral_strong_rec = rmean(Q2_1 Q9_1 Q11_1 Q14_1 Q19_1)
alpha Q2_1 Q9_1 Q11_1 Q14_1 Q19_1, asis std
pwcorr *strong_rec

** I feel uncomfortable - image categories
egen threat_uncomfortable = rmean(Q3_2 Q6_2 Q7_2 Q8_2 Q13_2 Q18_2)
alpha Q3_2 Q6_2 Q7_2 Q8_2 Q13_2 Q18_2, asis std
egen disgust_uncomfortable = rmean(Q1_2 Q12_2 Q16_2 Q20_2 Q21_2 Q24_2)
alpha Q1_2 Q12_2 Q16_2 Q20_2 Q21_2 Q24_2, asis std
egen positive_uncomfortable = rmean(Q4_2 Q5_2 Q10_2 Q17_2 Q22_2 Q23_2)
alpha Q4_2 Q5_2 Q10_2 Q17_2 Q22_2 Q23_2, asis std
egen neutral_uncomfortable = rmean(Q2_2 Q9_2 Q11_2 Q14_2 Q19_2)
alpha Q2_2 Q9_2 Q11_2 Q14_2 Q19_2, asis std
pwcorr *uncomfortable

** I feel happy
egen threat_happy= rmean(Q3_3 Q6_3 Q7_3 Q8_3 Q13_3 Q18_3)
alpha Q3_3 Q6_3 Q7_3 Q8_3 Q13_3 Q18_3, asis std
egen disgust_happy = rmean(Q1_3 Q12_3 Q16_3 Q20_3 Q21_3 Q24_3)
alpha Q1_3 Q12_3 Q16_3 Q20_3 Q21_3 Q24_3, asis std
egen positive_happy = rmean(Q4_3 Q5_3 Q10_3 Q17_3 Q22_3 Q23_3)
alpha Q4_3 Q5_3 Q10_3 Q17_3 Q22_3 Q23_3, asis std
egen neutral_happy = rmean(Q2_3 Q9_3 Q11_3 Q14_3 Q19_3)
alpha Q2_3 Q9_3 Q11_3 Q14_3 Q19_3, asis std
pwcorr *happy

** I feel threatened
egen threat_threatened = rmean(Q3_4 Q6_4 Q7_4 Q8_4 Q13_4 Q18_4)
alpha Q3_4 Q6_4 Q7_4 Q8_4 Q13_4 Q18_4, asis std
egen disgust_threatened = rmean(Q1_4 Q12_4 Q16_4 Q20_4 Q21_4 Q24_4)
alpha Q1_4 Q12_4 Q16_4 Q20_4 Q21_4 Q24_4, asis std
egen positive_threatened = rmean(Q4_4 Q5_4 Q10_4 Q17_4 Q22_4 Q23_4)
alpha Q4_4 Q5_4 Q10_4 Q17_4 Q22_4 Q23_4, asis std
egen neutral_threatened = rmean(Q2_4 Q9_4 Q11_4 Q14_4 Q19_4)
alpha Q2_4 Q9_4 Q11_4 Q14_4 Q19_4, asis std
pwcorr *threatened

** I feel disgusted
egen threat_disgusted= rmean(Q3_5 Q6_5 Q7_5 Q8_5 Q13_5 Q18_5)
alpha Q3_5 Q6_5 Q7_5 Q8_5 Q13_5 Q18_5, asis std
egen disgust_disgusted = rmean(Q1_5 Q12_5 Q16_5 Q20_5 Q21_5 Q24_5)
alpha Q1_5 Q12_5 Q16_5 Q20_5 Q21_5 Q24_5, asis std
egen positive_disgusted = rmean(Q4_5 Q5_5 Q10_5 Q17_5 Q22_5 Q23_5)
alpha Q4_5 Q5_5 Q10_5 Q17_5 Q22_5 Q23_5, asis std
egen neutral_disgusted = rmean(Q2_5 Q9_5 Q11_5 Q14_5 Q19_5)
alpha Q2_5 Q9_5 Q11_5 Q14_5 Q19_5, asis std
pwcorr *disgusted

** ideology measures
* social conservatism 
alpha Q25_1 Q25_2 Q25_3 Q25_4 Q25_5, gen(social_conservatism)
* economic conservatism
recode Q26_1 (1=5) (2=4) (4=2) (5=1)
recode Q26_2 (1=5) (2=4) (4=2) (5=1)
recode Q26_3 (1=5) (2=4) (4=2) (5=1)
alpha Q26_1 Q26_2 Q26_3 Q26_4 Q26_5, gen(economic_conservatism)

foreach var in threat_strong_rec disgust_strong_rec positive_strong_rec ///
  neutral_strong_rec threat_uncomfortable disgust_uncomfortable ///
  positive_uncomfortable neutral_uncomfortable threat_happy disgust_happy ///
  positive_happy neutral_happy threat_threatened disgust_threatened ///
  positive_threatened neutral_threatened threat_disgusted disgust_disgusted ///
  positive_disgusted neutral_disgusted social_conservatism ///
  economic_conservatism Q27_1 {
  egen z_`var' = std(`var') 
}

label var z_threat_strong_rec "Threatening"
label var z_threat_uncomfortable "Threatening"
label var z_threat_happy "Threatening"
label var z_threat_threatened "Threatening"
label var z_threat_disgusted "Threatening"
label var z_disgust_strong_rec "Disgusting"
label var z_disgust_uncomfortable "Disgusting"
label var z_disgust_happy "Disgusting"
label var z_disgust_threatened "Disgusting"
label var z_disgust_disgusted "Disgusting"
label var z_positive_strong_rec "Positive"
label var z_positive_uncomfortable "Positive"
label var z_positive_happy "Positive"
label var z_positive_threatened "Positive"
label var z_positive_disgusted "Positive"
label var z_neutral_strong_rec "Neutral"
label var z_neutral_uncomfortable "Neutral"
label var z_neutral_happy "Neutral"
label var z_neutral_threatened "Neutral"
label var z_neutral_disgusted "Neutral"
label var z_social_conservatism "Social Conservatism"
label var z_economic_conservatism "Economic Conservatism"
label var profile_education "Education"
label var gender "Female (1 = Yes)"
label var profile_age1 "Age"



* save recodes as new Stata data file
save Data/OsmundsenEtAl-RatingStudy-JOP-Recodes.dta, replace

clear

* close log
*log close

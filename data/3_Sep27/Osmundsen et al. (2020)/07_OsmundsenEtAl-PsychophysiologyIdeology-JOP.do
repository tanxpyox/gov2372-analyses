********************************************************************************
********************************************************************************
****
**** File 7 of the replication archive for:
****   Osmundsen, Mathias, David J. Hendry, Lasse Laustsen, Kevin B. Smith, and
****     Michael Bang Petersen. 2020. "The Psychophysiology of Political
****     Ideology: Replications, Reanalysis, and Recommendations." Journal of
****     Politics XX(X): XXX-XXX.
****
**** Note: This file performs the necessary recodes and analyses for Online
**** Appendix 5.
****
********************************************************************************
********************************************************************************

* uncomment the following line and the closing line to create a log file for the
*   current run
*log using Logs/07_OsmundsenEtAl-PsychophysiologyIdeology-JOP-Log.txt, replace

* change the working directory to the location of this .do file (if necessary)
* cd

** preliminaries
* set plot style for all plots
set scheme s1mono

** merging picture ratings from the ratings survey with lab study data
* reading in ratings data and saving mean values as local variables
clear
use Data/OsmundsenEtAl-RatingStudy-JOP-Recodes.dta, clear

** Due to a coding error by the survey firm that fielded the picture rating
** survey, strength ratings were not recorded for picture 15. When the error was
** discovered, a follow-up study was conducted in which the same respondents
** were asked to provide strength ratings on just picture 15.

forvalues i = 1/24 {
  if (`i' == 15) {
    forvalues j = 2/5 {
	  quietly sum Q`i'_`j'
	  local rating`i'_`j' = `r(mean)'
      display "`rating`i'_`j''"
	}
  }
  else {
    forvalues j = 1/5 {
	  quietly sum Q`i'_`j'
	  local rating`i'_`j' = `r(mean)'
      display "`rating`i'_`j''"
    }
  }
}

clear
use Data/OsmundsenEtAl-RatingStudy-FollowUp-JOP-Original.dta, clear
quietly sum Q15_1
local rating15_1 = `r(mean)'

* read in recoded data file
clear
use Data/OsmundsenEtAl-PsychophysiologyIdeology-JOP-Recodes.dta, clear

* create variable that is the mean rating for each picture
forvalues i = 1/24 {
  gen strong_pic`i' = `rating`i'_1'
}
forvalues i = 1/24 {
  gen discomfort_pic`i' = `rating`i'_2'
}
forvalues i = 1/24 {
  gen happy_pic`i' = `rating`i'_3'
}
forvalues i = 1/24 {
  gen threatr_pic`i' = `rating`i'_4'
}
forvalues i = 1/24 {
  gen disgustr_pic`i' = `rating`i'_5'
}

* self-ratings for each picture
gen selfrateneg_pic3 = arosvy_q25
gen selfratearo_pic3 = arosvy_q27
gen selfrateneg_pic6 = arosvy_q37
gen selfratearo_pic6 = arosvy_q391
gen selfrateneg_pic20 = arosvy_q17
gen selfratearo_pic20 = arosvy_q19
gen selfrateneg_pic21 = arosvy_q21
gen selfratearo_pic21 = arosvy_q23
gen selfrateneg_pic14 = arosvy_q13
gen selfratearo_pic14 = arosvy_q15
gen selfrateneg_pic11 = arosvy_q8
gen selfratearo_pic11 = arosvy_q11
gen selfrateneg_pic4 = arosvy_q29
gen selfratearo_pic4 = arosvy_q31
gen selfrateneg_pic5 = arosvy_q33
gen selfratearo_pic5 = arosvy_q35

* consider complete cases to be those for whom we have complete eda measures and
*   complete survey-based ideology measures
quietly reg z_threat_change_total z_disgust_change_total ///
  z_neutral_change_total z_pos_change_total z_wp z_soccon z_ecocon ///
  z_leftright_alt
gen complete = 1 if e(sample)

* outlier indicators
egen max_z_threat_change_total_us = max(z_threat_change_total) if ///
  complete == 1 & country == 0
egen max_z_disgust_change_total_dk = max(z_disgust_change_total) if ///
  complete == 1 & country == 1
gen outlier_us = 1 if z_threat_change_total == ///
  max_z_threat_change_total_us & max_z_threat_change_total_us != .
gen outlier_dk = 1 if z_disgust_change_total == ///
  max_z_disgust_change_total_dk & max_z_disgust_change_total_dk != .
drop max_z_threat_change_total_us max_z_disgust_change_total_dk

********************************************************************************
** recode EDA measures using area-bounded-by-the-curve approach
********************************************************************************
* recoding stimulus intervals
foreach i of numlist 1/24 {
  replace ashtray_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image01Ashtray2"
}
foreach i of numlist 1/24 {
  replace basket_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image02Basket"
}
foreach i of numlist 1/24 {
  replace knife_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image03KnifeHeldByMan"
}
foreach i of numlist 1/24 {
  replace skysurfer_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image04Skysurfer"
}
foreach i of numlist 1/24 {
  replace couple_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image05Couple3"
}
foreach i of numlist 1/24 {
  replace snake_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image06Snake2"
}
foreach i of numlist 1/24 {
  replace spider_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image07Spider"
}
foreach i of numlist 1/24 {
  replace gunattack_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image08GunAttack1"
}
foreach i of numlist 1/24 {
  replace umbrella_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image09Umbrella"
}
foreach i of numlist 1/24 {
  replace snowskier_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image10SnowSkier"
}
foreach i of numlist 1/24 {
  replace bowl_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image11Bowl"
}
foreach i of numlist 1/24 {
  replace nastytoilet_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image12NastyToilet"
}
foreach i of numlist 1/24 {
  replace caraccident_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image13CarAccident1"
}
foreach i of numlist 1/24 {
  replace mug_gsravg = phys_pic`i'phasicarea if picep_imgfile`i' == "Image14Mug"
}
foreach i of numlist 1/24 {
  replace rollingpin_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image15RollingPin"
}
foreach i of numlist 1/24 {
  replace maggots_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image16Maggots"
}
foreach i of numlist 1/24 {
  replace waterfall_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "image17Waterfall"
}
foreach i of numlist 1/24 {
  replace shipsinking_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "image18ShipSinking1"
}
foreach i of numlist 1/24 {
  replace dustpan_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image19Dustpan"
}
foreach i of numlist 1/24 {
  replace worms_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image20WormsInMouth"
}
foreach i of numlist 1/24 {
  replace babytumor_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image21BabyWithTumor"
}
foreach i of numlist 1/24 {
  replace couple2_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image22Couple2"
}
foreach i of numlist 1/24 {
  replace sailboat_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image23YellowSailboat"
}
foreach i of numlist 1/24 {
  replace vomit_gsravg = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image24Vomit"
}

* recoding inter-stimulus intervals
foreach i of numlist 1/24 {
  replace ashtray_gsravgis = phys_isi`i'phasicarea if picep_imgid`i' == "img01"
}
foreach i of numlist 1/24 {
  replace basket_gsravgis = phys_isi`i'phasicarea if picep_imgid`i' == "img02"
}
foreach i of numlist 1/24 {
  replace knife_gsravgis = phys_isi`i'phasicarea if picep_imgid`i' == "img03"
}
foreach i of numlist 1/24 {
  replace skysurfer_gsravgis = phys_isi`i'phasicarea if ///
    picep_imgid`i' == "img04"
}
foreach i of numlist 1/24 {
  replace couple3_gsravgis = phys_isi`i'phasicarea if picep_imgid`i' == "img05"
}
foreach i of numlist 1/24 {
  replace snake_gsravgis = phys_isi`i'phasicarea if picep_imgid`i' == "img06"
}
foreach i of numlist 1/24 {
  replace spider_gsravgis = phys_isi`i'phasicarea if picep_imgid`i' == "img07"
}
foreach i of numlist 1/24 {
  replace gunattack_gsravgis = phys_isi`i'phasicarea if ///
    picep_imgid`i' == "img08"
}
foreach i of numlist 1/24 {
  replace umbrella_gsravgis = phys_isi`i'phasicarea if picep_imgid`i' == "img09"
}
foreach i of numlist 1/24 {
  replace snowskier_gsravgis = phys_isi`i'phasicarea if ///
    picep_imgid`i' == "img10"
}
foreach i of numlist 1/24 {
  replace bowl_gsravgis = phys_isi`i'phasicarea if picep_imgid`i' == "img11"
}
foreach i of numlist 1/24 {
  replace nastytoilet_gsravgis = phys_isi`i'phasicarea if ///
    picep_imgid`i' == "img12"
}
foreach i of numlist 1/24 {
  replace caraccident_gsravgis = phys_isi`i'phasicarea if ///
    picep_imgid`i' == "img13"
}
foreach i of numlist 1/24 {
  replace mug_gsravgis = phys_isi`i'phasicarea if picep_imgid`i' == "img14"
}
foreach i of numlist 1/24 {
  replace rollingpin_gsravgis = phys_isi`i'phasicarea if ///
    picep_imgid`i' == "img15"
}
foreach i of numlist 1/24 {
  replace maggots_gsravgis = phys_isi`i'phasicarea if picep_imgid`i' == "img16"
}
foreach i of numlist 1/24 {
  replace waterfall_gsravgis = phys_isi`i'phasicarea if ///
    picep_imgid`i' == "img17"
}
foreach i of numlist 1/24 {
  replace shipsinking_gsravgis = phys_isi`i'phasicarea if ///
    picep_imgid`i' == "img18"
}
foreach i of numlist 1/24 {
  replace dustpan_gsravgis = phys_isi`i'phasicarea if picep_imgid`i' == "img19"
}
foreach i of numlist 1/24 {
  replace worms_gsravgis = phys_isi`i'phasicarea if picep_imgid`i' == "img20"
}
foreach i of numlist 1/24 {
  replace babytumor_gsravgis = phys_isi`i'phasicarea if ///
    picep_imgid`i' == "img21"
}
foreach i of numlist 1/24 {
  replace couple2_gsravgis = phys_isi`i'phasicarea if picep_imgid`i' == "img22"
}
foreach i of numlist 1/24 {
  replace yellowsailboat_gsravgis = phys_isi`i'phasicarea if ///
    picep_imgid`i' == "img23"
}
foreach i of numlist 1/24 {
  replace vomit_gsravgis = phys_isi`i'phasicarea if picep_imgid`i' == "img24"
}

* create logged version of EDA response to threatening images
replace threat_pic3 = log(knife_gsravg)
replace threat_pic6 = log(snake_gsravg)
replace threat_pic7 = log(spider_gsravg)
replace threat_pic8 = log(gunattack_gsravg)
replace threat_pic13 = log(caraccident_gsravg)
replace threat_pic18 = log(shipsinking_gsravg)
* create logged version of EDA response to disgusting images
replace disgust_pic1 = log(ashtray_gsravg)
replace disgust_pic12 = log(nastytoilet_gsravg)
replace disgust_pic16 = log(maggots_gsravg)
replace disgust_pic20 = log(worms_gsravg)
replace disgust_pic21 = log(babytumor_gsravg)
replace disgust_pic24 = log(vomit_gsravg)
* create logged version of EDA response to neutral images
replace neutral_pic2 = log(basket_gsravg)
replace neutral_pic9 = log(umbrella_gsravg)
replace neutral_pic11 = log(bowl_gsravg)
replace neutral_pic14 = log(mug_gsravg)
replace neutral_pic15 = log(rollingpin_gsravg)
replace neutral_pic19 = log(dustpan_gsravg)
* create logged version of EDA response to positive images
replace pos_pic4 = log(skysurfer_gsravg)
replace pos_pic5 = log(couple_gsravg)
replace pos_pic10 = log(snowskier_gsravg)
replace pos_pic17 = log(waterfall_gsravg)
replace pos_pic22 = log(couple2_gsravg)
replace pos_pic23 = log(sailboat_gsravg)
* create logged version of EDA response to inter-stimulus interval for
* threatening images
replace threat_pic3_is = log(knife_gsravgis)
replace threat_pic6_is = log(snake_gsravgis)
replace threat_pic7_is = log(spider_gsravgis)
replace threat_pic8_is = log(gunattack_gsravgis)
replace threat_pic13_is = log(caraccident_gsravgis)
replace threat_pic18_is = log(shipsinking_gsravgis)
* create logged version of EDA response to inter-stimulus interval for
* disgusting images
replace disgust_pic1_is = log(ashtray_gsravgis)
replace disgust_pic12_is = log(nastytoilet_gsravgis)
replace disgust_pic16_is = log(maggots_gsravgis)
replace disgust_pic20_is = log(worms_gsravgis)
replace disgust_pic21_is = log(babytumor_gsravgis)
replace disgust_pic24_is = log(vomit_gsravgis)
* create logged version of EDA response to inter-stimulus interval for neutral
* images
replace neutral_pic2_is = log(basket_gsravgis) 
replace neutral_pic9_is = log(umbrella_gsravgis) 
replace neutral_pic11_is = log(bowl_gsravgis)
replace neutral_pic14_is = log(mug_gsravgis)
replace neutral_pic15_is = log(rollingpin_gsravgis)
replace neutral_pic19_is = log(dustpan_gsravgis)    
* create logged version of EDA response to inter-stimulus interval for positive
* images
replace pos_pic4_is = log(skysurfer_gsravgis) 
replace pos_pic5_is = log(couple3_gsravgis)
replace pos_pic10_is = log(snowskier_gsravgis)
replace pos_pic17_is = log(waterfall_gsravgis)
replace pos_pic22_is = log(couple2_gsravgis)
replace pos_pic23_is = log(yellowsailboat_gsravgis)

replace eda_pic3 = (threat_pic3)
replace eda_pic6 = (threat_pic6)
replace eda_pic7 = (threat_pic7)
replace eda_pic8 = (threat_pic8)
replace eda_pic13 = (threat_pic13)
replace eda_pic18 = (threat_pic18)
replace eda_pic1 = (disgust_pic1)
replace eda_pic12 = (disgust_pic12)
replace eda_pic16 = (disgust_pic16)
replace eda_pic20 = (disgust_pic20)
replace eda_pic21 = (disgust_pic21)
replace eda_pic24 = (disgust_pic24)
replace eda_pic2 = (neutral_pic2)
replace eda_pic9 = (neutral_pic9)
replace eda_pic11 = (neutral_pic11)
replace eda_pic14 = (neutral_pic14)
replace eda_pic15 = (neutral_pic15)
replace eda_pic19 = (neutral_pic19)
replace eda_pic4 = (pos_pic4)
replace eda_pic5 = (pos_pic5)
replace eda_pic10 = (pos_pic10)
replace eda_pic17 = (pos_pic17)
replace eda_pic22 = (pos_pic22)
replace eda_pic23 = (pos_pic23)

* dropping and recreating indices for average change between ISI and stimulus
drop threat_change_total disgust_change_total neutral_change_total ///
  pos_change_total z_threat_change_total z_disgust_change_total ///
  z_neutral_change_total z_pos_change_total
* Create index for change in EDA response to all threatening images
egen threat_change_total = rmean(threat_pic3 threat_pic6 threat_pic7 ///
  threat_pic8 threat_pic13 threat_pic18)
* Create index for change in EDA response to all disgusting images
egen disgust_change_total = rmean(disgust_pic1 disgust_pic12 disgust_pic16 ///
  disgust_pic20 disgust_pic21 disgust_pic24 )
* Create index for change in EDA response to all neutral images
egen neutral_change_total = rmean(neutral_pic2 neutral_pic9 neutral_pic11 ///
  neutral_pic14 neutral_pic15 neutral_pic19 )
* Create index for change in EDA response to all positive images
egen pos_change_total = rmean(pos_pic4 pos_pic5 pos_pic10 pos_pic17 ///
  pos_pic22 pos_pic23)

** Standardization of change indices
foreach var in threat_change_total disgust_change_total neutral_change_total ///
  pos_change_total {
  egen z_`var' = std(`var')
}
label var z_threat_change_total "Threatening: Standardized Area-Under-the-Curve"
label var z_disgust_change_total "Disgusting: Standardized Area-Under-the-Curve"
label var z_neutral_change_total "Neutral: Standardized Area-Under-the-Curve"
label var z_pos_change_total "Positive: Standardized Area-Under-the-Curve"

* self-reported valence 
rename arosvy_q8 plate_neutral_valence
rename arosvy_q13 mug_neutral_valence
rename arosvy_q17 worms_disgust_valence
rename arosvy_q21 baby_tumor_disgust_valence
rename arosvy_q25 knife_threat_valence
rename arosvy_q29 skydiving_pos_valence
rename arosvy_q33 sex_pos_valence
rename arosvy_q37 snake_threat_valence

gen SR_threat_valence = (knife_threat_valence + snake_threat_valence - 2) / 16
gen SR_disgust_valence = (worms_disgust_valence + ///
  baby_tumor_disgust_valence - 2) / 16
gen SR_positive_valence = (skydiving_pos_valence + sex_pos_valence - 2) / 16
gen SR_neutral_valence = (plate_neutral_valence + mug_neutral_valence - 2) / 16

* selfreported arousal
rename arosvy_q11 plate_neutral
rename arosvy_q15 mug_neutral
rename arosvy_q19 worms_disgust
rename arosvy_q23 baby_tumor_disgust
rename arosvy_q27 knife_threat
rename arosvy_q31 skydiving_pos
rename arosvy_q35 sex_pos
rename arosvy_q391 snake_threat

gen SR_threat = (knife_threat + snake_threat - 2) / 16
gen SR_disgust = (worms_disgust + baby_tumor_disgust - 2) / 16
gen SR_positive = (skydiving_pos + sex_pos - 2) / 16
gen SR_neutral = (plate_neutral + mug_neutral - 2) / 16

* gen EDA responses to subset of images we have self-reported valence and
* arousal ratings for
egen auc_threat = rmean(threat_pic3 threat_pic6)
egen auc_disgust = rmean(disgust_pic20 disgust_pic21)
egen auc_positive = rmean(pos_pic4 pos_pic5)
egen auc_neutral = rmean(neutral_pic11 neutral_pic14)
********************************************************************************

********************************************************************************
** alpha values reported in the main text
** Section: "A Meta-Analysis of All Published Studies
**   Subection: "Results"
********************************************************************************
bysort country: alpha threat_pic3 threat_pic6 threat_pic7 threat_pic8 ///
  threat_pic13 threat_pic18, std
********************************************************************************

********************************************************************************
********************************************************************************
** Online Appendix 5A
********************************************************************************
********************************************************************************

********************************************************************************
** Statistics reported in Online Appendix 5A
********************************************************************************
** Denmark: Correlations between stimulus and inter-stimulus interval
* Threatening
corr threat_pic3 threat_pic3_is if country == 1
local corr1 = `r(rho)'
corr threat_pic6 threat_pic6_is if country == 1
local corr2 = `r(rho)'
corr threat_pic7 threat_pic7_is if country == 1
local corr3 = `r(rho)'
corr threat_pic8 threat_pic8_is if country == 1
local corr4 = `r(rho)'
corr threat_pic13 threat_pic13_is if country == 1
local corr5 = `r(rho)'
corr threat_pic18 threat_pic18_is if country == 1
local corr6 = `r(rho)'
local avg_corr = (`corr1' + `corr2' + `corr3' + `corr4' + `corr5' + `corr6') / 6
display "`avg_corr'"
* Disgusting
corr disgust_pic1 disgust_pic1_is if country == 1
local corr1 = `r(rho)'
corr disgust_pic12 disgust_pic12_is if country == 1
local corr2 = `r(rho)'
corr disgust_pic16 disgust_pic16_is if country == 1
local corr3 = `r(rho)'
corr disgust_pic20 disgust_pic20_is if country == 1
local corr4 = `r(rho)'
corr disgust_pic21 disgust_pic21_is if country == 1
local corr5 = `r(rho)'
corr disgust_pic24 disgust_pic24_is if country == 1
local corr6 = `r(rho)'
local avg_corr = (`corr1' + `corr2' + `corr3' + `corr4' + `corr5' + `corr6') / 6
display "`avg_corr'"

** Denmark: Convergent validity and reliability of ABC reactions to the images
alpha threat_pic3 threat_pic6 threat_pic7 threat_pic8 threat_pic13 ///
  threat_pic18 if country == 1, std
alpha disgust_pic1 disgust_pic12 disgust_pic16 disgust_pic20 disgust_pic21 ///
  disgust_pic24 if country == 1, std
alpha pos_pic4 pos_pic5 pos_pic10 pos_pic17 pos_pic22 pos_pic23 ///
  if country == 1, std
alpha neutral_pic2 neutral_pic9 neutral_pic11 neutral_pic14 neutral_pic15 ///
  neutral_pic19 if country == 1, std

** Denmark: Discriminant validity AUC reactions to images
pwcorr threat_change_total disgust_change_total pos_change_total ///
  neutral_change_total if country == 1

** U.S.: Correlations between stimulus and inter-stimulus interval
* Threatening
corr threat_pic3 threat_pic3_is if country == 0
local corr1 = `r(rho)'
corr threat_pic6 threat_pic6_is if country == 0
local corr2 = `r(rho)'
corr threat_pic7 threat_pic7_is if country == 0
local corr3 = `r(rho)'
corr threat_pic8 threat_pic8_is if country == 0
local corr4 = `r(rho)'
corr threat_pic13 threat_pic13_is if country == 0
local corr5 = `r(rho)'
corr threat_pic18 threat_pic18_is if country == 0
local corr6 = `r(rho)'
local avg_corr = (`corr1' + `corr2' + `corr3' + `corr4' + `corr5' + `corr6') / 6
display "`avg_corr'"
* Disgusting
corr disgust_pic1 disgust_pic1_is if country == 0
local corr1 = `r(rho)'
corr disgust_pic12 disgust_pic12_is if country == 0
local corr2 = `r(rho)'
corr disgust_pic16 disgust_pic16_is if country == 0
local corr3 = `r(rho)'
corr disgust_pic20 disgust_pic20_is if country == 0
local corr4 = `r(rho)'
corr disgust_pic21 disgust_pic21_is if country == 0
local corr5 = `r(rho)'
corr disgust_pic24 disgust_pic24_is if country == 0
local corr6 = `r(rho)'
local avg_corr = (`corr1' + `corr2' + `corr3' + `corr4' + `corr5' + `corr6') / 6
display "`avg_corr'"

** U.S: Convergent validity and reliability of ABC reactions to the images
alpha threat_pic3 threat_pic6 threat_pic7 threat_pic8 threat_pic13 ///
  threat_pic18 if country == 0, std
alpha disgust_pic1 disgust_pic12 disgust_pic16 disgust_pic20 disgust_pic21 ///
  disgust_pic24 if country == 0, std
alpha pos_pic4 pos_pic5 pos_pic10 pos_pic17 pos_pic22 pos_pic23 ///
  if country == 1, std
alpha neutral_pic2 neutral_pic9 neutral_pic11 neutral_pic14 neutral_pic15 ///
  neutral_pic19 if country == 0, std

** U.S.: Discriminant validity AUC reactions to images
pwcorr threat_change_total disgust_change_total pos_change_total ///
  neutral_change_total if country == 0

** Standardized Histogram
hist z_threat_change_total if country == 0, title("United States") name(histUS)
hist z_threat_change_total if country == 1, title("Denmark") name(histDK)
graph combine histUS histDK
graph export Figures/OnlineAppendix-Appendix05-StandardizedAUC.pdf, replace
graph drop _all
graph close

** reshaping data to long format for next set of analyses
preserve

reshape long area_pic eda_pic strong_pic discomfort_pic happy_pic ///
  threatr_pic disgustr_pic selfrateneg_pic selfratearo_pic order_pic, ///
  i(id) j(picture)
foreach var in area_pic strong_pic discomfort_pic happy_pic threatr_pic ///
  disgustr_pic eda_pic selfrateneg_pic selfratearo_pic wp leftright_alt ///
  soccon ecocon {
  egen `var'01 = std(`var')
}

*************
** Table 5A.a
*************
pwcorr eda_pic01 threatr_pic01 disgustr_pic01 discomfort_pic01 strong_pic01 ///
 happy_pic01 if country == 1 
*************

*************
** Table 5A.b
*************
pwcorr eda_pic01 threatr_pic01 disgustr_pic01 discomfort_pic01 strong_pic01 ///
 happy_pic01 if country == 0
*************

**************
** Figure 5A.b
**************
gen IAthreat = eda_pic01 * threatr_pic01
gen IAdisgust = eda_pic01 * disgustr_pic01
gen IAdiscomfort = eda_pic01 * discomfort_pic01
gen IAstrong = eda_pic01 * strong_pic01
gen IAhappy = eda_pic01 * happy_pic01
gen IAownneg = eda_pic01 * selfrateneg_pic01
gen IAownarous = eda_pic01 * selfratearo_pic01

* negative reaction
gen IAownneg_dk = IAownneg if country == 1
label var IAownneg_dk "Danes"
gen IAownneg_us = IAownneg if country == 0
label var IAownneg_us "Americans"
gen IAownneg_total = IAownneg
label var IAownneg_total "Combined"
* strength of arousal
gen IAownarous_dk = IAownarous if country == 1
label var IAownarous_dk "Danes"
gen IAownarous_us = IAownarous if country == 0
label var IAownarous_us "Americans"
gen IAownarous_total = IAownarous
label var IAownarous_total "Combined"

foreach var of varlist  IAownneg_dk IAownneg_us IAownneg_total IAownarous_dk ///
  IAownarous_us IAownarous_total {
  quietly reg wp01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp_`var'
}
foreach var of varlist IAownneg_dk IAownneg_us IAownneg_total IAownarous_dk ///
  IAownarous_us IAownarous_total {
  quietly reg soccon01 `var' eda_pic01 selfrateneg_pic female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon_`var'
}
foreach var of varlist IAownneg_dk IAownneg_us IAownneg_total IAownarous_dk ///
  IAownarous_us IAownarous_total {
  quietly reg ecocon01 `var' eda_pic01 selfrateneg_pic female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store leftright_`var'
}
foreach var of varlist IAownneg_dk IAownneg_us IAownneg_total IAownarous_dk ///
  IAownarous_us IAownarous_total {
  quietly reg leftright_alt01 `var' eda_pic01 selfrateneg_pic female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store ecocon_`var'
}

coefplot wp_*, bylabel(Wilson-Patterson) ///
  || soccon_*, bylabel(Social Conservatism) ///
  || leftright_*, bylabel(Left-Right self-placement) ///
  || ecocon*, bylabel(Economic Conservatism) ///
  ||, drop(_cons female age education income_hh eda_pic01 selfrateneg_pic ///
  country) xline(0, lcolor(gray)) msymbol(square) color(black) ///
  ciopts(lcolor(black)) byopts(legend(off)) ///
  headings(IAownneg_dk = "{bf:IAownneg}" IAownarous_dk = "{bf:IAownarous}",) ///
  format(%9.1g) mlabel mlabposition(10) mlabsize(1.5) grid(none)
graph export Figures/OnlineAppendix-Figure05Ab.pdf, replace
graph drop _all
graph close

estimates drop wp_* soccon_* leftright_* ecocon_*
**************


**************
** Figure 5A.c
**************
gen IAthreat_dk = IAthreat if country == 1
label var IAthreat_dk "Danes"
gen IAthreat_us = IAthreat if country == 0
label var IAthreat_us "Americans"
gen IAthreat_total = IAthreat
label var IAthreat_total "Combined"

gen IAdisgust_dk = IAdisgust if country == 1
label var IAdisgust_dk "Danes"
gen IAdisgust_us = IAdisgust if country == 0
label var IAdisgust_us "Americans"
gen IAdisgust_total = IAdisgust
label var IAdisgust_total "Combined"

gen IAdiscomfort_dk = IAdiscomfort if country == 1
label var IAdiscomfort_dk "Danes"
gen IAdiscomfort_us = IAdiscomfort if country == 0
label var IAdiscomfort_us "Americans"
gen IAdiscomfort_total = IAdiscomfort
label var IAdiscomfort_total "Combined"

gen IAstrong_dk = IAstrong if country == 1
label var IAstrong_dk "Danes"
gen IAstrong_us = IAstrong if country == 0
label var IAstrong_us "Americans"
gen IAstrong_total = IAstrong
label var IAstrong_total "Combined"

gen IAhappy_dk = IAhappy if country == 1
label var IAhappy_dk "Danes"
gen IAhappy_us = IAhappy if country == 0
label var IAhappy_us "Americans"
gen IAhappy_total = IAhappy
label var IAhappy_total "Combined"

foreach var of varlist IAthreat_dk IAthreat_us IAthreat_total {
  quietly reg wp01 `var' eda_pic01 threatr_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly reg wp01 `var' eda_pic01 disgustr_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAdiscomfort_dk IAdiscomfort_us IAdiscomfort_total {
  quietly reg wp01 `var' eda_pic01 discomfort_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly reg wp01 `var' eda_pic01 strong_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAhappy_dk IAhappy_us IAhappy_total {
  quietly reg wp01 `var' eda_pic01 happy_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAthreat_dk IAthreat_us IAthreat_total {
  quietly reg soccon01 `var' eda_pic01 threatr_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon01_`var'
}

foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly reg soccon01 `var' eda_pic01 disgustr_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAdiscomfort_dk  IAdiscomfort_us IAdiscomfort_total {
  quietly reg soccon01 `var' eda_pic01 discomfort_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly reg soccon01 `var' eda_pic01 strong_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAhappy_dk IAhappy_us IAhappy_total {
  quietly reg soccon01 `var' eda_pic01 happy_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAthreat_dk IAthreat_us IAthreat_total {
  quietly reg leftright_alt01 `var' eda_pic01 threatr_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly reg leftright_alt01 `var' eda_pic01 disgustr_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAdiscomfort_dk IAdiscomfort_us IAdiscomfort_total {
  quietly reg leftright_alt01 `var' eda_pic01 discomfort_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly reg leftright_alt01 `var' eda_pic01 strong_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAhappy_dk IAhappy_us IAhappy_total {
  quietly reg leftright_alt01 `var' eda_pic01 happy_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist  IAthreat_dk IAthreat_us IAthreat_total {
  quietly reg ecocon01 `var' eda_pic01 threatr_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store ecocon01_`var'
}
foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly reg ecocon01 `var' eda_pic01 disgustr_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store ecocon01_`var'
}
foreach var of varlist IAdiscomfort_dk IAdiscomfort_us IAdiscomfort_total {
  quietly reg ecocon01 `var' eda_pic01 discomfort_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store ecocon01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly reg ecocon01 `var' eda_pic01 strong_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store ecocon01_`var'
}
foreach var of varlist IAhappy_dk IAhappy_us IAhappy_total {
  quietly regress ecocon01 `var' eda_pic01 happy_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store ecocon01_`var'
}

coefplot wp01_*, bylabel(Wilson-Patterson) ///
  || soccon01_*, bylabel(Social Conservatism) ///
  || ecocon01_*, bylabel(Economic Conservatism) ///
  || lr01_*, bylabel(Left-Right self-placement) ///
  ||, drop(_cons eda_pic01 threatr_pic01 disgustr_pic01 discomfort_pic01 ///
  happy_pic01 strong_pic01 female age education income_hh country) ///
  xline(0, lcolor(gray)) msymbol(square) color(black) ciopts(lcolor(black)) ///
  byopts(legend(off)) headings(IAthreat_dk = "{bf:Threatened}" ///
  IAdisgust_dk = "{bf:Disgusted}" IAdiscomfort_dk = "{bf:Uncomfortable}" ///
  IAstrong_dk = "{bf:Emotion Strength}" IAhappy_dk="{bf:Happy}",) ///
  format(%9.1g) mlabel mlabposition(10) mlabsize(1.5) grid(none) ///
  subtitle(, size(small)) xsize(7) ysize(10)
graph export Figures/OnlineAppendix-Figure05Ac.pdf, replace
graph drop _all
graph close

estimates drop wp01_* soccon01_* ecocon01_* lr01_*
**************

* restoring back to wide format
restore

*************
** Table 5A.c
*************
pwcorr auc_threat auc_disgust SR_threat_valence SR_disgust_valence SR_threat ///
  SR_disgust if country == 1
*************

*************
** Table 5A.d
*************
pwcorr auc_threat auc_disgust SR_threat_valence SR_disgust_valence SR_threat ///
  SR_disgust if outlier_us != 1 & country == 0
*************

**************
** Figure 5A.a
**************
gen threat_dk = z_threat_change_total if country == 1
label var threat_dk "Danes"
gen threat_us = z_threat_change_total if country == 0
label var threat_us "Americans"
gen threat_total = z_threat_change_total
label var threat_total "Combined"
gen disgust_dk = z_disgust_change_total if country == 1
label var disgust_dk "Danes"
gen disgust_us = z_disgust_change_total if country == 0
label var disgust_us "Americans"
gen disgust_total = z_disgust_change_total
label var disgust_total "Combined"
gen positive_dk = z_pos_change_total if country == 1
label var positive_dk "Danes"
gen positive_us = z_pos_change_total if country == 0
label var positive_us "Americans"
gen positive_total = z_pos_change_total
label var positive_total "Combined"
gen neutral_dk = z_neutral_change_total if country == 1
label var neutral_dk "Danes"
gen neutral_us = z_neutral_change_total if country == 0
label var neutral_us "Americans"
gen neutral_total = z_neutral_change_total
label var neutral_total "Combined"

foreach var of varlist threat_dk threat_us threat_total {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust 
  estimates store a1_`var'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store b1_`var'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store c1_`var'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store d1_`var'
}
foreach var of varlist threat_dk threat_us threat_total {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store a2_`var'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store b2_`var'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store c2_`var'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store d2_`var'
}
foreach var of varlist threat_dk threat_us threat_total {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store a3_`var'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store b3_`var'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store c3_`var'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store d3_`var'
}
foreach var of varlist threat_dk threat_us threat_total {
  quietly reg z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store a4_`var'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly reg z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store b4_`var'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly reg z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store c4_`var'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly reg z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store d4_`var'
}

coefplot a1* b1* c1* d1*, bylabel(Wilson-Patterson) ///
  || a2* b2* c2* d2*, bylabel(Social Conservatism) ///
  || a3* b3* c3* d3*, bylabel(Economic Conservatism) ///
  || a4* b4* c4* d4*, bylabel(Left-Right self-placement) ///
  ||, drop(_cons female age education income_hh country) ///
  xline(0, lcolor(gray)) msymbol(square) color(black) ciopts(lcolor(black)) ///
  byopts(legend(off)) headings(threat_dk = "{bf:EDA Threat}" ///
    disgust_dk = "{bf:EDA Disgust}" positive_dk = "{bf:EDA Positive}" ///
    neutral_dk = "{bf:EDA Neutral}",) format(%9.1g) mlabel mlabposition(10) ///
    mlabsize(1.5) grid(none) subtitle(, size(small)) xsize(7) ysize(10)
graph export Figures/OnlineAppendix-Figure05Aa.pdf, replace
graph drop _all
graph close

estimates drop a* b* c* d*
***********


********************************************************************************
********************************************************************************


** merging picture ratings from the ratings survey with lab study data
* reading in ratings data and saving mean values as local variables
clear
use Data/OsmundsenEtAl-RatingStudy-JOP-Recodes.dta, clear

** Due to a coding error by the survey firm that fielded the picture rating
** survey, strength ratings were not recorded for picture 15. When the error was
** discovered, a follow-up study was conducted in which the same respondents
** were asked to provide strength ratings on just picture 15.

forvalues i = 1/24 {
  if (`i' == 15) {
    forvalues j = 2/5 {
	  quietly sum Q`i'_`j'
	  local rating`i'_`j' = `r(mean)'
      display "`rating`i'_`j''"
	}
  }
  else {
    forvalues j = 1/5 {
	  quietly sum Q`i'_`j'
	  local rating`i'_`j' = `r(mean)'
      display "`rating`i'_`j''"
    }
  }
}

clear
use Data/OsmundsenEtAl-RatingStudy-FollowUp-JOP-Original.dta, clear
quietly sum Q15_1
local rating15_1 = `r(mean)'

* read in recoded data file
clear
use Data/OsmundsenEtAl-PsychophysiologyIdeology-JOP-Recodes.dta, clear

* create variable that is the mean rating for each picture
forvalues i = 1/24 {
  gen strong_pic`i' = `rating`i'_1'
}
forvalues i = 1/24 {
  gen discomfort_pic`i' = `rating`i'_2'
}
forvalues i = 1/24 {
  gen happy_pic`i' = `rating`i'_3'
}
forvalues i = 1/24 {
  gen threatr_pic`i' = `rating`i'_4'
}
forvalues i = 1/24 {
  gen disgustr_pic`i' = `rating`i'_5'
}

* self-ratings for each picture
gen selfrateneg_pic3 = arosvy_q25
gen selfratearo_pic3 = arosvy_q27
gen selfrateneg_pic6 = arosvy_q37
gen selfratearo_pic6 = arosvy_q391
gen selfrateneg_pic20 = arosvy_q17
gen selfratearo_pic20 = arosvy_q19
gen selfrateneg_pic21 = arosvy_q21
gen selfratearo_pic21 = arosvy_q23
gen selfrateneg_pic14 = arosvy_q13
gen selfratearo_pic14 = arosvy_q15
gen selfrateneg_pic11 = arosvy_q8
gen selfratearo_pic11 = arosvy_q11
gen selfrateneg_pic4 = arosvy_q29
gen selfratearo_pic4 = arosvy_q31
gen selfrateneg_pic5 = arosvy_q33
gen selfratearo_pic5 = arosvy_q35

* consider complete cases to be those for whom we have complete eda measures and
*   complete survey-based ideology measures
quietly reg z_threat_change_total z_disgust_change_total ///
  z_neutral_change_total z_pos_change_total z_wp z_soccon z_ecocon ///
  z_leftright_alt
gen complete = 1 if e(sample)

* outlier indicators
egen max_z_threat_change_total_us = max(z_threat_change_total) if ///
  complete == 1 & country == 0
egen max_z_disgust_change_total_dk = max(z_disgust_change_total) if ///
  complete == 1 & country == 1
gen outlier_us = 1 if z_threat_change_total == ///
  max_z_threat_change_total_us & max_z_threat_change_total_us != .
gen outlier_dk = 1 if z_disgust_change_total == ///
  max_z_disgust_change_total_dk & max_z_disgust_change_total_dk != .
drop max_z_threat_change_total_us max_z_disgust_change_total_dk

** recoding area-under-the-curve
* recoding stimulus intervals
gen ashtray_auc = .
foreach i of numlist 1/24 {
  replace ashtray_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image01Ashtray2"
}
gen basket_auc = .
foreach i of numlist 1/24 {
  replace basket_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image02Basket"
}
gen knife_auc = .
foreach i of numlist 1/24 {
  replace knife_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image03KnifeHeldByMan"
}
gen skysurfer_auc = .
foreach i of numlist 1/24 {
  replace skysurfer_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image04Skysurfer"
}
gen couple_auc = .
foreach i of numlist 1/24 {  
  replace couple_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image05Couple3"
}
gen snake_auc = .
foreach i of numlist 1/24 {
  replace snake_auc= phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image06Snake2"
}
gen spider_auc = .
foreach i of numlist 1/24 {
  replace spider_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image07Spider"
}
gen gunattack_auc = .
foreach i of numlist 1/24 {
  replace gunattack_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image08GunAttack1"
}
gen umbrella_auc = .
foreach i of numlist 1/24 {
  replace umbrella_auc= phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image09Umbrella"
}
gen snowskier_auc = .
foreach i of numlist 1/24 {
  replace snowskier_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image10SnowSkier"
}
gen bowl_auc = .
foreach i of numlist 1/24 {
  replace bowl_auc = phys_pic`i'phasicarea if picep_imgfile`i' == "Image11Bowl"
}
gen nastytoilet_auc = .
foreach i of numlist 1/24 {
  replace nastytoilet_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image12NastyToilet"
}
gen caraccident_auc = .
foreach i of numlist 1/24 {
  replace caraccident_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image13CarAccident1"
}
gen mug_auc = .
foreach i of numlist 1/24 {
  replace mug_auc = phys_pic`i'phasicarea if picep_imgfile`i' == "Image14Mug"
}
gen rollingpin_auc = .
foreach i of numlist 1/24 {
  replace rollingpin_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image15RollingPin"
}
gen maggots_auc = .
foreach i of numlist 1/24 {
  replace maggots_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image16Maggots"
}
gen waterfall_auc = .
foreach i of numlist 1/24 {
  replace waterfall_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "image17Waterfall"
}
gen shipsinking_auc = .
foreach i of numlist 1/24 {
  replace shipsinking_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "image18ShipSinking1"
}
gen dustpan_auc = .
foreach i of numlist 1/24 {
  replace dustpan_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image19Dustpan"
}
gen worms_auc = .
foreach i of numlist 1/24 {
  replace worms_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image20WormsInMouth"
}
gen babytumor_auc = .
foreach i of numlist 1/24 {
  replace babytumor_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image21BabyWithTumor"
}
gen couple2_auc = .
foreach i of numlist 1/24 {
  replace couple2_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image22Couple2"
}
gen sailboat_auc = .
foreach i of numlist 1/24 {
  replace sailboat_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image23YellowSailboat"
}
gen vomit_auc = .
foreach i of numlist 1/24 {
  replace vomit_auc = phys_pic`i'phasicarea if ///
    picep_imgfile`i' == "Image24Vomit"
}

** Change in SCL for threatening images, mean change from all threatening images
egen threat_change_ls = rmean(threat_pic3_change threat_pic6_change ///
  threat_pic7_change threat_pic8_change threat_pic13_change threat_pic18_change) 
** Change in SCL for disgusting images, mean change from all disgusting images
egen disgust_change_ls = rmean(disgust_pic1_change disgust_pic12_change ///
  disgust_pic16_change disgust_pic20_change disgust_pic21_change ///
  disgust_pic24_change)

*** EDA: Area-under-the-curve
** average AUC response to threatening images, logged
gen area_pic3_auc = (knife_auc)
gen area_pic6_auc = (snake_auc)
gen area_pic7_auc = (spider_auc)
gen area_pic8_auc = (gunattack_auc)
gen area_pic13_auc = (caraccident_auc)
gen area_pic18_auc = (shipsinking_auc)
** average AUC reponse to disgusting images, logged
gen area_pic1_auc = (ashtray_auc)
gen area_pic12_auc = (nastytoilet_auc)
gen area_pic16_auc = (maggots_auc)
gen area_pic20_auc = (worms_auc)
gen area_pic21_auc = (babytumor_auc)
gen area_pic24_auc = (vomit_auc)

egen threat_change_auc = rmean(area_pic3_auc area_pic6_auc area_pic7_auc ///
  area_pic8_auc area_pic13_auc area_pic18_auc)
egen disgust_change_auc = rmean(area_pic1_auc area_pic12_auc area_pic16_auc ///
  area_pic20_auc area_pic21_auc area_pic24_auc)

*** Logged area-under-the-curve measures
** average AUC response to threatening images, logged
gen area_pic3_auc_ln = log(knife_auc)
gen area_pic6_auc_ln = log(snake_auc)
gen area_pic7_auc_ln = log(spider_auc)
gen area_pic8_auc_ln = log(gunattack_auc)
gen area_pic13_auc_ln = log(caraccident_auc)
gen area_pic18_auc_ln = log(shipsinking_auc)
** average AUC reponse to disgusting images, logged
gen area_pic1_auc_ln = log(ashtray_auc)
gen area_pic12_auc_ln = log(nastytoilet_auc)
gen area_pic16_auc_ln = log(maggots_auc)
gen area_pic20_auc_ln = log(worms_auc)
gen area_pic21_auc_ln = log(babytumor_auc)
gen area_pic24_auc_ln = log(vomit_auc)

egen threat_change_auc_ln = rmean(area_pic3_auc_ln area_pic6_auc_ln ///
  area_pic7_auc_ln area_pic8_auc_ln area_pic13_auc_ln area_pic18_auc_ln)
egen disgust_change_auc_ln = rmean(area_pic1_auc_ln area_pic12_auc_ln ///
  area_pic16_auc_ln area_pic20_auc_ln area_pic21_auc_ln area_pic24_auc_ln)

** max AUC response during image exposure vs. during interstimulus interval
egen phasic_max = rmax(phys_pic1phasicarea phys_pic2phasicarea ///
  phys_pic3phasicarea phys_pic4phasicarea phys_pic5phasicarea ///
  phys_pic6phasicarea phys_pic7phasicarea phys_pic8phasicarea ///
  phys_pic9phasicarea phys_pic10phasicarea phys_pic11phasicarea ///
  phys_pic12phasicarea phys_pic13phasicarea phys_pic14phasicarea ///
  phys_pic15phasicarea phys_pic16phasicarea phys_pic17phasicarea ///
  phys_pic18phasicarea phys_pic19phasicarea phys_pic20phasicarea ///
  phys_pic21phasicarea phys_pic22phasicarea phys_pic23phasicarea ///
  phys_pic24phasicarea)
label var phasic_max "Maximum Phasic EDA Stimulus Response"

egen phasic_max_is = rmax(phys_isi1phasicarea phys_isi2phasicarea ///
  phys_isi3phasicarea phys_isi4phasicarea phys_isi5phasicarea ///
  phys_isi6phasicarea phys_isi7phasicarea phys_isi8phasicarea ///
  phys_isi9phasicarea phys_isi10phasicarea phys_isi11phasicarea ///
  phys_isi12phasicarea phys_isi13phasicarea phys_isi14phasicarea ///
  phys_isi15phasicarea phys_isi16phasicarea phys_isi17phasicarea ///
  phys_isi18phasicarea phys_isi19phasicarea phys_isi20phasicarea ///
  phys_isi21phasicarea phys_isi22phasicarea phys_isi23phasicarea ///
  phys_isi24phasicarea)
label var phasic_max_is "Maximum Phasic EDA Inter-stimulus Response"

** mean phasic vs. mean tonic signal
egen mean_phasic = rmean(phys_pic1absphasicavg phys_pic2absphasicavg ///
  phys_pic3absphasicavg phys_pic4absphasicavg phys_pic5absphasicavg ///
  phys_pic6absphasicavg phys_pic7absphasicavg phys_pic8absphasicavg ///
  phys_pic9absphasicavg phys_pic10absphasicavg phys_pic11absphasicavg ///
  phys_pic12absphasicavg phys_pic13absphasicavg phys_pic14absphasicavg ///
  phys_pic15absphasicavg phys_pic16absphasicavg phys_pic17absphasicavg ///
  phys_pic18absphasicavg phys_pic19absphasicavg phys_pic20absphasicavg ///
  phys_pic21absphasicavg phys_pic22absphasicavg phys_pic23absphasicavg ///
  phys_pic24absphasicavg)
label var mean_phasic "Mean Phasic EDA Stimulus Response"

egen mean_tonic = rmean(phys_pic1gsravg phys_pic2gsravg phys_pic3gsravg ///
  phys_pic4gsravg phys_pic5gsravg phys_pic6gsravg phys_pic7gsravg ///
  phys_pic8gsravg phys_pic9gsravg phys_pic10gsravg phys_pic11gsravg ///
  phys_pic12gsravg phys_pic13gsravg phys_pic14gsravg phys_pic15gsravg ///
  phys_pic16gsravg phys_pic17gsravg phys_pic18gsravg phys_pic19gsravg ///
  phys_pic20gsravg phys_pic21gsravg phys_pic22gsravg phys_pic23gsravg ///
  phys_pic24gsravg)
label var mean_tonic "Mean Tonic EDA Stimulus Response"
 
** phasic area vs. tonic area
egen area_tonic = rmean(phys_pic1gsrarea phys_pic2gsrarea phys_pic3gsrarea ///
  phys_pic4gsrarea phys_pic5gsrarea phys_pic6gsrarea phys_pic7gsrarea ///
  phys_pic8gsrarea phys_pic9gsrarea phys_pic10gsrarea phys_pic11gsrarea ///
  phys_pic12gsrarea phys_pic13gsrarea phys_pic14gsrarea phys_pic15gsrarea ///
  phys_pic16gsrarea phys_pic17gsrarea phys_pic18gsrarea phys_pic19gsrarea ///
  phys_pic20gsrarea phys_pic21gsrarea phys_pic22gsrarea phys_pic23gsrarea ///
  phys_pic24gsrarea)
label var area_tonic "Tonic Area EDA Stimulus Response"

egen area_phasic = rmean(phys_pic1phasicarea phys_pic2phasicarea ///
  phys_pic3phasicarea phys_pic4phasicarea phys_pic5phasicarea ///
  phys_pic6phasicarea phys_pic7phasicarea phys_pic8phasicarea ///
  phys_pic9phasicarea phys_pic10phasicarea phys_pic11phasicarea ///
  phys_pic12phasicarea phys_pic13phasicarea phys_pic14phasicarea ///
  phys_pic15phasicarea phys_pic16phasicarea phys_pic17phasicarea ///
  phys_pic18phasicarea phys_pic19phasicarea phys_pic20phasicarea ///
  phys_pic21phasicarea phys_pic22phasicarea phys_pic23phasicarea ///
  phys_pic24phasicarea)
label var area_phasic "Phasic Area EDA Stimulus Response"


**************
** Table 5A.1a
**************
pwcorr area_tonic mean_tonic area_phasic mean_phasic if country == 1
**************
  
**************
** Table 5A.1b
**************
pwcorr area_tonic mean_tonic area_phasic mean_phasic if outlier_us != 1 & ///
  country == 0
**************

***************
** Figure 5A.1a
***************
lowess mean_tonic area_phasic if country == 1, ///
  title("Phasic area vs Average tonic - Denmark") ///
  note("")
graph export Figures/OnlineAppendix-Figure05A1a.pdf, replace
graph drop _all
graph close
***************

***************
** Figure 5A.1b
***************
lowess mean_tonic area_phasic if outlier_us != 1 & country == 0, ///
  title("Phasic area vs Average tonic - United States") ///
  note("")
graph export Figures/OnlineAppendix-Figure05A1b.pdf, replace
graph drop _all
graph close
***************

*************
** Table 5A.e
*************
pwcorr threat_change_auc_ln disgust_change_auc_ln threat_change_ls ///
 disgust_change_ls if country == 1
*************

*************
** Table 5A.f
*************
pwcorr threat_change_auc_ln disgust_change_auc_ln threat_change_ls ///
 disgust_change_ls if country == 0
*************

********************************************************************************
********************************************************************************
** Online Appendix 5B
********************************************************************************
********************************************************************************

** merging picture ratings from the ratings survey with lab study data
* reading in ratings data and saving mean values as local variables
clear
use Data/OsmundsenEtAl-RatingStudy-JOP-Recodes.dta, clear

** Due to a coding error by the survey firm that fielded the picture rating
** survey, strength ratings were not recorded for picture 15. When the error was
** discovered, a follow-up study was conducted in which the same respondents
** were asked to provide strength ratings on just picture 15.

forvalues i = 1/24 {
  if (`i' == 15) {
    forvalues j = 2/5 {
	  quietly sum Q`i'_`j'
	  local rating`i'_`j' = `r(mean)'
      display "`rating`i'_`j''"
	}
  }
  else {
    forvalues j = 1/5 {
	  quietly sum Q`i'_`j'
	  local rating`i'_`j' = `r(mean)'
      display "`rating`i'_`j''"
    }
  }
}

clear
use Data/OsmundsenEtAl-RatingStudy-FollowUp-JOP-Original.dta, clear
quietly sum Q15_1
local rating15_1 = `r(mean)'

* read in recoded data file
clear
use Data/OsmundsenEtAl-PsychophysiologyIdeology-JOP-Recodes.dta, clear

* create variable that is the mean rating for each picture
forvalues i = 1/24 {
  gen strong_pic`i' = `rating`i'_1'
}
forvalues i = 1/24 {
  gen discomfort_pic`i' = `rating`i'_2'
}
forvalues i = 1/24 {
  gen happy_pic`i' = `rating`i'_3'
}
forvalues i = 1/24 {
  gen threatr_pic`i' = `rating`i'_4'
}
forvalues i = 1/24 {
  gen disgustr_pic`i' = `rating`i'_5'
}

* self-ratings for each picture
gen selfrateneg_pic3 = arosvy_q25
gen selfratearo_pic3 = arosvy_q27
gen selfrateneg_pic6 = arosvy_q37
gen selfratearo_pic6 = arosvy_q391
gen selfrateneg_pic20 = arosvy_q17
gen selfratearo_pic20 = arosvy_q19
gen selfrateneg_pic21 = arosvy_q21
gen selfratearo_pic21 = arosvy_q23
gen selfrateneg_pic14 = arosvy_q13
gen selfratearo_pic14 = arosvy_q15
gen selfrateneg_pic11 = arosvy_q8
gen selfratearo_pic11 = arosvy_q11
gen selfrateneg_pic4 = arosvy_q29
gen selfratearo_pic4 = arosvy_q31
gen selfrateneg_pic5 = arosvy_q33
gen selfratearo_pic5 = arosvy_q35

* consider complete cases to be those for whom we have complete eda measures and
*   complete survey-based ideology measures
quietly reg z_threat_change_total z_disgust_change_total ///
  z_neutral_change_total z_pos_change_total z_wp z_soccon z_ecocon ///
  z_leftright_alt
gen complete = 1 if e(sample)

* outlier indicators
egen max_z_threat_change_total_us = max(z_threat_change_total) if ///
  complete == 1 & country == 0
egen max_z_disgust_change_total_dk = max(z_disgust_change_total) if ///
  complete == 1 & country == 1
gen outlier_us = 1 if z_threat_change_total == ///
  max_z_threat_change_total_us & max_z_threat_change_total_us != .
gen outlier_dk = 1 if z_disgust_change_total == ///
  max_z_disgust_change_total_dk & max_z_disgust_change_total_dk != .
drop max_z_threat_change_total_us max_z_disgust_change_total_dk

gen ashtray_emgavg = .
foreach i of numlist 1/24 {
  replace ashtray_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image01Ashtray2"
}
gen basket_emgavg = .
foreach i of numlist 1/24 {
  replace basket_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image02Basket"
}
gen knife_emgavg = .
foreach i of numlist 1/24 {
  replace knife_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image03KnifeHeldByMan"
}
gen skysurfer_emgavg = .
foreach i of numlist 1/24 {
  replace skysurfer_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image04Skysurfer"
}
gen couple_emgavg = .
foreach i of numlist 1/24 {
  replace couple_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image05Couple3"
}
gen snake_emgavg = .
foreach i of numlist 1/24 {
  replace snake_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image06Snake2"
}
gen spider_emgavg = .
foreach i of numlist 1/24 {
  replace spider_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image07Spider"
}
gen gunattack_emgavg = .
foreach i of numlist 1/24 {
  replace gunattack_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image08GunAttack1"
}
gen umbrella_emgavg = .
foreach i of numlist 1/24 {
  replace umbrella_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image09Umbrella"
}
gen snowskier_emgavg = .
foreach i of numlist 1/24 {
  replace snowskier_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image10SnowSkier"
}
gen bowl_emgavg = .
foreach i of numlist 1/24 {
  replace bowl_emgavg = phys_pic`i'emgavg if picep_imgfile`i' == "Image11Bowl"
}
gen nastytoilet_emgavg = .
foreach i of numlist 1/24 {
  replace nastytoilet_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image12NastyToilet"
}
gen caraccident_emgavg = .
foreach i of numlist 1/24 {
  replace caraccident_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image13CarAccident1"
}
gen mug_emgavg = .
foreach i of numlist 1/24 {
  replace mug_emgavg = phys_pic`i'emgavg if picep_imgfile`i' == "Image14Mug"
}
gen rollingpin_emgavg = .
foreach i of numlist 1/24 {
  replace rollingpin_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image15RollingPin"
}
gen maggots_emgavg = .
foreach i of numlist 1/24 {
  replace maggots_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image16Maggots"
}
gen waterfall_emgavg = .
foreach i of numlist 1/24 {
  replace waterfall_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "image17Waterfall"
}
gen shipsinking_emgavg = .
foreach i of numlist 1/24 {
  replace shipsinking_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "image18ShipSinking1"
}
gen dustpan_emgavg = .
foreach i of numlist 1/24 {
  replace dustpan_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image19Dustpan"
}
gen worms_emgavg = .
foreach i of numlist 1/24 {
  replace worms_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image20WormsInMouth"
}
gen babytumor_emgavg = .
foreach i of numlist 1/24 {
  replace babytumor_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image21BabyWithTumor"
}
gen couple2_emgavg = .
foreach i of numlist 1/24 {
  replace couple2_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image22Couple2"
}
gen sailboat_emgavg = .
foreach i of numlist 1/24 {
  replace sailboat_emgavg = phys_pic`i'emgavg if ///
    picep_imgfile`i' == "Image23YellowSailboat"
}
gen vomit_emgavg = .
foreach i of numlist 1/24 {
  replace vomit_emgavg = phys_pic`i'emgavg if picep_imgfile`i' == "Image24Vomit"
}

* recoding inter-stimulus intervals
gen ashtray_emgavgis = .
foreach i of numlist 1/24 {
  replace ashtray_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img01"
}
gen basket_emgavgis = .
foreach i of numlist 1/24 {
  replace basket_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img02"
}
gen knife_emgavgis = .
foreach i of numlist 1/24 {
  replace knife_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img03"
}
gen skysurfer_emgavgis = .
foreach i of numlist 1/24 {
  replace skysurfer_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img04"
}
gen couple3_emgavgis = .
foreach i of numlist 1/24 {
  replace couple3_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img05"
}
gen snake_emgavgis= .
foreach i of numlist 1/24 {
  replace snake_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img06"
}
gen spider_emgavgis = .
foreach i of numlist 1/24 {
  replace spider_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img07"
}
gen gunattack_emgavgis = .
foreach i of numlist 1/24 {
  replace gunattack_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img08"
}
gen umbrella_emgavgis = .
foreach i of numlist 1/24 {
  replace umbrella_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img09"
}
gen snowskier_emgavgis = .
foreach i of numlist 1/24 {
  replace snowskier_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img10"
}
gen bowl_emgavgis = .
foreach i of numlist 1/24 {
  replace bowl_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img11"
}
gen nastytoilet_emgavgis = .
foreach i of numlist 1/24 {
  replace nastytoilet_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img12"
}
gen caraccident_emgavgis = .
foreach i of numlist 1/24 {
  replace caraccident_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img13"
}
gen mug_emgavgis = .
foreach i of numlist 1/24 {
  replace mug_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img14"
}
gen rollingpin_emgavgis = .
foreach i of numlist 1/24 {
  replace rollingpin_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img15"
}
gen maggots_emgavgis = .
foreach i of numlist 1/24 {
  replace maggots_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img16"
}
gen waterfall_emgavgis = .
foreach i of numlist 1/24 {
  replace waterfall_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img17"
}
gen shipsinking_emgavgis = .
foreach i of numlist 1/24 {
  replace shipsinking_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img18"
}
gen dustpan_emgavgis = .
foreach i of numlist 1/24 {
  replace dustpan_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img19"
}
gen worms_emgavgis = .
foreach i of numlist 1/24 {
  replace worms_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img20"
}
gen babytumor_emgavgis = .
foreach i of numlist 1/24 {
  replace babytumor_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img21"
}
gen couple2_emgavgis = .
foreach i of numlist 1/24 {
  replace couple2_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img22"
}
gen yellowsailboat_emgavgis = .
foreach i of numlist 1/24 {
  replace yellowsailboat_emgavgis = phys_isi`i'emgavg if ///
    picep_imgid`i' == "img23"
}
gen vomit_emgavgis = .
foreach i of numlist 1/24 {
  replace vomit_emgavgis = phys_isi`i'emgavg if picep_imgid`i' == "img24"
}

** average EMG response to threatening images, square root transformation
gen threat_pic3_emg = sqrt(knife_emgavg)
gen threat_pic6_emg = sqrt(snake_emgavg)
gen threat_pic7_emg = sqrt(spider_emgavg)
gen threat_pic8_emg = sqrt(gunattack_emgavg)
gen threat_pic13_emg = sqrt(caraccident_emgavg)
gen threat_pic18_emg = sqrt(shipsinking_emgavg)
** average EMG reponse to disgusting images, square root transformation
gen disgust_pic1_emg = sqrt(ashtray_emgavg)
gen disgust_pic12_emg = sqrt(nastytoilet_emgavg)
gen disgust_pic16_emg = sqrt(maggots_emgavg)
gen disgust_pic20_emg = sqrt(worms_emgavg)
gen disgust_pic21_emg = sqrt(babytumor_emgavg)
gen disgust_pic24_emg = sqrt(vomit_emgavg)
** average EMG response to neutral pictures, square root transformation
gen neutral_pic2_emg = sqrt(basket_emgavg)
gen neutral_pic9_emg = sqrt(umbrella_emgavg)
gen neutral_pic11_emg = sqrt(bowl_emgavg)
gen neutral_pic14_emg = sqrt(mug_emgavg)
gen neutral_pic15_emg = sqrt(rollingpin_emgavg)
gen neutral_pic19_emg = sqrt(dustpan_emgavg)
** average EMG response to positive images, square root transformation
gen pos_pic4_emg = sqrt(skysurfer_emgavg)
gen pos_pic5_emg = sqrt(couple_emgavg)
gen pos_pic10_emg = sqrt(snowskier_emgavg)
gen pos_pic17_emg = sqrt(waterfall_emgavg)
gen pos_pic22_emg = sqrt(couple2_emgavg)
gen pos_pic23_emg = sqrt(sailboat_emgavg)
** average EMG inter-stimulus threatening images, square root transformation
gen threat_pic3_is_emg = sqrt(knife_emgavgis)
gen threat_pic6_is_emg = sqrt(snake_emgavgis)
gen threat_pic7_is_emg = sqrt(spider_emgavgis)
gen threat_pic8_is_emg = sqrt(gunattack_emgavgis)
gen threat_pic13_is_emg = sqrt(caraccident_emgavgis)
gen threat_pic18_is_emg = sqrt(shipsinking_emgavgis)
** average EMG inter-stimulus disgusting images, square root transformation
gen disgust_pic1_is_emg = sqrt(ashtray_emgavgis)
gen disgust_pic12_is_emg = sqrt(nastytoilet_emgavgis)
gen disgust_pic16_is_emg = sqrt(maggots_emgavgis)
gen disgust_pic20_is_emg = sqrt(worms_emgavgis)
gen disgust_pic21_is_emg = sqrt(babytumor_emgavgis)
gen disgust_pic24_is_emg = sqrt(vomit_emgavgis)
** average EMG inter-stimulus neutral images, square root transformation
gen neutral_pic2_is_emg = sqrt(basket_emgavgis)
gen neutral_pic9_is_emg = sqrt(umbrella_emgavgis)
gen neutral_pic11_is_emg = sqrt(bowl_emgavgis)
gen neutral_pic14_is_emg = sqrt(mug_emgavgis)
gen neutral_pic15_is_emg = sqrt(rollingpin_emgavgis)
gen neutral_pic19_is_emg = sqrt(dustpan_emgavgis)
** average EMG inter-stimulus positive images, square root transformation
gen pos_pic4_is_emg = sqrt(skysurfer_emgavgis)
gen pos_pic5_is_emg = sqrt(couple3_emgavgis)
gen pos_pic10_is_emg = sqrt(snowskier_emgavgis)
gen pos_pic17_is_emg = sqrt(waterfall_emgavgis)
gen pos_pic22_is_emg = sqrt(couple2_emgavgis)
gen pos_pic23_is_emg = sqrt(yellowsailboat_emgavgis)
** Change in EMG, threatening images
replace threat_pic3_change = (threat_pic3_emg - threat_pic3_is_emg)
replace threat_pic6_change = (threat_pic6_emg - threat_pic6_is_emg)
replace threat_pic7_change = (threat_pic7_emg - threat_pic7_is_emg)
replace threat_pic8_change = (threat_pic8_emg - threat_pic8_is_emg)
replace threat_pic13_change = (threat_pic13_emg - threat_pic13_is_emg)
replace threat_pic18_change = (threat_pic18_emg - threat_pic18_is_emg)
** Change in EMG, disgusting images
replace disgust_pic1_change = (disgust_pic1_emg - disgust_pic1_is_emg)
replace disgust_pic12_change = (disgust_pic12_emg - disgust_pic12_is_emg)
replace disgust_pic16_change = (disgust_pic16_emg - disgust_pic16_is_emg)
replace disgust_pic20_change = (disgust_pic20_emg - disgust_pic20_is_emg)
replace disgust_pic21_change = (disgust_pic21_emg - disgust_pic21_is_emg)
replace disgust_pic24_change = (disgust_pic24_emg - disgust_pic24_is_emg)
** Change in EMG, neutral images
replace neutral_pic2_change = (neutral_pic2_emg - neutral_pic2_is_emg)
replace neutral_pic9_change = (neutral_pic9_emg - neutral_pic9_is_emg)
replace neutral_pic11_change = (neutral_pic11_emg - neutral_pic11_is_emg)
replace neutral_pic14_change = (neutral_pic14_emg - neutral_pic14_is_emg)
replace neutral_pic15_change = (neutral_pic15_emg - neutral_pic15_is_emg)
replace neutral_pic19_change = (neutral_pic19_emg - neutral_pic19_is_emg)
** Change in EMG, positive images
replace pos_pic4_change = (pos_pic4_emg - pos_pic4_is_emg)
replace pos_pic5_change = (pos_pic5_emg - pos_pic5_is_emg)
replace pos_pic10_change = (pos_pic10_emg - pos_pic10_is_emg)
replace pos_pic17_change = (pos_pic17_emg - pos_pic17_is_emg)
replace pos_pic22_change = (pos_pic22_emg - pos_pic22_is_emg)
replace pos_pic23_change = (pos_pic23_emg - pos_pic23_is_emg)

** Change in EMG for threatening images, mean change from all threatening images
egen threat_change_total_emg = rmean(threat_pic3_change threat_pic6_change ///
  threat_pic7_change threat_pic8_change threat_pic13_change threat_pic18_change)
** Change in EMG for disgusting images, mean change from all disgusting images
egen disgust_change_total_emg = rmean(disgust_pic1_change ///
  disgust_pic12_change disgust_pic16_change disgust_pic20_change ///
  disgust_pic21_change disgust_pic24_change)
** Change in EMG for disgusting + threatening images, mean change from all
** disgusting + threatening images
egen threat_disgust_change_toal_emg = rmean(threat_pic3_change ///
  threat_pic6_change threat_pic7_change threat_pic8_change ///
  threat_pic13_change threat_pic18_change disgust_pic1_change ///
  disgust_pic12_change disgust_pic16_change disgust_pic20_change ///
  disgust_pic21_change disgust_pic24_change)
** Change in EMG for neutral images, mean change from all neutral images
egen neutral_change_total_emg = rmean(neutral_pic2_change ///
  neutral_pic9_change neutral_pic11_change neutral_pic14_change ///
  neutral_pic15_change neutral_pic19_change)
** Change in EMG for positive images, mean change from all positive images
egen pos_change_total_emg = rmean(pos_pic4_change pos_pic5_change ///
  pos_pic10_change pos_pic17_change pos_pic22_change pos_pic23_change)

drop z_wp z_soccon z_ecocon z_leftright_alt
foreach var in threat_change_total_emg disgust_change_total_emg ///
  threat_disgust_change_toal_emg neutral_change_total_emg ///
  pos_change_total_emg  wp soccon ecocon leftright_alt {
  egen z_`var' = std(`var')
}

replace eda_pic3 = (threat_pic3_emg)
replace eda_pic6 = (threat_pic6_emg)
replace eda_pic7 = (threat_pic7_emg)
replace eda_pic8 = (threat_pic8_emg)
replace eda_pic13 = (threat_pic13_emg)
replace eda_pic18 = (threat_pic18_emg)
replace eda_pic1 = (disgust_pic1_emg)
replace eda_pic12 = (disgust_pic12_emg)
replace eda_pic16 = (disgust_pic16_emg)
replace eda_pic20 = (disgust_pic20_emg)
replace eda_pic21 = (disgust_pic21_emg)
replace eda_pic24 = (disgust_pic24_emg)
replace eda_pic2 = (neutral_pic2_emg)
replace eda_pic9 = (neutral_pic9_emg)
replace eda_pic11 = (neutral_pic11_emg)
replace eda_pic14 = (neutral_pic14_emg)
replace eda_pic15 = (neutral_pic15_emg)
replace eda_pic19 = (neutral_pic19_emg)
replace eda_pic4 = (pos_pic4_emg)
replace eda_pic5 = (pos_pic5_emg)
replace eda_pic10 = (pos_pic10_emg)
replace eda_pic17 = (pos_pic17_emg)
replace eda_pic22 = (pos_pic22_emg)
replace eda_pic23 = (pos_pic23_emg)


**************
** Figure 5B.a
**************
gen threat_dk = z_threat_change_total_emg if country == 1
label var threat_dk "Danes"
gen threat_us = z_threat_change_total_emg if country == 0
label var threat_us "Americans"
gen threat_total = z_threat_change_total_emg
label var threat_total "Combined"

gen disgust_dk = z_disgust_change_total_emg if country == 1
label var disgust_dk "Danes"
gen disgust_us = z_disgust_change_total_emg if country == 0
label var disgust_us "Americans"
gen disgust_total = z_disgust_change_total_emg
label var disgust_total "Combined"

gen positive_dk = z_pos_change_total_emg if country == 1
label var positive_dk "Danes"
gen positive_us = z_pos_change_total_emg if country == 0
label var positive_us "Americans"
gen positive_total = z_pos_change_total_emg
label var positive_total "Combined"

gen neutral_dk = z_neutral_change_total_emg if country == 1
label var neutral_dk "Danes"
gen neutral_us = z_neutral_change_total_emg if country == 0
label var neutral_us "Americans"
gen neutral_total = z_neutral_change_total_emg
label var neutral_total "Combined"

foreach var of varlist threat_dk threat_us threat_total {
  quietly reg z_wp `var'  female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store a1_`var'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store b1_`var'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store c1_`var'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store d1_`var'
}
foreach var of varlist threat_dk threat_us threat_total {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store a2_`var'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store b2_`var'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store c2_`var'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store d2_`var'
}
foreach var of varlist threat_dk threat_us threat_total {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store a3_`var'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store b3_`var'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store c3_`var'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store d3_`var'
}
foreach var of varlist threat_dk threat_us threat_total {
  quietly reg z_leftright_alt `var' female age education income_hh country ///
    if outlier_us != 1, robust
  estimates store a4_`var'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly reg z_leftright_alt `var' female age education income_hh country ///
    if outlier_us != 1, robust
  estimates store b4_`var'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly reg z_leftright_alt `var' female age education income_hh country ///
    if outlier_us != 1, robust
  estimates store c4_`var'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly reg z_leftright_alt `var' female age education income_hh country ///
    if outlier_us != 1, robust
estimates store d4_`var'
}

coefplot a1* b1* c1* d1*, bylabel(Wilson-Patterson) ///
  || a2* b2* c2* d2*, bylabel(Social Conservatism) ///
  || a3* b3* c3* d3*, bylabel(Economic Conservatism) ///
  || a4* b4* c4* d4*, bylabel(Left-Right self-placement) ///
  ||, drop(_cons female age education income_hh country) ///
  xline(0, lcolor(gray)) msymbol(square) color(black) ciopts(lcolor(black)) ///
  byopts(legend(off)) headings(threat_dk = "{bf:EDA Threat}" ///
    disgust_dk = "{bf:EDA Disgust}" positive_dk = "{bf:EDA Positive}" ///
    neutral_dk = "{bf:EDA Neutral}",) format(%9.1g) mlabel mlabposition(10) ///
    mlabsize(1.5) grid(none) subtitle(, size(small)) xsize(7) ysize(10)
graph export Figures/OnlineAppendix-Figure05Ba.pdf, replace
graph drop _all
graph close

estimates drop a* b* c* d*
**************

preserve

**************
** Figure 5B.b
**************
reshape long area_pic eda_pic strong_pic discomfort_pic happy_pic ///
  threatr_pic disgustr_pic selfrateneg_pic selfratearo_pic order_pic, i(id) ///
  j(picture)
foreach var in area_pic strong_pic discomfort_pic happy_pic threatr_pic ///
  disgustr_pic eda_pic selfrateneg_pic selfratearo_pic wp leftright_alt ///
  soccon ecocon {
  egen `var'01 = std(`var')
}
gen IAthreat = eda_pic01 * threatr_pic01
gen IAdisgust = eda_pic01 * disgustr_pic01
gen IAdiscomfort = eda_pic01 * discomfort_pic01
gen IAstrong = eda_pic01 * strong_pic01
gen IAhappy = eda_pic01 * happy_pic01
gen IAownneg = eda_pic01 * selfrateneg_pic01
gen IAownarous = eda_pic01 * selfratearo_pic01

gen IAownneg_dk = IAownneg if country == 1
label var IAownneg_dk "Danes"
gen IAownneg_us = IAownneg if country == 0
label var IAownneg_us "Americans"
gen IAownneg_total = IAownneg
label var IAownneg_total "Combined"

foreach var of varlist IAownneg_dk IAownneg_us IAownneg_total {
  quietly reg wp01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country, cluster(id)
  estimates store wp_`var'
}
foreach var of varlist IAownneg_dk IAownneg_us IAownneg_total {
  quietly reg soccon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country, cluster(id)
  estimates store soccon_`var'
}
foreach var of varlist IAownneg_dk IAownneg_us IAownneg_total {
  quietly reg leftright_alt01 `var' eda_pic01 selfrateneg_pic female age ///
    education income_hh country, cluster(id)
  estimates store leftright_`var'
}
foreach var of varlist IAownneg_dk IAownneg_us IAownneg_total {
  quietly reg ecocon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country, cluster(id)
  estimates store ecocon_`var'
}

coefplot wp_* , bylabel(Wilson-Patterson) ///
  || soccon_*, bylabel(Social Conservatism) ///
  || leftright_*, bylabel(Economic Conservatism) ///
  || ecocon*, bylabel(Left-Right Self-placement) ///
  ||, drop(_cons female age education income_hh eda_pic01 selfrateneg_pic ///
  country ) xline(0, lcolor(gray)) msymbol(square) color(black) ///
  ciopts(lcolor(black)) byopts(legend(off)) ///
  headings(IAownneg_dk = "{bf:IAownneg}",) ///
  format(%9.1g) mlabel mlabposition(10) mlabsize(1.5) grid(none)
graph export Figures/OnlineAppendix-Figure05Bb.pdf, replace
graph drop _all
graph close

estimates drop wp_* soccon_* leftright_* ecocon_*
**************

**************
** Figure 5B.c
**************
gen IAthreat_dk = IAthreat if country == 1
label var IAthreat_dk "Danes"
gen IAthreat_us = IAthreat if country == 0
label var IAthreat_us "Americans"
gen IAthreat_total = IAthreat
label var IAthreat_total "Combined"

gen IAdisgust_dk = IAdisgust if country == 1
label var IAdisgust_dk "Danes"
gen IAdisgust_us = IAdisgust if country == 0
label var IAdisgust_us "Americans"
gen IAdisgust_total = IAdisgust
label var IAdisgust_total "Combined"

gen IAdiscomfort_dk = IAdiscomfort if country == 1
label var IAdiscomfort_dk "Danes"
gen IAdiscomfort_us = IAdiscomfort if country == 0
label var IAdiscomfort_us "Americans"
gen IAdiscomfort_total = IAdiscomfort
label var IAdiscomfort_total "Combined"

gen IAstrong_dk = IAstrong if country == 1
label var IAstrong_dk "Danes"
gen IAstrong_us = IAstrong if country == 0
label var IAstrong_us "Americans"
gen IAstrong_total = IAstrong
label var IAstrong_total "Combined"

gen IAhappy_dk = IAhappy if country == 1
label var IAhappy_dk "Danes"
gen IAhappy_us = IAhappy if country == 0
label var IAhappy_us "Americans"
gen IAhappy_total = IAhappy
label var IAhappy_total "Combined"

foreach var of varlist IAthreat_dk IAthreat_us IAthreat_total {
  quietly reg wp01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly reg wp01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAdiscomfort_dk IAdiscomfort_us IAdiscomfort_total {
  quietly reg wp01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly reg wp01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAhappy_dk IAhappy_us IAhappy_total {
  quietly reg wp01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAthreat_dk IAthreat_us IAthreat_total {
  quietly reg soccon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly reg soccon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAdiscomfort_dk IAdiscomfort_us IAdiscomfort_total {
  quietly reg soccon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly reg soccon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAhappy_dk IAhappy_us IAhappy_total {
  quietly reg soccon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAthreat_dk IAthreat_us IAthreat_total {
  quietly reg leftright_alt01 `var' eda_pic01 selfrateneg_pic female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly reg leftright_alt01 `var' eda_pic01 selfrateneg_pic female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAdiscomfort_dk IAdiscomfort_us IAdiscomfort_total {
  quietly reg leftright_alt01 `var' eda_pic01 selfrateneg_pic female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly reg leftright_alt01 `var' eda_pic01 selfrateneg_pic female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAhappy_dk IAhappy_us IAhappy_total {
  quietly reg leftright_alt01 `var' eda_pic01 selfrateneg_pic female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist  IAthreat_dk IAthreat_us IAthreat_total {
  quietly reg ecocon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store ecocon01_`var'
}
foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly reg ecocon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store ecocon01_`var'
}
foreach var of varlist IAdiscomfort_dk IAdiscomfort_us IAdiscomfort_total {
  quietly reg ecocon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store ecocon01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly reg ecocon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store ecocon01_`var'
}
foreach var of varlist IAhappy_dk IAhappy_us IAhappy_total {
  quietly reg ecocon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store ecocon01_`var'
}

coefplot wp01_*, bylabel(Wilson-Patterson) ///
  || soccon01_*, bylabel(Social Conservatism) ///
  || ecocon01_*, bylabel(Economic Conservatism) ///
  || lr01_*, bylabel(Left-Right self-placement) ///
  ||, drop(_cons eda_pic01 threatr_pic01 disgustr_pic01 discomfort_pic01 ///
  happy_pic01 strong_pic01 selfrateneg_pic female age education income_hh ///
  country) xline(0, lcolor(gray)) msymbol(square) color(black) ///
  ciopts(lcolor(black)) byopts(legend(off)) ///
  headings(IAthreat_dk = "{bf:Threatened}" IAdisgust_dk = "{bf:Disgusted}" ///
  IAdiscomfort_dk = "{bf:Uncomfortable}" ///
  IAstrong_dk = "{bf:Emotion Strength}" IAhappy_dk="{bf:Happy}",) ///
  format(%9.1g) mlabel mlabposition(10) mlabsize(1.5) grid(none) ///
  subtitle(, size(small)) xsize(7) ysize(10)
graph export Figures/OnlineAppendix-Figure05Bc.pdf, replace
graph drop _all
graph close

estimates drop wp01_* soccon01_* ecocon01_* lr01_*
**************

restore


********************************************************************************
********************************************************************************
** Online Appendix 5C
********************************************************************************
********************************************************************************

** merging picture ratings from the ratings survey with lab study data
* reading in ratings data and saving mean values as local variables
clear
use Data/OsmundsenEtAl-RatingStudy-JOP-Recodes.dta, clear

** Due to a coding error by the survey firm that fielded the picture rating
** survey, strength ratings were not recorded for picture 15. When the error was
** discovered, a follow-up study was conducted in which the same respondents
** were asked to provide strength ratings on just picture 15.

forvalues i = 1/24 {
  if (`i' == 15) {
    forvalues j = 2/5 {
	  quietly sum Q`i'_`j'
	  local rating`i'_`j' = `r(mean)'
      display "`rating`i'_`j''"
	}
  }
  else {
    forvalues j = 1/5 {
	  quietly sum Q`i'_`j'
	  local rating`i'_`j' = `r(mean)'
      display "`rating`i'_`j''"
    }
  }
}

clear
use Data/OsmundsenEtAl-RatingStudy-FollowUp-JOP-Original.dta, clear
quietly sum Q15_1
local rating15_1 = `r(mean)'

* read in recoded data file
clear
use Data/OsmundsenEtAl-PsychophysiologyIdeology-JOP-Recodes.dta, clear

* create variable that is the mean rating for each picture
forvalues i = 1/24 {
  gen strong_pic`i' = `rating`i'_1'
}
forvalues i = 1/24 {
  gen discomfort_pic`i' = `rating`i'_2'
}
forvalues i = 1/24 {
  gen happy_pic`i' = `rating`i'_3'
}
forvalues i = 1/24 {
  gen threatr_pic`i' = `rating`i'_4'
}
forvalues i = 1/24 {
  gen disgustr_pic`i' = `rating`i'_5'
}

* self-ratings for each picture
gen selfrateneg_pic3 = arosvy_q25
gen selfratearo_pic3 = arosvy_q27
gen selfrateneg_pic6 = arosvy_q37
gen selfratearo_pic6 = arosvy_q391
gen selfrateneg_pic20 = arosvy_q17
gen selfratearo_pic20 = arosvy_q19
gen selfrateneg_pic21 = arosvy_q21
gen selfratearo_pic21 = arosvy_q23
gen selfrateneg_pic14 = arosvy_q13
gen selfratearo_pic14 = arosvy_q15
gen selfrateneg_pic11 = arosvy_q8
gen selfratearo_pic11 = arosvy_q11
gen selfrateneg_pic4 = arosvy_q29
gen selfratearo_pic4 = arosvy_q31
gen selfrateneg_pic5 = arosvy_q33
gen selfratearo_pic5 = arosvy_q35

* consider complete cases to be those for whom we have complete eda measures and
*   complete survey-based ideology measures
quietly reg z_threat_change_total z_disgust_change_total ///
  z_neutral_change_total z_pos_change_total z_wp z_soccon z_ecocon ///
  z_leftright_alt
gen complete = 1 if e(sample)

* outlier indicators
egen max_z_threat_change_total_us = max(z_threat_change_total) if ///
  complete == 1 & country == 0
egen max_z_disgust_change_total_dk = max(z_disgust_change_total) if ///
  complete == 1 & country == 1
gen outlier_us = 1 if z_threat_change_total == ///
  max_z_threat_change_total_us & max_z_threat_change_total_us != .
gen outlier_dk = 1 if z_disgust_change_total == ///
  max_z_disgust_change_total_dk & max_z_disgust_change_total_dk != .
drop max_z_threat_change_total_us max_z_disgust_change_total_dk

* generate strong political convictions dummy to condition on in analyses
recode presvy_q4 (2 / 5 = 0) (6 / 11 = 1), gen(pol_strength)
gen pol_convict = 0
replace pol_convict = 1 if presvy_q3 == 1 & pol_strength == 1

gen threat_minus_neutral = threat_change_total - neutral_change_total
egen z_threat_minus_neutral = std(threat_minus_neutral) 

**************
** Figure 5C.a
**************
gen threat_dk = z_threat_change_total if country == 1
label var threat_dk "Danes"
gen threat_us = z_threat_change_total if country == 0
label var threat_us "Americans"
gen threat_total = z_threat_change_total
label var threat_total "Combined"

gen disgust_dk = z_disgust_change_total if country == 1
label var disgust_dk "Danes"
gen disgust_us = z_disgust_change_total if country == 0
label var disgust_us "Americans"
gen disgust_total = z_disgust_change_total
label var disgust_total "Combined"

gen positive_dk = z_pos_change_total if country == 1
label var positive_dk "Danes"
gen positive_us = z_pos_change_total if country == 0
label var positive_us "Americans"
gen positive_total = z_pos_change_total
label var positive_total "Combined"

gen neutral_dk = z_neutral_change_total if country == 1
label var neutral_dk "Danes"
gen neutral_us = z_neutral_change_total if country == 0
label var neutral_us "Americans"
gen neutral_total = z_neutral_change_total
label var neutral_total "Combined"

foreach var of varlist threat_dk threat_us threat_total {
  quietly reg z_wp `var'  female age education income_hh country if ///
    outlier_us != 1 & pol_convict == 1, robust
  estimates store a1_`var'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1 & pol_convict == 1, robust
  estimates store b1_`var'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1 & pol_convict == 1, robust
  estimates store c1_`var'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1 & pol_convict == 1, robust
  estimates store d1_`var'
}
foreach var of varlist threat_dk threat_us threat_total {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1 & pol_convict == 1, robust
  estimates store a2_`var'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1 & pol_convict == 1, robust
  estimates store b2_`var'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1 & pol_convict == 1, robust
  estimates store c2_`var'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1 & pol_convict == 1 , robust
  estimates store d2_`var'
}
foreach var of varlist threat_dk threat_us threat_total {
  quietly reg z_ecocon `var'  female age education income_hh country if ///
    outlier_us != 1 & pol_convict == 1, robust
  estimates store a3_`var'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1 & pol_convict == 1, robust
  estimates store b3_`var'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1 & pol_convict == 1 , robust
  estimates store c3_`var'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1 & pol_convict == 1 , robust
  estimates store d3_`var'
}
foreach var of varlist threat_dk threat_us threat_total {
  quietly reg z_leftright_alt `var' female age education income_hh country ///
    if outlier_us != 1 & pol_convict == 1, robust
  estimates store a4_`var'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly reg z_leftright_alt `var' female age education income_hh country ///
    if outlier_us != 1 & pol_convict == 1, robust
  estimates store b4_`var'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly reg z_leftright_alt `var' female age education income_hh country ///
    if outlier_us != 1 & pol_convict == 1, robust
  estimates store c4_`var'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly reg z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1 & pol_convict == 1, robust
  estimates store d4_`var'
}

coefplot a1* b1* c1* d1*, bylabel(Wilson-Patterson) ///
  || a2* b2* c2* d2*, bylabel(Social Conservatism) ///
  || a3* b3* c3* d3*, bylabel(Economic Conservatism) ///
  || a4* b4* c4* d4*, bylabel(Left-Right Self-placement) ///
  ||, drop(_cons female age education income_hh country) ///
  xline(0, lcolor(gray)) msymbol(square) color(black) ciopts(lcolor(black)) ///
  byopts(legend(off)) headings(threat_dk = "{bf:Threatening SCL}" ///
  disgust_dk = "{bf:Disgusting SCL}" positive_dk = "{bf:Positive SCL}" ///
  neutral_dk = "{bf:Neutral SCL}", ) format(%9.1g) mlabel mlabposition(10) ///
  mlabsize(1.5) grid(none) subtitle(, size(small)) xsize(7) ysize(10)
graph export Figures/OnlineAppendix-Figure05Ca.pdf, replace
graph drop _all
graph close

estimates drop *
**************

preserve

**************
** Figure 5C.b
**************
reshape long area_pic eda_pic strong_pic discomfort_pic happy_pic ///
  threatr_pic disgustr_pic selfrateneg_pic selfratearo_pic order_pic, i(id) ///
  j(picture)
foreach var in area_pic strong_pic discomfort_pic happy_pic threatr_pic ///
  disgustr_pic eda_pic selfrateneg_pic selfratearo_pic wp leftright_alt ///
  soccon ecocon {
  egen `var'01 = std(`var')
}
gen IAthreat = eda_pic01 * threatr_pic01
gen IAdisgust = eda_pic01 * disgustr_pic01
gen IAdiscomfort = eda_pic01 * discomfort_pic01
gen IAstrong = eda_pic01 * strong_pic01
gen IAhappy = eda_pic01 * happy_pic01
gen IAownneg = eda_pic01 * selfrateneg_pic01
gen IAownarous = eda_pic01 * selfratearo_pic01

gen IAownneg_dk = IAownneg if country == 1
label var IAownneg_dk "Danes"
gen IAownneg_us = IAownneg if country == 0
label var IAownneg_us "Americans"
gen IAownneg_total = IAownneg
label var IAownneg_total "Combined"

foreach var of varlist IAownneg_dk IAownneg_us IAownneg_total {
  quietly reg wp01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store wp_`var'
}
foreach var of varlist IAownneg_dk IAownneg_us IAownneg_total {
  quietly reg soccon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store soccon_`var'
}
foreach var of varlist IAownneg_dk IAownneg_us IAownneg_total {
  quietly reg leftright_alt01 `var' eda_pic01 selfrateneg_pic female age ///
    education income_hh country if outlier_us != 1 & pol_convict == 1, ///
	cluster(id)
  estimates store leftright_`var'
}
foreach var of varlist IAownneg_dk IAownneg_us IAownneg_total {
  quietly reg ecocon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store ecocon_`var'
}

coefplot wp_* , bylabel(Wilson-Patterson) ///
  || soccon_*, bylabel(Social Conservatism) ///
  || leftright_*, bylabel(Economic Conservatism) ///
  || ecocon*, bylabel(Left-Right Self-placement) ///
  ||, drop(_cons female age education income_hh eda_pic01 selfrateneg_pic ///
  country ) xline(0, lcolor(gray)) msymbol(square) color(black) ///
  ciopts(lcolor(black)) byopts(legend(off)) ///
  headings(IAownneg_dk = "{bf:IAownneg}",) ///
  format(%9.1g) mlabel mlabposition(10) mlabsize(1.5) grid(none)
graph export Figures/OnlineAppendix-Figure05Cb.pdf, replace
graph drop _all
graph close

estimates drop wp_* soccon_* leftright_* ecocon_*
**************


**************
** Figure 5C.c
**************
gen IAthreat_dk = IAthreat if country == 1
label var IAthreat_dk "Danes"
gen IAthreat_us = IAthreat if country == 0
label var IAthreat_us "Americans"
gen IAthreat_total = IAthreat
label var IAthreat_total "Combined"

gen IAdisgust_dk = IAdisgust if country == 1
label var IAdisgust_dk "Danes"
gen IAdisgust_us = IAdisgust if country == 0
label var IAdisgust_us "Americans"
gen IAdisgust_total = IAdisgust
label var IAdisgust_total "Combined"

gen IAdiscomfort_dk = IAdiscomfort if country == 1
label var IAdiscomfort_dk "Danes"
gen IAdiscomfort_us = IAdiscomfort if country == 0
label var IAdiscomfort_us "Americans"
gen IAdiscomfort_total = IAdiscomfort
label var IAdiscomfort_total "Combined"

gen IAstrong_dk = IAstrong if country == 1
label var IAstrong_dk "Danes"
gen IAstrong_us = IAstrong if country == 0
label var IAstrong_us "Americans"
gen IAstrong_total = IAstrong
label var IAstrong_total "Combined"

gen IAhappy_dk = IAhappy if country == 1
label var IAhappy_dk "Danes"
gen IAhappy_us = IAhappy if country == 0
label var IAhappy_us "Americans"
gen IAhappy_total = IAhappy
label var IAhappy_total "Combined"

foreach var of varlist IAthreat_dk IAthreat_us IAthreat_total {
  quietly reg wp01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly reg wp01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAdiscomfort_dk IAdiscomfort_us IAdiscomfort_total {
  quietly reg wp01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly reg wp01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAhappy_dk IAhappy_us IAhappy_total {
  quietly reg wp01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAthreat_dk IAthreat_us IAthreat_total {
  quietly reg soccon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly reg soccon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAdiscomfort_dk IAdiscomfort_us IAdiscomfort_total {
  quietly reg soccon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly reg soccon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAhappy_dk IAhappy_us IAhappy_total {
  quietly reg soccon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAthreat_dk IAthreat_us IAthreat_total {
  quietly reg leftright_alt01 `var' eda_pic01 selfrateneg_pic female age ///
    education income_hh country if outlier_us != 1 & pol_convict == 1, ///
	cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly reg leftright_alt01 `var' eda_pic01 selfrateneg_pic female age ///
    education income_hh country if outlier_us != 1 & pol_convict == 1, ///
	cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAdiscomfort_dk IAdiscomfort_us IAdiscomfort_total {
  quietly reg leftright_alt01 `var' eda_pic01 selfrateneg_pic female age ///
    education income_hh country if outlier_us != 1 & pol_convict == 1, ///
	cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly reg leftright_alt01 `var' eda_pic01 selfrateneg_pic female age ///
    education income_hh country if outlier_us != 1 & pol_convict == 1, ///
	cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAhappy_dk IAhappy_us IAhappy_total {
  quietly reg leftright_alt01 `var' eda_pic01 selfrateneg_pic female age ///
    education income_hh country if outlier_us != 1 & pol_convict == 1, ///
	cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist  IAthreat_dk IAthreat_us IAthreat_total {
  quietly reg ecocon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store ecocon01_`var'
}
foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly reg ecocon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store ecocon01_`var'
}
foreach var of varlist IAdiscomfort_dk IAdiscomfort_us IAdiscomfort_total {
  quietly reg ecocon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store ecocon01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly reg ecocon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store ecocon01_`var'
}
foreach var of varlist IAhappy_dk IAhappy_us IAhappy_total {
  quietly reg ecocon01 `var' eda_pic01 selfrateneg_pic female age education ///
    income_hh country if outlier_us != 1 & pol_convict == 1, cluster(id)
  estimates store ecocon01_`var'
}

coefplot wp01_*, bylabel(Wilson-Patterson) ///
  || soccon01_*, bylabel(Social Conservatism) ///
  || ecocon01_*, bylabel(Economic Conservatism) ///
  || lr01_*, bylabel(Left-Right self-placement) ///
  ||, drop(_cons eda_pic01 threatr_pic01 disgustr_pic01 discomfort_pic01 ///
  happy_pic01 strong_pic01 selfrateneg_pic female age education income_hh ///
  country) xline(0, lcolor(gray)) msymbol(square) color(black) ///
  ciopts(lcolor(black)) byopts(legend(off)) ///
  headings(IAthreat_dk = "{bf:Threatened}" IAdisgust_dk = "{bf:Disgusted}" ///
  IAdiscomfort_dk = "{bf:Uncomfortable}" ///
  IAstrong_dk = "{bf:Emotion Strength}" IAhappy_dk="{bf:Happy}",) ///
  format(%9.1g) mlabel mlabposition(10) mlabsize(1.5) grid(none) ///
  subtitle(, size(small)) xsize(7) ysize(10)
graph export Figures/OnlineAppendix-Figure05Cc.pdf, replace
graph drop _all
graph close

estimates drop wp01_* soccon01_* ecocon01_* lr01_*
**************

restore


********************************************************************************
********************************************************************************
** Online Appendix 5D
********************************************************************************
********************************************************************************

**** This portion of the analysis uses a dataset generated using the
**** AcqKnowledge software for recording physiological data

clear
use ///
  Data/OsmundsenEtAl-PsychophysiologyIdeology-JOP-ManualIdentificationOfProblematicSubjects.dta, clear

destring STIM_SCL* IST_SCL_*, force replace

** Indicator for subjectively determined problematic subjects
gen probsubj = 0
replace probsubj = 1 if id == 93
replace probsubj = 1 if id == 99
replace probsubj = 1 if id == 141
replace probsubj = 1 if id == 147
replace probsubj = 1 if id == 193
replace probsubj = 1 if id == 60
replace probsubj = 1 if id == 68
tab probsubj

* Generating new physiological variables with corrected data
foreach var of varlist STIM_SCL* IST_SCL_* {
  gen ln_`var' = ln(`var')
}

* EDA responses to threatening images
gen threat_pic3_changeR = ln_STIM_SCL_MEAN3 - ln_IST_SCL_MEAN3
gen threat_pic6_changeR = ln_STIM_SCL_MEAN6 - ln_IST_SCL_MEAN6
gen threat_pic7_changeR = ln_STIM_SCL_MEAN7 - ln_IST_SCL_MEAN7
gen threat_pic8_changeR = ln_STIM_SCL_MEAN8 - ln_IST_SCL_MEAN8
gen threat_pic13_changeR = ln_STIM_SCL_MEAN13 - ln_IST_SCL_MEAN13
gen threat_pic18_changeR = ln_STIM_SCL_MEAN18 - ln_IST_SCL_MEAN18
egen threat_change_totalR = rmean(threat_pic3_changeR threat_pic6_changeR ///
  threat_pic7_changeR threat_pic8_changeR threat_pic13_changeR ///
  threat_pic18_changeR)
* EDA responses to disgusting images
gen disgust_pic1r = ln_STIM_SCL_MEAN1 - ln_IST_SCL_MEAN1
gen disgust_pic12r = ln_STIM_SCL_MEAN12 - ln_IST_SCL_MEAN12
gen disgust_pic16r = ln_STIM_SCL_MEAN16 - ln_IST_SCL_MEAN16
gen disgust_pic20r = ln_STIM_SCL_MEAN20 - ln_IST_SCL_MEAN20
gen disgust_pic21r = ln_STIM_SCL_MEAN21 - ln_IST_SCL_MEAN21
gen disgust_pic24r = ln_STIM_SCL_MEAN24 - ln_IST_SCL_MEAN24
egen disgust_change_totalR = rmean(disgust_pic1r disgust_pic12r ///
  disgust_pic16r disgust_pic20r disgust_pic21r disgust_pic24r) 
* EDA responses to neutral images
gen neutral_pic2r = ln_STIM_SCL_MEAN2 - ln_IST_SCL_MEAN2
gen neutral_pic9r = ln_STIM_SCL_MEAN9 - ln_IST_SCL_MEAN9
gen neutral_pic11r = ln_STIM_SCL_MEAN11 - ln_IST_SCL_MEAN11
gen neutral_pic14r = ln_STIM_SCL_MEAN14 - ln_IST_SCL_MEAN14
gen neutral_pic15r = ln_STIM_SCL_MEAN15 - ln_IST_SCL_MEAN15
gen neutral_pic19r = ln_STIM_SCL_MEAN19 - ln_IST_SCL_MEAN19
egen neutral_change_totalR = rmean(neutral_pic2r neutral_pic9r ///
  neutral_pic11r neutral_pic14r neutral_pic15r neutral_pic19r)
* EDA responses to positive images
gen pos_pic4r=ln_STIM_SCL_MEAN4-ln_IST_SCL_MEAN4
gen pos_pic5r=ln_STIM_SCL_MEAN5-ln_IST_SCL_MEAN5
gen pos_pic10r=ln_STIM_SCL_MEAN10-ln_IST_SCL_MEAN10
gen pos_pic17r=ln_STIM_SCL_MEAN17-ln_IST_SCL_MEAN17
gen pos_pic22r=ln_STIM_SCL_MEAN22-ln_IST_SCL_MEAN22
gen pos_pic23r=ln_STIM_SCL_MEAN23-ln_IST_SCL_MEAN23
egen pos_change_totalR = rmean(pos_pic4r pos_pic5r pos_pic10r pos_pic17r ///
  pos_pic22r pos_pic23r)

** variables for original analysis
gen ashtray_gsravg = .
foreach i of numlist 1/24 {
  replace ashtray_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image01Ashtray2"
}
gen basket_gsravg = .
foreach i of numlist 1/24 {
  replace basket_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image02Basket"
}
gen knife_gsravg = .
foreach i of numlist 1/24 {
  replace knife_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image03KnifeHeldByMan"
}
gen skysurfer_gsravg = .
foreach i of numlist 1/24 {
  replace skysurfer_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image04Skysurfer"
}
gen couple_gsravg = .
foreach i of numlist 1/24 {
  replace couple_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image05Couple3"
}
gen snake_gsravg = .
foreach i of numlist 1/24 {
  replace snake_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image06Snake2"
}
gen spider_gsravg = .
foreach i of numlist 1/24 {
  replace spider_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image07Spider"
}
gen gunattack_gsravg = .
foreach i of numlist 1/24 {
  replace gunattack_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image08GunAttack1"
}
gen umbrella_gsravg = .
foreach i of numlist 1/24 {
  replace umbrella_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image09Umbrella"
}
gen snowskier_gsravg = .
foreach i of numlist 1/24 {
  replace snowskier_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image10SnowSkier"
}
gen bowl_gsravg = .
foreach i of numlist 1/24 {
  replace bowl_gsravg = phys_pic`i'gsravg if picep_imgfile`i' == "Image11Bowl"
}
gen nastytoilet_gsravg = .
foreach i of numlist 1/24 {
  replace nastytoilet_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image12NastyToilet"
}
gen caraccident_gsravg = .
foreach i of numlist 1/24 {
  replace caraccident_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image13CarAccident1"
}
gen mug_gsravg = .
foreach i of numlist 1/24 {
  replace mug_gsravg = phys_pic`i'gsravg if picep_imgfile`i' == "Image14Mug"
}
gen rollingpin_gsravg = .
foreach i of numlist 1/24 {
  replace rollingpin_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image15RollingPin"
}
gen maggots_gsravg = .
foreach i of numlist 1/24 {
  replace maggots_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image16Maggots"
}
gen waterfall_gsravg = .
foreach i of numlist 1/24 {
  replace waterfall_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "image17Waterfall"
}
gen shipsinking_gsravg = .
foreach i of numlist 1/24 {
  replace shipsinking_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "image18ShipSinking1"
}
gen dustpan_gsravg = .
foreach i of numlist 1/24 {
  replace dustpan_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image19Dustpan"
}
gen worms_gsravg = .
foreach i of numlist 1/24 {
  replace worms_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image20WormsInMouth"
}
gen babytumor_gsravg = .
foreach i of numlist 1/24 {
  replace babytumor_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image21BabyWithTumor"
}
gen couple2_gsravg = .
foreach i of numlist 1/24 {
  replace couple2_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image22Couple2"
}
gen sailboat_gsravg = .
foreach i of numlist 1/24 {
  replace sailboat_gsravg = phys_pic`i'gsravg if ///
    picep_imgfile`i' == "Image23YellowSailboat"
}
gen vomit_gsravg = .
foreach i of numlist 1/24 {
  replace vomit_gsravg = phys_pic`i'gsravg if picep_imgfile`i' == "Image24Vomit"
}

gen ashtray_gsravgis = .
foreach i of numlist 1/24 {
  replace ashtray_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img01"
}
gen basket_gsravgis = .
foreach i of numlist 1/24 {
  replace basket_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img02"
}
gen knife_gsravgis = .
foreach i of numlist 1/24 {
  replace knife_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img03"
}
gen skysurfer_gsravgis = .
foreach i of numlist 1/24 {
  replace skysurfer_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img04"
}
gen couple3_gsravgis = .
foreach i of numlist 1/24 {
  replace couple3_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img05"
}
gen snake_gsravgis = .
foreach i of numlist 1/24 {
  replace snake_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img06"
}
gen spider_gsravgis = .
foreach i of numlist 1/24 {
  replace spider_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img07"
}
gen gunattack_gsravgis = .
foreach i of numlist 1/24 {
  replace gunattack_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img08"
}
gen umbrella_gsravgis = .
foreach i of numlist 1/24 {
  replace umbrella_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img09"
}
gen snowskier_gsravgis = .
foreach i of numlist 1/24 {
  replace snowskier_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img10"
}
gen bowl_gsravgis = .
foreach i of numlist 1/24 {
  replace bowl_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img11"
}
gen nastytoilet_gsravgis = .
foreach i of numlist 1/24 {
  replace nastytoilet_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img12"
}
gen caraccident_gsravgis = .
foreach i of numlist 1/24 {
  replace caraccident_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img13"
}
gen mug_gsravgis = .
foreach i of numlist 1/24 {
  replace mug_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img14"
}
gen rollingpin_gsravgis = .
foreach i of numlist 1/24 {
  replace rollingpin_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img15"
}
gen maggots_gsravgis = .
foreach i of numlist 1/24 {
  replace maggots_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img16"
}
gen waterfall_gsravgis = .
foreach i of numlist 1/24 {
  replace waterfall_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img17"
}
gen shipsinking_gsravgis = .
foreach i of numlist 1/24 {
  replace shipsinking_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img18"
}
gen dustpan_gsravgis = .
foreach i of numlist 1/24 {
  replace dustpan_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img19"
}
gen worms_gsravgis = .
foreach i of numlist 1/24 {
  replace worms_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img20"
}
gen babytumor_gsravgis = .
foreach i of numlist 1/24 {
  replace babytumor_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img21"
}
gen couple2_gsravgis = .
foreach i of numlist 1/24 {
  replace couple2_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img22"
}
gen yellowsailboat_gsravgis = .
foreach i of numlist 1/24 {
  replace yellowsailboat_gsravgis = phys_isi`i'gsravg if ///
    picep_imgid`i' == "img23"
}
gen vomit_gsravgis = .
foreach i of numlist 1/24 {
  replace vomit_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img24"
}

gen threat_pic3 = log(knife_gsravg)
gen threat_pic6 = log(snake_gsravg)
gen threat_pic7 = log(spider_gsravg)
gen threat_pic8 = log(gunattack_gsravg)
gen threat_pic13 = log(caraccident_gsravg)
gen threat_pic18 = log(shipsinking_gsravg)

gen disgust_pic1 = log(ashtray_gsravg)
gen disgust_pic12 = log(nastytoilet_gsravg)
gen disgust_pic16 = log(maggots_gsravg)
gen disgust_pic20 = log(worms_gsravg)
gen disgust_pic21 = log(babytumor_gsravg)
gen disgust_pic24 = log(vomit_gsravg)

gen neutral_pic2 = log(basket_gsravg)
gen neutral_pic9 = log(umbrella_gsravg)
gen neutral_pic11 = log(bowl_gsravg)
gen neutral_pic14 = log(mug_gsravg)
gen neutral_pic15 = log(rollingpin_gsravg)
gen neutral_pic19 = log(dustpan_gsravg)

gen pos_pic4 = log(skysurfer_gsravg)
gen pos_pic5 = log(couple_gsravg)
gen pos_pic10 = log(snowskier_gsravg)
gen pos_pic17 = log(waterfall_gsravg)
gen pos_pic22 = log(couple2_gsravg)
gen pos_pic23 = log(sailboat_gsravg)

gen threat_pic3_is = log(knife_gsravgis)
gen threat_pic6_is = log(snake_gsravgis)
gen threat_pic7_is = log(spider_gsravgis)
gen threat_pic8_is = log(gunattack_gsravgis)
gen threat_pic13_is = log(caraccident_gsravgis)
gen threat_pic18_is = log(shipsinking_gsravgis)

gen disgust_pic1_is = log(ashtray_gsravgis)
gen disgust_pic12_is = log(nastytoilet_gsravgis)
gen disgust_pic16_is = log(maggots_gsravgis)
gen disgust_pic20_is = log(worms_gsravgis)
gen disgust_pic21_is = log(babytumor_gsravgis)
gen disgust_pic24_is = log(vomit_gsravgis)

gen neutral_pic2_is = log(basket_gsravgis)
gen neutral_pic9_is = log(umbrella_gsravgis)
gen neutral_pic11_is = log(bowl_gsravgis)
gen neutral_pic14_is = log(mug_gsravgis)
gen neutral_pic15_is = log(rollingpin_gsravgis)
gen neutral_pic19_is = log(dustpan_gsravgis)

gen pos_pic4_is = log(skysurfer_gsravgis)
gen pos_pic5_is = log(couple3_gsravgis)
gen pos_pic10_is = log(snowskier_gsravgis)
gen pos_pic17_is = log(waterfall_gsravgis)
gen pos_pic22_is = log(couple2_gsravgis)
gen pos_pic23_is = log(yellowsailboat_gsravgis)

gen threat_pic3_change = (threat_pic3 - threat_pic3_is)
gen threat_pic6_change = (threat_pic6 - threat_pic6_is)
gen threat_pic7_change = (threat_pic7 - threat_pic7_is)
gen threat_pic8_change = (threat_pic8 - threat_pic8_is)
gen threat_pic13_change = (threat_pic13 - threat_pic13_is)
gen threat_pic18_change = (threat_pic18 - threat_pic18_is)

gen disgust_pic1_change = (disgust_pic1 - disgust_pic1_is)
gen disgust_pic12_change = (disgust_pic12 - disgust_pic12_is)
gen disgust_pic16_change = (disgust_pic16 - disgust_pic16_is)
gen disgust_pic20_change = (disgust_pic20 - disgust_pic20_is)
gen disgust_pic21_change = (disgust_pic21 - disgust_pic21_is)
gen disgust_pic24_change = (disgust_pic24 - disgust_pic24_is)

gen neutral_pic2_change = (neutral_pic2 - neutral_pic2_is)
gen neutral_pic9_change = (neutral_pic9 - neutral_pic9_is)
gen neutral_pic11_change = (neutral_pic11 - neutral_pic11_is)
gen neutral_pic14_change = (neutral_pic14 - neutral_pic14_is)
gen neutral_pic15_change = (neutral_pic15 - neutral_pic15_is)
gen neutral_pic19_change = (neutral_pic19 - neutral_pic19_is)

gen pos_pic4_change = (pos_pic4 - pos_pic4_is)
gen pos_pic5_change = (pos_pic5 - pos_pic5_is)
gen pos_pic10_change = (pos_pic10 - pos_pic10_is)
gen pos_pic17_change = (pos_pic17 - pos_pic17_is)
gen pos_pic22_change = (pos_pic22 - pos_pic22_is)
gen pos_pic23_change = (pos_pic23 - pos_pic23_is)

* Change in SCL for threatening images, mean change from all threatening images
egen threat_change_total = rmean(threat_pic3_change threat_pic6_change ///
  threat_pic7_change threat_pic8_change threat_pic13_change threat_pic18_change)
* Change in SCL for disgusting images, mean change from all disgusting images
egen disgust_change_total = rmean(disgust_pic1_change disgust_pic12_change ///
  disgust_pic16_change disgust_pic20_change disgust_pic21_change ///
  disgust_pic24_change)
* Change in SCL for disgusting + threatening images, mean change from all
* disgusting + threatening images
egen threat_disgust_change_toal = rmean(threat_pic3_change ///
  threat_pic6_change threat_pic7_change threat_pic8_change ///
  threat_pic13_change threat_pic18_change disgust_pic1_change ///
  disgust_pic12_change disgust_pic16_change disgust_pic20_change ///
  disgust_pic21_change disgust_pic24_change)
* Change in SCL for neutral images, mean change from all neutral images
egen neutral_change_total = rmean(neutral_pic2_change neutral_pic9_change ///
  neutral_pic11_change neutral_pic14_change neutral_pic15_change ///
  neutral_pic19_change)
* Change in SCL for positive images, mean change from all positive images
egen pos_change_total = rmean(pos_pic4_change pos_pic5_change ///
  pos_pic10_change pos_pic17_change pos_pic22_change pos_pic23_change)

foreach var in threat_change_totalR threat_change_total disgust_change_total ///
  disgust_change_totalR neutral_change_total neutral_change_totalR ///
  pos_change_total pos_change_totalR wp soccon ecocon leftright_alt {
  egen z_`var' = std(`var') if country == 1
}

* consider complete cases to be those for whom we have complete eda measures and
*   complete survey-based ideology measures
quietly reg z_threat_change_total z_disgust_change_total ///
  z_neutral_change_total z_pos_change_total z_wp z_soccon z_ecocon ///
  z_leftright_alt
gen complete = 1 if e(sample)

* outlier indicators
egen max_z_threat_change_total_us = max(z_threat_change_total) if ///
  complete == 1 & country == 0
egen max_z_disgust_change_total_dk = max(z_disgust_change_total) if ///
  complete == 1 & country == 1
gen outlier_us = 1 if z_threat_change_total == ///
  max_z_threat_change_total_us & max_z_threat_change_total_us != .
gen outlier_dk = 1 if z_disgust_change_total == ///
  max_z_disgust_change_total_dk & max_z_disgust_change_total_dk != .
drop max_z_threat_change_total_us max_z_disgust_change_total_dk

label var z_threat_change_total "Threatening Original"
label var z_threat_change_totalR "Threatening Replication"
label var z_disgust_change_total "Disgusting Original"
label var z_disgust_change_totalR "Disgusting Replication"
label var z_neutral_change_total "Neutral Original"
label var z_neutral_change_totalR "Neutral Replication"
label var z_pos_change_total "Positive Original"
label var z_pos_change_totalR "Positive Replication"

foreach var of varlist z_threat_change_total z_threat_change_totalR {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store a1_`var'
}
foreach var of varlist z_disgust_change_total z_disgust_change_totalR {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store b1_`var'
}
foreach var of varlist z_pos_change_total z_pos_change_totalR {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store c1_`var'
}
foreach var of varlist z_neutral_change_total z_neutral_change_totalR {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store d1_`var'
}
foreach var of varlist z_threat_change_total z_threat_change_totalR {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store a2_`var'
}
foreach var of varlist z_disgust_change_total z_disgust_change_totalR {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store b2_`var'
}
foreach var of varlist z_pos_change_total z_pos_change_totalR {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store c2_`var'
}
foreach var of varlist z_neutral_change_total z_neutral_change_totalR {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store d2_`var'
}
foreach var of varlist z_threat_change_total z_threat_change_totalR {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store a3_`var'
}
foreach var of varlist z_disgust_change_total z_disgust_change_totalR {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store b3_`var'
}
foreach var of varlist z_pos_change_total z_pos_change_totalR {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store c3_`var'
}
foreach var of varlist z_neutral_change_total z_neutral_change_totalR {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store d3_`var'
}
foreach var of varlist z_threat_change_total z_threat_change_totalR {
  quietly reg z_leftright_alt `var' female age education income_hh country ///
    if outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store a4_`var'
}
foreach var of varlist z_disgust_change_total z_disgust_change_totalR {
  quietly reg z_leftright_alt `var' female age education income_hh country ///
    if outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store b4_`var'
}
foreach var of varlist z_pos_change_total z_pos_change_totalR {
  quietly reg z_leftright_alt `var' female age education income_hh country ///
    if outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store c4_`var'
}
foreach var of varlist z_neutral_change_total z_neutral_change_totalR {
  quietly reg z_leftright_alt `var' female age education income_hh country ///
    if outlier_us != 1 & country == 1 & probsubj == 0, robust
  estimates store d4_`var'
}

**************
** Figure 5D.a
**************
coefplot a1* b1* c1* d1*, bylabel(Wilson-Patterson) ///
  || a2* b2* c2* d2*, bylabel(Social Conservatism) ///
  || a3* b3* c3* d3*, bylabel(Economic Conservatism) ///
  || a4* b4* c4* d4*, bylabel(Left-Right Self-placement) ///
  ||, drop(_cons female age education income_hh country) ///
  xline(0, lcolor(gray)) msymbol(square) color(black) ciopts(color(black)) ///
  byopts(legend(off)) headings(z_threat_change_total = "{bf:Threat EDA}" ///
  z_disgust_change_total = "{bf:Disgust EDA}" ///
  z_pos_change_total = "{bf:Positive EDA}" ///
  z_neutral_change_total = "{bf:Neutral EDA}",) format(%9.1g) mlabel ///
  mlabposition(10) mlabsize(1.5) grid(none)
graph export Figures/OnlineAppendix-Figure05Da.pdf, replace
graph drop _all
graph close

estimates drop a* b* c* d*
**************


********************************************************************************
********************************************************************************
** Online Appendix 5E
********************************************************************************
********************************************************************************

clear
use Data/OsmundsenEtAl-PsychophysiologyIdeology-JOP-Recodes.dta, clear

* consider complete cases to be those for whom we have complete eda measures and
*   complete survey-based ideology measures
quietly reg z_threat_change_total z_disgust_change_total ///
  z_neutral_change_total z_pos_change_total z_wp z_soccon z_ecocon ///
  z_leftright_alt
gen complete = 1 if e(sample)

* outlier indicators
egen max_z_threat_change_total_us = max(z_threat_change_total) if ///
  complete == 1 & country == 0
egen max_z_disgust_change_total_dk = max(z_disgust_change_total) if ///
  complete == 1 & country == 1
gen outlier_us = 1 if z_threat_change_total == ///
  max_z_threat_change_total_us & max_z_threat_change_total_us != .
gen outlier_dk = 1 if z_disgust_change_total == ///
  max_z_disgust_change_total_dk & max_z_disgust_change_total_dk != .
drop max_z_threat_change_total_us max_z_disgust_change_total_dk


* Calculate mean EDA response, threatening images
egen threat_total = rmean(threat_pic3 threat_pic6 threat_pic7 threat_pic8 ///
  threat_pic13 threat_pic18)
* Calculate mean EDA response, threatening images
egen disgust_total = rmean(disgust_pic1 disgust_pic12 disgust_pic16 ///
  disgust_pic20 disgust_pic21 disgust_pic24)
* Calculate mean EDA response, threatening images
egen neutral_total = rmean(neutral_pic2 neutral_pic9 neutral_pic11 ///
  neutral_pic14 neutral_pic15 neutral_pic19)
* Calculate meanEDA response, threatening images
egen pos_total = rmean(pos_pic4 pos_pic5 pos_pic10 pos_pic17 pos_pic22 ///
  pos_pic23)

* EDA responses relative to neutral reactions
gen threat_minus_neutral = threat_total - neutral_total
gen disgust_minus_neutral = disgust_total - neutral_total
gen positive_minus_neutral = pos_total - neutral_total

foreach var in threat_minus_neutral disgust_minus_neutral ///
  positive_minus_neutral {
  egen z_`var' = std(`var')
}

gen threatN_dk = z_threat_minus_neutral if country == 1
label var threatN_dk "Danes"
gen threatN_us = z_threat_minus_neutral if country == 0
label var threatN_us "Americans"
gen threatN_total = z_threat_minus_neutral
label var threatN_total "Combined"

gen disgustN_dk = z_disgust_minus_neutral if country == 1
label var disgustN_dk "Danes"
gen disgustN_us = z_disgust_minus_neutral if country == 0
label var disgustN_us "Americans"
gen disgustN_total = z_disgust_minus_neutral
label var disgustN_total "Combined"

gen positiveN_dk = z_positive_minus_neutral if country == 1
label var positiveN_dk "Danes"
gen positiveN_us = z_positive_minus_neutral if country == 0
label var positiveN_us "Americans"
gen positiveN_total = z_positive_minus_neutral
label var positiveN_total "Combined"

**************
** Figure 5E.a
**************
foreach var of varlist threatN_dk threatN_us threatN_total {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store a1_`var'
}
foreach var of varlist disgustN_dk disgustN_us disgustN_total {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store b1_`var'
}
foreach var of varlist positiveN_dk positiveN_us positiveN_total {
  quietly reg z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store c1_`var'
}
foreach var of varlist threatN_dk threatN_us threatN_total {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store a2_`var'
}
foreach var of varlist disgustN_dk disgustN_us disgustN_total {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store b2_`var'
}
foreach var of varlist positiveN_dk positiveN_us positiveN_total {
  quietly reg z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store c2_`var'
}
foreach var of varlist threatN_dk threatN_us threatN_total {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store a3_`var'
}
foreach var of varlist disgustN_dk disgustN_us disgustN_total {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store b3_`var'
}
foreach var of varlist positiveN_dk positiveN_us positiveN_total {
  quietly reg z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store c3_`var'
}
foreach var of varlist threatN_dk threatN_us threatN_total {
  quietly reg z_leftright_alt `var' female age education income_hh country ///
    if outlier_us != 1 , robust
  estimates store a4_`var'
}
foreach var of varlist disgustN_dk disgustN_us disgustN_total {
  quietly reg z_leftright_alt `var' female age education income_hh country ///
    if outlier_us != 1, robust
  estimates store b4_`var'
}
foreach var of varlist positiveN_dk positiveN_us positiveN_total {
  quietly reg z_leftright_alt `var' female age education income_hh country ///
    if outlier_us != 1 , robust
  estimates store c4_`var'
}

coefplot a1* b1* c1*, bylabel(Wilson-Patterson) ///
  || a2* b2* c2*, bylabel(Social Conservatism) ///
  || a3* b3* c3*, bylabel(Economic Conservatism) ///
  || a4* b4* c4*, bylabel(Left-Right Self-placement) ///
  ||, drop(_cons female age education income_hh country) ///
  xline(0, lcolor(gray)) msymbol(square) color(black) ciopts(lcolor(black)) ///
  byopts(legend(off)) headings(threatN_dk="{bf:Threat SCL}" ///
  disgustN_dk="{bf:Disgust SCL}" positiveN_dk="{bf:Positive SCL}", ) ///
  format(%9.1g) mlabel mlabposition(10) mlabsize(1.5) grid(none) ///
  subtitle(, size(small)) xsize(7) ysize(10)
graph export Figures/OnlineAppendix-Figure05Ea.pdf, replace
graph drop _all
graph close

estimates drop *
**************


********************************************************************************
********************************************************************************
** Online Appendix 5F
********************************************************************************
********************************************************************************

* read in recoded data file
clear
use Data/OsmundsenEtAl-PsychophysiologyIdeology-JOP-Recodes.dta, clear

* consider complete cases to be those for whom we have complete eda measures and
*   complete survey-based ideology measures
quietly reg z_threat_change_total z_disgust_change_total ///
  z_neutral_change_total z_pos_change_total z_wp z_soccon z_ecocon ///
  z_leftright_alt
gen complete = 1 if e(sample)

* outlier indicators
egen max_z_threat_change_total_us = max(z_threat_change_total) if ///
  complete == 1 & country == 0
egen max_z_disgust_change_total_dk = max(z_disgust_change_total) if ///
  complete == 1 & country == 1
gen outlier_us = 1 if z_threat_change_total == ///
  max_z_threat_change_total_us & max_z_threat_change_total_us != .
gen outlier_dk = 1 if z_disgust_change_total == ///
  max_z_disgust_change_total_dk & max_z_disgust_change_total_dk != .
drop max_z_threat_change_total_us max_z_disgust_change_total_dk

*standardize three alternative dependent variables
foreach var in  swb rwa sdo  {
egen z_`var' = std(`var') 
}

**************
** Figure 5F.a
**************
gen threat_dk = z_threat_change_total if country == 1
label var threat_dk "Danes"
gen threat_us = z_threat_change_total if country == 0
label var threat_us "Americans"
gen threat_total = z_threat_change_total
label var threat_total "Combined"

gen disgust_dk = z_disgust_change_total if country == 1
label var disgust_dk "Danes"
gen disgust_us = z_disgust_change_total if country == 0
label var disgust_us "Americans"
gen disgust_total = z_disgust_change_total
label var disgust_total "Combined"

gen positive_dk = z_pos_change_total if country == 1
label var positive_dk "Danes"
gen positive_us = z_pos_change_total if country == 0
label var positive_us "Americans"
gen positive_total = z_pos_change_total
label var positive_total "Combined"

gen neutral_dk = z_neutral_change_total if country == 1
label var neutral_dk "Danes"
gen neutral_us = z_neutral_change_total if country == 0
label var neutral_us "Americans"
gen neutral_total = z_neutral_change_total
label var neutral_total "Combined"


foreach var of varlist threat_dk threat_us threat_total {
  quietly reg z_swb `var'  female age education income_hh country if ///
    outlier_us != 1 , robust
  estimates store a1_`var'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly reg z_swb `var' female age education income_hh country if ///
    outlier_us != 1 , robust
  estimates store b1_`var'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly reg z_swb `var' female age education income_hh country if ///
    outlier_us != 1 , robust
  estimates store c1_`var'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly reg z_swb `var' female age education income_hh country if ///
    outlier_us != 1 , robust
  estimates store d1_`var'
}
foreach var of varlist threat_dk threat_us threat_total {
  quietly reg z_rwa `var' female age education income_hh country if ///
    outlier_us != 1 , robust
  estimates store a2_`var'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly reg z_rwa `var' female age education income_hh country if ///
    outlier_us != 1 , robust
  estimates store b2_`var'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly reg z_rwa `var' female age education income_hh country if ///
    outlier_us != 1 , robust
  estimates store c2_`var'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly reg z_rwa `var' female age education income_hh country if ///
    outlier_us != 1  , robust
  estimates store d2_`var'
}
foreach var of varlist threat_dk threat_us threat_total {
  quietly reg z_sdo `var'  female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store a3_`var'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly reg z_sdo `var' female age education income_hh country if ///
    outlier_us != 1 , robust
  estimates store b3_`var'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly reg z_sdo `var' female age education income_hh country if ///
    outlier_us != 1 , robust
  estimates store c3_`var'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly reg z_sdo `var' female age education income_hh country if ///
    outlier_us != 1  , robust
  estimates store d3_`var'
}

coefplot a1* b1* c1* d1*, bylabel(Society Works Best) ///
  || a2* b2* c2* d2*, bylabel(Social Dominance Orien.) ///
  || a3* b3* c3* d3*, bylabel(Right-Wing Auth.) ///
  ||, drop(_cons female age education income_hh country) ///
  xline(0, lcolor(gray)) msymbol(square) color(black) ciopts(lcolor(black)) ///
  byopts(legend(off)) headings(threat_dk = "{bf:Threatening SCL}" ///
  disgust_dk = "{bf:Disgusting SCL}" positive_dk = "{bf:Positive SCL}" ///
  neutral_dk = "{bf:Neutral SCL}", ) format(%9.1g) mlabel mlabposition(10) ///
  mlabsize(1.5) grid(none) subtitle(, size(small)) xsize(7) ysize(10)
graph export Figures/OnlineAppendix-Figure05Fa.pdf, replace
graph drop _all
graph close

estimates drop *
**************

********************************************************************************
********************************************************************************
** Online Appendix 5G
********************************************************************************
********************************************************************************

* read in recoded data file
clear
use Data/OsmundsenEtAl-PsychophysiologyIdeology-JOP-Recodes.dta, clear

* consider complete cases to be those for whom we have complete eda measures and
*   complete survey-based ideology measures
quietly reg z_threat_change_total z_disgust_change_total ///
  z_neutral_change_total z_pos_change_total z_wp z_soccon z_ecocon ///
  z_leftright_alt
gen complete = 1 if e(sample)

* outlier indicators
egen max_z_threat_change_total_us = max(z_threat_change_total) if ///
  complete == 1 & country == 0
egen max_z_disgust_change_total_dk = max(z_disgust_change_total) if ///
  complete == 1 & country == 1
gen outlier_us = 1 if z_threat_change_total == ///
  max_z_threat_change_total_us & max_z_threat_change_total_us != .
gen outlier_dk = 1 if z_disgust_change_total == ///
  max_z_disgust_change_total_dk & max_z_disgust_change_total_dk != .
drop max_z_threat_change_total_us max_z_disgust_change_total_dk

* rename physiological vatriables for recoding
rename phys_pic*gsravg phys_picgsravg*
rename phys_isi*gsravg phys_isigsravg*

* generate variables for the four image categories
forvalues i = 1/24 {
  gen categ`i' = .
}

* replace missing values with values indicating image category: 1 = threat,
* 2 = disgust, 3 = neutral, 4 = positive
forvalues i = 1/24 {
  replace categ`i' = 2 if picep_imgfile`i' == "Image01Ashtray2"
  replace categ`i' = 3 if picep_imgfile`i' == "Image02Basket"
  replace categ`i' = 1 if picep_imgfile`i' == "Image03KnifeHeldByMan"
  replace categ`i' = 4 if picep_imgfile`i' == "Image04Skysurfer"
  replace categ`i' = 4 if picep_imgfile`i' == "Image05Couple3"
  replace categ`i' = 1 if picep_imgfile`i' == "Image06Snake2"
  replace categ`i' = 1 if picep_imgfile`i' == "Image07Spider"
  replace categ`i' = 1 if picep_imgfile`i' == "Image08GunAttack1"
  replace categ`i' = 3 if picep_imgfile`i' == "Image09Umbrella"
  replace categ`i' = 4 if picep_imgfile`i' == "Image10SnowSkier"
  replace categ`i' = 3 if picep_imgfile`i' == "Image11Bowl"
  replace categ`i' = 2 if picep_imgfile`i' == "Image12NastyToilet"
  replace categ`i' = 1 if picep_imgfile`i' == "Image13CarAccident1"
  replace categ`i' = 3 if picep_imgfile`i' == "Image14Mug"
  replace categ`i' = 3 if picep_imgfile`i' == "Image15RollingPin"
  replace categ`i' = 2 if picep_imgfile`i' == "Image16Maggots"
  replace categ`i' = 4 if picep_imgfile`i' == "image17Waterfall"
  replace categ`i' = 1 if picep_imgfile`i' == "image18ShipSinking1"
  replace categ`i' = 3 if picep_imgfile`i' == "Image19Dustpan"
  replace categ`i' = 2 if picep_imgfile`i' == "Image20WormsInMouth"
  replace categ`i' = 2 if picep_imgfile`i' == "Image21BabyWithTumor"
  replace categ`i' = 4 if picep_imgfile`i' == "Image22Couple2"
  replace categ`i' = 4 if picep_imgfile`i' == "Image23YellowSailboat"
  replace categ`i' = 2 if picep_imgfile`i' == "Image24Vomit"
}

preserve

* reshape data from wide to long format 
reshape long categ picep_imgfile phys_picgsravg phys_isigsravg, i(id) j(pic)

* generate image indicator variables 
gen picno = .
replace picno = 1 if picep_imgfile == "Image01Ashtray2"
replace picno = 2 if picep_imgfile == "Image02Basket"
replace picno = 3 if picep_imgfile == "Image03KnifeHeldByMan"
replace picno = 4 if picep_imgfile == "Image04Skysurfer"
replace picno = 5 if picep_imgfile == "Image05Couple3"
replace picno = 6 if picep_imgfile == "Image06Snake2"
replace picno = 7 if picep_imgfile == "Image07Spider"
replace picno = 8 if picep_imgfile == "Image08GunAttack1"
replace picno = 9 if picep_imgfile == "Image09Umbrella"
replace picno = 10 if picep_imgfile == "Image10SnowSkier"
replace picno = 11 if picep_imgfile == "Image11Bowl"
replace picno = 12 if picep_imgfile == "Image12NastyToilet"
replace picno = 13 if picep_imgfile == "Image13CarAccident1"
replace picno = 14 if picep_imgfile == "Image14Mug"
replace picno = 15 if picep_imgfile == "image17Waterfall"
replace picno = 16 if picep_imgfile == "image18ShipSinking1"
replace picno = 17 if picep_imgfile == "Image15RollingPin"
replace picno = 18 if picep_imgfile == "Image16Maggots"
replace picno = 19 if picep_imgfile == "Image19Dustpan"
replace picno = 20 if picep_imgfile == "Image20WormsInMouth"
replace picno = 21 if picep_imgfile == "Image21BabyWithTumor"
replace picno = 22 if picep_imgfile == "Image22Couple2"
replace picno = 23 if picep_imgfile == "Image23YellowSailboat"
replace picno = 24 if picep_imgfile == "Image24Vomit"

* generate measure of physiological reactions to images: log-and-subtract method
generate logEDAs = ln(phys_picgsravg) 
generate logEDAis = ln(phys_isigsravg)
generate logEDA = logEDAs - logEDAis

* standardize variables 
foreach var in logEDA {
  egen z_`var' = std(`var')
}

* Run models separately in Denmark and the United States 
xtset id

*************
** Table 5G.a
*************
xtreg z_logEDA i.categ##c.z_wp i.pic female age education income_hh if ///
  outlier_us != 1 & country == 0, re
outreg2 using Tables/OnlineAppendixTable05Ga, excel aster(se) dec(3) label ///
  title("Table 5G.a Correlation between EDA and political ideology. Random effects models. United States.") replace
xtreg z_logEDA i.categ##c.z_soccon i.pic female age education income_hh if ///
  outlier_us != 1 & country == 0, re
outreg2 using Tables/OnlineAppendixTable05Ga, excel aster(se) dec(3) label ///
  append
xtreg z_logEDA i.categ##c.z_ecocon i.pic female age education income_hh if ///
  outlier_us != 1 & country == 0, re
outreg2 using Tables/OnlineAppendixTable05Ga, excel aster(se) dec(3) label ///
  append
xtreg z_logEDA i.categ##c.z_leftright_alt i.pic female age education ///
  income_hh if outlier_us != 1 & country == 0, re
outreg2 using Tables/OnlineAppendixTable05Ga, excel aster(se) dec(3) label ///
  append
*************

*************
** Table 5G.b
*************
xtreg z_logEDA i.categ##c.z_wp i.pic female age education income_hh if ///
  country == 1, re
outreg2 using Tables/OnlineAppendixTable05Gb, excel aster(se) dec(3) label ///
  title("Table 5G.b Correlation between EDA and political ideology. Random effects models. Denmark.") replace
xtreg z_logEDA i.categ##c.z_soccon i.pic female age education income_hh if ///
  country == 1, re
outreg2 using Tables/OnlineAppendixTable05Gb, excel aster(se) dec(3) label ///
  append
xtreg z_logEDA i.categ##c.z_ecocon i.pic female age education income_hh if ///
  country == 1, re
outreg2 using Tables/OnlineAppendixTable05Gb, excel aster(se) dec(3) label ///
  append
xtreg z_logEDA i.categ##c.z_leftright_alt i.pic female age education ///
  income_hh if country == 1, re
outreg2 using Tables/OnlineAppendixTable05Gb, excel aster(se) dec(3) label ///
  append
*************



********************************************************************************
clear

* close log
*log close


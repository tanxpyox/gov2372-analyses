********************************************************************************
********************************************************************************
****
**** File 1 of the replication archive for:
****   Osmundsen, Mathias, David J. Hendry, Lasse Laustsen, Kevin B. Smith, and
****     Michael Bang Petersen. 2020. "The Psychophysiology of Political
****     Ideology: Replications, Reanalysis, and Recommendations." Journal of
****     Politics XX(X): XXX-XXX.
****
**** Note: This file reads in the raw data and produces recodes for self-
**** reported survey measures collected in the Danish and American lab studies.
****
********************************************************************************
********************************************************************************

* uncomment the following line and the closing line to create a log file for the
*   current run
*log using Logs/01_OsmundsenEtAl-PsychophysiologyIdeology-JOP-Log.txt, replace

* change the working directory to the location of this .do file (if necessary)
* cd

* read in raw data file
clear
use Data/OsmundsenEtAl-PsychophysiologyIdeology-JOP-Original.dta, clear

**** recode variables

*** meta data

** country dummy
gen country = .
replace country = 1 if id < 500
replace country = 0 if id > 500
label var country "1 = DK; 0 = US"

*** survey demographics

** gender
* Denmark
recode bkgrnd_gender (1 = 1) (2 = 0), gen(female_dk)
* U.S.
recode presvy_q63 (1=0) (2=1), gen(female_us)
* combined
gen female = .
replace female = female_dk
replace female = female_us if female_us != .
label var female "Female (Yes = 1)"
drop female_dk female_us

** age
* Denmark
gen age_dk = bkgrnd_age
* U.S.	
gen age_us = presvy_q62
* combined
gen age = .
replace age = age_dk
replace age = age_us if age_us != .
label var age "Age (Years)"
drop age_dk age_us

** education
* Denmark
recode bkgrnd_udd1 (4 = 2) (2 = 3)
egen education_dk_min = min(bkgrnd_udd1)
egen education_dk_max = max(bkgrnd_udd1)
gen education_dk = ///
  (bkgrnd_udd1 - education_dk_min) / (education_dk_max - education_dk_min)
drop education_dk_min education_dk_max
* U.S.
recode presvy_q65 (2 = 1) (6 = 2) (7 = 4) (4 = 5), gen(education_us_temp)
egen education_us_temp_min = min(education_us_temp)
egen education_us_temp_max = max(education_us_temp)
gen education_us = (education_us_temp - education_us_temp_min) / ///
  (education_us_temp_max - education_us_temp_min)
drop education_us_temp education_us_temp_min education_us_temp_max
* combined
gen education = education_dk
replace education = education_us if education_us != .
label var education "Education Level (0 = Lowest; 1 = Highest)"
drop education_dk education_us

** income 
* U.S.
egen income_hh_us_min = min(presvy_q64)
egen income_hh_us_max = max(presvy_q64)
gen income_hh_us = ///
  (presvy_q64 - income_hh_us_min) / (income_hh_us_max - income_hh_us_min)
drop income_hh_us_min income_hh_us_max
* Denmark
recode bkgrnd_household_income (12 13 = .), gen(income_hh_dk_temp)
egen income_hh_dk_min = min(income_hh_dk_temp)
egen income_hh_dk_max = max(income_hh_dk_temp)
gen income_hh_dk = ///
  (income_hh_dk_temp - income_hh_dk_min) / (income_hh_dk_max - income_hh_dk_min)
drop income_hh_dk_min income_hh_dk_max income_hh_dk_temp
recode bkgrnd_personal_income (12 13 = .), gen(income_per_dk_temp)
egen income_per_dk_min = min(income_per_dk_temp)
egen income_per_dk_max = max(income_per_dk_temp)
gen income_per_dk = (income_per_dk_temp - income_per_dk_min) / ///
  (income_per_dk_max - income_per_dk_min)
drop income_per_dk_min income_per_dk_max income_per_dk_temp
label var income_per_dk "Personal Income (0: Lowest; 1: Highest)"
* combined
gen income_hh = income_hh_us
replace income_hh = income_hh_dk if income_hh_dk != .
label var income_hh "Household Income (0: Lowest; 1: Highest)"
drop income_hh_us income_hh_dk income_per_dk

*** survey ideological self reports

** ideological left-right self placement
* Denmark
egen leftright_min = min(bkgrnd_q8a)
egen leftright_max = max(bkgrnd_q8a)
gen leftright_dk = ///
  (bkgrnd_q8a - leftright_min) / (leftright_max - leftright_min)
drop leftright_max leftright_min
* U.S.
egen leftright_min = min(presvy_q56)
egen leftright_max = max(presvy_q56)
gen leftright_us = ///
  (presvy_q56 - leftright_min) / (leftright_max - leftright_min)
drop leftright_max leftright_min
* combined
gen leftright = leftright_dk
replace leftright = leftright_us if leftright_us != .
label var leftright "Left-Right Self-Placement (0-1)"
* measure that first aggregates the Danish 11-point scale into a 5-point scale,
* then combines the two
recode bkgrnd_q8a (0 = 1) (3 = 2) (4 5 6 = 3) (7 8 = 4) (9 10 = 5), ///
  gen(leftright_alt)
replace leftright_alt = leftright_us if leftright_us != .
label var leftright_alt "Left-Right Self-Placement (0-1)"
drop leftright_dk leftright_us

** society works best when.... (higher values are more conservative)
forvalues i = 32/45 {
  recode presvy_q`i' (2 = 0), gen(presvy_temp_q`i')
}
alpha presvy_temp_q32-presvy_temp_q45, asis gen(swb_temp)
egen swb_temp_min = min(swb_temp)
egen swb_temp_max = max(swb_temp)
gen swb = (swb_temp - swb_temp_min) / (swb_temp_max - swb_temp_min)
drop swb_temp swb_temp_max swb_temp_min presvy_temp_q32-presvy_temp_q45
label var swb "Society Works Best When... (0-1)"

** social conservatism (higher values are more conservative)
destring presvy_q46*, force replace
forvalues i = 1/5 {
  recode presvy_q46_`i' (6 = .), gen(presvy_temp_q46_`i')
}
alpha presvy_temp_q46_1-presvy_temp_q46_5, asis gen(soccon_temp)
egen soccon_temp_min = min(soccon_temp)
egen soccon_temp_max = max(soccon_temp)
gen soccon = ///
  (soccon_temp - soccon_temp_min) / (soccon_temp_max - soccon_temp_min)
*drop soccon_temp soccon_temp_max soccon_temp_min ///
*  presvy_temp_q46_1-presvy_temp_q46_5
label var soccon "Social Conservatism (0-1)"

** economic conservatism (higher values are more conservative)
forvalues i = 1/3 {
  recode presvy_q47_`i' (6 = .) (5 = 1) (4 = 2) (2 = 4) (1 = 5), ///
    gen(presvy_q47_`i'_rev_temp)
}
forvalues i = 4/5 {
  recode presvy_q47_`i' (6 = .), gen(presvy_q47_`i'_temp)
}
alpha presvy_q47_1_rev_temp presvy_q47_2_rev_temp presvy_q47_3_rev_temp ///
  presvy_q47_4_temp presvy_q47_5_temp, asis gen(ecocon_temp)
egen ecocon_temp_min = min(ecocon_temp)
egen ecocon_temp_max = max(ecocon_temp)
gen ecocon = (ecocon_temp - ecocon_temp_min) / (ecocon_temp_max - ///
  ecocon_temp_min)
*drop ecocon_temp ecocon_temp_max ecocon_temp_min presvy_q47_1_rev_temp ///
*  presvy_q47_2_rev_temp presvy_q47_3_rev_temp presvy_q47_4_temp ///
*  presvy_q47_5_temp
label var ecocon "Economic Conservatism (0-1)"
	
** wilson-patterson scale (higher values are more conservative)
foreach i in 1 2 4 10 12 13 15 16 17 19 20 {
  recode presvy_q48_`i' (5 = 1) (4 = 2) (2 = 4) (1 = 5), ///
  gen(presvy_q48_`i'_temp)
}
alpha presvy_q48_1_temp presvy_q48_2_temp presvy_q48_3 presvy_q48_4_temp ///
  presvy_q48_5 presvy_q48_6 presvy_q48_7 presvy_q48_8 presvy_q48_9 ///
  presvy_q48_10_temp presvy_q48_11 presvy_q48_12_temp presvy_q48_13_temp ///
  presvy_q48_14 presvy_q48_15_temp presvy_q48_16_temp presvy_q48_17_temp ///
  presvy_q48_18 presvy_q48_19_temp presvy_q48_20_temp, asis gen(wp_temp)
egen wp_temp_min = min(wp_temp)
egen wp_temp_max = max(wp_temp)
gen wp = (wp_temp - wp_temp_min) / (wp_temp_max - wp_temp_min)
*drop wp_temp wp_temp_max wp_temp_min presvy_q48_1_temp presvy_q48_2_temp ///
*  presvy_q48_3 presvy_q48_4_temp presvy_q48_5 presvy_q48_6 presvy_q48_7 ///
*  presvy_q48_8 presvy_q48_9 presvy_q48_10_temp presvy_q48_11 ///
*  presvy_q48_12_temp presvy_q48_13_temp presvy_q48_14 presvy_q48_15_temp ///
*  presvy_q48_16_temp presvy_q48_17_temp presvy_q48_18 presvy_q48_19_temp ///
*  presvy_q48_20_temp
label var wp "Wilson-Patterson Scale (0-1)"
	
** rwa (higher values are more conservative)
* Denmark
foreach i in 2 6 7 8 {
  recode bkgrnd_q10_`i' (1 = 7) (2 = 6) (3 = 5) (5 = 3) (6 = 2) (7 = 1), ///
  gen(bkgrnd_q10_`i'_dk_temp)
}
alpha bkgrnd_q10_1 bkgrnd_q10_2_dk_temp bkgrnd_q10_3 bkgrnd_q10_4 ///
  bkgrnd_q10_5 bkgrnd_q10_6_dk_temp bkgrnd_q10_7_dk_temp ///
  bkgrnd_q10_8_dk_temp, asis gen(rwa_dk_temp)
egen rwa_dk_min = min(rwa_dk_temp)
egen rwa_dk_max = max(rwa_dk_temp)
gen rwa_dk = (rwa_dk_temp - rwa_dk_min) / (rwa_dk_max - rwa_dk_min)
drop rwa_dk_temp rwa_dk_min rwa_dk_max bkgrnd_q10_2_dk_temp ///
  bkgrnd_q10_6_dk_temp bkgrnd_q10_7_dk_temp bkgrnd_q10_8_dk_temp
* U.S.	
foreach i in 1 2 10 {
  recode presvy_q58_`i' (7 = 1) (6 = 2) (5 = 3) (3 = 5) (2 = 6) (1 = 7), ///
  gen(presvy_q58_`i'_us_temp)
}
alpha presvy_q58_1_us_temp presvy_q58_2_us_temp presvy_q58_3 ///
  presvy_q58_4 presvy_q58_7 presvy_q58_9 presvy_q58_10_us_temp, ///
  asis gen(rwa_us_temp)
egen rwa_us_min = min(rwa_us_temp)
egen rwa_us_max = max(rwa_us_temp)
gen rwa_us = (rwa_us_temp - rwa_us_min) / (rwa_us_max - rwa_us_min)
drop rwa_us_temp rwa_us_min rwa_us_max presvy_q58_1_us_temp ///
  presvy_q58_2_us_temp presvy_q58_10_us_temp
* combined
gen rwa = rwa_us
replace rwa = rwa_dk if rwa_dk != .
label var rwa "Right-Wing Authoritarianism (0-1)"
drop rwa_dk rwa_us
	
** social dominance orientation (higher values are more conservative)
* Denmark
foreach i in 3 4 7 8 {
  recode bkgrnd_q9_`i' (1 = 7) (2 = 6) (3 = 5) (5 = 3) (6 = 2) (7 = 1), ///
  gen(bkgrnd_q9_`i'_rev)
}
alpha bkgrnd_q9_1 bkgrnd_q9_2 bkgrnd_q9_3_rev bkgrnd_q9_4_rev bkgrnd_q9_5 ///
  bkgrnd_q9_6 bkgrnd_q9_7_rev bkgrnd_q9_8_rev, asis gen(sdo_temp)
egen sdo_temp_min = min(sdo_temp)
egen sdo_temp_max = max(sdo_temp)
gen sdo_dk = (sdo_temp - sdo_temp_min) / (sdo_temp_max - sdo_temp_min)
drop sdo_temp sdo_temp_max sdo_temp_min bkgrnd_q9_3_rev bkgrnd_q9_4_rev ///
  bkgrnd_q9_7_rev bkgrnd_q9_8_rev
* U.S.
foreach i in 3 4 7 8 {
  recode presvy_q57_`i' (1 = 7) (2 = 6) (3 = 5) (5 = 3) (6 = 2) (7 = 1), ///
  gen(presvy_q57_`i'_rev)
}
alpha presvy_q57_1 presvy_q57_2 presvy_q57_3_rev presvy_q57_4_rev ///
  presvy_q57_5 presvy_q57_6 presvy_q57_7_rev presvy_q57_8_rev, asis ///
  gen(sdo_temp)
egen sdo_temp_min = min(sdo_temp)
egen sdo_temp_max = max(sdo_temp)
gen sdo_us = (sdo_temp - sdo_temp_min) / (sdo_temp_max - sdo_temp_min)
drop sdo_temp sdo_temp_max sdo_temp_min presvy_q57_3_rev presvy_q57_4_rev ///
  presvy_q57_7_rev presvy_q57_8_rev
* combined
gen sdo = sdo_us
replace sdo = sdo_dk if sdo_dk != .
label var sdo "Social Dominance Orientation (0-1)"
drop sdo_dk sdo_us

*** stimulus measures

** recoding stimulus intervals
gen ashtray_gsravg = .
foreach i of numlist 1/24 {  
  replace ashtray_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image01Ashtray2"
}
gen basket_gsravg = .
foreach i of numlist 1/24 {  
  replace basket_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image02Basket"
}
gen knife_gsravg = .
foreach i of numlist 1/24 {  
  replace knife_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image03KnifeHeldByMan"
}
gen skysurfer_gsravg = .
foreach i of numlist 1/24 {  
  replace skysurfer_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image04Skysurfer"
}
gen couple_gsravg = .
foreach i of numlist 1/24 {  
  replace couple_gsravg = phys_pic`i'gsravg ///
     if picep_imgfile`i' == "Image05Couple3"
}
gen snake_gsravg = .
foreach i of numlist 1/24 {  
  replace snake_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image06Snake2"
}
gen spider_gsravg = .
foreach i of numlist 1/24 {  
  replace spider_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image07Spider"
}
gen gunattack_gsravg = .
foreach i of numlist 1/24 {  
  replace gunattack_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image08GunAttack1"
}
gen umbrella_gsravg = .
foreach i of numlist 1/24 {  
  replace umbrella_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image09Umbrella"
}
gen snowskier_gsravg = .
foreach i of numlist 1/24 {  
  replace snowskier_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image10SnowSkier"
}
gen bowl_gsravg = .
foreach i of numlist 1/24 {  
  replace bowl_gsravg = phys_pic`i'gsravg if picep_imgfile`i' == "Image11Bowl"
}
gen nastytoilet_gsravg = .
foreach i of numlist 1/24 {  
  replace nastytoilet_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image12NastyToilet"
}
gen caraccident_gsravg = .
foreach i of numlist 1/24 {  
  replace caraccident_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image13CarAccident1"
}
gen mug_gsravg = .
foreach i of numlist 1/24 {  
  replace mug_gsravg = phys_pic`i'gsravg if picep_imgfile`i' == "Image14Mug"
}
gen rollingpin_gsravg = .
foreach i of numlist 1/24 {  
  replace rollingpin_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image15RollingPin"
}
gen maggots_gsravg = .
foreach i of numlist 1/24 {  
  replace maggots_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image16Maggots"
}
gen waterfall_gsravg = .
foreach i of numlist 1/24 {  
  replace waterfall_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "image17Waterfall"
}
gen shipsinking_gsravg = .
foreach i of numlist 1/24 {  
  replace shipsinking_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "image18ShipSinking1"
}
gen dustpan_gsravg = .
foreach i of numlist 1/24 {  
  replace dustpan_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image19Dustpan"
}
gen worms_gsravg = .
foreach i of numlist 1/24 {  
  replace worms_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image20WormsInMouth"
}
gen babytumor_gsravg = .
foreach i of numlist 1/24 {  
  replace babytumor_gsravg = phys_pic`i'gsravg ///
  if picep_imgfile`i' == "Image21BabyWithTumor"
}
gen couple2_gsravg = .
foreach i of numlist 1/24 {  
  replace couple2_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image22Couple2"
}
gen sailboat_gsravg = .
foreach i of numlist 1/24 {  
  replace sailboat_gsravg = phys_pic`i'gsravg ///
    if picep_imgfile`i' == "Image23YellowSailboat"
}
gen vomit_gsravg = .
foreach i of numlist 1/24 {  
  replace vomit_gsravg = phys_pic`i'gsravg if picep_imgfile`i' == "Image24Vomit"
}

** recoding inter-stimulus intervals
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
  replace yellowsailboat_gsravgis = phys_isi`i'gsravg ///
    if picep_imgid`i' == "img23"
}
gen vomit_gsravgis = .
foreach i of numlist 1/24 {  
  replace vomit_gsravgis = phys_isi`i'gsravg if picep_imgid`i' == "img24"
}

** constructing physiological measures based on log-and-subtract method
* creating logged version of EDA response to threatening images
gen threat_pic3 = log(knife_gsravg)
gen threat_pic6 = log(snake_gsravg)
gen threat_pic7 = log(spider_gsravg)
gen threat_pic8 = log(gunattack_gsravg)
gen threat_pic13 = log(caraccident_gsravg)
gen threat_pic18 = log(shipsinking_gsravg)
* creating logged version of EDA response to disgusting images
gen disgust_pic1 = log(ashtray_gsravg)
gen disgust_pic12 = log(nastytoilet_gsravg)
gen disgust_pic16 = log(maggots_gsravg)
gen disgust_pic20 = log(worms_gsravg)
gen disgust_pic21 = log(babytumor_gsravg)
gen disgust_pic24 = log(vomit_gsravg)
* creating logged version of EDA response to neutral images
gen neutral_pic2 = log(basket_gsravg)
gen neutral_pic9 = log(umbrella_gsravg)
gen neutral_pic11 = log(bowl_gsravg)
gen neutral_pic14 = log(mug_gsravg)
gen neutral_pic15 = log(rollingpin_gsravg)
gen neutral_pic19 = log(dustpan_gsravg)
* creating logged version of EDA response to positive images
gen pos_pic4 = log(skysurfer_gsravg)
gen pos_pic5 = log(couple_gsravg)
gen pos_pic10 = log(snowskier_gsravg)
gen pos_pic17 = log(waterfall_gsravg)
gen pos_pic22 = log(couple2_gsravg)
gen pos_pic23 = log(sailboat_gsravg)
* creating logged version of EDA response to inter-stimulus interval for
*   threatening images
gen threat_pic3_is = log(knife_gsravgis)
gen threat_pic6_is = log(snake_gsravgis)
gen threat_pic7_is = log(spider_gsravgis)
gen threat_pic8_is = log(gunattack_gsravgis)
gen threat_pic13_is = log(caraccident_gsravgis)
gen threat_pic18_is = log(shipsinking_gsravgis)
* creating logged version of EDA response to inter-stimulus interval for
*   disgusting images
gen disgust_pic1_is = log(ashtray_gsravgis)
gen disgust_pic12_is = log(nastytoilet_gsravgis)
gen disgust_pic16_is = log(maggots_gsravgis)
gen disgust_pic20_is = log(worms_gsravgis)
gen disgust_pic21_is = log(babytumor_gsravgis)
gen disgust_pic24_is = log(vomit_gsravgis)
* creating logged version of EDA response to inter-stimulus interval for neutral
*   images
gen neutral_pic2_is = log(basket_gsravgis) 
gen neutral_pic9_is = log(umbrella_gsravgis) 
gen neutral_pic11_is = log(bowl_gsravgis)
gen neutral_pic14_is = log(mug_gsravgis)
gen neutral_pic15_is = log(rollingpin_gsravgis)
gen neutral_pic19_is = log(dustpan_gsravgis)    
* creating logged version of EDA response to inter-stimulus interval for
*   positive images
gen pos_pic4_is = log(skysurfer_gsravgis) 
gen pos_pic5_is = log(couple3_gsravgis)
gen pos_pic10_is = log(snowskier_gsravgis)
gen pos_pic17_is = log(waterfall_gsravgis)
gen pos_pic22_is = log(couple2_gsravgis)
gen pos_pic23_is = log(yellowsailboat_gsravgis) 
* calculating change in EDA response to threatening images
gen threat_pic3_change = (threat_pic3 - threat_pic3_is)
gen threat_pic6_change = (threat_pic6 - threat_pic6_is)
gen threat_pic7_change = (threat_pic7 - threat_pic7_is)
gen threat_pic8_change = (threat_pic8 - threat_pic8_is)
gen threat_pic13_change = (threat_pic13 - threat_pic13_is)
gen threat_pic18_change = (threat_pic18 - threat_pic18_is)
* calculating change in EDA response to disgusting images
gen disgust_pic1_change = (disgust_pic1 - disgust_pic1_is)
gen disgust_pic12_change = (disgust_pic12 - disgust_pic12_is)
gen disgust_pic16_change = (disgust_pic16 - disgust_pic16_is)
gen disgust_pic20_change = (disgust_pic20 - disgust_pic20_is)
gen disgust_pic21_change = (disgust_pic21 - disgust_pic21_is)
gen disgust_pic24_change = (disgust_pic24 - disgust_pic24_is)
* calculating change in EDA response to neutral images
gen neutral_pic2_change = (neutral_pic2 - neutral_pic2_is)
gen neutral_pic9_change = (neutral_pic9 - neutral_pic9_is)
gen neutral_pic11_change = (neutral_pic11 - neutral_pic11_is)
gen neutral_pic14_change = (neutral_pic14 - neutral_pic14_is)
gen neutral_pic15_change = (neutral_pic15 - neutral_pic15_is)
gen neutral_pic19_change = (neutral_pic19 - neutral_pic19_is)
* calculating change in EDA response to positive images
gen pos_pic4_change = (pos_pic4 - pos_pic4_is)
gen pos_pic5_change = (pos_pic5 - pos_pic5_is)
gen pos_pic10_change = (pos_pic10 - pos_pic10_is)
gen pos_pic17_change = (pos_pic17 - pos_pic17_is)
gen pos_pic22_change = (pos_pic22 - pos_pic22_is)
gen pos_pic23_change = (pos_pic23 - pos_pic23_is)

* generating picture-specific EDA responses using the log-and-subtract method
gen eda_pic3 = (threat_pic3 - threat_pic3_is)
gen eda_pic6 = (threat_pic6 - threat_pic6_is)
gen eda_pic7 = (threat_pic7 - threat_pic7_is)
gen eda_pic8 = (threat_pic8 - threat_pic8_is)
gen eda_pic13 = (threat_pic13 - threat_pic13_is)
gen eda_pic18 = (threat_pic18 - threat_pic18_is)
gen eda_pic1 = (disgust_pic1 - disgust_pic1_is)
gen eda_pic12 = (disgust_pic12 - disgust_pic12_is)
gen eda_pic16 = (disgust_pic16 - disgust_pic16_is)
gen eda_pic20 = (disgust_pic20 - disgust_pic20_is)
gen eda_pic21 = (disgust_pic21 - disgust_pic21_is)
gen eda_pic24 = (disgust_pic24 - disgust_pic24_is)
gen eda_pic2 = (neutral_pic2 - neutral_pic2_is)
gen eda_pic9 = (neutral_pic9 - neutral_pic9_is)
gen eda_pic11 = (neutral_pic11 - neutral_pic11_is)
gen eda_pic14 = (neutral_pic14 - neutral_pic14_is)
gen eda_pic15 = (neutral_pic15 - neutral_pic15_is)
gen eda_pic19 = (neutral_pic19 - neutral_pic19_is)
gen eda_pic4 = (pos_pic4 - pos_pic4_is)
gen eda_pic5 = (pos_pic5 - pos_pic5_is)
gen eda_pic10 = (pos_pic10 - pos_pic10_is)
gen eda_pic17 = (pos_pic17 - pos_pic17_is)
gen eda_pic22 = (pos_pic22 - pos_pic22_is)
gen eda_pic23 = (pos_pic23 - pos_pic23_is)

* construct variables for the order in which participants saw images
gen order_pic1 = .
foreach i of numlist 1/24 {  
  replace order_pic1 = `i' if picep_imgid`i' == "img01"
}
gen order_pic2 = .
foreach i of numlist 1/24 {  
  replace order_pic2 = `i' if picep_imgid`i' == "img02"
}
gen order_pic3 = .
foreach i of numlist 1/24 {  
  replace order_pic3 = `i' if picep_imgid`i' == "img03"
}
gen order_pic4 = .
foreach i of numlist 1/24 {  
  replace order_pic4 = `i' if picep_imgid`i' == "img04"
}
gen order_pic5 = .
foreach i of numlist 1/24 {  
  replace order_pic5 = `i' if picep_imgid`i' == "img05"
}
gen order_pic6 = .
foreach i of numlist 1/24 {  
  replace order_pic6 = `i' if picep_imgid`i' == "img06"
}
gen order_pic7 = .
foreach i of numlist 1/24 {  
  replace order_pic7 = `i' if picep_imgid`i' == "img07"
}
gen order_pic8 = .
foreach i of numlist 1/24 {  
  replace order_pic8 = `i' if picep_imgid`i' == "img08"
}
gen order_pic9 = .
foreach i of numlist 1/24 {  
  replace order_pic9 = `i' if picep_imgid`i' == "img09"
}
gen order_pic10 = .
foreach i of numlist 1/24 {  
  replace order_pic10 = `i' if picep_imgid`i' == "img10"
}
gen order_pic11 = .
foreach i of numlist 1/24 {  
  replace order_pic11 = `i' if picep_imgid`i' == "img11"
}
gen order_pic12 = .
foreach i of numlist 1/24 {  
  replace order_pic12 = `i' if picep_imgid`i' == "img12"
}
gen order_pic13 = .
foreach i of numlist 1/24 {  
  replace order_pic13 = `i' if picep_imgid`i' == "img13"
}
gen order_pic14 = .
foreach i of numlist 1/24 {  
  replace order_pic14 = `i' if picep_imgid`i' == "img14"
}
gen order_pic15 = .
foreach i of numlist 1/24 {  
  replace order_pic15 = `i' if picep_imgid`i' == "img15"
}
gen order_pic16 = .
foreach i of numlist 1/24 {  
  replace order_pic16 = `i' if picep_imgid`i' == "img16"
}
gen order_pic17 = .
foreach i of numlist 1/24 {  
  replace order_pic17 = `i' if picep_imgid`i' == "img17"
}
gen order_pic18 = .
foreach i of numlist 1/24 {  
  replace order_pic18 = `i' if picep_imgid`i' == "img18"
}
gen order_pic19 = .
foreach i of numlist 1/24 {  
  replace order_pic19 = `i' if picep_imgid`i' == "img19"
}
gen order_pic20 = .
foreach i of numlist 1/24 {  
  replace order_pic20 = `i' if picep_imgid`i' == "img20"
}
gen order_pic21 = .
foreach i of numlist 1/24 {  
  replace order_pic21 = `i' if picep_imgid`i' == "img21"
}
gen order_pic22 = .
foreach i of numlist 1/24 {  
  replace order_pic22 = `i' if picep_imgid`i' == "img22"
}
gen order_pic23 = .
foreach i of numlist 1/24 {  
  replace order_pic23 = `i' if picep_imgid`i' == "img23"
}
gen order_pic24 = .
foreach i of numlist 1/24 {  
  replace order_pic24 = `i' if picep_imgid`i' == "img24"
}


* creating index for change in EDA response to all threatening images
egen threat_change_total = rmean(threat_pic3_change threat_pic6_change ///
  threat_pic7_change threat_pic8_change threat_pic13_change threat_pic18_change)
label var threat_change_total ///
  "Total Change in EDA Response To Threatening Images"
* creating index for change in EDA response to all disgusting images
egen disgust_change_total = rmean(disgust_pic1_change disgust_pic12_change ///
  disgust_pic16_change disgust_pic20_change disgust_pic21_change ///
  disgust_pic24_change)
label var disgust_change_total ///
  "Total Change in EDA Response To Disgusting Images"
* create index for change in EDA response to all neutral images
egen neutral_change_total = rmean(neutral_pic2_change neutral_pic9_change ///
  neutral_pic11_change neutral_pic14_change neutral_pic15_change ///
  neutral_pic19_change)
label var neutral_change_total "Total Change in EDA Response To Neutral Images"
* creating index for change in EDA response to all positive images
egen pos_change_total = rmean(pos_pic4_change pos_pic5_change ///
  pos_pic10_change pos_pic17_change pos_pic22_change pos_pic23_change)
label var pos_change_total "Total Change in EDA Response To Positive Images"

** standardization
foreach var in threat_change_total disgust_change_total neutral_change_total ///
  pos_change_total wp soccon ecocon leftright_alt {
    egen z_`var' = std(`var')
}

label var z_threat_change_total ///
  "Standardized Change in EDA Response to Threatening Images"
label var z_disgust_change_total ///
  "Standardized Change in EDA Response to Disgusting Images"
label var z_neutral_change_total ///
  "Standardized Change in EDA Response to Neutral Images"
label var z_pos_change_total ///
  "Standardized Change in EDA Response to Positive Images"
label var z_wp "Standardized Wilson-Patterson Scale"
label var z_soccon "Standardized Social Conservatism"
label var z_ecocon "Standardized Economic Conservatism"
label var z_leftright_alt "Standardized Left-Right Self-Placement"

     

* save recodes as new Stata data file
save Data/OsmundsenEtAl-PsychophysiologyIdeology-JOP-Recodes.dta, replace

clear

* close log
*log close



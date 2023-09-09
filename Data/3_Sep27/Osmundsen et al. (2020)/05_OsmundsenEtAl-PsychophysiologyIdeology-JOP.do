********************************************************************************
********************************************************************************
****
**** File 5 of the replication archive for:
****   Osmundsen, Mathias, David J. Hendry, Lasse Laustsen, Kevin B. Smith, and
****     Michael Bang Petersen. 2020. "The Psychophysiology of Political
****     Ideology: Replications, Reanalysis, and Recommendations." Journal of
****     Politics XX(X): XXX-XXX.
****
**** Note: This file performs the analysis for Online Appendix 4 and the
**** portion of associated section "Exploring Possible Methods for Increasing
**** the Measurement Properties of Physiological Reactions" of the main text.
****
********************************************************************************
********************************************************************************

* uncomment the following line and the closing line to create a log file for the
*   current run
*log using Logs/05_OsmundsenEtAl-PsychophysiologyIdeology-JOP-Log.txt, replace

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

* reading in lab study data
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

* self-reported arousal and valence ratings from the laboratory study on a
*   subset of images
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

* reshape dataframe
reshape long area_pic eda_pic strong_pic discomfort_pic happy_pic ///
  threatr_pic disgustr_pic selfrateneg_pic selfratearo_pic order_pic, i(id) ///
  j(picture)

* standardize variables
foreach var in area_pic strong_pic discomfort_pic happy_pic threatr_pic ///
  disgustr_pic eda_pic selfrateneg_pic selfratearo_pic wp leftright_alt ///
  soccon ecocon {
  egen `var'01 = std(`var') 
}

********************************************************************************
** Online Appendix 4A
********************************************************************************

* consider complete cases to be those for whom we have complete eda measures and
*   complete survey-based ideology measures
quietly reg z_threat_change_total z_disgust_change_total ///
  z_neutral_change_total z_pos_change_total z_wp z_soccon z_ecocon ///
  z_leftright_alt
gen complete = 1 if e(sample)

* generate outlier indicator - see Online Appendix 2C
egen max_z_threat_change_total_us = max(z_threat_change_total) if ///
  complete == 1 & country == 0
gen outlier_us = 1 if z_threat_change_total == ///
  max_z_threat_change_total_us & max_z_threat_change_total_us != .


*************
** Table 4A.a
*************
reg wp01 c.eda_pic01##c.selfrateneg_pic01 female age education income_hh if ///
  outlier_us != 1 & country == 0, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Aa, excel aster(se) dec(3) label ///
  title("Table 4A.a: Interaction Effect Between Self-Reported Valence and Physiological Reactions to Images on Political Ideology. Denmark.") replace
reg soccon01 c.eda_pic01##c.selfrateneg_pic01 female age education ///
  income_hh if outlier_us != 1 & country == 0, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Aa, excel aster(se) dec(3) label ///
  append
reg ecocon01 c.eda_pic01##c.selfrateneg_pic01 female age education ///
  income_hh if outlier_us != 1 & country == 0, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Aa, excel aster(se) dec(3) label ///
  append
reg leftright_alt01 c.eda_pic01##c.selfrateneg_pic01 female age education ///
  income_hh if outlier_us != 1 & country == 0, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Aa, excel aster(se) dec(3) label ///
  append
*************

*************
** Table 4A.b
*************
reg wp01 c.eda_pic01##c.selfrateneg_pic01 female age education income_hh if ///
  outlier_us != 1 & country == 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ab, excel aster(se) dec(3) label ///
  title("Table 4A.b: Interaction Effect Between Self-Reported Valence and Physiological Reactions to Images on Political Ideology. United States.") replace
reg soccon01 c.eda_pic01##c.selfrateneg_pic01 female age education ///
  income_hh if outlier_us != 1 & country == 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ab, excel aster(se) dec(3) label ///
  append
reg ecocon01 c.eda_pic01##c.selfrateneg_pic01 female age education ///
  income_hh if outlier_us != 1 & country == 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ab, excel aster(se) dec(3) label ///
  append
reg leftright_alt01 c.eda_pic01##c.selfrateneg_pic01 female age education ///
  income_hh if outlier_us != 1 & country == 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ab, excel aster(se) dec(3) label ///
  append
*************

*************
** Table 4A.c
*************
reg wp01 c.eda_pic01##c.selfratearo_pic01 female age education income_hh if ///
  outlier_us != 1 & country == 0, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ac, excel aster(se) dec(3) label ///
  title("Table 4A.c: Interaction Effect Between Self-Reported Arousal and Physiological Reactions to Images on Political Ideology. Denmark.") replace
reg soccon01 c.eda_pic01##c.selfratearo_pic01 female age education ///
  income_hh if outlier_us != 1 & country == 0, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ac, excel aster(se) dec(3) label ///
  append
reg ecocon01 c.eda_pic01##c.selfratearo_pic01 female age education ///
  income_hh if outlier_us != 1 & country == 0, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ac, excel aster(se) dec(3) label ///
  append
reg leftright_alt01 c.eda_pic01##c.selfratearo_pic01 female age education ///
  income_hh if outlier_us != 1 & country == 0, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ac, excel aster(se) dec(3) label ///
  append
*************

*************
** Table 4A.d
*************
reg wp01 c.eda_pic01##c.selfratearo_pic01 female age education income_hh if ///
  outlier_us != 1 & country == 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ad, excel aster(se) dec(3) label ///
  title("Table 4A.d: Interaction Effect Between Self-Reported Arousal and Physiological Reactions to Images on Political Ideology. United States.") replace
reg soccon01 c.eda_pic01##c.selfratearo_pic01 female age education ///
  income_hh if outlier_us != 1 & country == 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ad, excel aster(se) dec(3) label ///
  append
reg ecocon01 c.eda_pic01##c.selfratearo_pic01 female age education ///
  income_hh if outlier_us != 1 & country == 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ad, excel aster(se) dec(3) label ///
  append
reg leftright_alt01 c.eda_pic01##c.selfratearo_pic01 female age education ///
  income_hh if outlier_us != 1  & country == 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ad, excel aster(se) dec(3) label ///
  append
*************

**************
** Figure 4A.a
**************
quietly reg wp01 c.eda_pic01##c.selfratearo_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(selfratearo_pic01 = (-1.5 (.1) 1.6))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Wilson-Patterson") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Arousal") name(frame1)

quietly reg soccon01 c.eda_pic01##c.selfratearo_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(selfratearo_pic01 = (-1.5 (.1) 1.6))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Social Conservatism") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Arousal") name(frame2)

quietly reg ecocon01 c.eda_pic01##c.selfratearo_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(selfratearo_pic01 = (-1.5 (.1) 1.6))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Economic Conservatism") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Arousal") name(frame3)

quietly reg leftright_alt01 c.eda_pic01##c.selfratearo_pic01 female age ///
  education income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(selfratearo_pic01 = (-1.5 (.1) 1.6))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Left-Right Self-placement") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Arousal") name(frame4)

graph combine frame1 frame2 frame3 frame4
graph export Figures/OnlineAppendix-Figure04Aa.pdf, replace
graph drop _all
graph close
**************

**************
** Figure 4A.b
**************
quietly reg wp01 c.eda_pic01##c.selfrateneg_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(selfrateneg_pic01 = (-1.7 (.1) 1.403))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Wilson-Patterson") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Valence") name(frame1)

quietly reg soccon01 c.eda_pic01##c.selfrateneg_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(selfrateneg_pic01 = (-1.7 (.1) 1.403))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Social Conservatism") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Valence") name(frame2)
  
quietly reg ecocon01 c.eda_pic01##c.selfrateneg_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(selfrateneg_pic01 = (-1.7 (.1) 1.403))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Economic Conservatism") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Valence") name(frame3)

quietly reg leftright_alt01 c.eda_pic01##c.selfrateneg_pic01 female age ///
  education income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(selfrateneg_pic01 = (-1.7 (.1) 1.403))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Left-Right Self-placement") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Valence") name(frame4)

graph combine frame1 frame2 frame3 frame4
graph export Figures/OnlineAppendix-Figure04Ab.pdf, replace
graph drop _all
graph close
**************


********************************************************************************
** Online Appendix 4B
********************************************************************************

* Interactions between EDA responses and self-reported ratings of images from
* ratings survey
gen IAthreat = eda_pic01 * threatr_pic01
gen IAdisgust = eda_pic01 * disgustr_pic01
gen IAdiscomfort = eda_pic01 * discomfort_pic01
gen IAstrong = eda_pic01 * strong_pic01
gen IAhappy = eda_pic01 * happy_pic01

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

*************
** Table 4B.a
*************
reg wp01 c.eda_pic01##c.threatr_pic01 i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ba, excel aster(se) dec(3) label ///
  title("Table 4B.a: Interaction Effect Between Self-Reported Threat and EDA Responses to Images on Political Ideology. Collapsed Across Countries") replace
reg soccon01 c.eda_pic01##c.threatr_pic01 i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ba, excel aster(se) dec(3) label ///
  append
reg ecocon01 c.eda_pic01##c.threatr_pic01 i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ba, excel aster(se) dec(3) label ///
  append
reg leftright_alt01 c.eda_pic01##c.threatr_pic01 i.country female age ///
  education income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Ba, excel aster(se) dec(3) label ///
  append
*************

*************
** Table 4B.b
*************
reg wp01 c.eda_pic01##c.disgustr_pic01 i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Bb, excel aster(se) dec(3) label ///
  title("Table 4B.b: Interaction Effect Between Self-Reported Disgust and EDA Responses to Images on Political Ideology. Collapsed Across Countries") replace
reg soccon01 c.eda_pic01##c.disgustr_pic01 i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Bb, excel aster(se) dec(3) label ///
  append
reg ecocon01 c.eda_pic01##c.disgustr_pic01 i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Bb, excel aster(se) dec(3) label ///
  append
reg leftright_alt01 c.eda_pic01##c.disgustr_pic01 i.country female age ///
  education income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Bb, excel aster(se) dec(3) label ///
  append
*************

*************
** Table 4B.c
*************
reg wp01 c.eda_pic01##c.discomfort_pic01 i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Bc, excel aster(se) dec(3) label ///
  title("Table 4B.c: Interaction Effect Between Self-Reported Discomfort and EDA Responses to Images on Political Ideology. Collapsed Across Countries") replace
reg soccon01 c.eda_pic01##c.discomfort_pic01 i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Bc, excel aster(se) dec(3) label ///
  append
reg ecocon01 c.eda_pic01##c.discomfort_pic01 i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Bc, excel aster(se) dec(3) label ///
  append
reg leftright_alt01 c.eda_pic01##c.discomfort_pic01 i.country female age ///
  education income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Bc, excel aster(se) dec(3) label ///
  append
*************

*************
** Table 4B.d
*************
reg wp01 c.eda_pic01##c.strong_pic01 i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Bd, excel aster(se) dec(3) label ///
  title("Table 4B.d: Interaction Effect Between Self-Reported Arousal and EDA Responses to Images on Political Ideology. Collapsed Across Countries") replace
reg soccon01 c.eda_pic01##c.strong_pic01 i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Bd, excel aster(se) dec(3) label ///
  append
reg ecocon01 c.eda_pic01##c.strong_pic01 i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Bd, excel aster(se) dec(3) label ///
  append
reg leftright_alt01 c.eda_pic01##c.strong_pic01 i.country female age ///
  education income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Bd, excel aster(se) dec(3) label ///
  append
*************

*************
** Table 4B.e
*************
reg wp01 c.eda_pic01##c.happy_pic01 i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Be, excel aster(se) dec(3) label ///
  title("Table 4B.e: Interaction Effect Between Self-Reported Joy and EDA Responses to Images on Political Ideology. Collapsed Across Countries") replace
reg soccon01 c.eda_pic01##c.happy_pic01 i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Be, excel aster(se) dec(3) label ///
  append
reg ecocon01 c.eda_pic01##c.happy_pic01 i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Be, excel aster(se) dec(3) label ///
  append
reg leftright_alt01 c.eda_pic01##c.happy_pic01 i.country female age ///
  education income_hh if outlier_us != 1, cluster(id)
outreg2 using Tables/OnlineAppendixTable04Be, excel aster(se) dec(3) label ///
  append
*************

********************************************************************************
** Supplemental - Not presented in text
**   - alternative specifications for models in Online Appendix 4B using three-
**     way interactions between EDA, ratings, and country
**   - p-values of three-way interactions noted in Section "Exploring Possible
**     Methods for Increasing the Measurement Properties of Physiological
**     Reactions," Subsection "Results"
********************************************************************************
** threat ratings
reg wp01 c.eda_pic01##c.threatr_pic01##i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
reg soccon01 c.eda_pic01##c.threatr_pic01##i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
reg ecocon01 c.eda_pic01##c.threatr_pic01##i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
reg leftright_alt01 c.eda_pic01##c.threatr_pic01##i.country female age ///
  education income_hh if outlier_us != 1, cluster(id)

** disgust ratings
reg wp01 c.eda_pic01##c.disgustr_pic01##i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
reg soccon01 c.eda_pic01##c.disgustr_pic01##i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
reg ecocon01 c.eda_pic01##c.disgustr_pic01##i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
reg leftright_alt01 c.eda_pic01##c.disgustr_pic01##i.country female age ///
  education income_hh if outlier_us != 1, cluster(id)

** discomfort
reg wp01 c.eda_pic01##c.discomfort_pic01##i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
reg soccon01 c.eda_pic01##c.discomfort_pic01##i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
reg ecocon01 c.eda_pic01##c.discomfort_pic01##i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
reg leftright_alt01 c.eda_pic01##c.discomfort_pic01##i.country female age ///
  education income_hh if outlier_us != 1, cluster(id)

** strength ratings
reg wp01 c.eda_pic01##c.strong_pic01##i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
reg soccon01 c.eda_pic01##c.strong_pic01##i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
reg ecocon01 c.eda_pic01##c.strong_pic01##i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
reg leftright_alt01 c.eda_pic01##c.strong_pic01##i.country female age ///
  education income_hh if outlier_us != 1, cluster(id)

** joy ratings
reg wp01 c.eda_pic01##c.happy_pic01##i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
reg soccon01 c.eda_pic01##c.happy_pic01##i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
reg ecocon01 c.eda_pic01##c.happy_pic01##i.country female age education ///
  income_hh if outlier_us != 1, cluster(id)
reg leftright_alt01 c.eda_pic01##c.happy_pic01##i.country female age ///
  education income_hh if outlier_us != 1, cluster(id)


**************
** Figure 4B.a
**************
foreach var of varlist IAthreat_dk IAthreat_us IAthreat_total {
  quietly regress wp01 `var' eda_pic01 threatr_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly regress wp01 `var' eda_pic01 disgustr_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAdiscomfort_dk IAdiscomfort_us IAdiscomfort_total {
  quietly regress wp01 `var' eda_pic01 discomfort_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly regress wp01 `var' eda_pic01 strong_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAhappy_dk IAhappy_us IAhappy_total {
  quietly regress wp01 `var' eda_pic01 happy_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store wp01_`var'
}
foreach var of varlist IAthreat_dk IAthreat_us IAthreat_total {
  quietly regress soccon01 `var' eda_pic01 threatr_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly regress soccon01 `var' eda_pic01 disgustr_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAdiscomfort_dk IAdiscomfort_us IAdiscomfort_total {
  quietly regress soccon01 `var' eda_pic01 discomfort_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly regress soccon01 `var' eda_pic01 strong_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAhappy_dk IAhappy_us IAhappy_total {
  quietly regress soccon01 `var' eda_pic01 happy_pic01 female age education ///
    income_hh country if outlier_us != 1, cluster(id)
  estimates store soccon01_`var'
}
foreach var of varlist IAthreat_dk IAthreat_us IAthreat_total {
  quietly regress leftright_alt01 `var' eda_pic01 threatr_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly regress leftright_alt01 `var' eda_pic01 disgustr_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAdiscomfort_dk IAdiscomfort_us IAdiscomfort_total {
  quietly regress leftright_alt01 `var' eda_pic01 discomfort_pic01 female ///
    age education income_hh country if outlier_us != 1, cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly regress leftright_alt01 `var' eda_pic01 strong_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist IAhappy_dk IAhappy_us IAhappy_total {
  quietly regress leftright_alt01 `var' eda_pic01 happy_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store lr01_`var'
}
foreach var of varlist  IAthreat_dk IAthreat_us IAthreat_total {
  quietly regress ecocon01 `var' eda_pic01 threatr_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store ecocon01_`var'
}
foreach var of varlist IAdisgust_dk IAdisgust_us IAdisgust_total {
  quietly regress ecocon01 `var' eda_pic01 disgustr_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store ecocon01_`var'
}
foreach var of varlist IAdiscomfort_dk IAdiscomfort_us IAdiscomfort_total {
  quietly regress ecocon01 `var' eda_pic01 discomfort_pic01 female age ///
    education income_hh country if outlier_us != 1, cluster(id)
  estimates store ecocon01_`var'
}
foreach var of varlist IAstrong_dk IAstrong_us IAstrong_total {
  quietly regress ecocon01 `var' eda_pic01 strong_pic01 female age education ///
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
  || lr01_*, bylabel(Left-Right Self-placement) ///
  ||, drop(_cons eda_pic01 threatr_pic01 disgustr_pic01 discomfort_pic01 ///
  happy_pic01 strong_pic01 female age education income_hh country) ///
  subtitle(, size(vsmall)) xline(0, lcolor(gray)) msymbol(square) ///
  headings(IAthreat_dk = "{bf:Self-Reported Threat}" ///
  IAdisgust_dk = "{bf:Self-Reported Disgust}" ///
  IAdiscomfort_dk = "{bf:Self-Reported Discomfort}" ///
  IAstrong_dk = "{bf:Self-Reported Arousal}" ///
  IAhappy_dk = "{bf:Self-Reported Happy}", ) byopts(legend(off)) ///
  format(%9.1g) mlabel mlabposition(10) mlabsize(1.5) grid(none) xsize(7) ///
  ysize(10)
graph export Figures/OnlineAppendix-Figure04Ba.pdf, replace
graph drop _all
graph close

estimates drop *
**************

**************
** Figure 4B.b
**************
quietly reg wp01 c.eda_pic01##c.threatr_pic01 female age education income_hh ///
  country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(threatr_pic01 = (-1 (.1) 3))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Wilson-Patterson") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Threat") name(frame1)
quietly reg soccon01 c.eda_pic01##c.threatr_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(threatr_pic01 = (-1 (.1) 3))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Social Conservatism") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Threat") name(frame2)
quietly reg ecocon01 c.eda_pic01##c.threatr_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(threatr_pic01 = (-1 (.1) 3))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Economic Conservatism") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Threat") name(frame3)
quietly reg leftright_alt01 c.eda_pic01##c.threatr_pic01 female age ///
  education income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(threatr_pic01 = (-1 (.1) 3))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Left-Right Self-placement") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Threat") name(frame4)
graph combine frame1 frame2 frame3 frame4
graph export Figures/OnlineAppendix-Figure04Bb.pdf, replace
graph drop _all
graph close
**************

**************
** Figure 4B.c
**************
quietly reg wp01 c.eda_pic01##c.disgustr_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(disgustr_pic01 = (-1.1 (.1) 1.6))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Wilson-Patterson") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Disgust") name(frame1)
quietly reg soccon01 c.eda_pic01##c.disgustr_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(disgustr_pic01 = (-1.1 (.1) 1.6))
marginsplot,  recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Social Conservatism") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Disgust") name(frame2)
quietly reg ecocon01 c.eda_pic01##c.disgustr_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(disgustr_pic01 = (-1.1 (.1) 1.6))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Economic Conservatism") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Disgust") name(frame3)
quietly reg leftright_alt01 c.eda_pic01##c.disgustr_pic01 female age ///
  education income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(disgustr_pic01 = (-1.1 (.1) 1.6))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Left-Right Self-placement") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Disgust") name(frame4)
graph combine frame1 frame2 frame3 frame4
graph export Figures/OnlineAppendix-Figure04Bc.pdf, replace
graph drop _all
graph close
**************

**************
** Figure 4B.d
**************
quietly reg wp01 c.eda_pic01##c.discomfort_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(discomfort_pic01 = (-1.2 (.1) 1.33))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Wilson-Patterson") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Discomfort") name(frame1)
quietly reg soccon01 c.eda_pic01##c.discomfort_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(discomfort_pic01 = (-1.2 (.1) 1.33))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Social Conservatism") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Discomfort") name(frame2)
quietly reg ecocon01 c.eda_pic01##c.discomfort_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(discomfort_pic01 = (-1.2 (.1) 1.33))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Economic Conservatism") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Discomfort") name(frame3)
quietly reg leftright_alt01 c.eda_pic01##c.discomfort_pic01 female age ///
  education income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(discomfort_pic01 = (-1.2 (.1) 1.33))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Left-Right Self-placement") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Discomfort") name(frame4)
graph combine frame1 frame2 frame3 frame4
graph export Figures/OnlineAppendix-Figure04Bd.pdf, replace
graph drop _all
graph close
**************

**************
** Figure 4B.e
**************
quietly reg wp01 c.eda_pic01##c.happy_pic01 female age education income_hh ///
  country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(happy_pic01 = (-1 (.1) 2))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Wilson-Patterson") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Happy") name(frame1)
quietly reg soccon01 c.eda_pic01##c.happy_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(happy_pic01 = (-1 (.1) 2))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Social Conservatism") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Happy") name(frame2)
quietly reg ecocon01 c.eda_pic01##c.happy_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(happy_pic01 = (-1 (.1) 2))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Economic Conservatism") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Happy") name(frame3)
quietly reg leftright_alt01 c.eda_pic01##c.happy_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(happy_pic01 = (-1 (.1) 2))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Left-Right Self-placement") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Happy") name(frame4)
graph combine frame1 frame2 frame3 frame4
graph export Figures/OnlineAppendix-Figure04Be.pdf, replace
graph drop _all
graph close
**************

**************
** Figure 4B.f
**************
quietly reg wp01 c.eda_pic01##c.strong_pic01 female age education income_hh ///
  country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(strong_pic01 = (-1.6 (.1) 2))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Wilson-Patterson") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Arousal") name(frame1)
quietly reg soccon01 c.eda_pic01##c.strong_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(strong_pic01 = (-1.6 (.1) 2))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Social Conservatism") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Arousal") name(frame2)
quietly reg ecocon01 c.eda_pic01##c.strong_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(strong_pic01 = (-1.6 (.1) 2))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Economic Conservatism") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Arousal") name(frame3)
quietly reg leftright_alt01 c.eda_pic01##c.strong_pic01 female age education ///
  income_hh country if outlier_us != 1, cluster(id)
margins, dydx(eda_pic01) at(strong_pic01 = (-1.6 (.1) 2))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash)) yline(0) ///
  title("Left-Right Self-placement") ytitle("Marginal Effect of Physiology") ///
  xtitle("Self-reported Arousal") name(frame4)
graph combine frame1 frame2 frame3 frame4
graph export Figures/OnlineAppendix-Figure04Bf.pdf, replace
graph drop _all
graph close
**************

clear

* close log
*log close


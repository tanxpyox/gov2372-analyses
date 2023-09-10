********************************************************************************
********************************************************************************
****
**** File 2 of the replication archive for:
****   Osmundsen, Mathias, David J. Hendry, Lasse Laustsen, Kevin B. Smith, and
****     Michael Bang Petersen. 2020. "The Psychophysiology of Political
****     Ideology: Replications, Reanalysis, and Recommendations." Journal of
****     Politics XX(X): XXX-XXX.
****
**** Note: This file performs the analyses based on the Danish and American lab
**** studies.
****
********************************************************************************
********************************************************************************

* uncomment the following line and the closing line to create a log file for the
*   current run
*log using Logs/02_OsmundsenEtAl-PsychophysiologyIdeology-JOP-Log.txt, replace

* change the working directory to the location of this .do file (if necessary)
* cd

** preliminaries
* set plot style for all plots
set scheme s1mono

* read in recoded data file
clear
use Data/OsmundsenEtAl-PsychophysiologyIdeology-JOP-Recodes.dta, clear

********************************************************************************
** Analyses based on EDA
********************************************************************************

* consider complete cases to be those for whom we have complete eda measures and
*   complete survey-based ideology measures
quietly reg z_threat_change_total z_disgust_change_total ///
  z_neutral_change_total z_pos_change_total z_wp z_soccon z_ecocon ///
  z_leftright_alt
gen complete = 1 if e(sample)
bysort country: tab complete

* detecting outliers
bysort country: sum z_threat_change_total z_disgust_change_total ///
  z_neutral_change_total z_pos_change_total if complete == 1

* full distribution of standardized eda responses without respect to country
local i = 0
local divider : display ///
  "----------------------------------------------------------------------"
foreach var in z_threat_change_total z_disgust_change_total ///
  z_neutral_change_total z_pos_change_total {
  local i = `i' + 1
  local label : variable label `var'
  quietly sum `var'
  local num : display %6.4f (`r(max)' - `r(mean)') / `r(sd)'
  if (`i' == 1) {
    display "`divider'"
  }
  display "`label':"
  display "  Maximum is `num' standard deviations above the mean."
  display "`divider'"
}

* distributions of standardized eda responses by country
local i = 0
local divider : display ///
  "----------------------------------------------------------------------"
foreach var in z_threat_change_total z_disgust_change_total ///
  z_neutral_change_total z_pos_change_total {
  local i = `i' + 1
  local label : variable label `var'
  quietly sum `var' if country == 0
  local usnum : display %6.4f (`r(max)' - `r(mean)') / `r(sd)'
  quietly sum `var' if country == 1
  local dknum : display %6.4f (`r(max)' - `r(mean)') / `r(sd)'
  if (`i' == 1) {
    display "`divider'"
  }
  display "`label':"
  display "  Maximum in U.S. is `usnum' standard deviations above the mean."
  display "  Maximum in Denmark is `dknum' standard deviations above the mean."
  display "`divider'"
}

***********************
** Appendix Figure 2C.a
***********************
local threatlab : var label z_threat_change_total
local disgustlab : var label z_disgust_change_total
local threatlab1 = substr("`threatlab'", 1, 35)
local disgustlab1 = substr("`disgustlab'", 1, 35)
local threatlab2 = substr("`threatlab'", 37, 21)
local disgustlab2 = substr("`disgustlab'", 37, 20)

hist z_threat_change_total if country == 0, ///
  xtitle("`threatlab1'" "`threatlab2'") title("United States") ///
  name(histUSthreat)
hist z_threat_change_total if country == 1, ///
  xtitle("`threatlab1'" "`threatlab2'") title("Denmark") ///
  name(histDKthreat)
hist z_disgust_change_total if country == 1, ///
  xtitle("`disgustlab1'" "`disgustlab2'") title("Denmark") ///
  name(histDKdisgust)
graph combine histUSthreat histDKthreat
graph export Figures/OnlineAppendix-Figure2Ca.pdf, replace
graph combine histUSthreat histDKdisgust
graph export Figures/OnlineAppendix-Figure2Ca_alt.pdf, replace
graph drop _all
graph close
***********************

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

* complete cases, removing outliers
gen completenooutl1 = complete
replace completenooutl1 = . if outlier_us == 1
gen completenooutl2 = complete
replace completenooutl2 = . if outlier_us == 1
replace completenooutl2 = . if outlier_dk == 1

* standardized eda summaries after outlier removal
bysort country: sum z_threat_change_total z_disgust_change_total ///
  z_neutral_change_total z_pos_change_total if completenooutl2 == 1

**************************************************************
** Summary statistics reported in text
**   Section: "Replications: Cross-National Laboratory Studies"
**   Subsection: "Sampling"
**************************************************************
* all
bysort country: sum age
foreach var in female education {
  bysort country: tab `var'
}
bysort country: sum income_hh, detail

* removing the U.S. outlier
bysort country: sum age if outlier_us != 1
foreach var in female education {
  bysort country: tab `var' if outlier_us != 1
}
bysort country: sum income_hh if outlier_us != 1, detail

* subsetting by complete cases and no outliers
bysort country: sum age if completenooutl1 == 1
foreach var in female education {
  bysort country: tab `var' if completenooutl1 == 1
}
bysort country: sum income_hh if completenooutl1 == 1, detail

bysort country: sum age if completenooutl2 == 1
foreach var in female education {
  bysort country: tab `var' if completenooutl2 == 1
}
bysort country: sum income_hh if completenooutl2 == 1, detail
**************************************************************


***************************************************************
** Alpha values reported in text
**   Section: "Replications: Cross-National Laboratory Studies"
**   Subsection: "Measures"
***************************************************************
* Wilson-Patterson
* all
bysort country: alpha presvy_q48_1_temp presvy_q48_2_temp presvy_q48_3 ///
  presvy_q48_4_temp presvy_q48_5 presvy_q48_6 presvy_q48_7 presvy_q48_8 ///
  presvy_q48_9 presvy_q48_10_temp presvy_q48_11 presvy_q48_12_temp ///
  presvy_q48_13_temp presvy_q48_14 presvy_q48_15_temp presvy_q48_16_temp ///
  presvy_q48_17_temp presvy_q48_18 presvy_q48_19_temp presvy_q48_20_temp, asis

* removing the U.S. outlier
bysort country: alpha presvy_q48_1_temp presvy_q48_2_temp presvy_q48_3 ///
  presvy_q48_4_temp presvy_q48_5 presvy_q48_6 presvy_q48_7 presvy_q48_8 ///
  presvy_q48_9 presvy_q48_10_temp presvy_q48_11 presvy_q48_12_temp ///
  presvy_q48_13_temp presvy_q48_14 presvy_q48_15_temp presvy_q48_16_temp ///
  presvy_q48_17_temp presvy_q48_18 presvy_q48_19_temp presvy_q48_20_temp if ///
  outlier_us != 1, asis

* subsetting by complete cases and no outliers
bysort country: alpha presvy_q48_1_temp presvy_q48_2_temp presvy_q48_3 ///
  presvy_q48_4_temp presvy_q48_5 presvy_q48_6 presvy_q48_7 presvy_q48_8 ///
  presvy_q48_9 presvy_q48_10_temp presvy_q48_11 presvy_q48_12_temp ///
  presvy_q48_13_temp presvy_q48_14 presvy_q48_15_temp presvy_q48_16_temp ///
  presvy_q48_17_temp presvy_q48_18 presvy_q48_19_temp presvy_q48_20_temp if ///
  completenooutl1 == 1, asis

bysort country: alpha presvy_q48_1_temp presvy_q48_2_temp presvy_q48_3 ///
  presvy_q48_4_temp presvy_q48_5 presvy_q48_6 presvy_q48_7 presvy_q48_8 ///
  presvy_q48_9 presvy_q48_10_temp presvy_q48_11 presvy_q48_12_temp ///
  presvy_q48_13_temp presvy_q48_14 presvy_q48_15_temp presvy_q48_16_temp ///
  presvy_q48_17_temp presvy_q48_18 presvy_q48_19_temp presvy_q48_20_temp if ///
  completenooutl2 == 1, asis

* Social Conservatism
* all
bysort country: alpha presvy_temp_q46_1-presvy_temp_q46_5, asis

* removing the U.S. outlier
bysort country: alpha presvy_temp_q46_1-presvy_temp_q46_5 if ///
  outlier_us != 1, asis

* subsetting by complete cases and no outliers
bysort country: alpha presvy_temp_q46_1-presvy_temp_q46_5 if ///
  completenooutl1 == 1, asis

bysort country: alpha presvy_temp_q46_1-presvy_temp_q46_5 if ///
  completenooutl2 == 1, asis

* Economic Conservatism
* all
bysort country: alpha presvy_q47_1_rev_temp presvy_q47_2_rev_temp ///
  presvy_q47_3_rev_temp presvy_q47_4_temp presvy_q47_5_temp, asis

* removing the U.S. outlier
bysort country: alpha presvy_q47_1_rev_temp presvy_q47_2_rev_temp ///
  presvy_q47_3_rev_temp presvy_q47_4_temp presvy_q47_5_temp if ///
  outlier_us != 1, asis

* subsetting by complete cases and no outliers
bysort country: alpha presvy_q47_1_rev_temp presvy_q47_2_rev_temp ///
  presvy_q47_3_rev_temp presvy_q47_4_temp presvy_q47_5_temp if ///
  completenooutl1 == 1, asis

bysort country: alpha presvy_q47_1_rev_temp presvy_q47_2_rev_temp ///
  presvy_q47_3_rev_temp presvy_q47_4_temp presvy_q47_5_temp if ///
  completenooutl2 == 1, asis
***************************************************************

*************************************************************************
** Alpha values and pairwise correlations reported in text
**   Section: "Replications: Cross-National Laboratory Studies"
**   Subsection: "Assessing Reliability and Validity of the EDA Measures"
*************************************************************************
* Threatening Images: Stimulus vs. Inter-stimulus Intervals
foreach i in 3 6 7 8 13 18 {
  pwcorr threat_pic`i' threat_pic`i'_is if outlier_us != 1
}
foreach i in 3 6 7 8 13 18 {
  bysort country: pwcorr threat_pic`i' threat_pic`i'_is if outlier_us != 1
}

* Disgusting Images: Stimulus vs. Inter-stimulus Intervals
foreach i in 1 12 16 20 21 24 {
  pwcorr disgust_pic`i' disgust_pic`i'_is if outlier_us != 1
}
foreach i in 1 12 16 20 21 24 {
  bysort country: pwcorr disgust_pic`i' disgust_pic`i'_is if outlier_us != 1
}

* Convergent Validity: Correlations between EDA responses to threatening images
alpha threat_pic3_change threat_pic6_change threat_pic7_change ///
  threat_pic8_change threat_pic13_change threat_pic18_change if ///
  outlier_us != 1, asis std
bysort country: alpha threat_pic3_change threat_pic6_change ///
  threat_pic7_change threat_pic8_change threat_pic13_change ///
  threat_pic18_change if outlier_us != 1, asis std

* Convergent Validity: Correlations between EDA responses to disgusting images
alpha disgust_pic1_change disgust_pic12_change disgust_pic16_change ///
  disgust_pic20_change disgust_pic21_change disgust_pic24_change if ///
  outlier_us != 1, std asis
bysort country: alpha disgust_pic1_change disgust_pic12_change ///
  disgust_pic16_change disgust_pic20_change disgust_pic21_change ///
  disgust_pic24_change if outlier_us != 1, std asis

* tests of whether participants have stronger EDA responses to threat and
*   disgust images than the preceding inter-stimulus intervals
egen threat_S = rmean(threat_pic3 threat_pic6 threat_pic7 threat_pic8 ///
  threat_pic13 threat_pic18) 
egen threat_IS = rmean(threat_pic3_is threat_pic6_is threat_pic7_is ///
  threat_pic8_is threat_pic13_is  threat_pic18_is) 
egen disgust_S = rmean(disgust_pic1 disgust_pic12 disgust_pic16 ///
  disgust_pic20 disgust_pic21 disgust_pic24) 
egen disgust_IS = rmean(disgust_pic1_is disgust_pic12_is disgust_pic16_is ///
  disgust_pic20_is disgust_pic21_is disgust_pic24_is ) 

** t-tests reported in footnote 8
bysort country: ttest threat_S == threat_IS if outlier_us != 1
bysort country: ttest disgust_S == disgust_IS if outlier_us != 1

* Divergent Validity: Correlations between EDA responses to different types of
*   images
pwcorr threat_change_total disgust_change_total neutral_change_total ///
  pos_change_total if outlier_us != 1, sig
bys country: pwcorr threat_change_total disgust_change_total ///
  neutral_change_total pos_change_total if outlier_us != 1, sig
*************************************************************************

***********************
** Appendix Figure 2C.a
***********************




***********************

***********
** Figure 1
***********
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
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust 
  estimates store a1_`var'
  local a1_`var'_b : display %6.2f _b[`var']
  local a1_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local a1_`var'_n `e(N)'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store b1_`var'
  local b1_`var'_b : display %6.2f _b[`var']
  local b1_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local b1_`var'_n `e(N)'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store c1_`var'
  local c1_`var'_b : display %6.2f _b[`var']
  local c1_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local c1_`var'_n `e(N)'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store d1_`var'
  local d1_`var'_b : display %6.2f _b[`var']
  local d1_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local d1_`var'_n `e(N)'
}
foreach var of varlist threat_dk threat_us threat_total {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store a2_`var'
  local a2_`var'_b : display %6.2f _b[`var']
  local a2_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local a2_`var'_n `e(N)'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store b2_`var'
  local b2_`var'_b : display %6.2f _b[`var']
  local b2_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local b2_`var'_n `e(N)'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store c2_`var'
  local c2_`var'_b : display %6.2f _b[`var']
  local c2_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local c2_`var'_n `e(N)'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store d2_`var'
  local d2_`var'_b : display %6.2f _b[`var']
  local d2_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local d2_`var'_n `e(N)'
}
foreach var of varlist threat_dk threat_us threat_total {
  quietly regress z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store a3_`var'
  local a3_`var'_b : display %6.2f _b[`var']
  local a3_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local a3_`var'_n `e(N)'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly regress z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store b3_`var'
  local b3_`var'_b : display %6.2f _b[`var']
  local b3_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local b3_`var'_n `e(N)'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly regress z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store c3_`var'
  local c3_`var'_b : display %6.2f _b[`var']
  local c3_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local c3_`var'_n `e(N)'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly regress z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store d3_`var'
  local d3_`var'_b : display %6.2f _b[`var']
  local d3_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local d3_`var'_n `e(N)'
}
foreach var of varlist threat_dk threat_us threat_total {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store a4_`var'
  local a4_`var'_b : display %6.2f _b[`var']
  local a4_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local a4_`var'_n `e(N)'
}
foreach var of varlist disgust_dk disgust_us disgust_total {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store b4_`var'
  local b4_`var'_b : display %6.2f _b[`var']
  local b4_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local b4_`var'_n `e(N)'
}
foreach var of varlist positive_dk positive_us positive_total {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store c4_`var'
  local c4_`var'_b : display %6.2f _b[`var']
  local c4_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local c4_`var'_n `e(N)'
}
foreach var of varlist neutral_dk neutral_us neutral_total {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store d4_`var'
  local d4_`var'_b : display %6.2f _b[`var']
  local d4_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local d4_`var'_n `e(N)'
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
graph export Figures/Figure01.pdf, replace
graph drop _all
graph close

estimates drop a* b* c* d*
***********

******************************************
** Numbers from the text based on Figure 1
******************************************
local blist `a1_threat_us_b' `a1_threat_dk_b' `a1_threat_total_b' ///
  `b1_disgust_us_b' `b1_disgust_dk_b' `b1_disgust_total_b' ///
  `c1_positive_us_b' `c1_positive_dk_b' `c1_positive_total_b' ///
  `d1_neutral_us_b' `d1_neutral_dk_b' `d1_neutral_total_b' `a2_threat_us_b' ///
  `a2_threat_dk_b' `a2_threat_total_b' `b2_disgust_us_b' `b2_disgust_dk_b' ///
  `b2_disgust_total_b' `c2_positive_us_b' `c2_positive_dk_b' ///
  `c2_positive_total_b' `d2_neutral_us_b' `d2_neutral_dk_b' ///
  `d2_neutral_total_b' `a3_threat_us_b' `a3_threat_dk_b' `a3_threat_total_b' ///
  `b3_disgust_us_b' `b3_disgust_dk_b' `b3_disgust_total_b' ///
  `c3_positive_us_b' `c3_positive_dk_b' `c3_positive_total_b' ///
  `d3_neutral_us_b' `d3_neutral_dk_b' `d3_neutral_total_b' `a4_threat_us_b' ///
  `a4_threat_dk_b' `a4_threat_total_b' `b4_disgust_us_b' `b4_disgust_dk_b' ///
  `b4_disgust_total_b' `c4_positive_us_b' `c4_positive_dk_b' ///
  `c4_positive_total_b' `d4_neutral_us_b' `d4_neutral_dk_b' `d4_neutral_total_b'

local threatuslist_b ///
  "`a1_threat_us_b' `a2_threat_us_b' `a3_threat_us_b' `a4_threat_us_b'"
local threatdklist_b ///
  "`a1_threat_dk_b' `a2_threat_dk_b' `a3_threat_dk_b' `a4_threat_dk_b'"
local threattotlist_b ///
  "`a1_threat_total_b' `a2_threat_total_b' `a3_threat_total_b' `a4_threat_total_b'"

local disgustuslist_b ///
  "`b1_disgust_us_b' `b2_disgust_us_b' `b3_disgust_us_b' `b4_disgust_us_b'"
local disgustdklist_b ///
  "`b1_disgust_dk_b' `b2_disgust_dk_b' `b3_disgust_dk_b' `b4_disgust_dk_b'"
local disgusttotlist_b ///
  "`b1_disgust_total_b' `b2_disgust_total_b' `b3_disgust_total_b' `b4_disgust_total_b'"

local posuslist_b ///
  "`c1_positive_us_b' `c2_positive_us_b' `c3_positive_us_b' `c4_positive_us_b'"
local posdklist_b ///
  "`c1_positive_dk_b' `c2_positive_dk_b' `c3_positive_dk_b' `c4_positive_dk_b'"
local postotlist_b ///
  "`c1_positive_total_b' `c2_positive_total_b' `c3_positive_total_b' `c4_positive_total_b'"

local neutuslist_b ///
  "`d1_neutral_us_b' `d2_neutral_us_b' `d3_neutral_us_b' `d4_neutral_us_b'"
local neutdklist_b ///
  "`d1_neutral_dk_b' `d2_neutral_dk_b' `d3_neutral_dk_b' `d4_neutral_dk_b'"
local neuttotlist_b ///
  "`d1_neutral_total_b' `d2_neutral_total_b' `d3_neutral_total_b' `d4_neutral_total_b'"

local threatuslist_p ///
  "`a1_threat_us_p' `a2_threat_us_p' `a3_threat_us_p' `a4_threat_us_p'"
local threatdklist_p ///
  "`a1_threat_dk_p' `a2_threat_dk_p' `a3_threat_dk_p' `a4_threat_dk_p'"
local threattotlist_p ///
  "`a1_threat_total_p' `a2_threat_total_p' `a3_threat_total_p' `a4_threat_total_p'"

local disgustuslist_p ///
  "`b1_disgust_us_p' `b2_disgust_us_p' `b3_disgust_us_p' `b4_disgust_us_p'"
local disgustdklist_p ///
  "`b1_disgust_dk_p' `b2_disgust_dk_p' `b3_disgust_dk_p' `b4_disgust_dk_p'"
local disgusttotlist_p ///
  "`b1_disgust_total_p' `b2_disgust_total_p' `b3_disgust_total_p' `b4_disgust_total_p'"

local posuslist_p ///
  "`c1_positive_us_p' `c2_positive_us_p' `c3_positive_us_p' `c4_positive_us_p'"
local posdklist_p ///
  "`c1_positive_dk_p' `c2_positive_dk_p' `c3_positive_dk_p' `c4_positive_dk_p'"
local postotlist_p ///
  "`c1_positive_total_p' `c2_positive_total_p' `c3_positive_total_p' `c4_positive_total_p'"

local neutuslist_p ///
  "`d1_neutral_us_p' `d2_neutral_us_p' `d3_neutral_us_p' `d4_neutral_us_p'"
local neutdklist_p ///
  "`d1_neutral_dk_p' `d2_neutral_dk_p' `d3_neutral_dk_p' `d4_neutral_dk_p'"
local neuttotlist_p ///
  "`d1_neutral_total_p' `d2_neutral_total_p' `d3_neutral_total_p' `d4_neutral_total_p'"

forvalues i = 1/4 {
  local tb`i'temp : word `i' of `threatuslist_b'
  
  if (`tb`i'temp' == 0) {
    local tb`i'temp : display %6.2f `tb`i'temp' * 0
    local tb`i'temp : display substr("`tb`i'temp'", 3, 6)
  }

  if (`tb`i'temp' >= 0) {
    local tb`i' : display " `tb`i'temp'"
  }
  else {
    local tb`i' : display "`tb`i'temp'"
  }
  local db`i'temp : word `i' of `disgustuslist_b'

  if (`db`i'temp' == 0) {
    local db`i'temp : display %6.2f `db`i'temp' * 0
    local db`i'temp : display substr("`db`i'temp'", 3, 6)
  }

  if (`db`i'temp' >= 0) {
    local db`i' : display " `db`i'temp'"
  }
  else {
    local db`i' : display "`db`i'temp'"
  }
  local pb`i'temp : word `i' of `posuslist_b'
  
  if (`pb`i'temp' == 0) {
    local pb`i'temp : display %6.2f `pb`i'temp' * 0
    local pb`i'temp : display substr("`pb`i'temp'", 3, 6)
  }

  if (`pb`i'temp' >= 0) {
    local pb`i' : display " `pb`i'temp'"
  }
  else {
    local pb`i' : display "`pb`i'temp'"
  }
  local nb`i'temp : word `i' of `neutuslist_b'

  if (`nb`i'temp' == 0) {
    local nb`i'temp : display %6.2f `nb`i'temp' * 0
    local nb`i'temp : display substr("`nb`i'temp'", 3, 6)
  }

  if (`nb`i'temp' >= 0) {
    local nb`i' : display " `nb`i'temp'"
  }
  else {
    local nb`i' : display "`nb`i'temp'"
  }
  local tp`i' : word `i' of `threatuslist_p'
  local dp`i' : word `i' of `disgustuslist_p'
  local pp`i' : word `i' of `posuslist_p'
  local np`i' : word `i' of `neutuslist_p'
}
local threatuslist_formatted ///
  "     U.S.     | `tb1'  `tp1' | `tb2'  `tp2' | `tb3'  `tp3' | `tb4'  `tp4'   |"
local disgustuslist_formatted ///
  "     U.S.     | `db1'  `dp1' | `db2'  `dp2' | `db3'  `dp3' | `db4'  `dp4'   |"
local posuslist_formatted ///
  "     U.S.     | `pb1'  `pp1' | `pb2'  `pp2' | `pb3'  `pp3' | `pb4'  `pp4'   |"
local neutuslist_formatted ///
  "     U.S.     | `nb1'  `np1' | `nb2'  `np2' | `nb3'  `np3' | `nb4'  `np4'   |"

forvalues i = 1/4 {
  local tb`i'temp : word `i' of `threatdklist_b'

  if (`tb`i'temp' == 0) {
    local tb`i'temp : display %6.2f `tb`i'temp' * 0
    local tb`i'temp : display substr("`tb`i'temp'", 3, 6)
  }

  if (`tb`i'temp' >= 0) {
    local tb`i' : display " `tb`i'temp'"
  }
  else {
    local tb`i' : display "`tb`i'temp'"
  }
  local db`i'temp : word `i' of `disgustdklist_b'

  if (`db`i'temp' == 0) {
    local db`i'temp : display %6.2f `db`i'temp' * 0
    local db`i'temp : display substr("`db`i'temp'", 3, 6)
  }

  if (`db`i'temp' >= 0) {
    local db`i' : display " `db`i'temp'"
  }
  else {
    local db`i' : display "`db`i'temp'"
  }
  local pb`i'temp : word `i' of `posdklist_b'

  if (`pb`i'temp' == 0) {
    local pb`i'temp : display %6.2f `pb`i'temp' * 0
    local pb`i'temp : display substr("`pb`i'temp'", 3, 6)
  }

  if (`pb`i'temp' >= 0) {
    local pb`i' : display " `pb`i'temp'"
  }
  else {
    local pb`i' : display "`pb`i'temp'"
  }
  local nb`i'temp : word `i' of `neutdklist_b'

  if (`nb`i'temp' == 0) {
    local nb`i'temp : display %6.2f `nb`i'temp' * 0
    local nb`i'temp : display substr("`nb`i'temp'", 3, 6)
  }

  if (`nb`i'temp' >= 0) {
    local nb`i' : display " `nb`i'temp'"
  }
  else {
    local nb`i' : display "`nb`i'temp'"
  }
  local tp`i' : word `i' of `threatdklist_p'
  local dp`i' : word `i' of `disgustdklist_p'
  local pp`i' : word `i' of `posdklist_p'
  local np`i' : word `i' of `neutdklist_p'
}
local threatdklist_formatted ///
  "     Denmark  | `tb1'  `tp1' | `tb2'  `tp2' | `tb3'  `tp3' | `tb4'  `tp4'   |"
local disgustdklist_formatted ///
  "     Denmark  | `db1'  `dp1' | `db2'  `dp2' | `db3'  `dp3' | `db4'  `dp4'   |"
local posdklist_formatted ///
  "     Denmark  | `pb1'  `pp1' | `pb2'  `pp2' | `pb3'  `pp3' | `pb4'  `pp4'   |"
local neutdklist_formatted ///
  "     Denmark  | `nb1'  `np1' | `nb2'  `np2' | `nb3'  `np3' | `nb4'  `np4'   |"

forvalues i = 1/4 {
  local tb`i'temp : word `i' of `threattotlist_b'

  if (`tb`i'temp' == 0) {
    local tb`i'temp : display %6.2f `tb`i'temp' * 0
    local tb`i'temp : display substr("`tb`i'temp'", 3, 6)
  }

  if (`tb`i'temp' > 0) {
    local tb`i' : display " `tb`i'temp'"
  }
  else {
    local tb`i' : display "`tb`i'temp'"
  }
  local db`i'temp : word `i' of `disgusttotlist_b'

  if (`db`i'temp' == 0) {
    local db`i'temp : display %6.2f `db`i'temp' * 0
    local db`i'temp : display substr("`db`i'temp'", 3, 6)
  }

  if (`db`i'temp' > 0) {
    local db`i' : display " `db`i'temp'"
  }
  else {
    local db`i' : display "`db`i'temp'"
  }
  local pb`i'temp : word `i' of `postotlist_b'

  if (`pb`i'temp' == 0) {
    local pb`i'temp : display %6.2f `pb`i'temp' * 0
    local pb`i'temp : display substr("`pb`i'temp'", 3, 6)
  }

  if (`pb`i'temp' > 0) {
    local pb`i' : display " `pb`i'temp'"
  }
  else {
    local pb`i' : display "`pb`i'temp'"
  }
  local nb`i'temp : word `i' of `neuttotlist_b'

  if (`nb`i'temp' == 0) {
    local nb`i'temp : display %6.2f `nb`i'temp' * 0
    local nb`i'temp : display substr("`nb`i'temp'", 3, 6)
  }

  if (`nb`i'temp' > 0) {
    local nb`i' : display " `nb`i'temp'"
  }
  else {
    local nb`i' : display "`nb`i'temp'"
  }
  local tp`i' : word `i' of `threattotlist_p'
  local dp`i' : word `i' of `disgusttotlist_p'
  local pp`i' : word `i' of `postotlist_p'
  local np`i' : word `i' of `neuttotlist_p'
}
local threattotlist_formatted ///
  "     Combined | `tb1'  `tp1' | `tb2'  `tp2' | `tb3'  `tp3' | `tb4'  `tp4'   |"
local disgusttotlist_formatted ///
  "     Combined | `db1'  `dp1' | `db2'  `dp2' | `db3'  `dp3' | `db4'  `dp4'   |"
local postotlist_formatted ///
  "     Combined | `pb1'  `pp1' | `pb2'  `pp2' | `pb3'  `pp3' | `pb4'  `pp4'   |"
local neuttotlist_formatted ///
  "     Combined | `nb1'  `np1' | `nb2'  `np2' | `nb3'  `np3' | `nb4'  `np4'   |"

local header1 : display ///
  "----------------------------------------------------------------------------"
local header2 : display ///
  "              | Wilson-      | Social       | Economic     | Left-Right     |"
local header3 : display ///
  "              | Patterson    | Conservatism | Conservatism | Self-Placement |"
local header4 : display ///
  "              |--------------+--------------+--------------+----------------|"
local header5 : display ///
  "              | beta   p-val | beta   p-val | beta   p-val | beta   p-val   |"
local header6 : display ///
  "--------------+--------------+--------------+--------------+----------------|"
local thead : display ///
  " Threatening: |              |              |              |                |"
local dhead : display ///
  " Disgusting:  |              |              |              |                |"
local phead : display ///
  " Positive     |              |              |              |                |"
local nhead : display ///
  " Neutral      |              |              |              |                |"

local varlist ///
  " "`threatuslist_formatted'" "`threatdklist_formatted'" "`threattotlist_formatted'" "`disgustuslist_formatted'" "`disgustdklist_formatted'" "`disgusttotlist_formatted'" "`posuslist_formatted'" "`posdklist_formatted'" "`postotlist_formatted'" "`neutuslist_formatted'" "`neutdklist_formatted'" "`neuttotlist_formatted'" "
local i = 0
foreach var of local varlist {
  local i = `i' + 1
  if (`i' == 1) {
    display "`header1'"
    display "`header2'"
    display "`header3'"
    display "`header4'"
    display "`header5'"
    display "`header6'"
    display "`thead'"
  }
  if (`i' == 4) {
    display "`header6'"
	display "`dhead'"
  }
  if (`i' == 7) {
    display "`header6'"
	display "`phead'"
  }
  if (`i' == 10) {
    display "`header6'"
	display "`nhead'"
  }
  display "`var'"
  if (`i' == 12) {
    display "`header1'"
  }
}

eststo clear

** comparisons of estimates between models using seemingly unrelated regressions
* Denmark
sureg (z_soccon z_threat_change female age education income_hh ) ///
  (z_ecocon z_threat_change female age education income_hh ) if ///
  outlier_us != 1 & country == 1
test [z_soccon]z_threat_change = [z_ecocon]z_threat_change

* US
sureg (z_soccon z_threat_change female age education income_hh ) ///
  (z_ecocon z_threat_change female age education income_hh ) if ///
  outlier_us != 1 & country == 0
test [z_soccon]z_threat_change = [z_ecocon]z_threat_change


**********************
** Appendix Table 2D.a
**********************
reg z_wp c.z_threat_change##i.country female age education income_hh if ///
  outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Da, excel aster(se) dec(3) label ///
  title("Table 2D.a: Association Between EDA Responses to Threatening Images and Political Ideology") replace
reg z_soccon c.z_threat_change##i.country female age education income_hh if ///
  outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Da, excel aster(se) dec(3) label ///
  append
reg z_ecocon c.z_threat_change##i.country female age education income_hh if ///
  outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Da, excel aster(se) dec(3) label ///
  append
reg z_leftright_alt c.z_threat_change##i.country female age education ///
  income_hh if outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Da, excel aster(se) dec(3) label ///
  append
**********************

**********************
** Appendix Table 2D.b
**********************
reg z_wp c.z_disgust_change_total##i.country female age education income_hh ///
  if outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Db, excel aster(se) dec(3) label ///
  title("Table 2D.b: Association Between EDA Responses to Disgusting Images and Political Ideology") replace
reg z_soccon c.z_disgust_change_total##i.country female age education ///
  income_hh if outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Db, excel aster(se) dec(3) label ///
  append
reg z_ecocon c.z_disgust_change_total##i.country female age education ///
  income_hh if outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Db, excel aster(se) dec(3) label ///
  append
reg z_leftright_alt c.z_disgust_change_total##i.country female age education ///
  income_hh if outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Db, excel aster(se) dec(3) label ///
  append
**********************

**********************
** Appendix Table 2D.c
**********************
reg z_wp c.z_neutral_change_total##i.country female age education income_hh ///
  if outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Dc, excel aster(se) dec(3) label ///
  title("Table 2D.c: Association Between EDA Responses to Neutral Images and Political Ideology") replace
reg z_soccon c.z_neutral_change_total##i.country female age education ///
  income_hh if outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Dc, excel aster(se) dec(3) label ///
  append
reg z_ecocon c.z_neutral_change_total##i.country female age education ///
  income_hh if outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Dc, excel aster(se) dec(3) label ///
  append
reg z_leftright_alt c.z_neutral_change_total##i.country female age education ///
  income_hh if outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Dc, excel aster(se) dec(3) label ///
  append
**********************

**********************
** Appendix Table 2D.d
**********************
reg z_wp c.z_pos_change_total##i.country female age education income_hh if ///
  outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Dd, excel aster(se) dec(3) label ///
  title("Table 2D.d: Association Between EDA Responses to Positive Images and Political Ideology") replace
reg z_soccon c.z_pos_change_total##i.country female age education income_hh ///
  if outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Dd, excel aster(se) dec(3) label ///
  append
reg z_ecocon c.z_pos_change_total##i.country female age education income_hh ///
  if outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Dd, excel aster(se) dec(3) label ///
  append
reg z_leftright_alt c.z_pos_change_total##i.country female age education ///
  income_hh if outlier_us != 1, robust
outreg2 using Tables/OnlineAppendixTable02Dd, excel aster(se) dec(3) label ///
  append
**********************

********************************************************************************
** Analyses based on self reports
********************************************************************************

**************
** Appendix 2C
**************

** recoding self-reported measures to subset of images
* valence of emotional reaction
rename arosvy_q8 plate_neutral_valence
rename arosvy_q13 mug_neutral_valence
rename arosvy_q17 worms_disgust_valence
rename arosvy_q21 baby_tumor_disgust_valence
rename arosvy_q25 knife_threat_valence
rename arosvy_q29 skydiving_pos_valence
rename arosvy_q33 sex_pos_valence
rename arosvy_q37 snake_threat_valence
gen SR_threat_valence = (knife_threat_valence + snake_threat_valence - 2) / 16
gen SR_disgust_valence = ///
  (worms_disgust_valence + baby_tumor_disgust_valence - 2) / 16
gen SR_positive_valence = (skydiving_pos_valence + sex_pos_valence - 2) / 16
gen SR_neutral_valence = (plate_neutral_valence + mug_neutral_valence - 2) / 16

* self-reported strength of arousal
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

** validity checks
* threat valence
alpha knife_threat_valence snake_threat_valence if outlier_us != 1, std
bysort country: alpha knife_threat_valence snake_threat_valence if ///
  outlier_us != 1
* disgust valence
alpha worms_disgust_valence baby_tumor_disgust_valence if outlier_us != 1, std
bysort country: alpha worms_disgust_valence baby_tumor_disgust_valence if ///
  outlier_us != 1 
* positive valence
alpha skydiving_pos_valence sex_pos_valence if outlier_us != 1, std
bysort country: alpha skydiving_pos_valence sex_pos_valence if outlier_us != 1
* neutral valence
alpha plate_neutral_valence mug_neutral_valence if outlier_us != 1, std
bysort country: alpha plate_neutral_valence mug_neutral_valence if ///
  outlier_us != 1 

* valence correlations
pwcorr SR_threat_valence SR_disgust_valence SR_positive_valence ///
  SR_neutral_valence if outlier_us != 1

* threat arousal strength
alpha knife_threat snake_threat if outlier_us != 1, std
bysort country: alpha knife_threat snake_threat if outlier_us != 1
* disgust strength
alpha worms_disgust baby_tumor_disgust if outlier_us != 1, std
bysort country: alpha worms_disgust baby_tumor_disgust if outlier_us != 1
* positive strength
alpha skydiving_pos sex_pos if outlier_us != 1, std
bysort country: alpha skydiving_pos sex_pos if outlier_us != 1
* neutral strength
alpha plate_neutral mug_neutral if outlier_us != 1, std
bysort country: alpha plate_neutral mug_neutral if outlier_us != 1

* strength correlations
pwcorr SR_threat SR_disgust SR_positive SR_neutral if outlier_us != 1

* correlations between valence and arousal ratings
pwcorr SR_threat SR_threat_valence if outlier_us != 1
pwcorr SR_disgust SR_disgust_valence if outlier_us != 1
pwcorr SR_positive SR_positive_valence if outlier_us != 1
pwcorr SR_neutral SR_neutral_valence if outlier_us != 1

* create measures that combine valence and arousal measures
egen SR_threat_tot = rmean(knife_threat_valence snake_threat_valence ///
  knife_threat snake_threat)
egen SR_disgust_tot = rmean(worms_disgust_valence baby_tumor_disgust_valence ///
  worms_disgust baby_tumor_disgust)
egen SR_positive_tot = rmean(skydiving_pos_valence sex_pos_valence ///
  skydiving_pos sex_pos)
egen SR_neutral_tot = rmean(plate_neutral_valence  mug_neutral_valence ///
  plate_neutral  mug_neutral)

* create EDA responses for subset of images for which we have self-reported
*   valence and arousal ratings for
egen eda_threat = rmean(threat_pic3_change threat_pic6_change)
egen eda_disgust = rmean(disgust_pic20_change disgust_pic21_change)
egen eda_positive = rmean(pos_pic4_change pos_pic5_change)
egen eda_neutral = rmean(neutral_pic11_change neutral_pic14_change)

* correlations between EDA responses and self-reported valence
bysort country: pwcorr eda_threat SR_threat_valence if outlier_us != 1, sig 
bysort country: pwcorr eda_disgust SR_disgust_valence if outlier_us != 1, sig
bysort country: pwcorr eda_positive SR_positive_valence if outlier_us != 1, sig
bysort country: pwcorr eda_neutral SR_neutral_valence if outlier_us != 1, sig

* correlations between EDA responses and self-reporte
bysort country: pwcorr eda_threat SR_threat if outlier_us != 1, sig
bysort country: pwcorr eda_disgust SR_disgust if outlier_us != 1, sig
bysort country: pwcorr eda_positive SR_positive if outlier_us != 1, sig
bysort country: pwcorr eda_neutral SR_neutral if outlier_us != 1, sig


***************************
** Figure 2 and Figure 2E.a
***************************
* standardize variables
foreach var in SR_threat SR_disgust SR_positive SR_neutral SR_threat_valence ///
  SR_disgust_valence SR_positive_valence SR_neutral_valence SR_threat_tot ///
  SR_disgust_tot SR_positive_tot SR_neutral_tot {
  egen z_`var' = std(`var') 
}

gen SR_threat_dk_v = z_SR_threat_valence if country == 1
label var SR_threat_dk_v "Danes"
gen SR_threat_us_v = z_SR_threat_valence if country == 0
label var SR_threat_us_v "Americans"
gen SR_threat_total_v = z_SR_threat_valence
label var SR_threat_total_v "Combined"
gen SR_disgust_dk_v = z_SR_disgust_valence if country == 1
label var SR_disgust_dk_v "Danes"
gen SR_disgust_us_v = z_SR_disgust_valence if country == 0
label var SR_disgust_us_v "Americans"
gen SR_disgust_total_v = z_SR_disgust_valence
label var SR_disgust_total_v "Combined"
gen SR_positive_dk_v = z_SR_positive_valence if country == 1
label var SR_positive_dk_v "Danes"
gen SR_positive_us_v = z_SR_positive_valence if country == 0
label var SR_positive_us_v "Americans"
gen SR_positive_total_v = z_SR_positive_valence
label var SR_positive_total_v "Combined"
gen SR_neutral_dk_v = z_SR_neutral_valence if country == 1
label var SR_neutral_dk_v "Danes"
gen SR_neutral_us_v = z_SR_neutral_valence if country == 0
label var SR_neutral_us_v "Americans"
gen SR_neutral_total_v = z_SR_neutral_valence
label var SR_neutral_total_v "Combined"

foreach var of varlist SR_threat_dk_v SR_threat_us_v SR_threat_total_v {
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store e1_`var'
  local e1_`var'_b : display %6.2f _b[`var']
  local e1_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local e1_`var'_n `e(N)'
}
foreach var of varlist SR_disgust_dk_v SR_disgust_us_v SR_disgust_total_v {
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store f1_`var'
  local f1_`var'_b : display %6.2f _b[`var']
  local f1_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local f1_`var'_n `e(N)'
}
foreach var of varlist SR_positive_dk_v SR_positive_us_v SR_positive_total_v {
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store g1_`var'
  local g1_`var'_b : display %6.2f _b[`var']
  local g1_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local g1_`var'_n `e(N)'
}
foreach var of varlist SR_neutral_dk_v SR_neutral_us_v SR_neutral_total_v {
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store h1_`var'
  local h1_`var'_b : display %6.2f _b[`var']
  local h1_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local h1_`var'_n `e(N)'
}
foreach var of varlist SR_threat_dk_v SR_threat_us_v SR_threat_total_v {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store e2_`var'
  local e2_`var'_b : display %6.2f _b[`var']
  local e2_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local e2_`var'_n `e(N)'
}
foreach var of varlist SR_disgust_dk_v SR_disgust_us_v SR_disgust_total_v {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store f2_`var'
  local f2_`var'_b : display %6.2f _b[`var']
  local f2_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local f2_`var'_n `e(N)'
}
foreach var of varlist SR_positive_dk_v SR_positive_us_v SR_positive_total_v {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store g2_`var'
  local g2_`var'_b : display %6.2f _b[`var']
  local g2_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local g2_`var'_n `e(N)'
}
foreach var of varlist SR_neutral_dk_v SR_neutral_us_v SR_neutral_total_v {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store h2_`var'
  local h2_`var'_b : display %6.2f _b[`var']
  local h2_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local h2_`var'_n `e(N)'
}
foreach var of varlist SR_threat_dk_v SR_threat_us_v SR_threat_total_v {
  quietly regress z_ecocon `var' female age education income_hh  country if ///
    outlier_us != 1, robust
  estimates store e3_`var'
  local e3_`var'_b : display %6.2f _b[`var']
  local e3_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local e3_`var'_n `e(N)'
}
foreach var of varlist SR_disgust_dk_v SR_disgust_us_v SR_disgust_total_v {
  quietly regress z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store f3_`var'
  local f3_`var'_b : display %6.2f _b[`var']
  local f3_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local f3_`var'_n `e(N)'
}
foreach var of varlist SR_positive_dk_v SR_positive_us_v SR_positive_total_v {
  quietly regress z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store g3_`var'
  local g3_`var'_b : display %6.2f _b[`var']
  local g3_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local g3_`var'_n `e(N)'
}
foreach var of varlist SR_neutral_dk_v SR_neutral_us_v SR_neutral_total_v {
  quietly regress z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store h3_`var'
  local h3_`var'_b : display %6.2f _b[`var']
  local h3_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local h3_`var'_n `e(N)'
}
foreach var of varlist SR_threat_dk_v SR_threat_us_v SR_threat_total_v {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store e4_`var'
  local e4_`var'_b : display %6.2f _b[`var']
  local e4_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local e4_`var'_n `e(N)'
}
foreach var of varlist SR_disgust_dk_v SR_disgust_us_v SR_disgust_total_v {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store f4_`var'
  local f4_`var'_b : display %6.2f _b[`var']
  local f4_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local f4_`var'_n `e(N)'
}
foreach var of varlist SR_positive_dk_v SR_positive_us_v SR_positive_total_v {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store g4_`var'
  local g4_`var'_b : display %6.2f _b[`var']
  local g4_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local g4_`var'_n `e(N)'
}
foreach var of varlist SR_neutral_dk_v SR_neutral_us_v SR_neutral_total_v {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store h4_`var'
  local h4_`var'_b : display %6.2f _b[`var']
  local h4_`var'_p : display %6.3f (2 * ttail(e(df_r), ///
    abs(_b[`var'] / _se[`var'])))
  local h4_`var'_n `e(N)'
}

coefplot e1* f1* g1* h1*, bylabel(Wilson-Patterson) ///
  || e2* f2* g2* h2*, bylabel(Social Conservatism) ///
  || e3* f3* g3* h3*, bylabel(Economic Conservatism) ///
  || e4* f4* g4* h4*, bylabel(Left-Right self-placement) ///
  ||, drop(_cons female age education income_hh country) ///
  xline(0, lcolor(gray)) msymbol(square) color(black) ciopts(lcolor(black)) ///
  byopts(legend(off)) headings(SR_threat_dk_v = "{bf: Threat Self-Report}" ///
  SR_disgust_dk_v = "{bf: Disgust Self-Report}" ///
  SR_positive_dk_v = "{bf: Positive Self-Report}" ///
  SR_neutral_dk_v = "{bf: Neutral Self-Report}",) format(%9.1g) mlabel ///
  mlabposition(10) mlabsize(1.5) grid(none) subtitle(, size(small)) xsize(7) ///
  ysize(10)
graph export Figures/Figure02.pdf, replace
graph export Figures/OnlineAppendix-Figure02Ea.pdf, replace
graph drop _all
graph close

estimates drop e* f* g* h*

***************************

******************************************
** Numbers from the text based on Figure 2
******************************************
local blist `e1_SR_threat_us_v_b' `e1_SR_threat_dk_v_b' ///
  `e1_SR_threat_total_v_b' `f1_SR_disgust_us_v_b' `f1_SR_disgust_dk_v_b' ///
  `f1_SR_disgust_total_v_b' `g1_SR_positive_us_v_b' `g1_SR_positive_dk_v_b' ///
  `g1_SR_positive_total_v_b' `h1_SR_neutral_us_v_b' `h1_SR_neutral_dk_v_b' ///
  `h1_SR_neutral_total_v_b' `e2_SR_threat_us_v_b' `e2_SR_threat_dk_v_b' ///
  `e2_SR_threat_total_v_b' `f2_SR_disgust_us_v_b' `f2_SR_disgust_dk_v_b' ///
  `f2_SR_disgust_total_v_b' `g2_SR_positive_us_v_b' `g2_SR_positive_dk_v_b' ///
  `g2_SR_positive_total_v_b' `h2_SR_neutral_us_v_b' `h2_SR_neutral_dk_v_b' ///
  `h2_SR_neutral_total_v_b' `e3_SR_threat_us_v_b' `e3_SR_threat_dk_v_b' ///
  `e3_SR_threat_total_v_b' `f3_SR_disgust_us_v_b' `f3_SR_disgust_dk_v_b' ///
  `f3_SR_disgust_total_v_b' `g3_SR_positive_us_v_b' `g3_SR_positive_dk_v_b' ///
  `g3_SR_positive_total_v_b' `h3_SR_neutral_us_v_b' `h3_SR_neutral_dk_v_b' ///
  `h3_SR_neutral_total_v_b' `e4_SR_threat_us_v_b' `e4_SR_threat_dk_v_b' ///
  `e4_SR_threat_total_v_b' `f4_SR_disgust_us_v_b' `f4_SR_disgust_dk_v_b' ///
  `f4_SR_disgust_total_v_b' `g4_SR_positive_us_v_b' `g4_SR_positive_dk_v_b' ///
  `g4_SR_positive_total_v_b' `h4_SR_neutral_us_v_b' `h4_SR_neutral_dk_v_b' ///
  `h4_SR_neutral_total_v_b'

local threatuslist_b ///
  "`e1_SR_threat_us_v_b' `e2_SR_threat_us_v_b' `e3_SR_threat_us_v_b' `e4_SR_threat_us_v_b'"
local threatdklist_b ///
  "`e1_SR_threat_dk_v_b' `e2_SR_threat_dk_v_b' `e3_SR_threat_dk_v_b' `e4_SR_threat_dk_v_b'"
local threattotlist_b ///
  "`e1_SR_threat_total_v_b' `e2_SR_threat_total_v_b' `e3_SR_threat_total_v_b' `e4_SR_threat_total_v_b'"

local disgustuslist_b ///
  "`f1_SR_disgust_us_v_b' `f2_SR_disgust_us_v_b' `f3_SR_disgust_us_v_b' `f4_SR_disgust_us_v_b'"
local disgustdklist_b ///
  "`f1_SR_disgust_dk_v_b' `f2_SR_disgust_dk_v_b' `f3_SR_disgust_dk_v_b' `f4_SR_disgust_dk_v_b'"
local disgusttotlist_b ///
  "`f1_SR_disgust_total_v_b' `f2_SR_disgust_total_v_b' `f3_SR_disgust_total_v_b' `f4_SR_disgust_total_v_b'"

local posuslist_b ///
  "`g1_SR_positive_us_v_b' `g2_SR_positive_us_v_b' `g3_SR_positive_us_v_b' `g4_SR_positive_us_v_b'"
local posdklist_b ///
  "`g1_SR_positive_dk_v_b' `g2_SR_positive_dk_v_b' `g3_SR_positive_dk_v_b' `g4_SR_positive_dk_v_b'"
local postotlist_b ///
  "`g1_SR_positive_total_v_b' `g2_SR_positive_total_v_b' `g3_SR_positive_total_v_b' `g4_SR_positive_total_v_b'"

local neutuslist_b ///
  "`h1_SR_neutral_us_v_b' `h2_SR_neutral_us_v_b' `h3_SR_neutral_us_v_b' `h4_SR_neutral_us_v_b'"
local neutdklist_b ///
  "`h1_SR_neutral_dk_v_b' `h2_SR_neutral_dk_v_b' `h3_SR_neutral_dk_v_b' `h4_SR_neutral_dk_v_b'"
local neuttotlist_b ///
  "`h1_SR_neutral_total_v_b' `h2_SR_neutral_total_v_b' `h3_SR_neutral_total_v_b' `h4_SR_neutral_total_v_b'"

local threatuslist_p ///
  "`e1_SR_threat_us_v_p' `e2_SR_threat_us_v_p' `e3_SR_threat_us_v_p' `e4_SR_threat_us_v_p'"
local threatdklist_p ///
  "`e1_SR_threat_dk_v_p' `e2_SR_threat_dk_v_p' `e3_SR_threat_dk_v_p' `e4_SR_threat_dk_v_p'"
local threattotlist_p ///
  "`e1_SR_threat_total_v_p' `e2_SR_threat_total_v_p' `e3_SR_threat_total_v_p' `e4_SR_threat_total_v_p'"

local disgustuslist_p ///
  "`f1_SR_disgust_us_v_p' `f2_SR_disgust_us_v_p' `f3_SR_disgust_us_v_p' `f4_SR_disgust_us_v_p'"
local disgustdklist_p ///
  "`f1_SR_disgust_dk_v_p' `f2_SR_disgust_dk_v_p' `f3_SR_disgust_dk_v_p' `f4_SR_disgust_dk_v_p'"
local disgusttotlist_p ///
  "`f1_SR_disgust_total_v_p' `f2_SR_disgust_total_v_p' `f3_SR_disgust_total_v_p' `f4_SR_disgust_total_v_p'"

local posuslist_p ///
  "`g1_SR_positive_us_v_p' `g2_SR_positive_us_v_p' `g3_SR_positive_us_v_p' `g4_SR_positive_us_v_p'"
local posdklist_p ///
  "`g1_SR_positive_dk_v_p' `g2_SR_positive_dk_v_p' `g3_SR_positive_dk_v_p' `g4_SR_positive_dk_v_p'"
local postotlist_p ///
  "`g1_SR_positive_total_v_p' `g2_SR_positive_total_v_p' `g3_SR_positive_total_v_p' `g4_SR_positive_total_v_p'"

local neutuslist_p ///
  "`h1_SR_neutral_us_v_p' `h2_SR_neutral_us_v_p' `h3_SR_neutral_us_v_p' `h4_SR_neutral_us_v_p'"
local neutdklist_p ///
  "`h1_SR_neutral_dk_v_p' `h2_SR_neutral_dk_v_p' `h3_SR_neutral_dk_v_p' `h4_SR_neutral_dk_v_p'"
local neuttotlist_p ///
  "`h1_SR_neutral_total_v_p' `h2_SR_neutral_total_v_p' `h3_SR_neutral_total_v_p' `h4_SR_neutral_total_v_p'"

forvalues i = 1/4 {
  local tb`i'temp : word `i' of `threatuslist_b'
  
  if (`tb`i'temp' == 0) {
    local tb`i'temp : display %6.2f `tb`i'temp' * 0
    local tb`i'temp : display substr("`tb`i'temp'", 2, 5)
  }

  if (`tb`i'temp' >= 0) {
    local tb`i' : display " `tb`i'temp'"
  }
  else {
    local tb`i' : display "`tb`i'temp'"
  }
  local db`i'temp : word `i' of `disgustuslist_b'

  if (`db`i'temp' == 0) {
    local db`i'temp : display %6.2f `db`i'temp' * 0
    local db`i'temp : display substr("`db`i'temp'", 2, 5)
  }

  if (`db`i'temp' >= 0) {
    local db`i' : display " `db`i'temp'"
  }
  else {
    local db`i' : display "`db`i'temp'"
  }
  local pb`i'temp : word `i' of `posuslist_b'
  
  if (`pb`i'temp' == 0) {
    local pb`i'temp : display %6.2f `pb`i'temp' * 0
    local pb`i'temp : display substr("`pb`i'temp'", 2, 5)
  }

  if (`pb`i'temp' >= 0) {
    local pb`i' : display " `pb`i'temp'"
  }
  else {
    local pb`i' : display "`pb`i'temp'"
  }
  local nb`i'temp : word `i' of `neutuslist_b'

  if (`nb`i'temp' == 0) {
    local nb`i'temp : display %6.2f `nb`i'temp' * 0
    local nb`i'temp : display substr("`nb`i'temp'", 2, 5)
  }

  if (`nb`i'temp' >= 0) {
    local nb`i' : display " `nb`i'temp'"
  }
  else {
    local nb`i' : display "`nb`i'temp'"
  }
  local tp`i' : word `i' of `threatuslist_p'
  local dp`i' : word `i' of `disgustuslist_p'
  local pp`i' : word `i' of `posuslist_p'
  local np`i' : word `i' of `neutuslist_p'
}
local threatuslist_formatted ///
  "     U.S.     | `tb1'  `tp1' | `tb2'  `tp2' | `tb3'  `tp3' | `tb4'  `tp4'   |"
local disgustuslist_formatted ///
  "     U.S.     | `db1'  `dp1' | `db2'  `dp2' | `db3'  `dp3' | `db4'  `dp4'   |"
local posuslist_formatted ///
  "     U.S.     | `pb1'  `pp1' | `pb2'  `pp2' | `pb3'  `pp3' | `pb4'  `pp4'   |"
local neutuslist_formatted ///
  "     U.S.     | `nb1'  `np1' | `nb2'  `np2' | `nb3'  `np3' | `nb4'  `np4'   |"

forvalues i = 1/4 {
  local tb`i'temp : word `i' of `threatdklist_b'

  if (`tb`i'temp' == 0) {
    local tb`i'temp : display %6.2f `tb`i'temp' * 0
    local tb`i'temp : display substr("`tb`i'temp'", 2, 5)
  }

  if (`tb`i'temp' >= 0) {
    local tb`i' : display " `tb`i'temp'"
  }
  else {
    local tb`i' : display "`tb`i'temp'"
  }
  local db`i'temp : word `i' of `disgustdklist_b'

  if (`db`i'temp' == 0) {
    local db`i'temp : display %6.2f `db`i'temp' * 0
    local db`i'temp : display substr("`db`i'temp'", 2, 5)
  }

  if (`db`i'temp' >= 0) {
    local db`i' : display " `db`i'temp'"
  }
  else {
    local db`i' : display "`db`i'temp'"
  }
  local pb`i'temp : word `i' of `posdklist_b'

  if (`pb`i'temp' == 0) {
    local pb`i'temp : display %6.2f `pb`i'temp' * 0
    local pb`i'temp : display substr("`pb`i'temp'", 2, 5)
  }

  if (`pb`i'temp' >= 0) {
    local pb`i' : display " `pb`i'temp'"
  }
  else {
    local pb`i' : display "`pb`i'temp'"
  }
  local nb`i'temp : word `i' of `neutdklist_b'

  if (`nb`i'temp' == 0) {
    local nb`i'temp : display %6.2f `nb`i'temp' * 0
    local nb`i'temp : display substr("`nb`i'temp'", 3, 6)
  }

  if (`nb`i'temp' >= 0) {
    local nb`i' : display " `nb`i'temp'"
  }
  else {
    local nb`i' : display "`nb`i'temp'"
  }
  local tp`i' : word `i' of `threatdklist_p'
  local dp`i' : word `i' of `disgustdklist_p'
  local pp`i' : word `i' of `posdklist_p'
  local np`i' : word `i' of `neutdklist_p'
}
local threatdklist_formatted ///
  "     Denmark  | `tb1'  `tp1' | `tb2'  `tp2' | `tb3'  `tp3' | `tb4'  `tp4'   |"
local disgustdklist_formatted ///
  "     Denmark  | `db1'  `dp1' | `db2'  `dp2' | `db3'  `dp3' | `db4'  `dp4'   |"
local posdklist_formatted ///
  "     Denmark  | `pb1'  `pp1' | `pb2'  `pp2' | `pb3'  `pp3' | `pb4'  `pp4'   |"
local neutdklist_formatted ///
  "     Denmark  | `nb1'  `np1' | `nb2'  `np2' | `nb3'  `np3' | `nb4'  `np4'   |"

forvalues i = 1/4 {
  local tb`i'temp : word `i' of `threattotlist_b'

  if (`tb`i'temp' == 0) {
    local tb`i'temp : display %6.2f `tb`i'temp' * 0
    local tb`i'temp : display substr("`tb`i'temp'", 2, 5)
  }

  if (`tb`i'temp' > 0) {
    local tb`i' : display " `tb`i'temp'"
  }
  else {
    local tb`i' : display "`tb`i'temp'"
  }
  local db`i'temp : word `i' of `disgusttotlist_b'

  if (`db`i'temp' == 0) {
    local db`i'temp : display %6.2f `db`i'temp' * 0
    local db`i'temp : display substr("`db`i'temp'", 2, 5)
  }

  if (`db`i'temp' > 0) {
    local db`i' : display " `db`i'temp'"
  }
  else {
    local db`i' : display "`db`i'temp'"
  }
  local pb`i'temp : word `i' of `postotlist_b'

  if (`pb`i'temp' == 0) {
    local pb`i'temp : display %6.2f `pb`i'temp' * 0
    local pb`i'temp : display substr("`pb`i'temp'", 2, 5)
  }

  if (`pb`i'temp' > 0) {
    local pb`i' : display " `pb`i'temp'"
  }
  else {
    local pb`i' : display "`pb`i'temp'"
  }
  local nb`i'temp : word `i' of `neuttotlist_b'

  if (`nb`i'temp' == 0) {
    local nb`i'temp : display %6.2f `nb`i'temp' * 0
    local nb`i'temp : display substr("`nb`i'temp'", 2, 5)
  }

  if (`nb`i'temp' > 0) {
    local nb`i' : display " `nb`i'temp'"
  }
  else {
    local nb`i' : display "`nb`i'temp'"
  }
  local tp`i' : word `i' of `threattotlist_p'
  local dp`i' : word `i' of `disgusttotlist_p'
  local pp`i' : word `i' of `postotlist_p'
  local np`i' : word `i' of `neuttotlist_p'
}
local threattotlist_formatted ///
  "     Combined | `tb1'  `tp1' | `tb2'  `tp2' | `tb3'  `tp3' | `tb4'  `tp4'   |"
local disgusttotlist_formatted ///
  "     Combined | `db1'  `dp1' | `db2'  `dp2' | `db3'  `dp3' | `db4'  `dp4'   |"
local postotlist_formatted ///
  "     Combined | `pb1'  `pp1' | `pb2'  `pp2' | `pb3'  `pp3' | `pb4'  `pp4'   |"
local neuttotlist_formatted ///
  "     Combined | `nb1'  `np1' | `nb2'  `np2' | `nb3'  `np3' | `nb4'  `np4'   |"

local header1 : display ///
  "----------------------------------------------------------------------------"
local header2 : display ///
  "              | Wilson-      | Social       | Economic     | Left-Right     |"
local header3 : display ///
  "              | Patterson    | Conservatism | Conservatism | Self-Placement |"
local header4 : display ///
  "              |--------------+--------------+--------------+----------------|"
local header5 : display ///
  "              | beta   p-val | beta   p-val | beta   p-val | beta   p-val   |"
local header6 : display ///
  "--------------+--------------+--------------+--------------+----------------|"
local thead : display ///
  " Threatening: |              |              |              |                |"
local dhead : display ///
  " Disgusting:  |              |              |              |                |"
local phead : display ///
  " Positive     |              |              |              |                |"
local nhead : display ///
  " Neutral      |              |              |              |                |"

local varlist ///
  " "`threatuslist_formatted'" "`threatdklist_formatted'" "`threattotlist_formatted'" "`disgustuslist_formatted'" "`disgustdklist_formatted'" "`disgusttotlist_formatted'" "`posuslist_formatted'" "`posdklist_formatted'" "`postotlist_formatted'" "`neutuslist_formatted'" "`neutdklist_formatted'" "`neuttotlist_formatted'" "
local i = 0
foreach var of local varlist {
  local i = `i' + 1
  if (`i' == 1) {
    display "`header1'"
    display "`header2'"
    display "`header3'"
    display "`header4'"
    display "`header5'"
    display "`header6'"
    display "`thead'"
  }
  if (`i' == 4) {
    display "`header6'"
	display "`dhead'"
  }
  if (`i' == 7) {
    display "`header6'"
	display "`phead'"
  }
  if (`i' == 10) {
    display "`header6'"
	display "`nhead'"
  }
  display "`var'"
  if (`i' == 12) {
    display "`header1'"
  }
}

** comparisons of estimates between models using seemingly unrelated regressions
* Denmark
sureg (z_soccon z_SR_threat_valence female age education income_hh ) ///
  (z_ecocon z_SR_threat_valence female age education income_hh ) if ///
  outlier_us != 1 & country == 1
test [z_soccon]z_SR_threat_valence = [z_ecocon]z_SR_threat_valence
* US
sureg (z_soccon z_SR_threat_valence female age education income_hh ) ///
  (z_ecocon z_SR_threat_valence female age education income_hh ) if ///
  outlier_us != 1 & country == 0
test [z_soccon]z_SR_threat_valence = [z_ecocon]z_SR_threat_valence


**************
** Appendix 2E
**************

**************
** Figure 2E.b
**************
gen SR_threat_dk = z_SR_threat if country == 1
label var SR_threat_dk "Danes"
gen SR_threat_us = z_SR_threat if country == 0
label var SR_threat_us "Americans"
gen SR_threat_total = z_SR_threat
label var SR_threat_total "Combined"
gen SR_disgust_dk = z_SR_disgust if country == 1
label var SR_disgust_dk "Danes"
gen SR_disgust_us = z_SR_disgust if country == 0
label var SR_disgust_us "Americans"
gen SR_disgust_total = z_SR_disgust
label var SR_disgust_total "Combined"
gen SR_positive_dk = z_SR_positive if country == 1
label var SR_positive_dk "Danes"
gen SR_positive_us = z_SR_positive if country == 0
label var SR_positive_us "Americans"
gen SR_positive_total = z_SR_positive
label var SR_positive_total "Combined"
gen SR_neutral_dk = z_SR_neutral if country == 1
label var SR_neutral_dk "Danes"
gen SR_neutral_us = z_SR_neutral if country == 0
label var SR_neutral_us "Americans"
gen SR_neutral_total = z_SR_neutral
label var SR_neutral_total "Combined"

foreach var of varlist SR_threat_dk SR_threat_us SR_threat_total {
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store e1_`var'
}
foreach var of varlist SR_disgust_dk SR_disgust_us SR_disgust_total {
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store f1_`var'
}
foreach var of varlist SR_positive_dk SR_positive_us SR_positive_total {
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store g1_`var'
}
foreach var of varlist SR_neutral_dk SR_neutral_us SR_neutral_total {
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store h1_`var'
}
foreach var of varlist SR_threat_dk SR_threat_us SR_threat_total {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store e2_`var'
}
foreach var of varlist SR_disgust_dk SR_disgust_us SR_disgust_total {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store f2_`var'
}
foreach var of varlist SR_positive_dk SR_positive_us SR_positive_total {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store g2_`var'
}
foreach var of varlist SR_neutral_dk SR_neutral_us SR_neutral_total {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store h2_`var'
}
foreach var of varlist SR_threat_dk SR_threat_us SR_threat_total {
  quietly regress z_ecocon `var' female age education income_hh  country if ///
    outlier_us != 1, robust
  estimates store e3_`var'
}
foreach var of varlist SR_disgust_dk SR_disgust_us SR_disgust_total {
  quietly regress z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store f3_`var'
}
foreach var of varlist SR_positive_dk SR_positive_us SR_positive_total {
  quietly regress z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store g3_`var'
}
foreach var of varlist SR_neutral_dk SR_neutral_us SR_neutral_total {
  quietly regress z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store h3_`var'
}
foreach var of varlist SR_threat_dk SR_threat_us SR_threat_total {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store e4_`var'
}
foreach var of varlist SR_disgust_dk SR_disgust_us SR_disgust_total {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store f4_`var'
}
foreach var of varlist SR_positive_dk SR_positive_us SR_positive_total {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store g4_`var'
}
foreach var of varlist SR_neutral_dk SR_neutral_us SR_neutral_total {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store h4_`var'
}

coefplot e1* f1* g1* h1*, bylabel(Wilson-Patterson) ///
  || e2* f2* g2* h2*, bylabel(Social Conservatism) ///
  || e3* f3* g3* h3*, bylabel(Economic Conservatism) ///
  || e4* f4* g4* h4*, bylabel(Left-Right self-placement) ///
  ||, drop(_cons female age education income_hh country) ///
  xline(0, lcolor(gray)) msymbol(square) color(black) ciopts(lcolor(black)) ///
  byopts(legend(off)) headings(SR_threat_dk = "{bf: Threat Self-Report}" ///
  SR_disgust_dk = "{bf: Disgust Self-Report}" ///
  SR_positive_dk = "{bf: Positive Self-Report}" ///
  SR_neutral_dk = "{bf: Neutral Self-Report}",) format(%9.1g) mlabel ///
  mlabposition(10) mlabsize(1.5) grid(none) subtitle(, size(small)) xsize(7) ///
  ysize(10)
graph export Figures/OnlineAppendix-Figure02Eb.pdf, replace
graph drop _all
graph close

estimates drop e* f* g* h*

***********


************************************************************************
** Additional figure not presented in text: combined arousal and valence
************************************************************************
gen SR_threat_dk_tot = SR_threat_tot if country == 1
label var SR_threat_dk_tot "Danes"
gen SR_threat_us_tot = SR_threat_tot if country == 0
label var SR_threat_us_tot "Americans"
gen SR_threat_total_tot = SR_threat_tot
label var SR_threat_total_tot "Combined"
gen SR_disgust_dk_tot = SR_disgust_tot if country == 1
label var SR_disgust_dk_tot "Danes"
gen SR_disgust_us_tot = SR_disgust_tot if country == 0
label var SR_disgust_us_tot "Americans"
gen SR_disgust_total_tot = SR_disgust_tot
label var SR_disgust_total_tot "Combined"
gen SR_positive_dk_tot = SR_positive_tot if country == 1
label var SR_positive_dk_tot "Danes"
gen SR_positive_us_tot = SR_positive_tot if country == 0
label var SR_positive_us_tot "Americans"
gen SR_positive_total_tot = SR_positive_tot
label var SR_positive_total_tot "Combined"
gen SR_neutral_dk_tot = SR_neutral_tot if country == 1
label var SR_neutral_dk_tot "Danes"
gen SR_neutral_us_tot = SR_neutral_tot if country == 0
label var SR_neutral_us_tot "Americans"
gen SR_neutral_total_tot = SR_neutral_tot
label var SR_neutral_total_tot "Combined"

foreach var of varlist SR_threat_dk_tot SR_threat_us_tot SR_threat_total_tot {
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store e1_`var'
}
foreach var of varlist SR_disgust_dk_tot SR_disgust_us_tot ///
  SR_disgust_total_tot {
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store f1_`var'
}
foreach var of varlist SR_positive_dk_tot SR_positive_us_tot ///
  SR_positive_total_tot {
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store g1_`var'
}
foreach var of varlist SR_neutral_dk_tot SR_neutral_us_tot ///
  SR_neutral_total_tot {
  quietly regress z_wp `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store h1_`var'
}
foreach var of varlist SR_threat_dk_tot SR_threat_us_tot SR_threat_total_tot {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store e2_`var'
}
foreach var of varlist SR_disgust_dk_tot SR_disgust_us_tot ///
  SR_disgust_total_tot {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store f2_`var'
}
foreach var of varlist SR_positive_dk_tot SR_positive_us_tot ///
  SR_positive_total_tot {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store g2_`var'
}
foreach var of varlist SR_neutral_dk_tot SR_neutral_us_tot ///
  SR_neutral_total_tot {
  quietly regress z_soccon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store h2_`var'
}
foreach var of varlist SR_threat_dk_tot SR_threat_us_tot SR_threat_total_tot {
  quietly regress z_ecocon `var' female age education income_hh  country if ///
    outlier_us != 1, robust
  estimates store e3_`var'
}
foreach var of varlist SR_disgust_dk_tot SR_disgust_us_tot ///
  SR_disgust_total_tot {
  quietly regress z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store f3_`var'
}
foreach var of varlist SR_positive_dk_tot SR_positive_us_tot ///
  SR_positive_total_tot {
  quietly regress z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store g3_`var'
}
foreach var of varlist SR_neutral_dk_tot SR_neutral_us_tot ///
  SR_neutral_total_tot {
  quietly regress z_ecocon `var' female age education income_hh country if ///
    outlier_us != 1, robust
  estimates store h3_`var'
}
foreach var of varlist SR_threat_dk_tot SR_threat_us_tot SR_threat_total_tot {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store e4_`var'
}
foreach var of varlist SR_disgust_dk_tot SR_disgust_us_tot ///
  SR_disgust_total_tot {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store f4_`var'
}
foreach var of varlist SR_positive_dk_tot SR_positive_us_tot ///
  SR_positive_total_tot {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store g4_`var'
}
foreach var of varlist SR_neutral_dk_tot SR_neutral_us_tot ///
  SR_neutral_total_tot {
  quietly regress z_leftright_alt `var' female age education income_hh ///
    country if outlier_us != 1, robust
  estimates store h4_`var'
}

coefplot e1* f1* g1* h1*, bylabel(Wilson-Patterson) ///
  || e2* f2* g2* h2*, bylabel(Social Conservatism) ///
  || e3* f3* g3* h3*, bylabel(Economic Conservatism) ///
  || e4* f4* g4* h4*, bylabel(Left-Right self-placement) ///
  ||, drop(_cons female age education income_hh country) ///
  xline(0, lcolor(gray)) msymbol(square) color(black) ciopts(lcolor(black)) ///
  byopts(legend(off)) headings(SR_threat_dk_tot = "{bf: Threat Self-Report}" ///
  SR_disgust_dk_tot = "{bf: Disgust Self-Report}" ///
  SR_positive_dk_tot = "{bf: Positive Self-Report}" ///
  SR_neutral_dk_tot = "{bf: Neutral Self-Report}",) format(%9.1g) mlabel ///
  mlabposition(10) mlabsize(1.5) grid(none) subtitle(, size(small)) xsize(7) ///
  ysize(10)
graph export Figures/Supplemental-Figure02-Alt.pdf, replace
graph drop _all
graph close

estimates drop e* f* g* h*

clear

* close log
*log close

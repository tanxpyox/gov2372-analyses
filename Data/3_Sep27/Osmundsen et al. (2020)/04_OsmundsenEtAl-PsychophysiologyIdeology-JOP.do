********************************************************************************
********************************************************************************
****
**** File 4 of the replication archive for:
****   Osmundsen, Mathias, David J. Hendry, Lasse Laustsen, Kevin B. Smith, and
****     Michael Bang Petersen. 2020. "The Psychophysiology of Political
****     Ideology: Replications, Reanalysis, and Recommendations." Journal of
****     Politics XX(X): XXX-XXX.
****
**** Note: This file performs the analysis for the online picture rating survey
**** presented in Online Appendix 3D.
****
********************************************************************************
********************************************************************************

* uncomment the following line and the closing line to create a log file for the
*   current run
*log using Logs/04_OsmundsenEtAl-PsychophysiologyIdeology-JOP-Log.txt, replace

* change the working directory to the location of this .do file (if necessary)
* cd

** preliminaries
* set plot style for all plots
set scheme s1mono

* read in recoded data file
clear
use Data/OsmundsenEtAl-RatingStudy-JOP-Recodes.dta, replace


********************************************************************************
** Appendix 3D
********************************************************************************

* Social conservatism
foreach var of varlist z_threat_strong_rec z_disgust_strong_rec ///
  z_positive_strong_rec z_neutral_strong_rec {
  quietly regress z_social_conservatism `var' profile_education gender ///
    profile_age1, robust
  estimates store d2_`var'
}
foreach var of varlist z_threat_uncomfortable z_disgust_uncomfortable ///
  z_positive_uncomfortable z_neutral_uncomfortable {
  quietly regress z_social_conservatism `var' profile_education gender ///
    profile_age1, robust
  estimates store e2_`var'
}
foreach var of varlist z_threat_happy z_disgust_happy z_positive_happy ///
  z_neutral_happy {
  quietly regress z_social_conservatism `var' profile_education gender ///
    profile_age1, robust
  estimates store f2_`var'
}
foreach var of varlist z_threat_threatened z_disgust_threatened ///
  z_positive_threatened z_neutral_threatened {
  quietly regress z_social_conservatism `var' profile_education gender ///
    profile_age1, robust
  estimates store g2_`var'
}
foreach var of varlist z_threat_disgusted z_disgust_disgusted ///
  z_positive_disgusted z_neutral_disgusted {
  quietly regress z_social_conservatism `var' profile_education gender ///
    profile_age1, robust
  estimates store h2_`var'
}

* Economic conservatism
foreach var of varlist z_threat_strong_rec z_disgust_strong_rec ///
  z_positive_strong_rec z_neutral_strong_rec {
  quietly regress z_economic_conservatism `var' profile_education gender ///
    profile_age1, robust
  estimates store d3_`var'
}
foreach var of varlist z_threat_uncomfortable z_disgust_uncomfortable ///
  z_positive_uncomfortable z_neutral_uncomfortable {
  quietly regress z_economic_conservatism `var' profile_education gender ///
    profile_age1, robust
  estimates store e3_`var'
}
foreach var of varlist z_threat_happy z_disgust_happy z_positive_happy ///
  z_neutral_happy {
  quietly regress z_economic_conservatism `var' profile_education gender ///
    profile_age1, robust
  estimates store f3_`var'
}
foreach var of varlist z_threat_threatened z_disgust_threatened ///
  z_positive_threatened z_neutral_threatened {
  quietly regress z_economic_conservatism `var' profile_education gender ///
    profile_age1, robust
  estimates store g3_`var'
}
foreach var of varlist z_threat_disgusted z_disgust_disgusted ///
  z_positive_disgusted z_neutral_disgusted {
  quietly regress z_economic_conservatism `var' profile_education gender ///
    profile_age1, robust
  estimates store h3_`var'
}

**************
** Figure 3D.a
**************
coefplot g2* h2* e2* f2* d2*, bylabel(Social Conservatism) ///
  || g3* h3* e3* f3* d3*, bylabel(Economic Conservatism) ///
  ||, drop(_cons profile_education gender profile_age1 ) ///
  xline(0, lcolor(gray)) msymbol(square) legend(off) ///
  headings(z_threat_strong_rec = "{bf:Emotion Strength}" ///
  z_threat_uncomfortable = "{bf:Uncomfortable}" ///
  z_threat_happy = "{bf:Happy}" z_threat_threatened="{bf:Threatened}" ///
  z_threat_disgusted="{bf:Disgusted}", ) byopts(legend(off)) format(%9.1g) ///
  mlabel mlabposition(10) mlabsize(1.5) grid(none) xsize(7) ysize(10)
graph export Figures/OnlineAppendix-Figure03Da.pdf, replace
graph drop _all
graph close
**************

estimates drop *

*************
** Table 3D.a
*************

reg z_social_conservatism z_threat_strong_rec profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Da, excel aster(se) dec(3) label ///
  title("Table 3D.a: Association Between Strength of Emotional Reaction to the Different Types of Images and Social Conservatism") replace
reg z_social_conservatism z_disgust_strong_rec profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Da, excel aster(se) dec(3) label ///
  append
reg z_social_conservatism z_positive_strong_rec profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Da, excel aster(se) dec(3) label ///
  append
reg z_social_conservatism z_neutral_strong_rec profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Da, excel aster(se) dec(3) label ///
  append

*************
** Table 3D.b
*************

reg z_economic_conservatism z_threat_strong_rec profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Db, excel aster(se) dec(3) label ///
  title("Table 3D.b: Association Between Strength of Emotional Reaction to the Different Types of Images and Economic Conservatism") replace
reg z_economic_conservatism z_disgust_strong_rec profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Db, excel aster(se) dec(3) label ///
  append
reg z_economic_conservatism z_positive_strong_rec profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Db, excel aster(se) dec(3) label ///
  append
reg z_economic_conservatism z_neutral_strong_rec profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Db, excel aster(se) dec(3) label ///
  append

*************
** Table 3D.c
*************

reg z_social_conservatism z_threat_uncomfortable profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dc, excel aster(se) dec(3) label ///
  title("Table 3D.c: Association Between Feeling Uncomfortable When Viewing the Different Types of Images and Social Conservatism") replace
reg z_social_conservatism z_disgust_uncomfortable profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dc, excel aster(se) dec(3) label ///
  append
reg z_social_conservatism z_positive_uncomfortable profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dc, excel aster(se) dec(3) label ///
  append
reg z_social_conservatism z_neutral_uncomfortable profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dc, excel aster(se) dec(3) label ///
  append

*************
** Table 3D.d
*************

reg z_economic_conservatism z_threat_uncomfortable profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dd, excel aster(se) dec(3) label ///
  title("Table 3D.d: Association Between Feeling Uncomfortable When Viewing the Different Types of Images and Economic Conservatism") replace
reg z_economic_conservatism z_disgust_uncomfortable profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dd, excel aster(se) dec(3) label ///
  append
reg z_economic_conservatism z_positive_uncomfortable profile_education ///
  gender profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dd, excel aster(se) dec(3) label ///
  append
reg z_economic_conservatism z_neutral_uncomfortable profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dd, excel aster(se) dec(3) label ///
  append

*************
** Table 3D.e
*************

reg z_social_conservatism z_threat_happy profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03De, excel aster(se) dec(3) label ///
  title("Table 3D.e: Association Between Feeling Happy When Viewing the Different Types of Images and Social Conservatism") replace
reg z_social_conservatism z_disgust_happy profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03De, excel aster(se) dec(3) label ///
  append
reg z_social_conservatism z_positive_happy profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03De, excel aster(se) dec(3) label ///
  append
reg z_social_conservatism z_neutral_happy profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03De, excel aster(se) dec(3) label ///
  append

*************
** Table 3D.f
*************

reg z_economic_conservatism z_threat_happy profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Df, excel aster(se) dec(3) label ///
  title("Table 3D.f: Association Between Feeling Happy When Viewing the Different Types of Images and Economic Conservatism") replace
reg z_economic_conservatism z_disgust_happy profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Df, excel aster(se) dec(3) label ///
  append
reg z_economic_conservatism z_positive_happy profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Df, excel aster(se) dec(3) label ///
  append
reg z_economic_conservatism z_neutral_happy profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Df, excel aster(se) dec(3) label ///
  append

*************
** Table 3D.g
*************

reg z_social_conservatism z_threat_threatened profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dg, excel aster(se) dec(3) label ///
  title("Table 3D.g: Association Between Feeling Threat When Viewing the Different Types of Images and Social Conservatism") replace
reg z_social_conservatism z_disgust_threatened profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dg, excel aster(se) dec(3) label ///
  append
reg z_social_conservatism z_positive_threatened profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dg, excel aster(se) dec(3) label ///
  append
reg z_social_conservatism z_neutral_threatened profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dg, excel aster(se) dec(3) label ///
  append

*************
** Table 3D.h
*************

reg z_economic_conservatism z_threat_threatened profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dh, excel aster(se) dec(3) label ///
  title("Table 3D.h: Association Between Feeling Threat When Viewing the Different Types of Images and Economic Conservatism") replace
reg z_economic_conservatism z_disgust_threatened profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dh, excel aster(se) dec(3) label ///
  append
reg z_economic_conservatism z_positive_threatened profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dh, excel aster(se) dec(3) label ///
  append
reg z_economic_conservatism z_neutral_threatened profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dh, excel aster(se) dec(3) label ///
  append

*************
** Table 3D.i
*************

reg z_social_conservatism z_threat_disgusted profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Di, excel aster(se) dec(3) label ///
  title("Table 3D.i: Association Between Feeling Disgust When Viewing the Different Types of Images and Social Conservatism") replace
reg z_social_conservatism z_disgust_disgusted profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Di, excel aster(se) dec(3) label ///
  append
reg z_social_conservatism z_positive_disgusted profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Di, excel aster(se) dec(3) label ///
  append
reg z_social_conservatism z_neutral_disgusted profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Di, excel aster(se) dec(3) label ///
  append

*************
** Table 3D.j
*************

reg z_economic_conservatism z_threat_disgusted profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dj, excel aster(se) dec(3) label ///
  title("Table 3D.j: Association Between Feeling Disgust When Viewing the Different Types of Images and Economic Conservatism") replace
reg z_economic_conservatism z_disgust_disgusted profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dj, excel aster(se) dec(3) label ///
  append
reg z_economic_conservatism z_positive_disgusted profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dj, excel aster(se) dec(3) label ///
  append
reg z_economic_conservatism z_neutral_disgusted profile_education gender ///
  profile_age1, robust
outreg2 using Tables/OnlineAppendixTable03Dj, excel aster(se) dec(3) label ///
  append


clear

* close log
*log close

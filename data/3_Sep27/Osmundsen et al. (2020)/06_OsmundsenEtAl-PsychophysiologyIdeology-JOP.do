********************************************************************************
********************************************************************************
****
**** File 6 of the replication archive for:
****   Osmundsen, Mathias, David J. Hendry, Lasse Laustsen, Kevin B. Smith, and
****     Michael Bang Petersen. 2020. "The Psychophysiology of Political
****     Ideology: Replications, Reanalysis, and Recommendations." Journal of
****     Politics XX(X): XXX-XXX.
****
**** Note: This file performs the necessary recodes and analyses for the meta-
**** analysis.
********************************************************************************
********************************************************************************

* uncomment the following line and the closing line to create a log file for the
*   current run
*log using Logs/06_OsmundsenEtAl-PsychophysiologyIdeology-JOP-Log.txt, replace

* change the working directory to the location of this .do file (if necessary)
* cd

** preliminaries
* set plot style for all plots
set scheme s1mono

********************************************************************************
** Aarøe et al. (2017)
********************************************************************************
clear
use Data/MetaAnalysis/Meta-AaroeEtAl.dta, clear

** recodes
* Independent Variable: Physiological reactions to highly disgusting images
alpha Area5 Area6 Area10 Area13  Area15 Area25, gen(high_disgust) asis std
local aaroeetal_mainalpha : display %4.2f `r(alpha)'
local aaroeetal_maincorr : display %4.2f `r(rho)'
alpha Area2 Area3 Area4 Area11 Area14 Area18 Area19 Area21 Area24 Area26, ///
  gen(positive) asis std
alpha Area9 Area12 Area16 Area20, gen(neutral) asis std

* Dependent Variable I: social conservatism scale
recode s1s12c (0 = 6) (1 = 5) (2 = 4) (3 = 3) (4 = 2) (5 = 1) (6 = 0)
recode s1s12e (0 = 6) (1 = 5) (2 = 4) (3 = 3) (4 = 2) (5 = 1) (6 = 0)
recode s1s12f (0 = 6) (1 = 5) (2 = 4) (3 = 3) (4 = 2) (5 = 1) (6 = 0)
recode s1s12g (0 = 6) (1 = 5) (2 = 4) (3 = 3) (4 = 2) (5 = 1) (6 = 0)
alpha s1s12a s1s12b s1s12c s1s12d s1s12e s1s12f s1s12g, gen(soc_con)

* Dependent Variable II: left-right self-placement
sum ideo01

** Validity checks for Table 1
alpha Area5 Area6 Area10 Area13 Area15 Area25, asis std
corr high_disgust positive
local aaroeetal_negothercorr1 : display %4.2f `r(rho)'
corr high_disgust neutral
local aaroeetal_negothercorr2 : display %4.2f `r(rho)'


** Standardize variables
foreach var of varlist high_disgust ideo01 soc_con {
  egen z_`var' = std(`var')
}

** Regressions for Figure 3
reg z_ideo01 z_high_disgust female age, robust
local aaroeetal_lr_beta = _b[z_high_disgust]
local aaroeetal_lr_ll = ///
  _b[z_high_disgust] - invttail(e(df_r), 0.025) * _se[z_high_disgust]
local aaroeetal_lr_ul = ///
  _b[z_high_disgust] + invttail(e(df_r), 0.025) * _se[z_high_disgust]
reg z_soc_con z_high_disgust female age, robust
local aaroeetal_sc_beta = _b[z_high_disgust]
local aaroeetal_sc_ll = ///
  _b[z_high_disgust] - invttail(e(df_r), 0.025) * _se[z_high_disgust]
local aaroeetal_sc_ul = ///
  _b[z_high_disgust] + invttail(e(df_r), 0.025) * _se[z_high_disgust]

local aaroeetal_imageisicorr = "NA"

** Table 1 values
local aaroeetal_cell_a = "Aarøe et al."
local aaroeetal_cell_b = "Denmark"
quietly count
local aaroeetal_cell_c = `r(N)'
local aaroeetal_cell_d = ///
  "6 highly disgusting;* 8 mildly disgusting; 10 positive; 4 neutral"
local aaroeetal_cell_e = "Area-Under-the-Curve"
local aaroeetal_cell_f = "Left-Right Self-placement; Social Conservatism"
local aaroeetal_cell_g = "`aaroeetal_imageisicorr'"
local aaroeetal_cell_h = ///
  "alpha_HighDisgust = `aaroeetal_mainalpha'; Corr_HighDisgust = `aaroeetal_maincorr'"
local aaroeetal_cell_i = ///
  "Corr_NegativePositive = `aaroeetal_negothercorr1'; Corr_NegativeNeutral = `aaroeetal_negothercorr2'"
********************************************************************************


********************************************************************************
** Coe et al. (2017)
********************************************************************************
clear
use Data/MetaAnalysis/Meta-CoeEtAl.dta, clear

** recodes
* Dependent Variable: Left-Right self-placement. Code ``Dont know'' as missing. 
recode ideo (8 = .)

* Independent Variable: Physiological reactions to threatening images
foreach var of varlist s_* isi_* {
  gen ln_`var' = ln(`var')
}
gen lndiff_S3 = ln_s_3 - ln_isi_3
gen lndiff_S8 = ln_s_8 - ln_isi_8
gen lndiff_S12 = ln_s_12 - ln_isi_12
gen lndiff_S20 = ln_s_20 - ln_isi_20
gen lndiff_S27 = ln_s_27 - ln_isi_27
gen lndiff_S32 = ln_s_32 - ln_isi_32
alpha lndiff_S3 lndiff_S8 lndiff_S12 lndiff_S20 lndiff_S27 lndiff_S32, ///
  gen(threat_index2) std asis
local coeetal_mainalpha : display %4.2f `r(alpha)'
local coeetal_maincorr : display %4.2f `r(rho)'

** Validity checks for Table 1
alpha lndiff_S3 lndiff_S8 lndiff_S12 lndiff_S20 lndiff_S27 lndiff_S32, std asis
corr ln_s_3 ln_isi_3
local corr1 = `r(rho)'
corr ln_s_8 ln_isi_8
local corr2 = `r(rho)'
corr ln_s_12 ln_isi_12
local corr3 = `r(rho)'
corr ln_s_20 ln_isi_20
local corr4 = `r(rho)'
corr ln_s_27 ln_isi_27
local corr5 = `r(rho)'
corr ln_s_32 ln_isi_32
local corr6 = `r(rho)'
local coeetal_imageisicorr : display %5.3f ///
  (`corr1' + `corr2' + `corr3' + `corr4' + `corr5' + `corr6') / 6

** Standardize variables
foreach var of varlist ideo threat_index2 {
  egen z_`var' = std(`var') 
}

** Regressions for Figure 3
reg z_ideo z_threat_index2, robust
local coeetal_lr_beta = _b[z_threat]
local coeetal_lr_ll = _b[z_threat] - invttail(e(df_r), 0.025) * _se[z_threat]
local coeetal_lr_ul = _b[z_threat] + invttail(e(df_r), 0.025) * _se[z_threat]

** Table 1 values
local coeetal_cell_a = "Coe et al."
local coeetal_cell_b = "United States"
quietly count
local coeetal_cell_c = `r(N)'
local coeetal_cell_d = "6 threatening*"
local coeetal_cell_e = "Log-and-Subtract"
local coeetal_cell_f = "Left-Right Self-placement"
local coeetal_cell_g = "Corr_Threat = `coeetal_imageisicorr'"
local coeetal_cell_h = ///
  "alpha_Threat = `coeetal_mainalpha'; Corr_Threat = `coeetal_maincorr'"
local coeetal_cell_i = "NA"
********************************************************************************


********************************************************************************
** Dodd et al. (2012)
********************************************************************************
clear
use Data/MetaAnalysis/Meta-DoddEtAl.dta, clear

** recodes
* Dependent Variable I: Wilson-Patterson scale
sum wpscale

* Dependent Variable II: Left-right self-placement
sum pol6

* Independent Variables: Physiological reactions to threatening and positive
* images
gen lnscilspider = ln(scilspider)
gen lnscilspiderIS = ln(scl7)
gen lnsclbloody = ln(sclbloody)
gen lnsclbloodyIS = ln(scl17)
gen lnsclmaggotts = ln(sclmaggotts)
gen lnsclmaggottsIS = ln(scl11)
gen lnsclattack2 = ln(sclattack2)
gen lnsclattack2IS = ln(scl63)
gen lnsclhappy = ln(sclhappy)
gen lnsclhappyIS = ln(scl19)
gen lnsclrabbit = ln(sclrabbit)
gen lnsclrabbitIS = ln(scl5)
gen lnsclfruit = ln(sclfruit)
gen lnsclfruitIS = ln(scl9)
gen lnspiderDIFF = lnscilspider - lnscilspiderIS
gen lnbloodyDIFF = lnsclbloody - lnsclbloodyIS
gen lnmaggotsDIFF = lnsclmaggotts - lnsclmaggottsIS
gen lnhappyDIFF = lnsclhappy - lnsclhappyIS
gen lnrabbitDIFF = lnsclrabbit - lnsclrabbitIS
gen lnfruitDIFF = lnsclfruit - lnsclfruitIS
gen lnattackDIFF = lnsclattack2 - lnsclattack2IS
alpha lnspiderDIFF lnbloodyDIFF lnmaggotsDIFF lnattackDIFF, ///
  gen(threat_index) asis std
local doddetal_mainalpha : display %4.2f `r(alpha)'
local doddetal_maincorr : display %4.2f `r(rho)'
alpha lnhappyDIFF lnrabbitDIFF lnfruitDIFF, gen(positive_index) asis std  

** Validity checks for Table 1
corr lnscilspiderIS lnscilspider
local corr1 = `r(rho)'
corr lnsclbloodyIS lnsclbloody
local corr2 = `r(rho)'
corr lnsclmaggottsIS lnsclmaggotts 
local corr3 = `r(rho)'
local doddetal_imageisicorr : display %5.3f (`corr1' + `corr2' + `corr3') / 3
alpha lnspiderDIFF lnbloodyDIFF lnmaggotsDIFF lnattackDIFF, asis std
corr threat_index positive_index
local doddetal_negothercorr : display %5.2f `r(rho)'

** Standardize variables
foreach var of varlist pol6 threat_index wpscale {
  egen z_`var' = std(`var') 
}

** Regressions for Figure 3
reg z_pol6 z_threat_index income education age female, robust
local doddetal_lr_beta = _b[z_threat]
local doddetal_lr_ll = _b[z_threat] - invttail(e(df_r), 0.025) * _se[z_threat]
local doddetal_lr_ul = _b[z_threat] + invttail(e(df_r), 0.025) * _se[z_threat]

reg z_wpscale z_threat_index income education age female, robust
local doddetal_sc_beta = _b[z_threat]
local doddetal_sc_ll = _b[z_threat] - invttail(e(df_r), 0.025) * _se[z_threat]
local doddetal_sc_ul = _b[z_threat] + invttail(e(df_r), 0.025) * _se[z_threat]
local doddetal_n = `e(N)'


** Table 1 values
local doddetal_cell_a = "Dodd et al."
local doddetal_cell_b = "United States"
local doddetal_cell_c = "`doddetal_n'"
local doddetal_cell_d = "3 threatening;* 3 positive"
local doddetal_cell_e = "Log-and-Subtract"
local doddetal_cell_f = "Wilson-Patterson; Left-Right Self-placement"
local doddetal_cell_g = "Corr_Threat = `doddetal_imageisicorr'"
local doddetal_cell_h = ///
  "alpha_Threat = `doddetal_mainalpha'; Corr_Threat = `doddetal_maincorr'"
local doddetal_cell_i = "Corr_NegativePositive = `doddetal_negothercorr'"
********************************************************************************


********************************************************************************
** Petersen et al. (2015)
********************************************************************************
clear
use Data/MetaAnalysis/Meta-PetersenEtAl.dta, clear

* original data is in panel format, but variables of interest do not vary within
* subjects; keeping first row of each case
keep subject log_fod_area10 log_edderkop_area10 log_blomst_area10 ///
  log_barn_area10 log_paere_area10 log_noegle_area10 log_mand_area10 ///
  log_kvinde_area10 vhskala nypol01 glpol01 kvinde q6n1
bysort subject: gen num = _n
keep if num == 1
drop num

** recodes
* Independent Variables: Physiological reactions to threatening and positive
* images
alpha log_fod_area10 log_edderkop_area10, gen(threat_index) std
local petersenetal_mainalpha : display %4.2f `r(alpha)'
local petersenetal_maincorr : display %4.2f `r(rho)'
alpha log_blomst_area10 log_barn_area10  log_paere_area10 log_noegle_area10 ///
  log_mand_area10 log_kvinde_area10, gen(positive_index) std

* Dependent Variable I: Left-Right Self-placement 
sum vhskala

* Dependent Variable II: Social Conservatism 
sum nypol01

* Dependent Variable III: Economic Conservatism 
sum glpol01

** Validity checks for Table 1
alpha log_fod_area10 log_edderkop_area10, std asis
corr threat_index positive_index
local petersenetal_negothercorr : display %4.2f `r(rho)'

** Standardize variables
foreach var of varlist vhskala nypol01 glpol01 threat_index  {
  egen z_`var' = std(`var') 
}

** Regressions for Figure 3
reg z_vhskala threat_index kvinde q6n1, robust
local petersenetal_lr_beta = _b[threat_index]
local petersenetal_lr_ll = ///
  _b[threat_index] - invttail(e(df_r), 0.025) * _se[threat_index]
local petersenetal_lr_ul = ///
  _b[threat_index] + invttail(e(df_r), 0.025) * _se[threat_index]

reg z_nypol01 threat_index kvinde q6n1, robust
local petersenetal_sc_beta = _b[threat_index]
local petersenetal_sc_ll = ///
  _b[threat_index] - invttail(e(df_r), 0.025) * _se[threat_index]
local petersenetal_sc_ul = ///
  _b[threat_index] + invttail(e(df_r), 0.025) * _se[threat_index]

reg z_glpol01 threat_index kvinde q6n1, robust
local petersenetal_ec_beta = _b[threat_index]
local petersenetal_ec_ll = ///
  _b[threat_index] - invttail(e(df_r), 0.025) * _se[threat_index]
local petersenetal_ec_ul = ///
  _b[threat_index] + invttail(e(df_r), 0.025) * _se[threat_index]

local petersenetal_imageisicorr = "NA"


** Table 1 values
local petersenetal_cell_a = "Petersen et al."
local petersenetal_cell_b = "Denmark"
count
local petersenetal_n = `r(N)'
local petersenetal_cell_c = "`petersenetal_n'"
local petersenetal_cell_d = "2 negative;* 6 positive/neutral"
local petersenetal_cell_e = "Area-Under-the-Curve"
local petersenetal_cell_f = ///
  "Left-Right Self-placement; Social Conservatism; Economic Conservatism"
local petersenetal_cell_g = "`petersenetal_imageisicorr'"
local petersenetal_cell_h = ///
  "alpha_Threat = `petersenetal_mainalpha'; Corr_Threat = `petersenetal_maincorr'"
local petersenetal_cell_i = ///
  "Corr_NegativeNonNegative = `petersenetal_negothercorr'"
********************************************************************************


********************************************************************************
** Smith et al. (2011)
********************************************************************************
clear
use Data/MetaAnalysis/Meta-SmithEtAl.dta, clear

** recodes
* Independent Variables: Physiological reactions to disgusting images
alpha NEWlnpoopdiff NEWlnanorexdiff NEWlnwormdiff, gen(disgust_index) std asis
local smithetal_mainalpha : display %4.2f `r(alpha)'
local smithetal_maincorr : display %4.2f `r(rho)'

* Dependent Variable I: Economic conservatism
alpha tpic16 tpic17 tpic26 tpic23, gen(economic_conservatism) 

* Dependent Variable II: Social conservatism
alpha tpic5 tpic19 tpic25 tpic20 tpic1 tpic18 tpic7 tpic14 tpic4 tpic11 ///
  tpic9 tpic10, gen(social_conservatism) 

** Validity checks for Table 1
alpha  NEWlnpoopdiff NEWlnanorexdiff NEWlnwormdiff, std asis
corr NEWlnanorex NEWlnanorexISI
local corr1 = `r(rho)'
corr NEWlnpoop NEWlnpoopISI 
local corr2 = `r(rho)'
corr NEWlnworm NEWlnwormISI
local corr2 = `r(rho)'
local smithetal_imageisicorr : display %5.3f (`corr1' + `corr2' + `corr3') / 3

local smithetal_negothercorr = "NA"

** Standardize variables
foreach var of varlist disgust_index pol6 social_conservatism ///
  economic_conservatism {
  egen z_`var' = std(`var') 
}

** Regressions for Figure 3
reg z_pol6 z_disgust_index demog4 demog6 demog1, robust
local smithetal_lr_beta = _b[z_disgust]
local smithetal_lr_ll = ///
  _b[z_disgust] - invttail(e(df_r), 0.025) * _se[z_disgust]
local smithetal_lr_ul = ///
_b[z_disgust] + invttail(e(df_r), 0.025) * _se[z_disgust]

reg z_social_conservatism z_disgust_index demog4 demog6 demog1, robust
local smithetal_sc_beta = _b[z_disgust]
local smithetal_sc_ll = ///
  _b[z_disgust] - invttail(e(df_r), 0.025) * _se[z_disgust]
local smithetal_sc_ul = ///
  _b[z_disgust] + invttail(e(df_r), 0.025) * _se[z_disgust]
 
reg z_economic_conservatism z_disgust_index demog4 demog6 demog1, robust
local smithetal_ec_beta = _b[z_disgust]
local smithetal_ec_ll = ///
  _b[z_disgust] - invttail(e(df_r), 0.025) * _se[z_disgust]
local smithetal_ec_ul = ///
  _b[z_disgust] + invttail(e(df_r), 0.025) * _se[z_disgust]


** Table 1 values
local smithetal_cell_a = "Smith et al."
local smithetal_cell_b = "United States"
count
local smithetal_n = `r(N)'
local smithetal_cell_c = "`smithetal_n'"
local smithetal_cell_d = "3 disgusting*"
local smithetal_cell_e = "Log-and-Subtract"
local smithetal_cell_f = "Social Conservatism; Left-Right Self-placement"
local smithetal_cell_g = "Corr_Threat = `smithetal_imageisicorr'"
local smithetal_cell_h = ///
  "alpha_Threat = `smithetal_mainalpha'; Corr_Threat = `smithetal_maincorr'"
local smithetal_cell_i = "`smithetal_negothercorr'"
********************************************************************************


********************************************************************************
** Oxley et al. (2008)
********************************************************************************
clear
use Data/MetaAnalysis/Meta-OxleyEtAl.dta, clear

** recodes
* Independent Variables: Physiological reactions to threatening and positive
* images
gen spider = log(scilspider)
gen spiderIS = log(scl7)
gen maggot = log(sclmaggotts)
gen maggotIS = log(scl11)
gen bloody = log(sclbloody)
gen bloodyIS = log(scl17)
gen ln_sclhappy = ln(sclhappy)
gen ln_sclhappyIS = ln(scl19)
gen ln_sclfruit = ln(sclfruit)
gen ln_sclfruitIS  = ln(scl9)
gen ln_sclrabbit = ln(sclrabbit)
gen ln_sclrabbitIS = ln(scl5)
gen spider_change = spider - spiderIS
gen maggot_change = maggot - maggotIS
gen bloody_change = bloody - bloodyIS
gen happy_change = ln_sclhappy - ln_sclhappyIS
gen fruit_change = ln_sclfruit - ln_sclfruitIS
gen rabbit_change = ln_sclrabbit - ln_sclrabbitIS
alpha spider_change maggot_change bloody_change, gen(threat_index) asis std
local oxleyetal_mainalpha : display %4.2f `r(alpha)'
local oxleyetal_maincorr : display %4.2f `r(rho)'
alpha happy_change fruit_change rabbit_change, gen(positive_index) asis std

* Dependent Variable: Wilson-Patterson Social Conservatism scale
sum threat

** Validity checks for Table 1
alpha spider_change maggot_change bloody_change, asis std
corr spider spiderIS
local corr1 = `r(rho)'
corr maggot maggotIS
local corr2 = `r(rho)'
corr bloody bloodyIS
local corr3 = `r(rho)'
local oxleyetal_imageisicorr : display %5.3f (`corr1' + `corr2' + `corr3') / 3
corr threat_index positive_index
local oxleyetal_negothercorr : display %5.2f `r(rho)'

** Standardize variables
foreach var of varlist threat threat_index {
  egen z_`var' = std(`var')
}

** Regressions for Figure 3
reg z_threat z_threat_index Yearborn female education income, robust
local oxleyetal_sc_beta = _b[z_threat_index]
local oxleyetal_sc_ll = ///
  _b[z_threat_index] - invttail(e(df_r), 0.025) * _se[z_threat_index]
local oxleyetal_sc_ul = ///
  _b[z_threat_index] + invttail(e(df_r), 0.025) * _se[z_threat_index]
local oxleyetal_n = `e(N)'


** Table 1 values
local oxleyetal_cell_a = "Oxley et al."
local oxleyetal_cell_b = "United States"
local oxleyetal_cell_c = "`oxleyetal_n'"
local oxleyetal_cell_d = "3 threatening;* 3 positive"
local oxleyetal_cell_e = "Log-and-Subtract"
local oxleyetal_cell_f = "Social Conservatism"
local oxleyetal_cell_g = "Corr_Threat = `oxleyetal_imageisicorr'"
local oxleyetal_cell_h = ///
  "alpha_Threat = `oxleyetal_mainalpha'; Corr_Threat = `oxleyetal_maincorr'"
local oxleyetal_cell_i = "Corr_NegativePositive = `oxleyetal_negothercorr'"
********************************************************************************


********************************************************************************
** Knoll et al. (2011)
********************************************************************************
clear
use Data/MetaAnalysis/Meta-KnollEtAl-1.dta, clear

** recodes
* Independent Variables: Physiological reactions to threatening and
* non-threatening images
gen bloody_face = log(BloodyFaceSCL15second)
gen bloody_face_IS = log(PreBloodyFaceSCL10second)
gen machete_face = log(MacheteFaceSCL15second)
gen machete_face_IS = log(PreMacheteFaceSCL10second)
gen spider_face = log(SpiderFaceonGirlSCL15secon)
gen spider_face_IS = log(PreSpiderFaceonGirlSCL10s)	
gen worms_foot = log(WormsonFoot1SCL15second)
gen worms_foot_IS = log(PreWormsonFoot1SCL10secon)	
gen spider_man = log(SpideronMansFaceSCL15seco)
gen spider_man_IS = log(PreSpideronMansFaceSCL10)
gen worms_foot2 = log(WormsonFoot2SCL15second)
gen worms_foot2_IS = log(PreWormsonFoot2SCL10secon)
gen family_meal = log(FamilyMealSCL15second)
gen family_meal_IS = log(PreFamilyMealSCL10second)
gen little_boy = log(LittleBoySCL15second)
gen little_boy_IS = log(PreLittleBoySCL10second)
gen daughter_flower = log(AJ)
gen daughter_flower_IS = log(AL)
gen bloody_face_change = (bloody_face - bloody_face_IS)
gen machete_face_change = (machete_face - machete_face_IS)	
gen spider_change = (spider_face - spider_face_IS)	
gen worms_foot_change = (worms_foot - worms_foot_IS)	
gen spider_man_change = (spider_man - spider_man_IS)
gen worms_foot2_change = (worms_foot2 - worms_foot2_IS)
gen family_meal_change = (family_meal - family_meal_IS)
gen little_boy_change = (little_boy - little_boy_IS)
gen daughter_flower_change = (daughter_flower - daughter_flower_IS)

** Validity checks for Table 1	
corr bloody_face bloody_face_IS
local corr1 = `r(rho)'
corr machete_face machete_face_IS
local corr2 = `r(rho)'
corr spider_face spider_face_IS
local corr3 = `r(rho)'
corr worms_foot worms_foot_IS
local corr4 = `r(rho)'
corr spider_man spider_man_IS
local corr5 = `r(rho)'
corr worms_foot2 worms_foot2_IS
local corr6 = `r(rho)'
local knolletal_imageisicorr : display %5.3f ///
  (`corr1' + `corr2' + `corr3' + `corr4' + `corr5' + `corr6') / 6

alpha bloody_face_change machete_face_change spider_change worms_foot_change ///
  spider_man_change worms_foot2_change, gen(threat_index) asis std
local knolletal_mainalpha : display %4.2f `r(alpha)'
local knolletal_maincorr : display %4.2f `r(rho)'
alpha family_meal_change little_boy_change daughter_flower_change, ///
  gen(positive_index) asis std
corr threat_index positive_index
local knolletal_negothercorr : display %4.2f `r(rho)'


** Table 1 values
local knolletal_cell_a = "Knoll et al."
local knolletal_cell_b = "United States"
count
local knolletal_n = `r(N)'
local knolletal_cell_c = "`knolletal_n'"
local knolletal_cell_d = "6 threatening;* 3 non-threatening"
local knolletal_cell_e = "Log-and-Subtract"
local knolletal_cell_f = "Social Conservatism; Economic Conservatism"
local knolletal_cell_g = "Corr_Threat = `knolletal_imageisicorr'"
local knolletal_cell_h = ///
  "alpha_Threat = `knolletal_mainalpha'; Corr_Threat = `knolletal_maincorr'"
local knolletal_cell_i = "Corr_NegativeNonNegative = `knolletal_negothercorr'"


clear 
use Data/MetaAnalysis/Meta-KnollEtAl-2.dta, clear

** Standardize Variables	
foreach var of varlist policysocial policyeconomic SCLthreat {
  egen z_`var' = std(`var')
}

** Regression coefficients for Figure 3
reg z_policysocial z_SCLthreat gender income if citizen == 1
local knolletal_sc_beta = _b[z_SCLthreat]
local knolletal_sc_ll = ///
  _b[z_SCLthreat] - invttail(e(df_r), 0.025) * _se[z_SCLthreat]
local knolletal_sc_ul = ///
  _b[z_SCLthreat] + invttail(e(df_r), 0.025) * _se[z_SCLthreat]

reg z_policyeconomic z_SCLthreat gender income if citizen == 1
local knolletal_ec_beta = _b[z_SCLthreat]
local knolletal_ec_ll = ///
  _b[z_SCLthreat] - invttail(e(df_r), 0.025) * _se[z_SCLthreat]
local knolletal_ec_ul = ///
  _b[z_SCLthreat] + invttail(e(df_r), 0.025) * _se[z_SCLthreat]

* supplemental
*reg z_policysocial z_SCLNONthreat gender income if citizen == 1
*reg z_policyeconomic z_SCLNONthreat gender income if citizen == 1
********************************************************************************


**********
** Table 1
**********
putexcel set Tables/Table01.xlsx, replace
putexcel A1 = ///
  ("Table 1. Overview and Measurement Properties of Published Studies with Ideological and Skin Conductance Measures")
putexcel A3 = ("Study") B3 = ("Country") C3 = ("N") ///
  D3 = ("Image Types (* images used for threat-sensitivity)") ///
  E3 = ("Method") F3 = ("Ideology Measures") ///
  G3 = ("Correlation: Image and ISI") ///
  H3 = ("alpha and Average Inter-item Correlation") ///
  I3 = ("Correlation: Negative Images and Other Types of Images")
putexcel A4 = ("`aaroeetal_cell_a'") B4 = ("`aaroeetal_cell_b'") ///
  C4 = ("`aaroeetal_cell_c'") D4 = ("`aaroeetal_cell_d'") ///
  E4 = ("`aaroeetal_cell_e'") F4 = ("`aaroeetal_cell_f'") ///
  G4 = ("`aaroeetal_cell_g'") H4 = ("`aaroeetal_cell_h'") ///
  I4 = ("`aaroeetal_cell_i'")
putexcel A5 = ("`oxleyetal_cell_a'") B5 = ("`oxleyetal_cell_b'") ///
  C5 = ("`oxleyetal_cell_c'") D5 = ("`oxleyetal_cell_d'") ///
  E5 = ("`oxleyetal_cell_e'") F5 = ("`oxleyetal_cell_f'") ///
  G5 = ("`oxleyetal_cell_g'") H5 = ("`oxleyetal_cell_h'") ///
  I5 = ("`oxleyetal_cell_i'")
putexcel A6 = ("`doddetal_cell_a'") B6 = ("`doddetal_cell_b'") ///
  C6 = ("`doddetal_cell_c'") D6 = ("`doddetal_cell_d'") ///
  E6 = ("`doddetal_cell_e'") F6 = ("`doddetal_cell_f'") ///
  G6 = ("`doddetal_cell_g'") H6 = ("`doddetal_cell_h'") ///
  I6 = ("`doddetal_cell_i'")
putexcel A7 = ("`smithetal_cell_a'") B7 = ("`smithetal_cell_b'") ///
  C7 = ("`smithetal_cell_c'") D7 = ("`smithetal_cell_d'") ///
  E7 = ("`smithetal_cell_e'") F7 = ("`smithetal_cell_f'") ///
  G7 = ("`smithetal_cell_g'") H7 = ("`smithetal_cell_h'") ///
  I7 = ("`smithetal_cell_i'")
putexcel A8 = ("`petersenetal_cell_a'") B8 = ("`petersenetal_cell_b'") ///
  C8 = ("`petersenetal_cell_c'") D8 = ("`petersenetal_cell_d'") ///
  E8 = ("`petersenetal_cell_e'") F8 = ("`petersenetal_cell_f'") ///
  G8 = ("`petersenetal_cell_g'") H8 = ("`petersenetal_cell_h'") ///
  I8 = ("`petersenetal_cell_i'")
putexcel A9 = ("`coeetal_cell_a'") B9 = ("`coeetal_cell_b'") ///
  C9 = ("`coeetal_cell_c'") D9 = ("`coeetal_cell_d'") ///
  E9 = ("`coeetal_cell_e'") F9 = ("`coeetal_cell_f'") ///
  G9 = ("`coeetal_cell_g'") H9 = ("`coeetal_cell_h'") ///
  I9 = ("`coeetal_cell_i'")
putexcel A10 = ("`knolletal_cell_a'") B10 = ("`knolletal_cell_b'") ///
  C10 = ("`knolletal_cell_c'") D10 = ("`knolletal_cell_d'") ///
  E10 = ("`knolletal_cell_e'") F10 = ("`knolletal_cell_f'") ///
  G10 = ("`knolletal_cell_g'") H10 = ("`knolletal_cell_h'") ///
  I10 = ("`knolletal_cell_i'")
**********


***********
** Figure 3
***********
matrix regs = ( ///
  `doddetal_sc_beta', `doddetal_sc_ll', `doddetal_sc_ul'\ ///
  `oxleyetal_sc_beta', `oxleyetal_sc_ll', `oxleyetal_sc_ul'\ ///
  `aaroeetal_sc_beta', `aaroeetal_sc_ll', `aaroeetal_sc_ul'\ ///
  `smithetal_sc_beta', `smithetal_sc_ll', `smithetal_sc_ul'\ ///
  `knolletal_sc_beta', `knolletal_sc_ll', `knolletal_sc_ul'\ ///
  `petersenetal_sc_beta', `petersenetal_sc_ll', `petersenetal_sc_ul'\ ///
  `doddetal_lr_beta', `doddetal_lr_ll', `doddetal_lr_ul'\ ///
  `smithetal_lr_beta', `smithetal_lr_ll', `smithetal_lr_ul'\ ///
  `aaroeetal_lr_beta', `aaroeetal_lr_ll', `aaroeetal_lr_ul'\ ///
  `coeetal_lr_beta', `coeetal_lr_ll', `coeetal_lr_ul'\ ///
  `petersenetal_lr_beta', `petersenetal_lr_ll', `petersenetal_lr_ul'\ ///
  `knolletal_ec_beta', `knolletal_ec_ll', `knolletal_ec_ul'\ ///
  `smithetal_ec_beta', `smithetal_ec_ll', `smithetal_ec_ul'\ ///
  `petersenetal_ec_beta', `petersenetal_ec_ll', `petersenetal_ec_ul')

matrix rownames regs = Dodd2012a Oxley2008 Aaroe2017a Smith2012a Knoll2015a ///
  Petersen2015a Dodd2012b Smith2012b Aaroe2017b Coe2017 Petersen2015b ///
  Knoll2015b Smith2012c Petersen2015c
  matrix list regs  

coefplot matrix(regs[,1]), ci((regs[,2] regs[,3])) xline(0, lcolor(gray)) ///
  msymbol(square) ///
headings(Dodd2012a = "{bf:Social Conservatism}" ///
  Dodd2012b = "{bf:Left-Right Self-placement}" ///
  Knoll2015b = "{bf:Economic Conservatism}", labgap(-110))   /// 
yscale(alt axis(1)) coeflabels(Dodd2012a = "Dodd et al. (2012)" ///
  Oxley2008 = "Oxley et al. (2008)" Aaroe2017a = "Aarøe et al. (2017)" ///
  Smith2012a = "Smith et al. (2012)" Knoll2015a = "Knoll et al. (2015)" ///
  Petersen2015a = "Petersen et al. (2015)" Dodd2012b = "Dodd et al. (2012)" ///
  Smith2012b = "Smith et al. (2012)" Aaroe2017b = "Aarøe et al. (2017)" ///
  Coe2017 = "Coe et al. (2017)" Petersen2015b = "Petersen et al. (2015)" ///
  Knoll2015b = "Knoll et al. (2015)" Smith2012c = "Smith et al. (2012)" ///
  Petersen2015c = "Petersen et al. (2015)", notick labgap(-100)) ///
  graphregion(margin(l = 75)) grid(none) mlabel(string(@b, "%5.2f") + ///
  " (" + string(@ll, "%5.2f") + "; " + string(@ul, "%5.2f") + ")") ///
  mlabposition(12) mlabsize(vsmall)  
graph export Figures/Figure03.pdf, replace
graph drop _all
graph close
***********


clear

* close log
*log close

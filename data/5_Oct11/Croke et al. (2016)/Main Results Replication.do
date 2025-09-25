*** Set working directory
capture cd "~\Replication"



*** Open main dataset
use "Final Dataset.dta", clear



*** Define lists for classes of variables to save space below
global a_l_education "edu a_l_incomplete_primary a_l_complete_primary a_l_incomplete_high a_l_complete_high a_l_incomplete_col a_l_complete_col"
global balance "shona ndebele male d_incumbent d_turnout closest_border_rebel ZIPRA ZANLA"
global participation "part_scale voted contacted_local_govt att_comm_meet raise_issue"
global econ_int "economic_scale employed good_own_liv_cond poverty news int_pub_aff" 
global demo "supp_demo multi" 
global view_govt "view_govt_scale close_inc_party close_main_opp_party incumbent_overall corruption" 
global alternatives "gift freedom_vote find_out_vote fear"



******************************************
***                Plots               ***
******************************************

*** Figure 3: Sample Distribution of Educational Attainment (8,014 observations)

graph bar (count), over(edu, label(labsize(tiny))) bar(1, fcolor(black)) ytitle(Number of respondents) ylabel(, nogrid) legend(off) subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) 



*** Figure 4: Operationalization of Secondary access, the Study’s Key Treatment Variable

preserve
duplicates drop year_birth, force
twoway (scatter treatment year_birth if year_birth>=1959 & year_birth<=1971, c(J) lcolor(black) msymbol(point)), ///
  ylabel(0(0.25)1, nogrid) ytitle(Secondary access) xtitle(Year of birth) xlabel(1959(1)1971) subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))
restore



*** Figure 5: Trends in Educational Attainment by Cohort

foreach x of varlist $a_l_education {
  twoway (lpoly `x' year_birth if year_birth>=1945 & year_birth<=1963, lcolor(black) clwidth(thick)) (lpoly `x' year_birth if year_birth>1966 & year_birth<=1985, lcolor(black) clwidth(thick)) ///
  (scatter mean_`x' year_birth [w=weight_cohort], msize(vsmall) color(gray)) if year_birth>=1945 & year_birth<=1985, ///
  title(`: variable label `x'', color(black)) xline(1963.5 1966.5, lcolor(black)) legend(off) ylabel(, nogrid) xtitle(Year of birth, size(medlarge)) ytitle(Proportion, size(medlarge)) ///
  subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) xlabel(1945(10)1985) ylabel(0(0.2)1) xline(1958.5 1971.5, lpattern(dash) lcolor(gs12))
  graph save Graph "g_`x'.gph", replace
}
graph combine g_edu.gph g_a_l_incomplete_primary.gph g_a_l_complete_primary.gph g_a_l_incomplete_high.gph g_a_l_complete_high.gph g_a_l_incomplete_college.gph, ///
  rows(2) cols(3) subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))
erase g_edu.gph 
erase g_a_l_incomplete_primary.gph 
erase g_a_l_complete_primary.gph 
erase g_a_l_incomplete_high.gph 
erase g_a_l_complete_high.gph 
erase g_a_l_incomplete_college.gph 
erase g_a_l_complete_college.gph 



*** Figure 6: Trends in Pre-treatment Variables by Cohort

foreach x of varlist $balance {
  twoway (lpoly `x' year_birth if year_birth>=1945 & year_birth<=1963, lcolor(black) clwidth(thick)) (lpoly `x' year_birth if year_birth>1966 & year_birth<=1985, lcolor(black) clwidth(thick)) ///
  (scatter mean_`x' year_birth [w=weight_cohort], msize(vsmall) color(gray)) if year_birth>=1945 & year_birth<=1985, ///
  title(`: variable label `x'', color(black)) xline(1963.5 1966.5, lcolor(black)) xline(1958.5 1971.5, lpattern(dash) lcolor(gs12)) legend(off) ylabel(, nogrid) xtitle(Year of birth, size(medlarge)) ytitle(Proportion, size(medlarge)) ///
  subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) xlabel(1945(10)1985)
  graph save Graph "g_`x'.gph", replace
}
twoway (scatter mean_age year_birth [w=weight_cohort], msize(vsmall) color(gray)) if year_birth>=1945 & year_birth<=1985, ///
  title(Age, color(black)) xline(1963.5 1966.5, lcolor(black)) legend(off) ylabel(, nogrid) xtitle(Year of birth, size(medlarge)) ytitle(Proportion, size(medlarge)) ///
  subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) xlabel(1945(10)1985) xline(1958.5 1971.5, lpattern(dash) lcolor(gs12)) 
graph save Graph "g_d_age.gph", replace
graph combine g_shona.gph g_ndebele.gph g_male.gph g_d_age.gph g_d_incumbent.gph g_d_turnout.gph g_closest_border_rebel.gph g_ZIPRA.gph g_ZANLA.gph, ///
  rows(2) cols(3) subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))
erase g_shona.gph 
erase g_ndebele.gph 
erase g_male.gph 
erase g_d_incumbent.gph 
erase g_d_turnout.gph 
erase g_d_age.gph 
erase g_closest_border_rebel.gph 
erase g_ZIPRA.gph 
erase g_ZANLA.gph



*** Figure 7: Trends in Political Participation by Cohort

foreach x of varlist $participation {
  twoway (lpoly `x' year_birth if year_birth>=1945 & year_birth<=1963, lcolor(black) clwidth(thick)) (lpoly `x' year_birth if year_birth>1966 & year_birth<=1985, lcolor(black) clwidth(thick)) ///
  (scatter mean_`x' year_birth [w=weight_cohort], msize(vsmall) color(gray)) if year_birth>=1945 & year_birth<=1985, ///
  title(`: variable label `x'', color(black)) xline(1963.5 1966.5, lcolor(black)) legend(off) ylabel(, nogrid) xtitle(Year of birth, size(medlarge)) ytitle(Proportion, size(medlarge)) ///
  subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) xlabel(1945(10)1985) xline(1958.5 1971.5, lpattern(dash) lcolor(gs12))
  graph save Graph "g_`x'.gph", replace
}
graph combine g_part_scale.gph g_voted.gph g_contacted_local_govt.gph g_att_comm_meet.gph g_raise_issue.gph, ///
  rows(2) cols(3) subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))



*** Figure A4: Trends in economic outcomes and political interest by cohort

foreach x of varlist $econ_int {
  twoway (lpoly `x' year_birth if year_birth>=1945 & year_birth<=1963, lcolor(black) clwidth(thick)) (lpoly `x' year_birth if year_birth>1966 & year_birth<=1985, lcolor(black) clwidth(thick)) ///
  (scatter mean_`x' year_birth [w=weight_cohort], msize(vsmall) color(gray)) if year_birth>=1945 & year_birth<=1985, ///
  title(`: variable label `x'', color(black)) xline(1963.5 1966.5, lcolor(black)) legend(off) ylabel(, nogrid) xtitle(Year of birth, size(medlarge)) ytitle(Proportion, size(medlarge)) ///
  subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) xlabel(1945(10)1985) xline(1958.5 1971.5, lpattern(dash) lcolor(gs12))
  graph save Graph "g_`x'.gph", replace
}
graph combine g_economic_scale.gph g_employed.gph g_good_own_liv_cond.gph g_poverty.gph g_news.gph g_int_pub_aff.gph, ///
  rows(2) cols(3) subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))


  
*** Figure A5: Trends in support for democracy in Zimbabwe by cohort

foreach x of varlist $demo {
  twoway (lpoly `x' year_birth if year_birth>=1945 & year_birth<=1963, lcolor(black) clwidth(thick)) (lpoly `x' year_birth if year_birth>1966 & year_birth<=1985, lcolor(black) clwidth(thick)) ///
  (scatter mean_`x' year_birth [w=weight_cohort], msize(vsmall) color(gray)) if year_birth>=1945 & year_birth<=1985, ///
  title(`: variable label `x'', color(black)) xline(1963.5 1966.5, lcolor(black)) legend(off) ylabel(, nogrid) xtitle(Year of birth, size(medlarge)) ytitle(Proportion, size(medlarge)) ///
  subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) xlabel(1945(10)1985) xline(1958.5 1971.5, lpattern(dash) lcolor(gs12))
  graph save Graph "g_`x'.gph", replace
}
graph combine g_supp_demo.gph g_multi.gph, ///
  rows(1) cols(2) subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))

  
  
*** Figure A6: Trends in support for the government by cohort

foreach x of varlist $view_govt {
  twoway (lpoly `x' year_birth if year_birth>=1945 & year_birth<=1963, lcolor(black) clwidth(thick)) (lpoly `x' year_birth if year_birth>1966 & year_birth<=1985, lcolor(black) clwidth(thick)) ///
  (scatter mean_`x' year_birth [w=weight_cohort], msize(vsmall) color(gray)) if year_birth>=1945 & year_birth<=1985, ///
  title(`: variable label `x'', color(black)) xline(1963.5 1966.5, lcolor(black)) legend(off) ylabel(, nogrid) xtitle(Year of birth, size(medlarge)) ytitle(Proportion, size(medlarge)) ///
  subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) xlabel(1945(10)1985) xline(1958.5 1971.5, lpattern(dash) lcolor(gs12))
  graph save Graph "g_`x'.gph", replace
}
graph combine g_view_govt_scale.gph g_close_inc_party.gph g_close_main_opp_party.gph g_incumbent_overall.gph g_corruption.gph, ///
  rows(2) cols(3) subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))

  
  
*** Figure A7: Trends in alternative explanations

foreach x of varlist $alternatives only_national_identity some_national_identity {
  twoway (lpoly `x' year_birth if year_birth>=1945 & year_birth<=1963, lcolor(black) clwidth(thick)) (lpoly `x' year_birth if year_birth>1966 & year_birth<=1985, lcolor(black) clwidth(thick)) ///
  (scatter mean_`x' year_birth [w=weight_cohort], msize(vsmall) color(gray)) if year_birth>=1945 & year_birth<=1985, ///
  title(`: variable label `x'', color(black)) xline(1963.5 1966.5, lcolor(black)) legend(off) ylabel(, nogrid) xtitle(Year of birth, size(medlarge)) ytitle(Proportion, size(medlarge)) ///
  subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) xlabel(1945(10)1985) xline(1958.5 1971.5, lpattern(dash) lcolor(gs12))
  graph save Graph "g_`x'.gph", replace
}
graph combine g_gift_for_vote.gph g_fear.gph g_freedom_vote.gph g_find_out_vote.gph g_only_national_identity.gph g_some_national_identity.gph, ///
  rows(2) cols(3) subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))


   
  
  
******************************************
***         Summary Statistics         ***
******************************************

*** Table A1: Pairwise correlation matrix of participation measures

pwcorr voted contacted_local_govt att_comm_meet raise_issue



*** Table A2: Summary statistics

summ $participation $econ_int $demo $view_govt $a_l_education treatment year_survey comp $balance age $alternatives events only_nat some_nat if treatment!=. & year_birth>=1959 & year_birth<=1971 & year_survey!=. & district!=""


  

  
  
  
  
******************************************
***         Regression results         ***
******************************************

*** Table A3: Balance tests

foreach x in balance {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' treatment i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
}



*** Table 1: Estimates of Education Reform on Educational Attainment

foreach x in a_l_education {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' treatment i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
}
foreach x in a_l_education {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' post i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(post) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(post) label 
}



*** Table 2: The Effects of Education on Political Participation

foreach x in participation {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' treatment i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu = treatment) i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' post i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(post) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(post) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu = post) i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
}
foreach x in participation {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' edu i.year_survey, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(edu) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(edu) label 
}



*** Figure A3: Reduced form estimates by bandwidth (95% confidence intervals)

foreach x in participation {
  foreach b of numlist 0/10 {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'_`b'): xi: reg `y' treatment i.year_survey if year_birth>=1964-`b' & year_birth<=1966+`b', cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  }
}

!!! ATTENTION !!! - the following data must be appended

* To be appended to the Stata dataset
bandwidth	coef_scale	coef_voted	coef_contact	coef_attend	coef_raise	se_scale	se_voted	se_contact	se_attend	se_raise
0	-0.086	-0.187	-0.021	-0.069	-0.07	0.09	0.124	0.149	0.101	0.136
1	-0.03	-0.084	-0.026	-0.048	-0.032	0.026	0.043	0.056	0.037	0.053
2	-0.055	-0.092	-0.055	-0.065	-0.03	0.021	0.035	0.044	0.029	0.04
3	-0.067	-0.068	-0.072	-0.085	-0.025	0.021	0.029	0.043	0.03	0.035
4	-0.064	-0.074	-0.058	-0.062	-0.029	0.019	0.025	0.035	0.028	0.036
5	-0.066	-0.078	-0.057	-0.064	-0.042	0.016	0.024	0.026	0.024	0.032
6	-0.062	-0.08	-0.07	-0.063	-0.028	0.014	0.021	0.024	0.019	0.023
7	-0.066	-0.079	-0.071	-0.066	-0.034	0.013	0.019	0.022	0.018	0.023
8	-0.066	-0.087	-0.058	-0.064	-0.037	0.013	0.018	0.021	0.017	0.022
9	-0.072	-0.091	-0.066	-0.068	-0.051	0.013	0.016	0.02	0.019	0.021
10	-0.081	-0.1	-0.079	-0.082	-0.061	0.013	0.015	0.021	0.019	0.021

foreach x in scale voted contact attend raise {
  g min95_`x' = coef_`x' - 1.96 * se_`x'
  g max95_`x' = coef_`x' + 1.96 * se_`x'
}

foreach x in scale voted contact attend raise {
twoway (scatter coef_`x' bandwidth, mcolor(black) msize(large)) ///
  (rcap min95_`x' max95_`x' bandwidth, vertical lcolor(black) lwidth(thick)), ///
  legend(off) graphregion(fcolor(white) lcolor(white)) ylab(, nogrid) yline(0, lcolor(black) lpattern(dash)) ///
  ytitle("Marginal effect of secondary access") title("`x'") xtitle("Bandwidth") 
  graph save Graph "g_bandwidth_`x'.gph", replace
}
graph combine g_bandwidth_scale.gph g_bandwidth_voted.gph g_bandwidth_contact.gph g_bandwidth_attend.gph g_bandwidth_raise.gph, ///
  rows(2) cols(3) subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))

drop order-max95



*** Table 3: Robustness Checks

* Panel A: 3 cohort bandwidth (reduced form)
foreach x in participation {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' treatment i.year_survey if year_birth>=1961 & year_birth<=1969, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
}

* Panel B: 10 cohort bandwidth (reduced form)
foreach x in participation {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' treatment i.year_survey if year_birth>=1954 & year_birth<=1976, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
}

* Panel C: Multiply imputed data (reduced form)
preserve
mi set wide
mi register imputed voted contacted_local_govt att_comm_meet raise_issue
xi: mi impute mvn voted contacted_local_govt att_comm_meet raise_issue = i.year_survey i.district shona ndebele male d_incumbent d_turnout if edu!=., add(10) force rseed(123456789)
mi passive : g part_scale_impute = (voted + contacted_local_govt + att_comm_meet + raise_issue)/4
foreach y in part_scale_impute voted contacted_local_govt att_comm_meet raise_issue {
  xi: mi estimate : reg `y' treatment i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
}
restore

* Panel D: Placebo 1970 reform (reduced form)
g placebo = year_birth>=1957
foreach x in participation {
  eststo clear
  foreach y of varlist $`x' {
     quietly eststo, title(`y'): xi: reg `y' placebo i.year_survey if year_birth>=1952 & year_birth<=1961, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(placebo) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(placebo) label 
}
drop placebo

* All other placebo reforms (referenced in the main text)
foreach x in participation {
foreach z of numlist 1947(1)1959 {
  local z1 = `z'+13
  di "************ Placebo reform year = `z1' *************"
  g placebo = year_birth>=`z'
  eststo clear
  foreach y of varlist $`x' {
     quietly eststo, title(`y'): xi: reg `y' placebo i.year_survey if year_birth>=`z'-5 & year_birth<=`z'+4, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(placebo) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(placebo) label 
  drop placebo
}
}

* Panel E: Regression discontinuity with linear cohort trends and 10 cohort bandwidth
foreach x in participation {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' any_treatment##c.running i.year_survey if year_birth>=1954 & year_birth<=1973, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(1.any_treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(1.any_treatment) label 
}

* Panel F: Respondents first eligible to vote in the 1985 Election (reduced form)
foreach x in participation {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' treatment i.year_survey if year_birth>=1963 & year_birth<=1966, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
}

* Panel G: Controlling for pre-treatment and district characteristics (reduced form)
foreach x in participation {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' treatment i.year_survey shona ndebele male d_incumbent d_turnout if year_birth>=1959 & year_birth<=1971, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
}

* Panel H: Controlling for district fixed effects (reduced form)
foreach x in participation {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: areg `y' treatment i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district) a(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
}



*** Table A5: Estimates of the effect of education on political participation, including age fixed effects

foreach x in participation {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: areg `y' treatment i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district) a(age)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: areg `y' post i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district) a(age)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(post) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(post) label 
}



*** Conley et al. exclusion restriction sensitivity test (cited in the main text) - note: this requires the install

capture ssc install plausexog
xi: plausexog uci part_scale (edu = treatment) i.year_survey if year_birth>=1959 & year_birth<=1971, gmin(-0.0302) gmax(0.0302) level(.95) vce(cluster district)
di 0.0302/0.060*100



*** Table A4: Estimates of the effect of education on missing responses

g missing_voted = voted==. if year_survey!=2004
g missing_contacted_local_govt = contacted_local_govt==. if year_survey!=2005 & year_survey!=2010
g missing_att_comm_meet = att_comm_meet==. if year_survey!=2010
g missing_raise_issue = raise_issue==. if year_survey!=1999 & year_survey!=2010
eststo clear
quietly foreach y of varlist voted contacted_local_govt att_comm_meet raise_issue {
  eststo : xi: areg missing_`y' treatment i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district) a(district)
}
estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
eststo clear
quietly foreach y of varlist voted contacted_local_govt att_comm_meet raise_issue {
  eststo : xi: areg missing_`y' treatment i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district) a(district)
}
estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 


  
*** Table 4: Estimates of Secondary Education on Economic Outcomes and Political Interest

foreach x in econ_int {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' treatment i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu = treatment) i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' post i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(post) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(post) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu = post) i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
}



*** Table 5: Estimates of Secondary Education on Support for Democracy

foreach x in demo {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' treatment i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu = treatment) i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' post i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(post) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(post) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu = post) i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
}



*** Table 6: Estimates of Secondary Education Reform on Support for the Government

foreach x in view_govt {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' treatment i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu = treatment) i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' post i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(post) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(post) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu = post) i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu) label 
}



*** Table 7: The Effects of Education on Political Participation, Before and After 2008

capture g edu_comp = edu * comp
capture g treatment_comp = treatment * comp
capture g any_treatment_comp = any_treatment * comp
capture g post_comp = post * comp

foreach x in participation {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' c.treatment##comp i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu edu_comp = treatment treatment_comp) i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' c.treatment##comp i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu edu_comp = post post_com) i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
}



*** Table A6: Estimates of the effect of education on economic outcomes and political interest, before and after 2008

foreach x in econ_int {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' c.treatment##comp i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu edu_comp = treatment treatment_comp) i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' c.treatment##comp i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu edu_comp = post post_com) i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
}



*** Table A7: Estimates of the effect of education on support for democracy, before and after 2008

foreach x in demo {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' c.treatment##comp i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu edu_comp = treatment treatment_comp) i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' c.treatment##comp i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu edu_comp = post post_com) i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
}



*** Table A8: Estimates of the effect of education on support for the government, before and after 2008

foreach x in view_govt {
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' c.treatment##comp i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu edu_comp = treatment treatment_comp) i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: reg `y' c.treatment##comp i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district)
  }
  estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.comp#c.treatment) label 
  eststo clear
  foreach y of varlist $`x' {
    quietly eststo, title(`y'): xi: ivreg2 `y' (edu edu_comp = post post_com) i.year_survey if year_birth>=1959 & year_birth<=1971 & partial==0, cluster(district) first
    quietly matrix A = e(first)
    quietly estadd scalar F_stat=A[3,1]
  }
  estout, cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
  estout, style(tex) cells(b(star fmt(3)) se(par)) starlevels(* .1 ** .05 *** .01) stats(N F_stat, fmt(0 1) labels("Observations" "First stage $ F$ statistic")) keep(edu edu_comp) label 
}



*** Table 8: Mobilization Explanations

eststo clear
foreach y of varlist gift_for_vote freedom_vote {
  quietly eststo, title(`y'): xi: reg `y' treatment i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
}
foreach x of varlist d_incumbent d_turnout {
  quietly eststo, title(part_scale): xi: reg part_scale c.treatment##c.`x' i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
}
estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment c.treatment#c.d_incumbent c.treatment#c.d_turnout) label ///
  varlabel(c.treatment#c.d_incumbent "Secondary access $ \times$ Incumbent share" c.treatment#c.d_turnout "Secondary access $ \times$ Turnout")
estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment c.treatment#c.d_incumbent c.treatment#c.d_turnout) ///
  varlabel(c.treatment#c.d_incumbent "Secondary access $ \times$ Incumbent share" c.treatment#c.d_turnout "Secondary access $ \times$ Turnout")

  
  
*** Table 9: Repression Explanations

eststo clear
foreach y of varlist find_out_vote fear {
  quietly eststo, title(`y'): xi: reg `y' treatment i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
  quietly eststo, title(`y'): xi: reg `y' c.treatment##ndebele i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
}
foreach x of varlist events {
  quietly eststo, title(part_scale): xi: reg part_scale c.treatment##c.`x' i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
}
estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.ndebele#c.treatment c.treatment#c.events) label ///
  varlabel(c.treatment#c.events "Secondary access $ \times$ Event" 1.ndebele#c.treatment "Secondary access $ \times$ Ndebele")
estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.ndebele#c.treatment c.treatment#c.events) ///
  varlabel(c.treatment#c.events "Secondary access $ \times$ Event" 1.ndebele#c.treatment "Secondary access $ \times$ Ndebele")

  
  
*** Table 10: Coming of Age Explanations

eststo clear
eststo, title("only_nat"): xi: reg only_nat treatment i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
eststo, title("some_nat"): xi: reg some_nat treatment i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
eststo, title("part_scale"): xi: reg part_scale c.treatment##c.closest_border_rebel i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
eststo, title("part_scale"): xi: reg part_scale c.treatment##c.ZANLA i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
eststo, title("part_scale"): xi: reg part_scale c.treatment##c.ZIPRA i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
eststo, title("part_scale"): xi: reg part_scale c.treatment##shona i.year_survey if year_birth>=1959 & year_birth<=1971, cluster(district)
estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.shona#c.treatment c.treatment#c.closest_border_rebel c.treatment#c.ZANLA c.treatment#c.ZIPRA) label ///
  varlabel(c.treatment#c.closest_border_rebel "Secondary access $ \times$ Distance to rebel border" c.treatment#c.ZANLA "Secondary access $ \times$ Distance to ZANLA border" c.treatment#c.ZIPRA "Secondary access $ \times$ Distance to ZIPRA border" 1.shona#c.treatment "Secondary access $ \times$ Shona")
estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(treatment 1.shona#c.treatment c.treatment#c.closest_border_rebel c.treatment#c.ZANLA c.treatment#c.ZIPRA) ///
  varlabel(c.treatment#c.closest_border_rebel "Secondary access $ \times$ Distance to rebel border" c.treatment#c.ZANLA "Secondary access $ \times$ Distance to ZANLA border" c.treatment#c.ZIPRA "Secondary access $ \times$ Distance to ZIPRA border" 1.shona#c.treatment "Secondary access $ \times$ Shona")

use "Final Dataset.dta", clear


*** Set working directory
capture cd "~\Replication"

/*
In this do file we create appendix figure A2, which examines differences in adult height around the paper's age discontinuity.
To do this we append the household member recode files from the 1994, 1999, 2005, and 2010 Zimbabwe DHS survey rounds. 
The household member recode file contains data on all surveyed household members, including adult height in centimeters. 
We then graph adult height before and after the education discontunuity using local polynomial of degree 2. 
*/

use "dhs_2010.DTA", clear
append using "dhs_2005.DTA"
append using "dhs_1999.DTA"
append using "dhs_1994.DTA"

gen female= (hv104==2)
rename hv105 age
replace age=. if age==99 | age==98

*the 1994 survey round variable is "94", we change it to 4 digits for consistency with other rounds. 
replace hv007=1994 if hv007==94
rename hv007 survey_year
gen year_birth=int(survey_year-age)

*code missing/non-response heights as missing
foreach var in ha3 hb3 {
replace `var'=. if `var'>9990
}
//

*there are different variables to measure height for men and women (ha3 and hb3).
*So we here we create a single adult height variable

gen height_adult=ha3 if female==1
replace height_adult=hb3 if female==0
replace height_adult=height_adult/10
lab var height_adult "adult height in centimeters"
bys year_birth : g weight_cohort = _N

*drop those under 20 at time of survey since they may not be at full adult height yet
drop if age<20

quietly foreach y of varlist height_adult {
  capture bys year_birth : egen mean_`y' = mean(`y')
}
//

*This code creates Appendix Fgure 2
foreach x of varlist height_adult {
  twoway (lpoly `x' year_birth if year_birth>=1945 & year_birth<=1963, lcolor(black) clwidth(thick) degree(2)) (lpoly `x' year_birth if year_birth>1966 & year_birth<=1985, lcolor(black) clwidth(thick) degree(2)) ///
  (scatter mean_`x' year_birth [w=weight_cohort], msize(vsmall) color(gray)) if year_birth>=1945 & year_birth<=1985, ///
  title("Height", color(black)) xline(1963.5 1966.5, lpattern(solid) lcolor(black)) legend(off) ylabel(, nogrid) xtitle(Year of birth, size(medlarge)) ytitle(adult height in centimeters, size(medlarge)) ///
  subtitle(, color(black) fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) xlabel(1945(10)1985)  xline(1958.5 1971.5, lpattern(dash) lcolor(gs12))
  graph save Graph "adult_height.gph", replace
  }
//

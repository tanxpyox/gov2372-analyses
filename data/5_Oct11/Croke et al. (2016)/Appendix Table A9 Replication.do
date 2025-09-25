*** Set working directory
capture cd "~\Replication"

**************************************************************************************************
**************************************************************************************************
**************************************************************************************************

*************** Data extraction *****************

*** Round 1 ***

use merged_r1_data.dta, clear

decode country, gen(country_name)
decode educ, gen(education)
decode parvot, gen(vote)

keep country_name education vote
rename country_name country

gen year =0
replace year =1999 if country=="Botswana" | country=="Ghana"
replace year =2000 if country=="Lesotho"
replace year =1999 if country=="Malawi"
replace year =2001 if country=="Mali"
replace year =1999 if country=="Namibia" | country=="Nigeria"
replace year =2000 if country=="South Africa"
replace year =2001 if country=="Tanzania"
replace year =2000 if country=="Uganda"
replace year =1999 if country=="Zambia" | country=="Zimbabwe"

gen round = 1

save merged_round1_edit.dta, replace

*************************************************
*************************************************
*************************************************

*** Round 2 ***

* Note: We do not use round 2 of Afrobaromenter since ther is no voting data.

*************************************************
*************************************************
*************************************************

*** Round 3 ***

use merged_r3_data.dta, clear

decode country, gen(country_name)
decode q90, gen(education)
decode q30, gen(vote)

keep country_name education vote
rename country_name country

gen year =0
replace year =2005 if country=="Benin"|  country=="Botswana" | country=="Cape Verde" | country=="Ghana" | country=="Kenya" | country=="Lesotho" | country=="Madagascar" | country=="Malawi" | country=="Mali" |country=="Mozambique" | country=="Nigeria"| country=="Senegal" | country=="Tanzania"| country=="Uganda" | country=="Zambia"| country=="Zimbabwe"
replace year = 2006 if country=="Namibia" | country=="South Africa"

gen round = 3

save merged_round3_edit.dta, replace

*************************************************
*************************************************
*************************************************

*** Round 4 ***

use merged_r4_data.dta, clear

decode country, gen(country_name)
decode Q89, gen(education)
decode Q23D, gen(vote)
 
keep country_name education vote
rename country_name country

gen year =0
replace year =2008 if country=="Benin" | country=="Botswana" | country=="Burkina Faso" | country=="Cape Verde" | country=="Ghana" | country=="Kenya" | country=="Lesotho" | country=="Liberia" | country=="Madagascar" | country=="Malawi" | country=="Mali"| country=="Mozambique" | country=="Namibia" | country=="Nigeria"| country=="Senegal" | country=="South Africa"| country=="Tanzania"| country=="Uganda"
replace year = 2009 if country=="Zambia"| country=="Zimbabwe"

gen round = 4

save merged_round4_edit.dta, replace

**************************************************************************************************
**************************************************************************************************
**************************************************************************************************

*** Data merge ***

use merged_round1_edit.dta, clear
append using merged_round3_edit.dta
append using merged_round4_edit.dta
erase merged_round1_edit.dta
erase merged_round3_edit.dta
erase merged_round4_edit.dta

merge m:1 country year using polityiv.dta
drop if _merge==2
drop _merge

**************************************************************************************************
**************************************************************************************************
**************************************************************************************************

*** Data coding ***

gen close_anocracy = (polity<=0)
gen open_anocracy = (polity>=1 & polity<=5)

gen schooling = .
replace schooling = 1 if strpos(education, "Informal")>0 | strpos(education, "No formal")>0 
replace schooling = 2 if strpos(education, "Some primary")>0 
replace schooling = 3 if strpos(education, "Primary only")>0 | strpos(education, "Primary school completed")>0 
replace schooling = 4 if strpos(education, "Some secondary")>0 
replace schooling = 5 if strpos(education, "Secondary")>0 | strpos(education, "Secondary school completed")>0 
replace schooling = 6 if strpos(education, "Some university")>0 | strpos(education, "Post-secondary qualifications, not univ ")>0 | strpos(education, "University completed")>0 | strpos(education, "college completed")>0  | strpos(education, "Post-graduate")>0 | strpos(education, "Post-secondary")>0 
drop education

gen secondary = 0 if schooling!=.
replace secondary = 1 if schooling>=5 & schooling!=.

gen voted = 0 if vote !=""
replace voted = . if  vote =="Can't remember" |  vote == "Don't know/Can't remember" | vote =="Missing" | vote =="Missing data" | vote =="Refused"
replace voted = 1 if strpos(vote, "I voted")>0 | strpos(vote, "You voted in the elections")>0 
drop vote

egen country_year = group(country year)

**************************************************************************************************
**************************************************************************************************
**************************************************************************************************

*** Appendix Table A9 ***

eststo clear
eststo, title("close_ano_sch"): xi: areg voted schooling i.year if close_anocracy==1, a(country) cluster(country_year) 
eststo, title("open_ano_sch"): xi: areg voted schooling i.year if open_anocracy==1, a(country) cluster(country_year) 
eststo, title("close_ano_sec"): xi: areg voted secondary i.year if close_anocracy==1, a(country) cluster(country_year) 
eststo, title("open_ano_sec"): xi: areg voted secondary i.year if open_anocracy==1, a(country) cluster(country_year) 
estout, cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(schooling secondary) label varlabel(schooling "Schooling" secondary "Secondary")
estout, style(tex) cells(b(star fmt(3)) se(par)) stats(N, fmt(0) labels("Observations")) starlevels(* .1 ** .05 *** .01) keep(schooling secondary) label varlabel(schooling "Schooling" secondary "Secondary")

*********
* Notes *
*********

* These commands will replicated the analyses in main text and online appendix of Petersen & Aar¿e, 2015
* The commands will work on the replication data set collect for the student sample 4  ///

*************
* Recodings *
*************

* generation of birth weight
gen bweight = s1s6
gen bweight01=(bweight-2460)/2040
ta bweight01

* generation of social trust
*primary measure of trust reported in main text
recode s2s18b (1=1) (2=.66) (3=.33) (4=0), gen(trust4) 

* secondary measure of trust mentionned in main text
*0-1 scaled from observed minimum to observed maximum
alpha s2s18b s2s18c s2s18d, gen(trust)


recode s2s18b s2s18c (1=4) (2=3) (3=2) (4=1) (2.5=.), gen (s2s18brq s2s18crq)
*OBS the answer 2.5 not possible on the variables - the recoding correct this coding mistake
cor s2s18b s2s18c s2s18brq s2s18crq 

egen trustx=rmean(s2s18brq s2s18crq s2s18d)
gen trust01 = (trustx-1)/3
ta trust01

* generation of cortisol awakening response

gen cortisol1=.
gen cortisol2=.
replace cortisol1=	7.7	if id==	1
replace cortisol1=	8.1	if id==	2
replace cortisol1=	5.3	if id==	3
replace cortisol1=	13.1	if id==	5
replace cortisol1=	7.2	if id==	8
replace cortisol1=	.	if id==	9
replace cortisol1=	7.1	if id==	10
replace cortisol1=	10.9	if id==	11
replace cortisol1=	6.6	if id==	12
replace cortisol1=	9.0	if id==	13
replace cortisol1=	12.8	if id==	14
replace cortisol1=	10.5	if id==	15
replace cortisol1=	7.6	if id==	16
replace cortisol1=	11.0	if id==	17
replace cortisol1=	8.6	if id==	18
replace cortisol1=	12.5	if id==	19
replace cortisol1=	.	if id==	20
replace cortisol1=	9.1	if id==	21
replace cortisol1=	2.6	if id==	23
replace cortisol1=	12.8	if id==	24
replace cortisol1=	5.1	if id==	26
replace cortisol1=	17.8	if id==	27
replace cortisol1=	5.7	if id==	28
replace cortisol1=	20.4	if id==	29
replace cortisol1=	4.2	if id==	31
replace cortisol1=	4.3	if id==	32
replace cortisol1=	1.9	if id==	33
replace cortisol1=	6.5	if id==	35
replace cortisol1=	13.9	if id==	36
replace cortisol1=	4.1	if id==	37
replace cortisol1=	9.9	if id==	38
replace cortisol1=	0.7	if id==	39
replace cortisol1=	1.9	if id==	40
replace cortisol1=	19.6	if id==	41
replace cortisol1=	8.7	if id==	42
replace cortisol2=	9.0	if id==	1
replace cortisol2=	16.6	if id==	2
replace cortisol2=	22.0	if id==	3
replace cortisol2=	11.2	if id==	5
replace cortisol2=	10.2	if id==	8
replace cortisol2=	.	if id==	9
replace cortisol2=	12.9	if id==	10
replace cortisol2=	17.7	if id==	11
replace cortisol2=	23.0	if id==	12
replace cortisol2=	18.6	if id==	13
replace cortisol2=	11.1	if id==	14
replace cortisol2=	13.2	if id==	15
replace cortisol2=	16.6	if id==	16
replace cortisol2=	11.4	if id==	17
replace cortisol2=	24.0	if id==	18
replace cortisol2=	11.4	if id==	19
replace cortisol2=	.	if id==	20
replace cortisol2=	11.8	if id==	21
replace cortisol2=	9.1	if id==	23
replace cortisol2=	21.1	if id==	24
replace cortisol2=	4.6	if id==	26
replace cortisol2=	9.7	if id==	27
replace cortisol2=	6.0	if id==	28
replace cortisol2=	14.4	if id==	29
replace cortisol2=	2.8	if id==	31
replace cortisol2=	15.6	if id==	32
replace cortisol2=	2.8	if id==	33
replace cortisol2=	10.6	if id==	35
replace cortisol2=	14.7	if id==	36
replace cortisol2=	8.6	if id==	37
replace cortisol2=	14.7	if id==	38
replace cortisol2=	4.3	if id==	39
replace cortisol2=	8.9	if id==	40
replace cortisol2=	29.5	if id==	41
replace cortisol2=	17.2	if id==	42

gen cortisolchange=cortisol2-cortisol1
sum cortisolchange

gen stresshorm01=(cortisolchange+8.099999)/(8.099999+16.7)
sum stresshorm01

* generation of interaction term
gen interact = stresshorm01*bweight01

** Generation of control variables

* finansial situation in early childhood
mvdecode s2s15c s2s15f s2s15g s2s15h s2s15i, mv(4)
recode s2s15c s2s15f s2s15g s2s15h (0=3) (1=2) (2=1) (3=0), gen (s2s15crq s2s15frq s2s15grq s2s15hrq)
egen incchild=rmean(s2s15crq s2s15frq s2s15grq s2s15hrq s2s15i)
gen incchild01 =(incchild/3)

* Mother's education
* Obs nobody answered that they had a phd
recode s2s11 (1=1) (4=2) (2=3) (3=3) (5=4) (6=5) (7=6) (8=7), gen(morudd)
gen morudd01=(morudd-1)/6
gen motheredu01=morudd01

* sex 
gen female = Køn

* age
gen alder01=(Alder-19)/(25-19)
gen age01 = alder01

**************************************
* Analysis reported in the main text *
**************************************

sum s1s6 if stresshorm01!=. & bweight01 !=.
sum trust4 if stresshorm01!=. & bweight !=.

reg trust4 c.bweight01 c.stresshorm01 interact i.female age01 motheredu01 incchild01, robust
test interact

reg trust4 c.bweight01##c.stresshorm01 i.female age01 motheredu01 incchild01, robust
margins, dydx(stresshorm01) at((mean) _all bweight01=(0 .20 .40 .60 .80 1)) level(95)

reg trust01 c.bweight01 c.stresshorm01 interact i.female age01 motheredu01 incchild01, robust
test interact 

*SOM8
ta female if stresshorm01!=. & bweight !=.
sum Alder if stresshorm01!=. & bweight !=.

* SOM9
sum s1s6 if stresshorm01!=. & bweight01 !=.
sum trust4 if stresshorm01!=. & bweight !=.

alpha s2s15crq s2s15frq s2s15grq s2s15hrq s2s15i

* SOM12 - table SOM6
* analysis reported in main text
reg trust4 c.bweight01##c.stresshorm01 i.female age01 motheredu01 incchild01, robust
* robustness with probit
oprobit trust4 c.bweight01##c.stresshorm01 i.female age01 motheredu01 incchild01, robust

* robustness with alternative 3-item scale
reg trust01 c.bweight01##c.stresshorm01 i.female age01 motheredu01 incchild01, robust

*The effect of birth weight for average levels of stress
reg trust4 c.bweight01 c.stresshorm01 i.female age01 motheredu01 incchild01, robust

* Table
eststo: quietly reg trust4 c.bweight01##c.stresshorm01 i.female age01 motheredu01 incchild01, robust
eststo: quietly oprobit trust4 c.bweight01##c.stresshorm01 i.female age01 motheredu01 incchild01, robust
eststo: quietly reg trust01 c.bweight01##c.stresshorm01 i.female age01 motheredu01 incchild01, robust
esttab, b(%5.2f) se(%5.2f), using results_cortisol.rtf, replace onecell star(* 0.10 ** 0.020 *** 0.002) label r2 pr2 constant 
eststo clear





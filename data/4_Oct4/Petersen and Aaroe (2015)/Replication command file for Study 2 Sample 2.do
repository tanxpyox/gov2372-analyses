*********
* Notes *
*********

* These commands will replicated the analyses in main text of Petersen & Aar¿e, 2015
* The commands will work on a merged dataset of all the 1996-2011 waves of the Danish ///
* Longitudinal Survey of Children (DALSC) ///
* Data can be accessed here: http://www.sfi.dk/about_the_research-11402.aspx (as of June 19 2015)

*************
* Recodings *
*************

** Generation of "Birth Weight"

sum m96v150
gen bw01=(m96v150-595)/(5770-595)

** Generation of "Social trust"

gen trust01=b11v317/10 
sum trust01

** Generation of "Delay Discounting" meausures

gen external7=m03hyper+m03conduct 
gen external11=m07hyper+m07conduct
gen external15=m11hyper+m11conduct
gen external701=external7/20
gen external1101=external11/20
gen external1501=external15/20
gen delay07=(external701-1)*-1
gen delay11=(external1101-1)*-1
gen delay15=(external1501-1)*-1

** Generation af "Sexually Inactive"

recode b11v179 (1=0) (5=1), gen(nosex)

** Generation of "Intelligence"

gen intel01=B11raven/12

** Generation of control variables

* Mother's education

recode m96v421 (1=.25) (2=.50) (3=.75) (4=1) (5=.25) (6=0), gen(educ01)
replace educ01=0 if m96v417==2

* Mother's income

mvdecode m96v464, mv(12, 88)
gen persinc01=(m96v464-1)/10

* Mother's tobacco use

recode m96v356 (.=0), copyrest gen(tobac1)
gen tobac01=tobac1/88

* Lenght of hospitalization

gen hospital01=m96v163/93

* Mother's assessment of child's health

gen health01=(m96v83-1)/4

* Child's gender

gen gender=(b11v6-1)/4

* Number of siblings

gen sibs01=(m96v18-1)/6

**************************
* Analyses for main text *
**************************

* Initial analyses of relationship between birth weight and trust

sem (trust01 <- bw01 gender), standardize nocapslatent

sem (trust01  <- bw01 gender educ01 persinc01 hospital01 health01 tobac01 sibs01), standardize nocapslatent

* Figure 1

sem (LHS -> delay15 delay11 delay07 nosex trust01) (trust01 <- intel01 bw01 gender educ01 persinc01 hospital01 health01 tobac01 sibs01) (LHS trust01 intel01 <- bw01 gender educ01 persinc01 hospital01 health01 tobac01 sibs01), standardize latent(LHS)
estat gof, stats(all)

* Test of indirect effect of birth weight through LHS

sem (LHS -> delay15 delay11 delay07 nosex trust01) (trust01 <- intel01 bw01 gender educ01 persinc01 hospital01 health01 tobac01 sibs01) (LHS trust01 <- bw01 gender educ01 persinc01 hospital01 health01 tobac01 sibs01), standardize latent(LHS)
estat teffects, standardize

* Test of indirect effect of birth weight through intelligence

sem (LHS -> delay15 delay11 delay07 nosex trust01) (trust01 <- intel01 bw01 gender educ01 persinc01 hospital01 health01 tobac01 sibs01) (trust01 intel01 <- bw01 gender educ01 persinc01 hospital01 health01 tobac01 sibs01), standardize latent(LHS)
estat teffects, standardize

********************
* Analyses for SOM *
********************

* SOM11 / Figure SOM1

sem (LHS -> delay15 nosex trust01) (LHS <- intel01 bw01 gender educ01 persinc01 hospital01 health01 tobac01 sibs01), standardize latent(LHS)
estat teffects, standardize
estat gof, stats(all)

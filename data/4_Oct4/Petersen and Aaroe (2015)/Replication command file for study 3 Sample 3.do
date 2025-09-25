*********
* Notes *
*********

* These commands will replicated the analyses in main text and online appendix of Petersen & Aar¿e, 2015
* The commands will work on the replicationdataset collected for the nationally representative sample 3  ///

*************
* Recodings *
*************

** Generation of "Birth Weight"
mvdecode Q17, mv(7)
gen bwcateg01=(Q17-1)/5
sum bwcateg01

mvdecode Q18_n1, mv(2 3 7 4)
gen BW01=(Q18_n1-425)/(5400-425)

** Generation of "Social trust"
* Single item measure
gen soctrust=(((Q38-1)/3)-1)*-1


* Alternative three item measure
recode Q38 Q39 (1=4) (2=3) (3=2) (4=1), gen (Q38rq Q39rq)
egen trustx= rmean(Q40 Q38rq Q39rq)
gen trust=(trustx-1)/3

** Experimental stress manipulation
recode rBlock12 (1=1) (2=0), gen(expstress)
label define expstress 1 "stress treatment" 0 "control condition"
label values expstress expstress

** Interaction term 
gen interact=bwcateg01*expstress

** Generation of control variables

* Finansial situation in early childhood
* the income scale was corrected in the middle of the data collection period and the coding below takes this correction into account
* Specifically, October 24-25 2013 income was measured on a 6 point scale (plus a "have no idea category)
* October 26 - 1 December 2013 income was measured on a nine point scale (plus a "have no idea category) - the three new categories were added at the lower end of the scale  

mvdecode Q5, mv(10)
gen inc01=(Q5-1)/8
recode Q5 (1=0) (5=0.25) (6=.5) (7=.75) (8=1), gen(inc01beforex)
gen float inc01before=inc01beforex
gen float Date=DataCollection_StartTime
replace inc01=inc01before if Date==19655 | Date==19656

* Mother's education
recode Q3_1_resp (1=1) (2=.8) (3=.6) (4=.4) (5=.2) (6=0) (7=.) (8=.), gen(motheredu01) 

* Sex
gen female=Q19-1

* Age
gen age=(Q20-14)/(79-14)

**************************************
* Analysis reported in the main text *
**************************************

ta Q17

reg soctrust c.bwcateg01##c.expstress female age inc01 motheredu01, robust
margins, dydx(expstress) at((mean) _all bwcateg01=(0 .20 .40 .60 .80 1)) level(95)

reg soctrust c.bwcateg01 interact c.expstress female age inc01 motheredu01, robust
test interact

*** ONLINE APPENDIX
* SOM6
ta female
sum Q20

recode Q22 (7=.) (8=.)
ta Q22

recode Q24 (10=.)
ta Q24

* SOM7
ta Q17
sum Q18_n1
sum soctrust
alpha Q40 Q38rq Q39rq
sum trust

*SOM12
reg soctrust c.bwcateg01##c.expstress female age inc01 motheredu01, robust
oprobit soctrust c.bwcateg01##c.expstress female age inc01 motheredu01, robust
oprobit soctrust c.BW01##c.expstress female age inc01 motheredu01, robust
reg trust c.bwcateg01##c.expstress female age inc01 motheredu01, robust
reg trust c.BW01##c.expstress female age inc01 motheredu01, robust

reg soctrust c.bwcateg01 c.expstress female age inc01 motheredu01, robust



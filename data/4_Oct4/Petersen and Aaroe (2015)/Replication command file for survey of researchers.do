*********
* Notes *
*********

* These commands will replicated the analyses in main text of Petersen & Aar¿e, 2015
* The commands will work on replication data set for the Survey of Researchers///

*************
* Recodings *
*************

*recodings
sum Q4_1
sum Q4_2
sum Q4_3
gen motheredu= Q4_1/100
gen motherincome= Q4_2/100
gen bw= Q4_3/100

*gender
ta Q7

gen birthyear3_n = real( Q6 )
sum birthyear3_n
gen age =2013- birthyear3_n
*somebody lists being born in 1864 - that is not possible - must be mistake and therefore excluded
replace age =. if birthyear3==1864
sum age


* SOM1

*sample characteristics
*discipline
ta Q2
*gender
ta Q7
*age
sum age
*rank
ta Q5

*findings - Table SOM1

* Mother's education
sum motheredu

* Mother's income
sum motherincome

*The person's birth weight
sum bw

*Socio-economic factors combined
gen mothereduincome=(motherincome+motheredu)
sum mothereduincome

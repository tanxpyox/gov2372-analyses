clear
insheet using "Rand_et_al_2014_Nature_Comm_megaanalysis_data.txt"

// Interpolate missing values for demographics
egen ageM=mean(age), by(study)
egen genderM=mean(gender), by(study)
egen indiaM=mean(india), by(study)
egen othernonusM=mean(othernonus), by(study)
replace age=ageM if age==.
replace gender=genderM if gender==.
replace india=indiaM if india==.
replace othernonus=othernonusM if othernonus==.
replace ed=7 if ed==.  // value 7 for education was 'unknown'


//////////////////////
// Table 1 - Time pressure effect among subjects obeying time constraint
char study [omit] "A"   // Set study A as the baseline
xi: reg decision fast i.study   if intime==1, cluster(ip)
xi: reg decision fast age gender india othernonus i.ed i.study  round failedcomp if intime==1, cluster(ip)
// Without RGN
xi: reg decision fast i.study   if  study~="A" & study~="D" & intime==1 , cluster(ip)
xi: reg decision fast age gender india othernonus i.ed i.study  round failedcomp if  study~="A" & study~="D" & intime==1 , cluster(ip)
// Only PGG studies
xi: reg decision fast i.study   if  pgg==1 & intime==1, cluster(ip)
xi: reg decision fast age gender india othernonus i.ed i.study  round failedcomp if pgg==1 & intime==1, cluster(ip)

//////////////////////
// Table 2 - Time pressure effect among all subjects
char study [omit] "A"   // Set study A as the baseline
xi: reg decision fast i.study  , cluster(ip)
xi: reg decision fast age gender india othernonus i.ed i.study  round failedcomp, cluster(ip)
// Without RGN
xi: reg decision fast i.study   if  study~="A" & study~="D" , cluster(ip)
xi: reg decision fast age gender india othernonus i.ed i.study  round failedcomp if  study~="A" & study~="D" , cluster(ip)
// Only PGG studies
xi: reg decision fast i.study   if  pgg==1, cluster(ip)
xi: reg decision fast age gender india othernonus i.ed i.study  round failedcomp if pgg==1, cluster(ip)

////////////////////////////////
// Table 3 - Date as moderator of time pressure effect on MTurk 
// (Note: there are 3 more subjects here than in the published version, but they do not qualitatively effect the results)
//    Subjects that obeyed time constraint
xi: reg decision i.fast*date_numer if intime==1 & mturk==1, cluster(ip)
xi: reg decision i.fast*date_numer failedcomp age gender india othernonus i.ed if intime==1 & mturk==1, cluster(ip)
test _IfasXdate+date_numer==0  // test net coefficient on date among fast subjects
//    All subjects
xi: reg decision i.fast*date_numer if mturk==1, cluster(ip)
xi: reg decision i.fast*date_numer failedcomp age gender india othernonus i.ed if mturk==1, cluster(ip)
test _IfasXdate+date_numer==0  // test net coefficient on date among fast subjects

//////////////////////////
// Table 4 - Decision times decreased over time on MTurk
// (Note: there are 3 more subjects here than in the published version, but they do not qualitatively effect the results)
gen Ltime=log10(decision_time)
replace Ltime=. if decision_time==.
//    Subjects that obeyed time constraint
xi: reg Ltime i.fast date_numer failedcomp age gender india othernonus i.ed if intime==1 & mturk==1, cluster(ip)
//    Subjects that obeyed time constraint, time pressure only
xi: reg Ltime date_numer failedcomp age gender india othernonus i.ed if intime==1 & mturk==1 & fast==1, cluster(ip)
//    All subjects
xi: reg Ltime i.fast date_numer failedcomp age gender india othernonus i.ed if  mturk==1 , cluster(ip)
//    All subjects, time pressure only
xi: reg Ltime date_numer failedcomp age gender india othernonus i.ed if mturk==1 & fast==1, cluster(ip)



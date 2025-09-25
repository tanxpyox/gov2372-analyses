clear
insheet using "Rand_et_al_2014_Nature_Comm_extractionPGG_data.txt"

/////////////////////
// Table 5
//
//  Subjects that obeyed the time constraint
xi: reg coop i.fast*experienced if intime==1, r
xi: reg coop i.fast*experienced failedcomp age gender i.ed social_cons fiscal_cons god if intime==1, r
//        no sig simple effect of time pressure among experienced subjects
test _Ifast_1 + _IfasXexper_1 =0
//        sig simple effect of experience among time pressured subjects
test experienced + _IfasXexper_1 =0
//    All subjects
xi: reg coop i.fast*experienced , r
xi: reg coop i.fast*experienced failedcomp age gender i.ed social_cons fiscal_cons god , r
//        no sig simple effect of time pressure among experienced subjects
test _Ifast_1 + _IfasXexper_1 =0
//        sig simple effect of experience among time pressured subjects
test experienced + _IfasXexper_1 =0


////////////////////////
// Also find the interaction between experience and time pressure using log10(total # of studies completed)
xi: reg coop i.fast*l_numstudies failedcomp age gender i.ed social_cons fiscal_cons god if intime==1, r
xi: reg coop i.fast*l_numstudies failedcomp age gender i.ed social_cons fiscal_cons god , r


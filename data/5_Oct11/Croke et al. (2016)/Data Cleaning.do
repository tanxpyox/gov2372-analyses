*** Set working directory
capture cd "~\Replication"



******************************************
***        Code up Afrobarometer       ***
******************************************

use zim_r1_data.dta, clear

*** Demographics ***
rename Q110 age
replace age =. if age>91

decode Q112, gen(schooling)
decode Q121B, gen(urban_rural)
decode Q122, gen(province)
decode Q125, gen(gender)
decode Q126, gen(race)

gen year_survey = 1999

*** Economic Conditions ***
decode Q13A, gen(no_food)
decode Q13C, gen(no_meds)
decode Q13D, gen(no_cash)
decode Q13E, gen(no_water)
decode Q13H, gen(no_fuel)

*** Political participation ***
decode Q89, gen(q89)
gen voted = (strpos(q89,"voted")>0)
replace voted = . if Q89>=5

*** Social Capital ***
decode Q18C, gen(att_comm_meet1)
decode Q18E, gen(att_comm_meet2)
decode Q92A, gen(att_demo)

*** Contact politician / party ***
decode Q34A, gen(q34a)
decode Q34B, gen(q34b)
decode Q34C, gen(q34c)
decode Q34D, gen(q34d)
decode Q34E, gen(q34e)

gen contacted_local_govt =0
gen contacted_mp=0
gen contacted_party=0
foreach var in q34a q34b q34c q34d q34e {
replace contacted_local_govt =  1 if strpos(`var', "ocal council")>0
replace contacted_mp = 1 if strpos(`var', "mp")>0 
replace contacted_party = 1 if strpos(`var', "Party official")>0 
}
replace contacted_local_govt = . if Q34A==98 | Q34A==99
replace contacted_mp = . if Q34A==98 | Q34A==99
replace contacted_party = . if Q34A==98 | Q34A==99

decode Q107, gen(close_party)
decode Q108, gen(party_close)

decode Q46C, gen(politcs_complicated)

decode Q50A, gen(problem1)
decode Q50B, gen(problem2)
decode Q50C, gen(problem3)

*** Contact traditional leaders ***
decode Q37A, gen(q37a)
decode Q37B, gen(q37b)
decode Q37C, gen(q37c)
decode Q37D, gen(q37d)

gen contacted_rel_leader =0
gen contacted_trad_leader=0
foreach var in q37a q37b q37c q37d { 
replace contacted_rel_leader =  1 if strpos(`var', "Church leader")>0
replace contacted_trad_leader = 1 if strpos(`var', "Traditional leader")>0 
}
replace contacted_rel_leader = . if Q37A==98
replace contacted_trad_leader = . if Q37A==98

*** News /Informedness ***
decode Q42A, gen(radio)
decode Q42B, gen(tv)
decode Q42C, gen(newspapers)
decode Q44 , gen(disc_pol)
decode Q45, gen(int_pub_aff)

*** Democracy ***
decode Q78, gen(supp_demo)
decode Q105, gen(sat_demo)
decode Q104, gen(democracy)
decode Q52, gen(reject_one_party)
decode Q55, gen(reject_one_man)
decode Q46D, gen(careful_say)

decode Q55, gen(parl_laws)

*** Performance ***
decode Q65, gen(good_perf_pres)
decode Q69, gen(good_perf_mps)
decode Q74, gen(good_perf_lg)
decode Q64, gen(trust_pres)
decode Q68, gen(trust_mp)
decode Q73, gen(trust_lg)

decode Q70, gen(corr_mps)
decode Q66, gen(corr_off)
decode Q75, gen(corr_lg_off)

decode Q62G, gen(sat_manag_econ)
decode Q62A, gen(sat_creating_jobs)
decode Q62C, gen(sat_prices_down)
decode Q62D, gen(sat_reducing_crime)
decode Q62E, gen(sat_impr_health_ss)
decode Q62F, gen(sat_address_educ_needs)
decode Q62H, gen(sat_prov_water_and_sanit_ss)
decode Q62H, gen(sat_prov_electricity)

*** Employment ***
decode Q113, gen(empl_stat)

*** Ethnicity ***
decode Q111, gen(language)

*** District ***
gen district=""
replace district= "Bikita" if province=="Masvingo"& ea=="Bikita"
replace district= "Bikita" if province=="Masvingo"& ea=="Bikita BC"
replace district= "Bikita" if province=="Masvingo"& ea=="Chinyika"
replace district= "Bikita" if province=="Masvingo"& ea=="Chinyika Gutu"
replace district= "Bikita" if province=="Masvingo"& ea=="Chinyika gutu"
replace district= "Bikita" if province=="Masvingo"& ea=="Devule Ranch"
replace district= "Bikita" if province=="Masvingo"& ea=="Devuli Ranch"
replace district= "Bikita" if province=="Masvingo"& ea=="Devure Ranch"
replace district= "Bikita" if province=="Masvingo"& ea=="Magocha"
replace district= "Bikita" if province=="Masvingo"& ea=="Magocha Bikita"
replace district= "Bikita" if province=="Masvingo"& ea=="Magoche Mabika"
replace district= "Bikita" if province=="Masvingo"& ea=="Magoche Mashoko"
replace district= "Bikita" if province=="Masvingo"& ea=="Mashoko"
replace district= "Bikita" if province=="Masvingo"& ea=="Mashoko Bikita"
replace district= "Bikita" if province=="Masvingo"& ea=="Mbare Harare"
replace district= "Bikita" if province=="Masvingo"& ea=="Mukanganwi Bikita"
replace district= "Bikita" if province=="Masvingo"& ea=="Mutengwa Bikita"
replace district= "Bikita" if province=="Masvingo"& ea=="Nyemba - Bikita"
replace district= "Bikita" if province=="Masvingo"& ea=="Nyemba Bikita"
replace district= "Bikita" if province=="Masvingo"& ea=="Shaka"
replace district= "Bikita" if province=="Masvingo"& ea=="Shaka Bikita"
replace district= "Bindura" if province=="Mashonaland Central"& ea=="Bindura"
replace district= "Bindura" if province=="Mashonaland Central"& ea=="Bindura Flats"
replace district= "Bindura" if province=="Mashonaland Central"& ea=="Guhwa Bindura"
replace district= "Bindura" if province=="Mashonaland Central"& ea=="Gvt Flat Bindura"
replace district= "Bindura" if province=="Mashonaland Central"& ea=="Gvt Flats Bindura"
replace district= "Buhera" if province=="Manicaland"& ea=="Bepe"
replace district= "Buhera" if province=="Manicaland"& ea=="Mdenden Buhera"
replace district= "Buhera" if province=="Manicaland"& ea=="Chirozva Buhera"
replace district= "Buhera" if province=="Manicaland"& ea=="Bepe Makumbe"
replace district= "Buhera" if province=="Manicaland"& ea=="Chirozva"
replace district= "Buhera" if province=="Manicaland"& ea=="Bepe Buhera"
replace district= "Buhera" if province=="Manicaland"& ea=="Madende"
replace district= "Buhera" if province=="Manicaland"& ea=="Chirozvi Buhera"
replace district= "Buhera" if province=="Manicaland"& ea=="Madende Buhera"
replace district= "Buhera" if province=="Manicaland"& ea=="Madende Makumbe"
replace district= "Bulilima (North)" if province=="Matabeleland South"& ea=="Lupane"
replace district= "Bulilima (North)" if province=="Matabeleland South"& ea=="Mene"
replace district= "Bulilima (North)" if province=="Matabeleland South"& ea=="Mene Plumtree"
replace district= "Bulilima (North)" if province=="Matabeleland South"& ea=="Ndiweni"
replace district= "Bulilima (North)" if province=="Matabeleland South"& ea=="Phakama"
replace district= "Bulilima (North)" if province=="Matabeleland South"& ea=="Thsolotsho"
replace district= "Bulilima (North)" if province=="Matabeleland South"& ea=="Tsholotsho"
replace district= "Chegutu" if province=="Mashonaland West"& ea=="Gadzema"
replace district= "Chegutu" if province=="Mashonaland West"& ea=="Norton"
replace district= "Chegutu" if province=="Mashonaland West"& ea=="Orange Grove"
replace district= "Chegutu" if province=="Mashonaland West"& ea=="Orange Grove Chinhoy"
replace district= "Chikomba" if province=="Mashonaland East"& ea=="Manyene"
replace district= "Chikomba" if province=="Mashonaland East"& ea=="Manyene Chipindu"
replace district= "Chikomba" if province=="Mashonaland East"& ea=="Manyene Chivu"
replace district= "Chikomba" if province=="Mashonaland East"& ea=="Manyene Mutiti"
replace district= "Chikomba" if province=="Mashonaland East"& ea=="Manyene Nyoka"
replace district= "Chikomba" if province=="Mashonaland East"& ea=="Rufu Manyene"
replace district= "Chikomba" if province=="Mashonaland East"& ea=="Wilsthire"
replace district= "Chikomba" if province=="Mashonaland East"& ea=="Wiltshire"
replace district= "Chikomba" if province=="Mashonaland East"& ea=="Wiltshire Chikomba"
replace district= "Chikomba" if province=="Mashonaland East"& ea=="Wiltshire Chivu"
replace district= "Chimanimani" if province=="Manicaland"& ea=="Chimanimani"
replace district= "Chimanimani" if province=="Manicaland"& ea=="Mutambara"
replace district= "Chimanimani" if province=="Manicaland"& ea=="Mtambara"
replace district= "Chimanimani" if province=="Manicaland"& ea=="Ngangu"
replace district= "Chimanimani" if province=="Manicaland"& ea=="Ngandu"
replace district= "Chipinge" if province=="Manicaland"& ea=="Musikavanhu"
replace district= "Chipinge" if province=="Manicaland"& ea=="Musikavanhu Chipinge"
replace district= "Chipinge" if province=="Manicaland"& ea=="Mwacheta"
replace district= "Chipinge" if province=="Manicaland"& ea=="Junction Gate"
replace district= "Chipinge" if province=="Manicaland"& ea=="Chipinge"
replace district= "Chiredzi" if province=="Masvingo"& ea=="Buffalo Ranch"
replace district= "Chiredzi" if province=="Masvingo"& ea=="Buffalo Range"
replace district= "Chiredzi" if province=="Masvingo"& ea=="Chibwedziva Chiredzi"
replace district= "Chiredzi" if province=="Masvingo"& ea=="Chipinda Chiredzi"
replace district= "Chiredzi" if province=="Masvingo"& ea=="Chiredzi urban"
replace district= "Chiredzi" if province=="Masvingo"& ea=="Railways Chiredzi"
replace district= "Chiredzi" if province=="Masvingo"& ea=="Triangle"
replace district= "Chirumanzu" if province=="Midlands"& ea=="Mutare Chirimanzu"
replace district= "Chirumanzu" if province=="Midlands"& ea=="Chinyika Chirimanzu"
replace district= "Chirumanzu" if province=="Midlands"& ea=="Chirimanzu"
replace district= "Chirumanzu" if province=="Midlands"& ea=="Anthens Mine Mvuma"
replace district= "Chirumanzu" if province=="Midlands"& ea=="Athens Mine Mvuma"
replace district= "Chirumanzu" if province=="Midlands"& ea=="Mvuma"
replace district= "Chivi" if province=="Masvingo"& ea=="Maringire"
replace district= "Chivi" if province=="Masvingo"& ea=="Maringire Chivi"
replace district= "Chivi" if province=="Masvingo"& ea=="Maringire Gwezuva"
replace district= "Chivi" if province=="Masvingo"& ea=="Maringire Mapanzure"
replace district= "Chivi" if province=="Masvingo"& ea=="Ngundu Halt"
replace district= "Chivi" if province=="Masvingo"& ea=="Ngundu Halt Chivi"
replace district= "Chivi" if province=="Masvingo"& ea=="Ngundu Halt Gororo"
replace district= "Chivi" if province=="Masvingo"& ea=="Taru"
replace district= "Chivi" if province=="Masvingo"& ea=="Taru BC"
replace district= "Chivi" if province=="Masvingo"& ea=="Taru Chigapa"
replace district= "Gokwe North" if province=="Midlands"& ea=="Madzivazvido"
replace district= "Gokwe North" if province=="Midlands"& ea=="Madzivazvido Gokwe"
replace district= "Gokwe South" if province=="Midlands"& ea=="Gwave Gokwe"
replace district= "Gokwe South" if province=="Midlands"& ea=="Chemumvuri Gokwe"
replace district= "Gokwe South" if province=="Midlands"& ea=="Nemangwe Gokwe"
replace district= "Gokwe South" if province=="Midlands"& ea=="Chitekete Gokwe"
replace district= "Gokwe South" if province=="Midlands"& ea=="Chomumvuri Gokwe"
replace district= "Gokwe South" if province=="Midlands"& ea=="Njelele Gokwe"
replace district= "Gokwe South" if province=="Midlands"& ea=="Gwakudza Gokwe"
replace district= "Gokwe South" if province=="Midlands"& ea=="Nemangwe"
replace district= "Gokwe South" if province=="Midlands"& ea=="Chemumvuri"
replace district= "Gokwe South" if province=="Midlands"& ea=="Sesame"
replace district= "Gokwe South" if province=="Midlands"& ea=="Chitekete"
replace district= "Gokwe South" if province=="Midlands"& ea=="Chomumvuri"
replace district= "Gokwe South" if province=="Midlands"& ea=="Njelele"
replace district= "Gokwe South" if province=="Midlands"& ea=="Gwavi"
replace district= "Gokwe South" if province=="Midlands"& ea=="Chomubvuri"
replace district= "Goromonzi" if province=="Mashonaland East"& ea=="Domboshava"
replace district= "Goromonzi" if province=="Mashonaland East"& ea=="Domboshawa"
replace district= "Goromonzi" if province=="Mashonaland East"& ea=="Goromonzi"
replace district= "Goromonzi" if province=="Mashonaland East"& ea=="Juru"
replace district= "Goromonzi" if province=="Mashonaland East"& ea=="Mukombami"
replace district= "Guruve" if province=="Mashonaland Central"& ea=="Chipangura Guruve"
replace district= "Guruve" if province=="Mashonaland Central"& ea=="Gakwi Guruve"
replace district= "Guruve" if province=="Mashonaland Central"& ea=="Guruve"
replace district= "Guruve" if province=="Mashonaland Central"& ea=="Nyangaro Guruve"
replace district= "Guruve" if province=="Mashonaland Central"& ea=="Nyangave Guruve"
replace district= "Gutu" if province=="Masvingo"& ea=="Chingai"
replace district= "Gutu" if province=="Masvingo"& ea=="Chingai Gutu"
replace district= "Gutu" if province=="Masvingo"& ea=="Mauringanzu Chinyika"
replace district= "Gutu" if province=="Masvingo"& ea=="Rafamoyo Gutu"
replace district= "Gutu" if province=="Masvingo"& ea=="Zvavahera"
replace district= "Gutu" if province=="Masvingo"& ea=="Zvavahera - Gutu"
replace district= "Gutu" if province=="Masvingo"& ea=="Zvavahera Gutu"
replace district= "Gwanda" if province=="Matabeleland South"& ea=="Jahunda"
replace district= "Gwanda" if province=="Matabeleland South"& ea=="Sandawana"
replace district= "Gweru" if province=="Midlands"& ea=="Insukamini Midlands"
replace district= "Gweru" if province=="Midlands"& ea=="Isikhamini Midlands"
replace district= "Gweru" if province=="Midlands"& ea=="Insukumini"
replace district= "Gweru" if province=="Midlands"& ea=="Senga Area 2"
replace district= "Gweru" if province=="Midlands"& ea=="Insukamini"
replace district= "Gweru" if province=="Midlands"& ea=="Maboleni Sigwala"
replace district= "Gweru" if province=="Midlands"& ea=="Maboleni"
replace district= "Gweru" if province=="Midlands"& ea=="Mkoba"
replace district= "Gweru" if province=="Midlands"& ea=="Senga"
replace district= "Gweru" if province=="Midlands"& ea=="Senga II"
replace district= "Gweru" if province=="Midlands"& ea=="Isukhamini"
replace district= "Gweru" if province=="Midlands"& ea=="Maboleni Sogwala"
replace district= "Hurungwe" if province=="Mashonaland West"& ea=="Birimahwe"
replace district= "Hurungwe" if province=="Mashonaland West"& ea=="Birimahwe Magunje"
replace district= "Hurungwe" if province=="Mashonaland West"& ea=="Charles Clark"
replace district= "Hurungwe" if province=="Mashonaland West"& ea=="Charlese Clark"
replace district= "Hurungwe" if province=="Mashonaland West"& ea=="Charlse Clark"
replace district= "Hurungwe" if province=="Mashonaland West"& ea=="Kamureza Karoi"
replace district= "Hurungwe" if province=="Mashonaland West"& ea=="Kemureza Magunje"
replace district= "Hurungwe" if province=="Mashonaland West"& ea=="Magunje"
replace district= "Hurungwe" if province=="Mashonaland West"& ea=="Urongonora"
replace district= "Hurungwe" if province=="Mashonaland West"& ea=="Urongonora Magunje"
replace district= "Hwange" if province=="Matabeleland North"& ea=="Lukhosi"
replace district= "Hwange" if province=="Matabeleland North"& ea=="Lukosi"
replace district= "Hwange" if province=="Matabeleland North"& ea=="Lwendule Hwange"
replace district= "Hwange" if province=="Matabeleland North"& ea=="Lwendulu Hwange"
replace district= "Hwange" if province=="Matabeleland North"& ea=="Lwendulu Hwange]"
replace district= "Hwange" if province=="Matabeleland North"& ea=="Mpumalanga"
replace district= "Hwange" if province=="Matabeleland North"& ea=="NdangabaLI"
replace district= "Hwange" if province=="Matabeleland North"& ea=="Ndangababi Hwange"
replace district= "Hwange" if province=="Matabeleland North"& ea=="Ndangabali"
replace district= "Hwange" if province=="Matabeleland North"& ea=="Nekabandama"
replace district= "Hwange" if province=="Matabeleland North"& ea=="Nekabandama Hwange"
replace district= "Insiza" if province=="Matabeleland South"& ea=="Dibilishamba"
replace district= "Insiza" if province=="Matabeleland South"& ea=="Isonomy"
replace district= "Insiza" if province=="Matabeleland South"& ea=="Mazeya"
replace district= "Insiza" if province=="Matabeleland South"& ea=="Mazeya Insiza"
replace district= "Kadoma" if province=="Mashonaland West"& ea=="Dalny Mine"
replace district= "Kadoma" if province=="Mashonaland West"& ea=="Dalyn Mine"
replace district= "Kadoma" if province=="Mashonaland West"& ea=="Golden Valley"
replace district= "Kadoma" if province=="Mashonaland West"& ea=="Golden Valley Mine"
replace district= "Kadoma" if province=="Mashonaland West"& ea=="Rimuka Kadoma"
replace district= "Kadoma" if province=="Mashonaland West"& ea=="Rimuka kadoma"
replace district= "Kwekwe" if province=="Midlands"& ea=="Zhombe Mission Gwese"
replace district= "Kwekwe" if province=="Midlands"& ea=="Zhombe"
replace district= "Kwekwe" if province=="Midlands"& ea=="Zhombe Gwesera"
replace district= "Kwekwe" if province=="Midlands"& ea=="Mbizo"
replace district= "Lupane" if province=="Matabeleland North"& ea=="Lupane"
replace district= "Makonde" if province=="Mashonaland West"& ea=="Alaska"
replace district= "Makonde" if province=="Mashonaland West"& ea=="Alaska Mine"
replace district= "Makonde" if province=="Mashonaland West"& ea=="Chebanga"
replace district= "Makonde" if province=="Mashonaland West"& ea=="Mangura"
replace district= "Makonde" if province=="Mashonaland West"& ea=="Mhangura"
replace district= "Makonde" if province=="Mashonaland West"& ea=="Mhangura Chebanda"
replace district= "Makonde" if province=="Mashonaland West"& ea=="Mhangura Mine"
replace district= "Makonde" if province=="Mashonaland West"& ea=="Mhangura mine"
replace district= "Makonde" if province=="Mashonaland West"& ea=="Umzari Chinhoyi"
replace district= "Makoni" if province=="Manicaland"& ea=="St Lukes Makoni"
replace district= "Makoni" if province=="Manicaland"& ea=="St Peters Cheche"
replace district= "Makoni" if province=="Manicaland"& ea=="St Kilians Makoni"
replace district= "Makoni" if province=="Manicaland"& ea=="St Lukes"
replace district= "Makoni" if province=="Manicaland"& ea=="St Peters Chipinge"
replace district= "Makoni" if province=="Manicaland"& ea=="St Kilians Rusape"
replace district= "Makoni" if province=="Manicaland"& ea=="St Lukes Rusape"
replace district= "Makoni" if province=="Manicaland"& ea=="St Kilians"
replace district= "Makoni" if province=="Manicaland"& ea=="St Peters"
replace district= "Makoni" if province=="Manicaland"& ea=="Headlands"
replace district= "Makoni" if province=="Manicaland"& ea=="Headland"
replace district= "Makoni" if province=="Manicaland"& ea=="Rusape Dame"
replace district= "Makoni" if province=="Manicaland"& ea=="Rusape Dam"
replace district= "Marondera" if province=="Mashonaland East"& ea=="Marondera"
replace district= "Marondera" if province=="Mashonaland East"& ea=="Waddilove"
replace district= "Masvingo" if province=="Masvingo"& ea=="Bondolf Mushawasha"
replace district= "Masvingo" if province=="Masvingo"& ea=="Chirimanzu"
replace district= "Masvingo" if province=="Masvingo"& ea=="Chirumanzu"
replace district= "Masvingo" if province=="Masvingo"& ea=="Chivika Chipinda Poo"
replace district= "Masvingo" if province=="Masvingo"& ea=="Mapanzure"
replace district= "Masvingo" if province=="Masvingo"& ea=="Mapanzure Shumba"
replace district= "Masvingo" if province=="Masvingo"& ea=="Mucheke"
replace district= "Masvingo" if province=="Masvingo"& ea=="Mucheke Masvingo"
replace district= "Masvingo" if province=="Masvingo"& ea=="Mucheke Masvingo Urb"
replace district= "Masvingo" if province=="Masvingo"& ea=="Mushawasha"
replace district= "Masvingo" if province=="Masvingo"& ea=="Mushawasha East"
replace district= "Masvingo" if province=="Masvingo"& ea=="Mushawasha Mapanzure"
replace district= "Masvingo" if province=="Masvingo"& ea=="Mushawasha Shumba"
replace district= "Masvingo" if province=="Masvingo"& ea=="Ruponeso Mucheke"
replace district= "Masvingo" if province=="Masvingo"& ea=="Shumba Mapanzure"
replace district= "Masvingo" if province=="Masvingo"& ea=="Tadzembwa"
replace district= "Masvingo" if province=="Masvingo"& ea=="Tadzembwa Mapanzure"
replace district= "Masvingo" if province=="Masvingo"& ea=="Tadzembwa Mushawasha"
replace district= "Matobo" if province=="Matabeleland South"& ea=="Kezi"
replace district= "Matobo" if province=="Matabeleland South"& ea=="Kezi Mat South"
replace district= "Mazowe" if province=="Mashonaland Central"& ea=="Mvurwi"
replace district= "Mazowe" if province=="Mashonaland Central"& ea=="Mvurwi Hospital"
replace district= "Mazowe" if province=="Mashonaland Central"& ea=="Nzvimbo"
replace district= "Mazowe" if province=="Mashonaland Central"& ea=="Nzvimbo Chiweshe"
replace district= "Mberengwa" if province=="Midlands"& ea=="Rupange Mberengwa"
replace district= "Mberengwa" if province=="Midlands"& ea=="Matedza Midlands"
replace district= "Mberengwa" if province=="Midlands"& ea=="Chipinda Chiredzi"
replace district= "Mberengwa" if province=="Midlands"& ea=="Mateki Mberengwa"
replace district= "Mberengwa" if province=="Midlands"& ea=="Matedzi"
replace district= "Mberengwa" if province=="Midlands"& ea=="Rupanye"
replace district= "Mberengwa" if province=="Midlands"& ea=="Nyamhondo"
replace district= "Mberengwa" if province=="Midlands"& ea=="Makuva Chingoma"
replace district= "Mberengwa" if province=="Midlands"& ea=="Makuva"
replace district= "Mberengwa" if province=="Midlands"& ea=="Matedzi Mafusha"
replace district= "Mberengwa" if province=="Midlands"& ea=="Makuwa"
replace district= "Mberengwa" if province=="Midlands"& ea=="Rupange"
replace district= "Mhondoro-Ngezi" if province=="Mashonaland West"& ea=="Dzumbunu Mhondoro"
replace district= "Mount Darwin" if province=="Mashonaland Central"& ea=="Chihoko"
replace district= "Mount Darwin" if province=="Mashonaland Central"& ea=="Chihoko Mt Darwin"
replace district= "Mount Darwin" if province=="Mashonaland Central"& ea=="Chikoko"
replace district= "Mount Darwin" if province=="Mashonaland Central"& ea=="Dotito"
replace district= "Mount Darwin" if province=="Mashonaland Central"& ea=="Dotito Mt Darwin"
replace district= "Mount Darwin" if province=="Mashonaland Central"& ea=="Nyajenje"
replace district= "Mount Darwin" if province=="Mashonaland Central"& ea=="Nyajenje Mt Darwin"
replace district= "Mudzi" if province=="Mashonaland East"& ea=="Nyamande"
replace district= "Mudzi" if province=="Mashonaland East"& ea=="Nyamapanda"
replace district= "Mudzi" if province=="Mashonaland East"& ea=="Nyamapandaz"
replace district= "Murehwa" if province=="Mashonaland East"& ea=="Chamapango"
replace district= "Murehwa" if province=="Mashonaland East"& ea=="Chemapango"
replace district= "Murehwa" if province=="Mashonaland East"& ea=="Chemapangu"
replace district= "Murehwa" if province=="Mashonaland East"& ea=="Nhow Mission"
replace district= "Murehwa" if province=="Mashonaland East"& ea=="Nhowe"
replace district= "Murehwa" if province=="Mashonaland East"& ea=="Nhowe Mission"
replace district= "Mutare" if province=="Manicaland"& ea=="Zimunya"
replace district= "Mutare" if province=="Manicaland"& ea=="Zimunya Mutare"
replace district= "Mutare" if province=="Manicaland"& ea=="Dora Pinto"
replace district= "Mutare" if province=="Manicaland"& ea=="Dora Pindo"
replace district= "Mutare" if province=="Manicaland"& ea=="Dangamvura"
replace district= "Mutare" if province=="Manicaland"& ea=="Dora"
replace district= "Mutare" if province=="Manicaland"& ea=="Nyamandwe"
replace district= "Mutare" if province=="Manicaland"& ea=="Dora pindo"
replace district= "Mutare" if province=="Manicaland"& ea=="Doropindo"
replace district= "Mutasa" if province=="Manicaland"& ea=="Bonda"
replace district= "Mutoko" if province=="Mashonaland East"& ea=="Mutoko"
replace district= "Mutoko" if province=="Mashonaland East"& ea=="Mutoko Mission"
replace district= "Muzarabani" if province=="Mashonaland Central"& ea=="Centenary"
replace district= "Muzarabani" if province=="Mashonaland Central"& ea=="Gatu"
replace district= "Muzarabani" if province=="Mashonaland Central"& ea=="Gatu Centenary"
replace district= "Nkayi" if province=="Matabeleland North"& ea=="Nkayi"
replace district= "Nyanga" if province=="Manicaland"& ea=="Nyatondo"
replace district= "Nyanga" if province=="Manicaland"& ea=="Dzembe Nyanga"
replace district= "Nyanga" if province=="Manicaland"& ea=="Nyatondo Nyanga"
replace district= "Rushinga" if province=="Mashonaland Central"& ea=="Chimhanda"
replace district= "Rushinga" if province=="Mashonaland Central"& ea=="Nyamarodza"
replace district= "Rushinga" if province=="Mashonaland Central"& ea=="Rushinga"
replace district= "Seke" if province=="Mashonaland East"& ea=="Beatrice"
replace district= "Seke" if province=="Mashonaland East"& ea=="Chirimamhunga"
replace district= "Seke" if province=="Mashonaland East"& ea=="Dema"
replace district= "Seke" if province=="Mashonaland East"& ea=="Marirangwe"
replace district= "Shamva" if province=="Mashonaland Central"& ea=="Bushu"
replace district= "Shamva" if province=="Mashonaland Central"& ea=="Bushu Shamva"
replace district= "Shamva" if province=="Mashonaland Central"& ea=="Jiti"
replace district= "Shurugwi" if province=="Midlands"& ea=="Jobolingo Midlands"
replace district= "Shurugwi" if province=="Midlands"& ea=="Jobolinko"
replace district= "Shurugwi" if province=="Midlands"& ea=="Jobolinko Shurugwi"
replace district= "Tsholotsho" if province=="Matabeleland North"& ea=="Ndiweni"
replace district= "Tsholotsho" if province=="Matabeleland North"& ea=="Sandawan"
replace district= "Tsholotsho" if province=="Matabeleland North"& ea=="Sandawana"
replace district= "Tsholotsho" if province=="Matabeleland North"& ea=="Tsholothso"
replace district= "Tsholotsho" if province=="Matabeleland North"& ea=="Tsholotsho"
replace district= "Ump" if province=="Mashonaland East"& ea=="UMP"
replace district= "Ump" if province=="Mashonaland East"& ea=="UMP Mutata"
replace district= "Ump" if province=="Mashonaland East"& ea=="Uzumba"
replace district= "Wedza" if province=="Mashonaland East"& ea=="Maruta"
replace district= "Wedza" if province=="Mashonaland East"& ea=="Maruta Wedza"
replace district= "Wedza" if province=="Mashonaland East"& ea=="Rujeko"
replace district= "Wedza" if province=="Mashonaland East"& ea=="Rujeko Marondera"
replace district= "Wedza" if province=="Mashonaland East"& ea=="Wedza"
replace district= "Zaka" if province=="Masvingo"& ea=="Chimedza"
replace district= "Zaka" if province=="Masvingo"& ea=="Chimedza Ndanga"
replace district= "Zaka" if province=="Masvingo"& ea=="Chinemba Ndanga"
replace district= "Zaka" if province=="Masvingo"& ea=="Jerera"
replace district= "Zaka" if province=="Masvingo"& ea=="Jerera Zaka"
replace district= "Zaka" if province=="Masvingo"& ea=="Ndanga"
replace district= "Zaka" if province=="Masvingo"& ea=="Zaka Ndanga"
replace district= "Zvimba" if province=="Mashonaland West"& ea=="Banket"
replace district= "Zvimba" if province=="Mashonaland West"& ea=="Chipangura Guruve"
replace district= "Zvimba" if province=="Mashonaland West"& ea=="Gakwe Guruve"
replace district= "Zvimba" if province=="Mashonaland West"& ea=="Gwebi"
replace district= "Zvimba" if province=="Mashonaland West"& ea=="Jack Farm Guruve"
replace district= "Zvimba" if province=="Mashonaland West"& ea=="Kasipit"
replace district= "Zvimba" if province=="Mashonaland West"& ea=="Kasipiti"
replace district= "Zvimba" if province=="Mashonaland West"& ea=="Nyabira"
replace district= "Zvimba" if province=="Mashonaland West"& ea=="Nyangara Guruve"
replace district= "Zvimba" if province=="Mashonaland West"& ea=="Nyangaro Guruve"
replace district= "Zvimba" if province=="Mashonaland West"& ea=="Nybira"
replace district= "Zvimba" if province=="Mashonaland West"& ea=="Woodley Farm Banket"
replace district= "Zvimba" if province=="Mashonaland West"& ea=="kasipit"
replace district= "Zvishavane" if province=="Midlands"& ea=="Hanawa Matenda"
replace district= "Zvishavane" if province=="Midlands"& ea=="Hanawa"
replace district= "Zvishavane" if province=="Midlands"& ea=="Matenda"
replace district= "Zvishavane" if province=="Midlands"& ea=="Hanana Matenda"

replace district= "Bulawayo" if province=="Bulawayo"
replace district= "Harare" if province=="Harare"

keep casenumb province district age schooling urban_rural gender race year_survey voted contact* radio tv news disc_pol int_pub_aff att_* close_party party_close empl_stat *demo* language politcs_complicated reject* careful_say good_* trust_* no_* corr_*  parl_laws sat_* problem*

rename casenumb respno

save round1_edit.dta, replace

*************************************************
*************************************************
*************************************************

use zim_r2_data.dta, clear

*** Demographics ***
decode q62, gen(surveyor_origin)
decode q42, gen(national_identity)

rename q51 age
replace age=. if  age >90
decode q53, gen(schooling)
decode urbrur, gen(urban_rural)
decode q63, gen(gender)
decode q64new, gen(race)

gen year_survey = 2004

*** Economic Conditions ***
decode q1b, gen(own_liv_cond )
decode q7a, gen(no_food)
decode q7c, gen(no_meds)
decode q7f, gen(no_cash)
decode q7b, gen(no_water)
decode q7e, gen(no_fuel)

*** Social Capital ***
decode q16b, gen(att_comm_meet)
decode q16c, gen(raise_issue)
decode q16d, gen(att_demo)

*** Contact politician / party ***
decode q19a, gen(Q19a)
decode q19b, gen(Q19b)
decode q19d, gen(Q19d)
decode q56a, gen(close_party)
decode q56a, gen(party_close)

gen contacted_local_govt = (Q19a=="Only once" | Q19a=="A few times" | Q19a=="Often" ) if q19a>=0 & q19a<=3
gen contacted_mp = (Q19b=="Only once" | Q19b=="A few times" | Q19b=="Often" )  if q19b>=0 & q19b<=3
gen contacted_party = (Q19d=="Only once" | Q19d=="A few times" | Q19d=="Often" )  if q19d>=0 & q19d<=3

gen contacted_local_govt_intensity = 0 if contacted_local_govt!=.
replace contacted_local_govt_intensity = 1 if Q19a=="Only once"
replace contacted_local_govt_intensity = 2 if Q19a=="A few times"
replace contacted_local_govt_intensity = 3 if Q19a=="Often"
gen contacted_mp_intensity = 0 if contacted_mp!=.
replace contacted_mp_intensity = 1 if Q19b=="Only once"
replace contacted_mp_intensity = 2 if Q19b=="A few times"
replace contacted_mp_intensity = 3 if Q19b=="Often"
gen contacted_party_intensity = 0 if contacted_party!=.
replace contacted_party_intensity = 1 if Q19d=="Only once"
replace contacted_party_intensity = 2 if Q19d=="A few times"
replace contacted_party_intensity = 3 if Q19d=="Often"

decode q33pt1, gen(problem1)
decode q33pt2, gen(problem2)
decode q33pt3, gen(problem3)


*** Contact traditional leaders ***
decode q19e, gen(Q19e)
decode q19f, gen(Q19f)
gen contacted_rel_leader = (Q19e=="Only once" | Q19e=="A few times" | Q19e=="Often" ) if q19e>=0 & q19e<=3
gen contacted_trad_leader = (Q19f=="Only once" | Q19f=="A few times" | Q19f=="Often" ) if q19f>=0 & q19f<=3

*** News /Informedness ***
decode q17a, gen(radio)
decode q17b, gen(tv)
decode q17c, gen(newspapers)
decode q16a , gen(disc_pol)
decode q18, gen(int_pub_aff)
drop q16a q17a q17b q17c

*** Democracy ***
decode q27, gen(supp_demo)
decode q29, gen(sat_demo)
decode q26, gen(democracy)
decode q21, gen(parties_needed)
decode q30b, gen(party_comp_confl)
decode q24a, gen(reject_one_party)
decode q24d, gen(reject_one_man)
decode q30a, gen(careful_say)
decode q47, gen(violence_never)

decode q22, gen(parl_laws)

*** Liberal Views ***
decode q20, gen(pro_elections)

*** Performance ***
decode q36a, gen(good_perf_pres)
decode q36d, gen(good_perf_lg)
decode q32a, gen(trust_pres)
decode q32b, gen(trust_mp)
decode q32e, gen(trust_lg)
decode q32f, gen(trust_inc)
decode q32g, gen(trust_opp)

decode q37b, gen(leaders_listen_all)

decode q38a, gen(corr_pres)
decode q38c, gen(corr_off)

decode q34a, gen(sat_manag_econ)
decode q34b, gen(sat_creating_jobs)
decode q34c, gen(sat_prices_down)
decode q34d, gen(sat_narrowing_inc_gap)
decode q34e, gen(sat_reducing_crime)
decode q34f, gen(sat_impr_health_ss)
decode q34g, gen(sat_address_educ_needs)
decode q34h, gen(sat_prov_water_and_sanit_ss)
decode q34i, gen(sat_ensuring_eat)
decode q34j, gen(sat_fight_corrupt)
decode q34l, gen(sat_combating_aids)

*** Employment ***
decode q57, gen(occupation)
decode q58, gen(empl_stat)

*** Ethnicity ***
decode q52, gen(language)

keep surveyor_origin national_identity  respno province district age schooling urban_rural gender race year_survey contact* radio tv news disc att_c* raise int_ *demo* pro_elections occupation empl* language parties_needed  reject* party_comp_confl careful_say violence_never good_*  trust_* no_*  own_liv_cond corr_* parl_laws close_party party_close  sat_*  problem* leaders_listen_all

save round2_edit.dta, replace

*************************************************
*************************************************
*************************************************

use zim_r3_data.dta, clear

*** Demographics ***
decode Q88, gen(surveyor_origin)

rename Q1 age
replace age =. if age>90

decode Q78, gen(schooling)
decode urbrur, gen(urban_rural)
decode region, gen(province)
decode Q89, gen(gender)
decode Q90, gen(race)

gen year_survey = 2005

*** Economic Conditions ***
decode Q6B, gen(own_liv_cond )
decode Q12A, gen(no_food)
decode Q12C, gen(no_meds)
decode Q12E, gen(no_cash)
decode Q12B, gen(no_water)
decode Q12D, gen(no_fuel)

*** Political Participation ***
decode Q28, gen(q28)
gen registered = (strpos(q28,"were not registered")>0)
replace registered = 1-registered
gen voted = (strpos(q28,"voted")>0)
gen prevented = (strpos(q28,"prevented")>0)

decode Q52F,  gen(gift)

*** Contact politician / party ***
decode Q72, gen(close_party)
decode Q73, gen(party_close)
decode Q21A, gen(politcs_complicated)

decode Q57PT1, gen(problem1)
decode Q57PT2, gen(problem2)
decode Q57PT3, gen(problem3)

*** Social Capital ***
decode Q29A, gen(att_comm_meet)
decode Q29B, gen(raise_issue)
decode Q29C, gen(att_demo)

decode Q26A, gen(assoc_rel)
decode Q26B, gen(assoc_comm1)
decode Q26C, gen(assoc_comm2)
decode Q26D, gen(assoc_comm3)
 
*** News /Informedness ***
decode Q18A, gen(radio)
decode Q18B, gen(tv)
decode Q18C, gen(newspapers)
decode Q19, gen(int_pub_aff)
decode Q20 , gen(disc_pol)

*** Democracy ***
decode Q33, gen(supp_demo)
decode Q42, gen(sat_demo)
decode Q41, gen(democracy)
decode Q35, gen(parties_needed)
decode Q47B, gen(party_comp_confl)
decode Q32A, gen(reject_one_party)
decode Q32C, gen(reject_one_man)
decode Q47A, gen(careful_say)
decode Q45, gen(violence_never)

decode Q36, gen(parl_laws)
decode Q37, gen(pres_obey_laws)

*** Performance ***
decode Q62A, gen(good_perf_pres)
decode Q62B, gen(good_perf_mps)
decode Q62C, gen(good_perf_lg)
decode Q50A, gen(trust_pres)
decode Q50B, gen(trust_mp)
decode Q50D, gen(trust_lg)
decode Q50E, gen(trust_inc)
decode Q50F, gen(trust_opp)

decode Q51A, gen(corr_pres)
decode Q51B, gen(corr_mps)
decode Q51C, gen(corr_lg_counc)
decode Q51D, gen(corr_nat_off)
decode Q51E, gen(corr_lg_off)

decode Q56A, gen(leaders_listen_MP)
decode Q56B, gen(leaders_listen_LG)

decode Q59A, gen(sat_manag_econ)
decode Q59B, gen(sat_creating_jobs)
decode Q59C, gen(sat_prices_down)
decode Q59D, gen(sat_narrowing_inc_gap)
decode Q59E, gen(sat_reducing_crime)
decode Q59F, gen(sat_impr_health_ss)
decode Q59G, gen(sat_address_educ_needs)
decode Q59H, gen(sat_prov_water_and_sanit_ss)
decode Q59I, gen(sat_ensuring_eat)
decode Q59K, gen(sat_fight_corrupt)
decode Q59L, gen(sat_combating_aids)

*** Liberal Views ***
decode Q22, gen(for_women_leaders)
decode  Q23, gen(agst_gov_ban_org)
decode  Q24, gen(agst_gov_close_news)
decode Q34, gen(pro_elections)
decode Q37, gen(agst_pres_discr)

*** Employment ***
decode Q83, gen(occupation)
decode Q82, gen(empl_stat)

*** Ethnicity ***
decode Q3, gen(language)

keep surveyor_origin resp province district age schooling urban_rural gender race year_survey voted registered radio tv news int_pub_aff disc_pol att_* raise_issue *demo* close_party party_close occupation empl_stat language politcs_complicated parties_needed  reject* party_comp_confl careful_say for_women_leaders agst_* violence_never  good_*  trust_* assoc_* pro_elections no_* own_liv_cond  corr_* parl_laws  pres_o  sat_* problem* prevented gift leaders_listen_*

save round3_edit.dta, replace

*************************************************
*************************************************
*************************************************

use zim_r4_data.dta, clear

*** Demographics ***
decode Q100, gen(surveyor_origin)
decode Q83, gen(national_identity)

rename Q1 age
replace age =. if age>94

decode Q89, gen(schooling)
decode urbrur, gen(urban_rural)
decode region, gen(province)
decode Q101, gen(gender)
decode Q102, gen(race)

gen year_survey = 2009

*** Economic Conditions ***
decode Q4B, gen(own_liv_cond )
decode Q8A, gen(no_food)
decode Q8C, gen(no_meds)
decode Q8E, gen(no_cash)
decode Q8B, gen(no_water)
decode Q8D, gen(no_fuel)

*** Political Participation ***
decode Q23D, gen(q23D)
gen registered = (strpos(q23D,"were not registered")>0) if Q23D!=9
replace registered = 1-registered
gen voted = (strpos(q23D,"voted")>0) if Q23D!=9
gen prevented = (strpos(q23D,"prevented")>0) if Q23D!=9

decode Q47, gen(fear_elect_violence)
decode Q56PT1, gen(problem1)
decode Q56PT2, gen(problem2)
decode Q56PT3, gen(problem3)

decode Q15C, gen(freedom_vote)
decode Q48A, gen(find_out_vote)
 
*** Social Capital ***
decode Q23A, gen(att_comm_meet)
decode Q23B, gen(raise_issue)
decode Q23C, gen(att_demo)

decode Q22A, gen(assoc_rel)
decode Q22B, gen(assoc_comm)

*** Contact politician / party ***
decode Q25A, gen(q25A)
decode Q25B, gen(q25B)
decode Q85, gen(close_party)
decode Q86, gen(party_close)

gen contacted_local_govt = (q25A=="Only once" | q25A=="A few times" | q25A=="Often" ) if Q25A!=9
gen contacted_mp = (q25B=="Only once" | q25B=="A few times" | q25B=="Often" ) if Q25B!=9

gen contacted_local_govt_intensity = 0 if contacted_local_govt!=.
replace contacted_local_govt_intensity = 1 if q25A=="Only once"
replace contacted_local_govt_intensity = 2 if q25A=="A few times"
replace contacted_local_govt_intensity = 3 if q25A=="Often"
gen contacted_mp_intensity = 0 if contacted_mp!=.
replace contacted_mp_intensity = 1 if q25B=="Only once"
replace contacted_mp_intensity = 2 if q25B=="A few times"
replace contacted_mp_intensity = 3 if q25B=="Often"

decode Q26B, gen(community_personal)

*** Contact traditional leaders ***
decode Q27A, gen(q27A)
decode Q27B, gen(q27B)
gen contacted_rel_leader = (q27A=="Only once" | q27A=="A few times" | q27A=="Often" ) if Q27A!=9 
gen contacted_trad_leader = (q27B=="Only once" | q27B=="A few times" | q27B=="Often" )  if Q27B!=9

*** News /Informedness ***
decode Q12A, gen(radio)
decode Q12B, gen(tv)
decode Q12C, gen(newspapers)
decode Q13, gen(int_pub_aff)
decode Q14 , gen(disc_pol)

*** Democracy ***
decode Q30, gen(supp_demo)
decode Q43, gen(sat_demo)
decode Q42A, gen(democracy)
decode Q32, gen(parties_needed)
decode Q45A, gen(party_comp_confl)
decode Q29A, gen(reject_one_party)
decode Q29C, gen(reject_one_man)
decode Q46, gen(careful_say)

decode Q36, gen(parl_laws)
decode Q37, gen(pres_obey_laws)
decode Q38, gen(supp_term_limits)

*** Performance ***
decode Q70A, gen(good_perf_pres)
decode Q70B, gen(good_perf_mps)
decode Q70C, gen(good_perf_lg)
decode Q49A_ZIM, gen(trust_pres)
decode Q49B, gen(trust_mp)
decode Q49D, gen(trust_lg)
decode Q49E, gen(trust_inc)
decode Q49F, gen(trust_opp)

decode Q50A, gen(corr_pres)
decode Q50B, gen(corr_mps)
decode Q50C, gen(corr_lg_counc)
decode Q50D, gen(corr_off)

decode Q54A, gen(leaders_listen_MP)
decode Q54B, gen(leaders_listen_LG)

decode Q57A, gen(sat_manag_econ)
decode Q57B, gen(sat_impr_liv_st_poor)
decode Q57C, gen(sat_creating_jobs)
decode Q57D, gen(sat_prices_down)
decode Q57E, gen(sat_narrowing_inc_gap)
decode Q57F, gen(sat_reducing_crime)
decode Q57G, gen(sat_impr_health_ss)
decode Q57H, gen(sat_address_educ_needs)
decode Q57I, gen(sat_prov_water_and_sanit_ss)
decode Q57J, gen(sat_ensuring_eat)
decode Q57K, gen(sat_fight_corrupt)
decode Q57L, gen(sat_combating_aids)
decode Q57M, gen(sat_maintain_roads)
decode Q57N, gen(sat_prov_electricity)
decode Q57P, gen(sat_empow_women)

*** Liberal Views ***
decode  Q19, gen(agst_gov_ban_org)
decode  Q20, gen(agst_gov_close_news)
decode Q31, gen(pro_elections)
decode Q37, gen(agst_pres_discr)

*** Employment ***
decode Q94, gen(empl_stat)

*** Ethnicity ***
decode Q3, gen(language)
decode Q79, gen(tribe)

keep surveyor_origin national_identity resp province district age schooling urban_rural gender race year_survey voted registered contact* close_party party_close community_personal radio tv news int_pub_aff disc_pol att_* raise_issue *demo* empl_stat language tribe parties_needed  reject* party_comp_confl careful_say  agst_*  good_*  trust_* assoc_* pro_elections no_* own_liv_cond  corr_* parl_laws pres_obey_laws supp_*  sat_* problem* fear prevented freedom_vote find_out_vote leaders_listen_*

save round4_edit.dta, replace

*************************************************
*************************************************
*************************************************

use zim_r4-5_data.dta, clear

*** Demographics ***
decode Q83, gen(surveyor_origin)
decode Q69, gen(national_identity)

rename Q1 age
replace age =. if age>93

decode Q73, gen(schooling)
decode urbrur, gen(urban_rural)
decode region, gen(province)
decode Q101, gen(gender)
decode Q102, gen(race)

gen year_survey = 2010

*** Economic Conditions ***
decode Q2B, gen(own_liv_cond )
decode Q6A, gen(no_food)
decode Q6C, gen(no_meds)
decode Q6E, gen(no_cash)
decode Q6B, gen(no_water)
decode Q6D, gen(no_fuel)

*** Political Participation ***
decode Q48A, gen(q48A)
gen registered = (strpos(q48A,"were not registered")>0) if Q48A!=9
replace registered = 1-registered
gen voted = (strpos(q48A,"voted")>0) if Q48A!=9
gen prevented = (strpos(q48A,"prevented")>0) if Q48A!=9

decode Q58C, gen(freedom_vote)
decode Q57A, gen(find_out_vote)

*** Contact politician / party ***
decode Q70, gen(close_party)
decode Q71, gen(party_close)

decode Q55, gen(fear_elect_violence)
decode Q37PT1, gen(problem1)
decode Q37PT2, gen(problem2)
decode Q37PT3, gen(problem3)

*** Democracy ***
decode Q22, gen(supp_demo)
decode Q30, gen(sat_demo)
decode Q29, gen(democracy)
decode Q24, gen(parties_needed)
decode Q32A, gen(party_comp_confl)
decode Q21A, gen(reject_one_party)
decode Q21C, gen(reject_one_man)
decode Q33, gen(careful_say)

decode Q25, gen(parl_laws)
decode Q26, gen(pres_obey_laws)
decode Q27, gen(supp_term_limits)

*** Performance ***
decode Q15A, gen(good_perf_pres)
decode Q15B, gen(good_perf_mps)
decode Q15C, gen(good_perf_lg)
decode Q34A, gen(trust_pres)
decode Q34B, gen(trust_mp)
decode Q34D, gen(trust_lg)
decode Q34E, gen(trust_inc)
decode Q34F, gen(trust_opp1)
decode Q34G, gen(trust_opp2)
decode Q34G, gen(trust_opp3)

decode Q35A, gen(corr_pres)
decode Q35B, gen(corr_mps)
decode Q35C, gen(corr_lg_counc)
decode Q35D, gen(corr_off)

decode Q41A, gen(sat_manag_econ)
decode Q41B, gen(sat_impr_liv_st_poor)
decode Q41C, gen(sat_creating_jobs)
decode Q41D, gen(sat_prices_down)
decode Q41E, gen(sat_narrowing_inc_gap)
decode Q41F, gen(sat_reducing_crime)
decode Q41G, gen(sat_impr_health_ss)
decode Q41H, gen(sat_address_educ_needs)
decode Q41I, gen(sat_prov_water_and_sanit_ss)
decode Q41J, gen(sat_ensuring_eat)
decode Q41K, gen(sat_fight_corrupt)
decode Q41L, gen(sat_combating_aids)
decode Q41M, gen(sat_maintain_roads)
decode Q41N, gen(sat_prov_electricity)
decode Q41P, gen(sat_empow_women)

*** Liberal Views ***
decode Q23, gen(pro_elections)

*** Employment ***
decode Q76, gen(empl_stat)

*** Ethnicity ***
decode Q68, gen(tribe)

keep surveyor_origin national_identity resp province district age schooling urban_rural gender race year_survey voted registered close_party party_close *demo* empl_stat tribe parties_needed  reject* party_comp_confl careful_say   good_*  trust_* pro_elections no_*  own_liv_cond   corr_* parl_laws pres_obey_laws supp_*  sat_* fear problem* prevented freedom_vote find_out_vote

save round4.5_edit.dta, replace

*************************************************
*************************************************
*************************************************

use zim_r5_data.dta, clear

*** Demographics ***
decode Q100, gen(surveyor_origin)
decode Q85B, gen(national_identity)

rename Q1 age
replace age =. if age<18| age>97

decode Q97, gen(schooling)
decode urbrur, gen(urban_rural)
decode region, gen(province)
decode Q101, gen(gender)
decode Q102, gen(race)

gen year_survey = 2012

*** Economic Conditions ***
decode Q3B, gen(own_liv_cond )
decode Q8A, gen(no_food)
decode Q8C, gen(no_meds)
decode Q8E, gen(no_cash)
decode Q8B, gen(no_water)
decode Q8D, gen(no_fuel)

*** Political Participation ***
decode Q27, gen(q27)
gen registered = (strpos(q27,"were not registered")>0) if Q27!=9
replace registered = 1-registered
gen voted = (strpos(q27,"voted")>0) if Q27!=9
gen prevented = (strpos(q27,"prevented")>0) if Q27!=9

decode Q61F,  gen(gift)

*** Social Capital ***
decode Q26A, gen(att_comm_meet)
decode Q26B, gen(raise_issue)
decode Q26D, gen(att_demo)

decode Q25A, gen(assoc_rel)
decode Q25B, gen(assoc_comm)

*** Contact politician / party ***
decode Q30A, gen(q30A)
decode Q30B, gen(q30B)
decode Q30D, gen(q30D)
gen contacted_local_govt = (q30A=="Only once" | q30A=="A few times" | q30A=="Often" ) if Q30A!=9
gen contacted_mp = (q30B=="Only once" | q30B=="A few times" | q30B=="Often" )  if Q30B!=9
gen contacted_party = (q30D=="Only once" | q30D=="A few times" | q30D=="Often" )  if Q30D!=9

gen contacted_local_govt_intensity = 0 if contacted_local_govt!=.
replace contacted_local_govt_intensity = 1 if q30A=="Only once"
replace contacted_local_govt_intensity = 2 if q30A=="A few times"
replace contacted_local_govt_intensity = 3 if q30A=="Often"
gen contacted_mp_intensity = 0 if contacted_mp!=.
replace contacted_mp_intensity = 1 if q30B=="Only once"
replace contacted_mp_intensity = 2 if q30B=="A few times"
replace contacted_mp_intensity = 3 if q30B=="Often"
gen contacted_party_intensity = 0 if contacted_party!=.
replace contacted_party_intensity = 1 if q30D=="Only once"
replace contacted_party_intensity = 2 if q30D=="A few times"
replace contacted_party_intensity = 3 if q30D=="Often"

decode Q89A, gen(close_party)
decode Q89B, gen(party_close)

decode Q16, gen(politcs_complicated)

decode Q54, gen(fear_elect_violence)

decode Q17C, gen(freedom_vote)
decode Q55, gen(find_out_vote)

decode Q63PT1, gen(problem1)
decode Q63PT2, gen(problem2)
decode Q63PT3, gen(problem3)

*** News /Informedness ***
decode Q13A, gen(radio)
decode Q13B, gen(tv)
decode Q13C, gen(newspapers)
decode Q14, gen(int_pub_aff)
decode Q15 , gen(disc_pol)

*** Democracy ***
decode Q32, gen(supp_demo)
decode Q43, gen(sat_demo)
decode Q42, gen(democracy)
decode Q35, gen(parties_needed)
decode Q52B, gen(party_comp_confl)
decode Q31A, gen(reject_one_party)
decode Q31C, gen(reject_one_man)
decode Q56A, gen(careful_say)
decode Q78, gen(violence_never)

decode Q39, gen(parl_laws)
decode Q40, gen(pres_obey_laws)
decode Q41, gen(supp_term_limits)

*** Performance ***
decode Q71A, gen(good_perf_pres)
decode Q71B, gen(good_perf_mps)
decode Q71C, gen(good_perf_lg)
decode Q59A, gen(trust_pres)
decode Q59B, gen(trust_mp)
decode Q59E, gen(trust_lg)
decode Q59F, gen(trust_inc)
decode Q59G, gen(trust_opp)

decode Q60A, gen(corr_pres)
decode Q60B, gen(corr_mps)
decode Q60D, gen(corr_lg_counc)
decode Q60C, gen(corr_off)

decode Q62A, gen(leaders_listen_MP)
decode Q62B, gen(leaders_listen_LG)

decode Q65A, gen(sat_manag_econ)
decode Q65B, gen(sat_impr_liv_st_poor)
decode Q65C, gen(sat_creating_jobs)
decode Q65D, gen(sat_prices_down)
decode Q65E, gen(sat_narrowing_inc_gap)
decode Q65F, gen(sat_reducing_crime)
decode Q65G, gen(sat_impr_health_ss)
decode Q65H, gen(sat_address_educ_needs)
decode Q65I, gen(sat_prov_water_and_sanit_ss)
decode Q65J, gen(sat_ensuring_eat)
decode Q65K, gen(sat_fight_corrupt)
decode Q65M, gen(sat_combating_aids)
decode Q65N, gen(sat_maintain_roads)
decode Q65O, gen(sat_prov_electricity)
decode Q65P, gen(sat_empow_women)

*** Liberal Views ***
decode Q22, gen(for_women_leaders)
decode  Q19, gen(agst_gov_ban_org)
decode  Q20, gen(agst_gov_close_news)
decode Q34, gen(pro_elections)
decode Q40, gen(agst_pres_discr)

*** Employment ***
decode Q96, gen(empl_stat)
 
*** Ethnicity ***
decode Q2, gen(language)
decode Q84, gen(tribe)
 
keep surveyor_origin  national_identity resp province district age schooling urban_rural gender race year_survey voted registered contact* radio tv news int_pub_aff disc_pol att_* raise_issue *demo* close_party party_close empl_stat language tribe politcs_complicated parties_needed  reject* party_comp_confl careful_say for_women_leaders  agst_* violence_never  good_*  trust_* assoc_* pro_elections no_* own_liv_cond   corr_* parl_laws pres_obey_laws supp_*  sat_* fear problem* prevented gift freedom_vote find_out_vote  leaders_listen_*

save round5_edit.dta, replace














******************************************
***           Combine Surveys          ***
******************************************

clear all
append using round1_edit.dta
append using round2_edit.dta
append using round3_edit.dta
append using round4_edit.dta
append using round4.5_edit.dta
append using round5_edit.dta

erase round1_edit.dta
erase round2_edit.dta
erase round3_edit.dta
erase round4_edit.dta
erase round4.5_edit.dta
erase round5_edit.dta













*************************************************************************************
***                    Province and District Name Cleaning                        ***
*************************************************************************************

replace province = "Bulawayo" if province=="BULAWAYO"
replace province = "Harare" if province=="HARARE"
replace province = "Manicaland" if province=="MANICALAND"
replace province = "Mashonaland Central" if province=="MASH CENTRAL"
replace province = "Mashonaland East" if province=="MASH EAST"
replace province = "Mashonaland West" if province=="MASH WEST"
replace province = "Masvingo" if province=="MASVINGO"
replace province = "Matabeleland North" if province=="MAT NORTH"
replace province = "Matabeleland North" if province=="Matebeland North"
replace province = "Matabeleland North" if province=="Matebeleland North"
replace province = "Matabeleland South" if province=="MAT SOUTH"
replace province = "Matabeleland South" if province=="Matebeland South"
replace province = "Matabeleland South" if province=="Matebeleland South"
replace province = "Midlands" if province=="MIDLANDS"

rename province Region
replace district=proper(district)

replace district="Harare" if district=="Chitungwiza" & Region=="Harare"
replace district="Harare" if district=="Epworth" & Region=="Harare"
replace district="Harare" if district=="Harare Rural" & Region=="Harare"
replace district="Harare" if district=="Harare Urban" & Region=="Harare"
replace district="Mutare" if district=="Mutare Rural" & Region=="Manicaland"
replace district="Mutare" if district=="Mutare Urban" & Region=="Manicaland"
replace district="Makoni" if district=="Rusape" & Region=="Manicaland"
replace district="Bindura" if district=="Bindura Rural" & Region=="Mashonaland Central"
replace district="Bindura" if district=="Bindura Urban" & Region=="Mashonaland Central"
replace district="Muzarabani" if district=="Centenary" & Region=="Mashonaland Central"
replace district="Guruve" if district=="Guruve/Mbire" & Region=="Mashonaland Central"
replace district="Mount Darwin" if district=="Mt Darwin" & Region=="Mashonaland Central"
replace district="Wedza" if district=="Hwedza" & Region=="Mashonaland East"
replace district="Marondera" if district=="Marondera Urban" & Region=="Mashonaland East"
replace district="Murehwa" if district=="Murewa" & Region=="Mashonaland East"
replace district="Goromonzi" if district=="Ruwa" & Region=="Mashonaland East"
replace district="Goromonzi" if district=="Ruwa Lb" & Region=="Mashonaland East"
replace district="Chegutu" if district=="Chegutu Rural" & Region=="Mashonaland West"
replace district="Chegutu" if district=="Chegutu Urban" & Region=="Mashonaland West"
replace district="Makonde" if district=="Chinhoyi" & Region=="Mashonaland West"
replace district="Makonde" if district=="Chinhoyi Urban" & Region=="Mashonaland West"
replace district="Kadoma" if district=="Kadoma Rural" & Region=="Mashonaland West"
replace district="Kadoma" if district=="Kadoma Urban" & Region=="Mashonaland West"
replace district="Kariba" if district=="Kariba Urban" & Region=="Mashonaland West"
replace district="Hurungwe" if district=="Karoi" & Region=="Mashonaland West"
replace district="Mhondoro-Ngezi" if district=="Mhondoro/Sanyati" & Region=="Mashonaland West"
replace district="Chegutu" if district=="Norton" & Region=="Mashonaland West"
replace district="Chiredzi" if district=="Chiredzi Rural" & Region=="Masvingo"
replace district="Masvingo" if district=="Masvingo Rural" & Region=="Masvingo"
replace district="Masvingo" if district=="Masvingo Urban" & Region=="Masvingo"
replace district="Masvingo" if district=="Renco Mine" & Region=="Masvingo"
replace district="Hwange" if district=="Victoria Falls" & Region=="Matabeleland North"
replace district="Beitbridge" if district=="Beit-Bridge" & Region=="Matabeleland South"
replace district="Beitbridge" if district=="Beitbridge Urban" & Region=="Matabeleland South"
replace district="Bulilima (North)" if district=="Bulilima" & Region=="Matabeleland South"
replace district="Bulilima (North)" if district=="Bulilima-Mangwe North" & Region=="Matabeleland South"
replace district="Bulilima (North)" if district=="Bulilimamangwe" & Region=="Matabeleland South"
replace district="Bulilima (North)" if district=="Bulilimamangwe North" & Region=="Matabeleland South"
replace district="Gwanda" if district=="Gwanda Rural" & Region=="Matabeleland South"
replace district="Matobo" if district=="Matopo" & Region=="Matabeleland South"
replace district="Chirumanzu" if district=="Chirumhanzu" & Region=="Midlands"
replace district="Gokwe South" if district=="Gokwe" & Region=="Midlands"
replace district="Gokwe South" if district=="Gokwe Town" & Region=="Midlands"
replace district="Gokwe South" if district=="Gokwe Urban" & Region=="Midlands"
replace district="Gweru" if district=="Gweru (Lower)" & Region=="Midlands"
replace district="Gweru" if district=="Gweru Rural" & Region=="Midlands"
replace district="Gweru" if district=="Gweru Urban" & Region=="Midlands"
replace district="Kwekwe" if district=="Kwekwe Rural" & Region=="Midlands"
replace district="Kwekwe" if district=="Kwekwe Urban" & Region=="Midlands"
replace district="Kwekwe" if district=="Red Cliff" & Region=="Midlands"
replace district="Kwekwe" if district=="Redcliff" & Region=="Midlands"
replace district="Shurugwi" if district=="Shurugwi Rural" & Region=="Midlands"
replace district="Zvishavane" if district=="Zvishavane Rural" & Region=="Midlands"
replace district="Zvishavane" if district=="Zvishavane Urban" & Region=="Midlands"















*************************************************************************************
***                           Adding Violence Variables                           ***
*************************************************************************************

capture merge m:1 Region district using "Violence_District.dta"
drop if _merge==2
drop _merge












*************************************************************************************
***                      Adding Electoral Competition Variables                   ***
*************************************************************************************

capture merge m:1 Region using "Election_Data_Region.dta"
foreach var in turnout2002 turnout2008 turnout2013 incumbent2002 incumbent2008 incumbent2013 {
rename `var' p_`var'
}
drop _merge

capture merge m:m Region district using "Election_Data_District.dta"
drop if _merge==2
drop _merge
foreach var in turnout2002 turnout2008 turnout2013 incumbent2002 incumbent2008 incumbent2013 {
rename `var' d_`var'
}

foreach var in incumbent turnout {
foreach option in p d {
gen `option'_`var' = `option'_`var'2002 if year_survey==1999 | year_survey==2004 | year_survey==2005
replace `option'_`var' = `option'_`var'2008 if year_survey==2009 | year_survey==2010
replace `option'_`var' = `option'_`var'2013 if year_survey==2012
}
}

label var p_turnout2002 "Provincial Turnout in 2002"
label var p_turnout2008 "Provincial Turnout in 2008"
label var p_turnout2013 "Provincial Turnout in 2013"
label var d_turnout2002 "District Turnout in 2002 (missing few in 1999)"
label var d_turnout2008 "District Turnout in 2008 (missing few in 1999)"
label var d_turnout2013 "District Turnout in 2013 (missing few in 1999)"

label var p_incumbent2002 "Provincial incumbent share in 2002"
label var p_incumbent2008 "Provincial incumbent share in 2008"
label var p_incumbent2013 "Provincial incumbent share in 2013"
label var d_incumbent2002 "District incumbent share in 2002 (missing few in 1999)"
label var d_incumbent2008 "District incumbent share in 2008 (missing few in 1999)"
label var d_incumbent2013 "District incumbent share in 2013 (missing few in 1999)"

label var p_turnout "Provincial Turnout in closest year"
label var d_turnout "District Turnout in closest year (missing few in 1999)"
label var p_incumbent "Provincial incumbent share in closest year"
label var d_incumbent "District incumbent share in closest year (missing few in 1999)"

















*************************************************************************************
***                        Variable preparation for analysis                      ***
*************************************************************************************

*** Restrict attention to black respondents ***
keep if race=="Black/African"

*** Year of birth ***
gen year_birth = year_survey - age

*** Education Outcomes ***
gen complete_college = (schooling=="University completed" | schooling=="University/college completed" | schooling=="Post-grad" | schooling=="Post-graduate") if schooling!="Don't know" & schooling!="Missing data" & schooling!="Refused"
gen incomplete_college = (schooling=="Some University/college" | schooling=="Some university"  | schooling=="Post-secondary qualifications, not univ") if schooling!="Don't know" & schooling!="Missing data" & schooling!="Refused"
replace incomplete_college= 1 if strpos(schooling,"Post-secondary qualifications, not univ")>0 | strpos(schooling,"Other post matric qualifications")>0
gen complete_high = (schooling=="High school completed"| schooling=="Secondary school completed/high school" ) if schooling!="Don't know" & schooling!="Missing data" & schooling!="Refused"
gen incomplete_high = (schooling=="Some high school" | schooling=="Some secondary school/high school") if schooling!="Don't know" & schooling!="Missing data" & schooling!="Refused"
gen complete_primary = (schooling=="Primary school completed") if schooling!="Don't know" & schooling!="Missing data" & schooling!="Refused"
gen incomplete_primary = (schooling=="Some Primary school" | schooling=="Some primary schooling") if schooling!="Don't know" & schooling!="Missing data" & schooling!="Refused"
gen no_schooling = (incomplete_primary==0 & complete_primary==0 & incomplete_high==0 & complete_high==0 & incomplete_college==0 & complete_college==0) if schooling!="Don't know" & schooling!="Missing data" & schooling!="Refused"
drop schooling

*** Economic Conditions Outcomes ***
gen good_own_liv_cond = (strpos(own_liv_cond,"ood")>0 & strpos(own_liv_cond,"bad")==0) if own_liv_cond!="" & own_liv_cond!="Don't know" & own_liv_cond!="Missing"

foreach var in no_food no_meds no_cash no_water no_fuel {
gen `var'1  = (strpos(`var', "lways")>0 | strpos(`var', "ften")>0 | strpos(`var', "times")>0) if `var'!="" & strpos(`var', "Don't")==0 & strpos(`var', "Missing")==0
drop `var'
rename `var'1 `var'
}

label var voted "Voted in last election"
label var prevented "Prevented from voting in last election"
label var contacted_local_govt  "Contacted Local Government Counncillor or Official"
label var contacted_mp "Contacted Memnber of Parliament"
label var contacted_party  "Contacted Party Representative"
label var contacted_rel_leader  "Contacted Religious Leader"
label var contacted_trad_leader "Contacted Traditonal Leader"

gen gift_for_vote = (strpos(gift,"A Few Times")>0 | strpos(gift,"Often")>0 | strpos(gift,"Once or Twice")>0) if gift!="" & gift!="Don't Know" & gift!="Missing"
drop gift

gen freedom_vote1 = (strpos(freedom_vote , "Completely free")>0 | strpos(freedom_vote , "Somewhat free")>0) if freedom_vote !="" & freedom_vote!="Don't know" & freedom_vote!="Missing"
drop freedom_vote
rename freedom_vote1 freedom_vote

gen find_out_vote1 = (strpos(find_out_vote , "Very likely")>0 | strpos(find_out_vote , "Somewhat likely")>0) if find_out_vote !="" & find_out_vote!="Don't know" & find_out_vote!="Missing"
drop find_out_vote
rename find_out_vote1 find_out_vote

*** Political and Social Outcomes ***
gen att_comm_meeting = 0 if year_survey!=2010
replace att_comm_meeting = 1 if  strpos(att_comm_meet,"ften")>0 |  strpos(att_comm_meet,"nce or twice")>0 |  strpos(att_comm_meet,"everal")>0
replace att_comm_meeting = 1 if  strpos(att_comm_meet1,"ften")>0 |  strpos(att_comm_meet1,"nce or twice")>0 |  strpos(att_comm_meet1,"everal")>0
replace att_comm_meeting = 1 if  strpos(att_comm_meet2,"ften")>0 |  strpos(att_comm_meet2,"nce or twice")>0 |  strpos(att_comm_meet2,"everal")>0
replace att_comm_meeting = . if strpos(att_comm_meet, "Don't")>0 | strpos(att_comm_meet, "Missing")>0 | strpos(att_comm_meet1, "Don't")>0 | strpos(att_comm_meet1, "Missing")>0 | strpos(att_comm_meet2, "Don't")>0 | strpos(att_comm_meet2, "Missing")>0 
gen att_comm_meet_intensity = 0 if att_comm_meeting!=.
replace att_comm_meet_intensity = 1 if  strpos(att_comm_meet,"nce or twice")>0 | strpos(att_comm_meet1,"nce or twice")>0 | strpos(att_comm_meet2,"nce or twice")>0
replace att_comm_meet_intensity = 2 if  strpos(att_comm_meet,"everal")>0 | strpos(att_comm_meet1,"everal")>0 | strpos(att_comm_meet2,"everal")>0
replace att_comm_meet_intensity = 3 if  strpos(att_comm_meet,"ften")>0 | strpos(att_comm_meet1,"ften")>0 | strpos(att_comm_meet2,"ften")>0
drop att_comm_meet att_comm_meet1 att_comm_meet2
rename att_comm_meeting att_comm_meet

gen assoc_rel_active = (strpos(assoc_rel, "Active Member")>0 | strpos(assoc_rel, "Official")>0) if  assoc_rel!="" & assoc_rel!="Don't Know" & assoc_rel!="Missing"
gen assoc_rel1 = (strpos(assoc_rel, "ctive Member")>0  | strpos(assoc_rel, "Official")>0) if  assoc_rel!="" & assoc_rel!="Don't Know" & assoc_rel!="Missing"
drop assoc_rel
rename assoc_rel1 assoc_rel

foreach var in assoc_comm assoc_comm1 assoc_comm2 assoc_comm3 {
gen `var'_active = (strpos(`var',"Active Member")>0 | strpos(`var',"Official")>0) if `var'!="" & strpos(`var', "Don't")==0
gen `var'bis = (strpos(`var',"ctive Member")>0 | strpos(`var',"Official")>0) if `var'!="" & strpos(`var', "Don't")==0
drop `var'
rename `var'bis `var'
}

replace assoc_comm = 0 if assoc_comm1!=.
replace assoc_comm = 1 if assoc_comm1==1
replace assoc_comm = 1 if assoc_comm2==1
replace assoc_comm = 1 if assoc_comm3==1
drop assoc_comm1 assoc_comm2 assoc_comm3

replace assoc_comm_active = 0 if assoc_comm1_active!=.
replace assoc_comm_active = 1 if assoc_comm1_active==1
replace assoc_comm_active = 1 if assoc_comm2_active==1
replace assoc_comm_active = 1 if assoc_comm3_active==1
drop assoc_comm1_active assoc_comm2_active assoc_comm3_active

gen raise_issue1 = 0 if raise_issue!="" & strpos(raise_issue, "Don't")==0 & strpos(raise_issue, "Missing")==0
replace raise_issue1 = 1 if  strpos(raise_issue,"ften")>0 |  strpos(raise_issue,"nce or twice")>0 |  strpos(raise_issue,"everal")>0
gen raise_issue_intensity = 0 if raise_issue1!=.
replace raise_issue_intensity = 1 if  strpos(raise_issue,"nce or twice")>0
replace raise_issue_intensity = 2 if  strpos(raise_issue,"everal")>0
replace raise_issue_intensity = 3 if  strpos(raise_issue,"ften")>0
drop raise_issue
rename raise_issue1 raise_issue

gen att_demo1 = 0 if att_demo!="" & strpos(att_demo, "Don't")==0 & strpos(att_demo, "Missing")==0 & strpos(att_demo, "Refused")==0
replace att_demo1 = 1 if  strpos(att_demo,"few times")>0 | strpos(att_demo,"ften")>0 |  strpos(att_demo,"nce or twice")>0 |  strpos(att_demo,"everal")>0
gen att_demo_intensity = 0 if att_demo1!=.
replace att_demo_intensity = 1 if  strpos(att_demo,"nce or twice")>0
replace att_demo_intensity = 2 if  strpos(att_demo,"everal")>0
replace att_demo_intensity = 3 if  strpos(att_demo,"ften")>0
drop att_demo
rename att_demo1 att_demo

gen close_party1 = (strpos(close_party,"Yes")>0 & close_party!="") if strpos(close_party, "Don't")==0 & strpos(close_party, "Missing")==0 & strpos(close_party, "Refused")==0
replace close_party1 = 1 if strpos(close_party,"ZANU-PF")>0 | strpos(close_party,"ZAPU 2000")>0  | strpos(close_party,"mdc")>0 
drop close_party
rename close_party1 close_party

label var close_party "Close to Political Party"

gen close_inc_party = (strpos(party_close,"ZANU")>0 |strpos(party_close,"Zimbabwe African National Union")>0 | strpos(party_close,"Zimbabwe African national Union")>0) if strpos(party_close, "Don't know")==0 & strpos(party_close, "Missing")==0 & strpos(party_close, "Refused")==0
gen close_main_opp_party = (strpos(party_close,"MDC")>0 |strpos(party_close,"mdc")>0 | strpos(party_close,"democratic change")>0 | strpos(party_close,"emocratic Change")>0) if strpos(party_close, "Don't know")==0 & strpos(party_close, "Missing")==0 & strpos(party_close, "Refused")==0
drop party_close

label var close_inc_party "Close to Incumbent Political Party"
label var close_main_opp_party "Close to Main Opposition Political Party"

gen fear = (strpos(fear_elect_violence,"A lot")>0 | strpos(fear_elect_violence,"Somewhat")>0) if fear_elect_violence!="" & strpos(fear_elect_violence, "Don't know")==0
drop fear_elect_violence

gen pol_violence_issue = 0 if problem1!="Missing" & problem1!="Don't know"
foreach var in problem1 problem2 problem3 {
replace pol_violence_issue = 1 if  strpos(`var', "iolence")>0
}
drop problem1 problem2 problem3 

gen discuss_politics_often = 0 if disc_pol!="" & strpos(disc_pol, "Don't know")==0 & strpos(disc_pol, "Missing")==0 & strpos(disc_pol, "Refused")==0
replace discuss_politics_often = 1 if strpos(disc_pol, "Frequentl")>0 | strpos(disc_pol, "Yes, often")>0 
gen discuss_politics = discuss_politics_often
replace discuss_politics = 1 if strpos(disc_pol, "Yes, once or twice")>0 | strpos(disc_pol, "Occasionally")>0 | strpos(disc_pol, "Yes, several times")>0 
drop disc_pol

gen politcs_not_complicated = (strpos(politcs_complicated, "isagree") >0) if politcs_complicated!="" & strpos(politcs_complicated, "Don't")==0 & strpos(politcs_complicated, "Missing")==0 & strpos(politcs_complicated, "Refused")==0
drop politcs_complicated

gen party_comp_confl1 = (party_comp_confl=="Always" | party_comp_confl=="Often") if party_comp_confl!="" & party_comp_confl!="Don't know" & party_comp_confl!="Missing"
drop party_comp_confl
rename party_comp_confl1 party_comp_confl

gen int_pub_aff1 = 0 if int_pub_aff!="" & int_pub_aff!="Don't know" & strpos(int_pub_aff, "Missing")==0 & int_pub_aff!="Refused"
replace int_pub_aff1 = 1 if strpos(int_pub_aff, "time")>0 | strpos(int_pub_aff, "Some")>0 | strpos(int_pub_aff, "Very")>0 
drop int_pub_aff
rename int_pub_aff1 int_pub_aff 

gen employed = strpos(empl_stat, "Yes,")>0 if empl_stat!="Don't know" & strpos(empl_stat, "Missing")==0 & strpos(empl_stat, "Refused")==0
drop empl_stat

gen supp_demo1  = 0 if supp_demo!="" & supp_demo!="Missing data" & supp_demo!="Refused"
replace supp_demo1  = 1 if strpos(supp_demo, "agree with statement1")>0 | strpos(supp_demo, "emocracy always preferable")>0 | strpos(supp_demo, "emocracy preferable")>0
drop supp_demo
rename supp_demo1 supp_demo
label var supp_demo "Supports Democracy"
gen sat_demo1 = 0 if sat_demo!="" & sat_demo!="Refused" & strpos(sat_demo, "Missing")==0
replace sat_demo1 = 1 if strpos(sat_demo, "Fairly Satisfied" )>0 | strpos(sat_demo, "Fairly satisfied" )>0 | strpos(sat_demo, "Very satisfied" )>0
drop sat_demo
rename sat_demo1 sat_demo
label var sat_demo "Satisfied with Democracy"

gen democracy1 = (strpos(democracy,"problems")>0 | strpos(democracy,"full democracy")>0 | strpos(democracy,"ompletely democratic")>0 | strpos(democracy,"exceptions")>0) if strpos(democracy, "Don't know")==0 & democracy!="Missing data" & democracy!="Refused"
drop democracy 
rename democracy1 democracy 

gen parties_needed1 = (strpos(parties_needed, "Agree very strongly with 2" )>0 | strpos(parties_needed, "Agree very strongly with B" )>0 | strpos(parties_needed, "Agree with 2" )>0 | strpos(parties_needed, "Agree with B" )>0) if parties_needed!="" & strpos(parties_needed, "Don't")==0 & parties_needed!="Missing"
drop  parties_needed
rename  parties_needed1  parties_needed

gen reject_one_party1 = (reject_one_party=="Disapprove" | reject_one_party=="Strongly Disapprove" | reject_one_party=="Strongly disapprove") if reject_one_party!="" & reject_one_party!="Refused" & strpos(reject_one_party, "Missing")==0 & strpos(reject_one_party, "Don't")==0
drop reject_one_party
rename reject_one_party1 reject_one_party

gen reject_one_man1 = (reject_one_man=="Disapprove" | reject_one_man=="Strongly Disapprove" | reject_one_man=="Strongly disapprove") if reject_one_man!="" & reject_one_man!="Refused" & strpos(reject_one_man, "Missing")==0 & strpos(reject_one_man, "Don't")==0
drop reject_one_man
rename reject_one_man1 reject_one_man

gen pro_elections1 = (strpos(pro_elections, "with 1")>0 | strpos(pro_elections, "with A")>0) if pro_elections!="" & pro_elections!="Don't know" & pro_elections!="Missing"
drop pro_elections
rename pro_elections1 pro_elections

gen agst_pres_discr1 = (strpos(agst_pres_discr,"with 2")>0 | strpos(agst_pres_discr,"with B")>0 ) if agst_pres_discr!="" & agst_pres_discr!="Don't know"
drop agst_pres_discr
rename agst_pres_discr1 agst_pres_discr

gen careful_say1 = (careful_say=="Agree" |careful_say=="Always" | careful_say=="Often" | careful_say=="Strongly agree") if careful_say!="Don't know" & strpos(careful_say, "Missing")==0 & careful_say!="Refused"
drop careful_say 
rename careful_say1 careful_say

gen parl_laws1= (strpos(parl_laws, "with 1")>0 | strpos(parl_laws, "with A")>0) if parl_laws!="" & strpos(parl_laws, "Don't")==0 & strpos(parl_laws, "Missing")==0
replace parl_laws1 = 1 if year_survey==1999 & (strpos(parl_laws, "Disapprove")>0 | strpos(parl_laws, "Strongly disapprove")>0)
drop parl_laws
rename parl_laws1 parl_laws

gen pres_obey_laws1= (strpos(pres_obey_laws, "with 2")>0 | strpos(pres_obey_laws, "with B")>0) if pres_obey_laws!="" & strpos(pres_obey_laws, "Don't")==0
drop pres_obey_laws
rename pres_obey_laws1 pres_obey_laws

gen supp_term_limits1= (strpos(supp_term_limits, "with 1")>0 | strpos(supp_term_limits, "with A")>0) if supp_term_limits!="" & strpos(supp_term_limits, "Don't")==0
drop supp_term_limits
rename supp_term_limits1 supp_term_limits

rename radio r
gen radio = (strpos(r,"week")>0 | strpos(r,"Every")>0) if r!="Don't know" & strpos(r, "Missing")==0
rename tv t
gen tv = (strpos(t,"week")>0 | strpos(t,"Every")>0) if t!="Don't know" & strpos(t, "Missing")==0
rename newspapers news
gen newspapers = (strpos(news,"week")>0 | strpos(news,"Every")>0) if news!="Don't know" & strpos(news, "Missing")==0
drop r t news

label var radio "Listens to radio"
label var tv "Watches television"
label var newspapers "Reads newspapers"

gen for_women_leaders1= (strpos(for_women_leaders, "ith 1")>0 | strpos(for_women_leaders, "ith B")>0) if  for_women_leaders!="" & for_women_leaders!="Don't know"
drop for_women_leaders
rename for_women_leaders1 for_women_leaders

gen agst_gov_ban_org1= (strpos(agst_gov_ban_org, "ith B")>0 | strpos(agst_gov_ban_org, "ith 2")>0) if  agst_gov_ban_org!="" & agst_gov_ban_org!="Don't know"
drop agst_gov_ban_org
rename agst_gov_ban_org1 agst_gov_ban_org

gen agst_gov_close_news1= (strpos(agst_gov_close_news, "ith B")>0 | strpos(agst_gov_close_news, "ith 2")>0) if  agst_gov_close_news!="" & agst_gov_close_news!="Don't know"
drop agst_gov_close_news
rename agst_gov_close_news1 agst_gov_close_news

gen violence_never1 = (strpos(violence_never, "with 1")>0 | strpos(violence_never, "with A")>0) if violence_never!="" & violence_never!="Don't know"
drop violence_never
rename violence_never1 violence_never

foreach var in good_perf_pres good_perf_mps good_perf_lg {
gen `var'1 = (strpos(`var', "Approve")>0 | strpos(`var', "Strongly Approve")>0 |  strpos(`var', "Strongly approve")>0) if `var'!="" & strpos(`var', "Don't")==0 & strpos(`var', "Missing")==0 & strpos(`var', "Refused")==0 & strpos(`var', "Haven't")==0 & `var'!="Not applicable for my area"
drop `var'
rename `var'1 `var'
}

foreach var in trust_pres trust_mp trust_lg trust_inc trust_opp trust_opp1 trust_opp2 trust_opp3 {
gen `var'bis = (strpos(`var',"Always")>0 | strpos(`var',"A lot")>0 | strpos(`var',"A very great deal")>0 | strpos(`var',"Sometimes")>0 | strpos(`var',"Most times")>0 | strpos(`var',"Somewhat")>0 ) if `var'!="" & strpos(`var', "Don't")==0 & strpos(`var', "Missing")==0 & strpos(`var', "Refused")==0 & strpos(`var', "Haven't")==0 & `var'!="Not applicable for my area"
drop `var'
rename `var'bis `var'
}

replace trust_opp = 0 if year_survey==2010
replace trust_opp = 1 if (trust_opp1==1 | trust_opp2==1 | trust_opp3==1) & year_survey==2010
drop trust_opp1 trust_opp2 trust_opp3

foreach var in corr_pres corr_mps corr_lg_counc corr_off corr_nat_off corr_lg_off {
gen `var'1  = (strpos(`var', "All")>0 | strpos(`var', "all")>0 | strpos(`var', "Most")>0 | strpos(`var', "some")>0 | strpos(`var', "Some")>0)  if `var'!="" & strpos(`var', "Don't")==0 & strpos(`var', "Missing")==0 & strpos(`var', "Refused")==0 & strpos(`var', "Have'nt")==0 & strpos(`var', "Haven't")==0 & `var'!="Not applicable for my area"
drop `var'
rename `var'1 `var'
}

replace corr_off = 0 if corr_off==. &  (corr_nat_off!=. | corr_lg_off!=.)
replace corr_off = 1 if corr_nat_off==1  
replace corr_off = 1 if corr_lg_off==1
drop corr_nat_off corr_lg_off
 
 
foreach var in sat_manag_econ sat_impr_liv_st_poor sat_creating_jobs sat_prices_down sat_narrowing_inc_gap sat_reducing_crime sat_impr_health_ss sat_address_educ_needs sat_prov_water_and_sanit_ss sat_ensuring_eat sat_fight_corrupt sat_combating_aids sat_maintain_roads sat_prov_electricity sat_empow_women  { 
gen `var'1 = (strpos(`var',"Fairly Well")>0 | strpos(`var',"Fairly well")>0 | strpos(`var',"Very Well")>0 | strpos(`var',"Very well")>0) if `var'!="" & strpos(`var', "Don't")==0 & strpos(`var', "Missing")==0 & strpos(`var', "Refused")==0 & strpos(`var', "Have'nt")==0 & strpos(`var', "Haven't")==0 & `var'!="Not applicable for my area"
drop `var'
rename `var'1 `var'
}

gen leaders_listen_MP_2 = 0 if leaders_listen_MP !="Don't know" & leaders_listen_MP !="Missing" & leaders_listen_MP !=""
gen leaders_listen_LG_2 = 0 if leaders_listen_LG !="Don't know" & leaders_listen_LG !="Missing" & leaders_listen_LG !=""
replace leaders_listen_MP_2 = 0 if leaders_listen_all !="Don't know" & leaders_listen_all !="Missing" & leaders_listen_all !=""
replace leaders_listen_LG_2 = 0 if leaders_listen_all !="Don't know" & leaders_listen_all !="Missing" & leaders_listen_all !=""

replace leaders_listen_MP_2 = 1 if  leaders_listen_all=="Always" | leaders_listen_all=="Most of the time" |  leaders_listen_MP=="Always" | leaders_listen_MP=="Often"
replace leaders_listen_LG_2 = 1 if leaders_listen_all=="Always" | leaders_listen_all=="Most of the time" |  leaders_listen_LG=="Always" | leaders_listen_LG=="Often"
drop leaders_listen_MP leaders_listen_LG leaders_listen_all

rename leaders_listen_MP_2 leaders_listen_MP
rename leaders_listen_LG_2  leaders_listen_LG
 
*** Survey and Nationalism Outcomes ***

gen surveyor_origin_govt = 0 if surveyor_origin!="" & surveyor_origin!="Missing" & surveyor_origin!="Refused"
replace surveyor_origin_govt = 1 if  strpos(surveyor_origin ,"overnment")>0 | strpos(surveyor_origin ,"inis")>0 | strpos(surveyor_origin ,"ommission")>0 | strpos(surveyor_origin ,"polit")>0 | strpos(surveyor_origin ,"resident")>0 | strpos(surveyor_origin ,"overnor")>0 | strpos(surveyor_origin ,"ublic")>0 | strpos(surveyor_origin ,"ZANU-PF")>0 | strpos(surveyor_origin ,"mdc")>0 | strpos(surveyor_origin ,"ntelligence")>0 | strpos(surveyor_origin ,"arliament")>0 

gen  only_national_identity = 0 if national_identity!="" & national_identity!="Don't know" & national_identity!="Missing"
replace only_national_identity = 1 if strpos(national_identity, "only Zimbabwean") >0 | strpos(national_identity, "more Zimbabwea") >0 | strpos(national_identity, "National identity") >0 | strpos(national_identity, "Not applicable") >0 

gen  some_national_identity = only_national_identity
replace some_national_identity = 1 if strpos(national_identity, "equally Zimbabwean") >0
drop  national_identity

*** Tribe Demographics ***

gen ndebele = (strpos(tribe, "Ndebele")>0) if tribe!="" & tribe!="Don't know" & tribe=="Refused" & tribe=="Missing"
gen shona = (strpos(tribe, "Shona")>0 | strpos(tribe,"Bocha")>0 | strpos(tribe,"Chewa")>0 | strpos(tribe,"Kalanga")>0 | strpos(tribe,"Karanga")>0 | strpos(tribe,"Korekore")>0 | strpos(tribe,"Manyika")>0 | strpos(tribe,"Nyanja")>0 | strpos(tribe,"Ndau")>0 | strpos(tribe,"Sezuru")>0 | strpos(tribe,"Zezuru")>0) if tribe!="" & tribe!="Don't know" & tribe=="Refused" & tribe=="Missing"

gen ndebele_language = (strpos(language, "Ndebele")>0) if language!="" & strpos(language, "Missing")==0
gen shona_language = (strpos(language,"Shona")>0 | strpos(language,"Bocha")>0 | strpos(language,"Chewa")>0 | strpos(language,"Kalanga")>0 | strpos(language,"Karanga")>0 | strpos(language,"Korekore")>0 | strpos(language,"Manyika")>0 | strpos(language,"Nyanja")>0 | strpos(language,"Ndau")>0 | strpos(language,"Sezuru")>0 | strpos(language,"Zezuru")>0) if language!="" & strpos(language, "Missing")==0

replace ndebele = ndebele_language if ndebele==.
replace ndebele = 0 if ndebele==.
replace ndebele = 1  if ndebele_language==1 & ndebele==0 & shona==0

replace shona = shona_language if shona==.
replace shona = 0 if shona==.
replace shona = 1  if shona_language==1 & shona==0 &  ndebele==0 

gen other = (ndebele ==0 & shona == 0)

drop ndebele_language shona_language

duplicates list respno year_survey
duplicates drop

*** More Useful variables ***

egen running = std(year_birth)

g new = year_birth
replace new = . if year_birth>=1964 & year_birth<=1966
replace new = year_birth - 3 if year_birth>1966
egen running2 = std(new)
replace running2 = running2 + .2878502

g post = year_birth>1966

g treatment = year_birth>1966
replace treatment = 3/4 if year_birth==1966
replace treatment = 2/4 if year_birth==1965
replace treatment = 1/4 if year_birth==1964

g partial = treatment>0 & treatment<1

g any_treatment = treatment>0
label var any_treatment "Any secondary access"

g edu = .
replace edu = 0 if no_schooling==1
replace edu = 1 if incomplete_primary==1
replace edu = 2 if complete_primary==1
replace edu = 3 if incomplete_high==1
replace edu = 4 if complete_high==1
replace edu = 5 if incomplete_college==1
replace edu = 6 if complete_college==1

label define edu_levels 0 "No schooling" 1 "Incomplete primary" 2 "Complete primary" 3 "Incomplete secondary" 4 "Complete secondary" 5 "Incomplete college" 6 "Complete college"
label values edu edu_levels

g a_l_incomplete_primary = edu>=1
g a_l_complete_primary = edu>=2
g a_l_incomplete_high = edu>=3
g a_l_complete_high = edu>=4
g a_l_incomplete_college = edu>=5
g a_l_complete_college = edu>=6

g male = gender=="Male"
g urban = urban=="Urban"
g lang_shona = language=="Shona"

drop if year_birth==. | district=="." | edu==.

bys year_birth : g weight_cohort = _N

g comp = year_survey>=2009

merge m:1 year_survey Region district using "Distances.dta"
drop _merge

replace agst_gov_close_news = 1-agst_gov_close_news
alpha parties_needed reject_one_party reject_one_man agst_gov_ban_org agst_gov_close_news agst_pres_discr parl_laws pres_obey_laws supp_term_limits, g(multi)
alpha parties_needed reject_one_party reject_one_man agst_gov_ban_org agst_gov_close_news agst_pres_discr parl_laws pres_obey_laws supp_term_limits if year_birth>=1959 & year_birth<=1971
replace agst_gov_close_news = 1-agst_gov_close_news
alpha no_food-no_cash, g(poverty)
alpha no_food-no_cash if year_birth>=1959 & year_birth<=1971
alpha good_perf_pres good_perf_mps good_perf_lg trust_pres trust_mp trust_inc trust_lg, g(incumbent_overall)
alpha good_perf_pres good_perf_mps good_perf_lg trust_pres trust_mp trust_inc trust_lg if year_birth>=1959 & year_birth<=1971
alpha corr_*, g(corruption)
alpha corr_* if year_birth>=1959 & year_birth<=1971
alpha radio tv newspaper, g(news)
alpha radio tv newspaper if year_birth>=1959 & year_birth<=1971

alpha voted contacted_local_govt att_comm_meet raise_issue, g(part_scale) 
alpha voted contacted_local_govt att_comm_meet raise_issue if year_birth>=1959 & year_birth<=1971
alpha news politcs_not_complicated discuss_politics, g(interest_scale) 
alpha news politcs_not_complicated discuss_politics if year_birth>=1959 & year_birth<=1971 
alpha supp_demo multi, g(demo_scale) 
alpha supp_demo multi if year_birth>=1959 & year_birth<=1971 
replace close_main_opp_party = 1-close_main_opp_party
replace corruption = 1-corruption
alpha close_inc_party close_main_opp_party incumbent_overall corruption, g(view_govt_scale)
alpha close_inc_party close_main_opp_party incumbent_overall corruption if year_birth>=1959 & year_birth<=1971
replace close_main_opp_party = 1-close_main_opp_party
replace corruption = 1-corruption
replace poverty = 1-poverty
alpha employed good_own_liv_cond poverty, g(economic_scale)
alpha employed good_own_liv_cond poverty if year_birth>=1959 & year_birth<=1971
replace poverty = 1-poverty

global a_l_education "edu a_l_incomplete_primary a_l_complete_primary a_l_incomplete_high a_l_complete_high a_l_incomplete_col a_l_complete_col"
global balance "shona ndebele male d_incumbent d_turnout closest_border_rebel ZIPRA ZANLA"
global participation "part_scale voted contacted_local_govt att_comm_meet raise_issue"
global econ_int "economic_scale employed good_own_liv_cond poverty news int_pub_aff" 
global demo "supp_demo multi" 
global view_govt "view_govt_scale close_inc_party close_main_opp_party incumbent_overall corruption" 
global alternatives "gift freedom_vote find_out_vote fear"

quietly foreach y of varlist incomplete_primary complete_primary $a_l_education no_schooling $participation $econ_int $view_govt $demo $balance $alternatives age only_national_identity some_national_identity {
  capture bys year_birth : egen mean_`y' = mean(`y')
}

label var edu "Education"
label var incomplete_high "Incomplete secondary school"
label var complete_high "Complete secondary school"
label var incomplete_college "Incomplete college"
label var complete_college "Complete college"
label var a_l_incomplete_primary "Incomplete primary school"
label var a_l_complete_primary "Complete primary school"
label var a_l_incomplete_high "Incomplete secondary school"
label var a_l_complete_high "Complete secondary school"
label var a_l_incomplete_college "Incomplete college"
label var a_l_complete_college "Complete college"

label var age "Age"
label var shona "Shona"
label var ndebele "Ndebele"
label var male "Male"
label var d_incumbent "District incumbent vote share" 
label var d_turnout "District turnout"

label var voted "Voted"
label var registered "Registered to vote"
label var contacted_mp "Contacted MP"
label var contacted_local_govt "Contacted local councilor"
label var att_comm_meet "Attended community meeting"
label var raise_issue "Raised issue at meeting"
label var news "News scale"
label var radio "News via radio"
label var tv "News via TV"
label var newspapers "News via newspaper"
label var politcs_not_complicated "Understanding politics not complicated"
label var discuss_politics "Discuss politics"
label var int_pub_aff "Interest in public affairs"
label var supp_demo "Support democracy"
label var multi "Support liberal institutions"
label var democracy "Zimbabwe is a democracy"
label var sat_demo "Satisfied with democracy"
label var incumbent_overall "Incumbent trust and performance"
label var corruption "Perceived government corruption"
label var close_inc_party "Close to ZANU-PF"
label var close_main_opp_party "Close to MDC"
label var employed "Employed"
label var good_own_liv_cond "Good living conditions" 
label var poverty "Poverty"
label var economic_scale "Economic scale"
label var urban "Urban"
label var gift "Received gift"
label var fear "Fear repression"
label var freedom_vote "Freedom to chose vote"
label var find_out_vote "Vote monitored"
label var only_national_identity "Only national identity"
label var some_national_identity "Some national identity"
label var closest_border_rebel "Distance to rebel border"
label var ZIPRA "Distance to ZIPRA border"
label var ZANLA "Distance to ZANLA border"
label var part_scale "Participation scale"
label var interest_scale "Interest scale"
label var demo_scale "Pro-democracy scale"
label var view_govt_scale "View of government scale"

drop if part_scale==.

*** Keep variables used in the final analysis
keep part_scale voted contacted_local_govt att_comm_meet raise_issue economic_scale employed good_own_liv_cond poverty news int_pub_aff supp_demo multi view_govt_scale close_inc_party close_main_opp_party incumbent_overall corruption edu a_l_incomplete_primary a_l_complete_primary a_l_incomplete_high a_l_complete_high a_l_incomplete_col a_l_complete_col treatment any_treatment post partial year_survey comp shona ndebele male d_incumbent d_turnout closest_border_rebel ZIPRA ZANLA age gift freedom_vote find_out_vote fear events only_nat some_nat district year_birth weight_cohort mean_* running
 
save "Final Dataset.dta", replace

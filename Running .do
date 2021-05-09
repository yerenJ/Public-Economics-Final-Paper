// Creating District cluster 
egen district_code = group(STATEID DISTID)

// Dummy Variables on Caste, Sex, Education, and Asset Quintile
gen sc = 0
replace sc = 1 if ID13== 4
label variable sc "SC"
gen st = 0
replace st = 1 if ID13 == 5
label variable st "ST"
gen obc = 0 
replace obc = 1 if ID13 == 3
label variable obc "OBC"
gen muslim = 0 
replace muslim = 1 if GROUPS == 6
label variable muslim "Muslim"
gen sc_st = 0 
replace sc_st = 1 if sc==1 | st ==1
label variable sc_st "SC/ST"

gen female = 0
replace female =1 if RO3==2

gen female_sc_st = female*sc_st
label variable female_sc_st "Female x SC/ST"

gen female_obc = female*obc
label variable female_obc "Female x OBC"

gen female_muslim = female*muslim
label variable female_muslim "Female x Muslim"

gen literate = 0 
replace literate = 1 if ED2 == 1
label variable literate "Literate"
gen sec_educ = 0 
replace sec_educ = 1 if HHEDUC > 8
label variable sec_educ "Secondary Educated"
gen middle_educ = 0 
replace middle_educ = 1 if HHEDUC>4 & HHEDUC<9 
label variable middle_educ "Middle Educated"
gen primary_educ = 0 
replace primary_educ = 1 if HHEDUC < 5 & HHEDUC > 0
label variable primary_educ "Primary Educated"
gen sec_educ_sc_st = sec_educ * sc_st
label variable sec_educ_sc_st "Secondary Educated x SC/ST"
gen sec_educ_obc = sec_educ*obc
label variable sec_educ_obc "Secondary Educated OBC"
gen sec_educ_muslim = sec_educ*muslim
label variable sec_educ_muslim "Secondary Educated x Muslim"
gen middle_educ_sc_st = middle_educ * sc_st
label variable middle_educ_sc_st "Middle Educated x SC/ST"
gen middle_educ_obc = middle_educ*obc
label variable middle_educ_obc "Middle Educated OBC"
gen middle_educ_muslim = middle_educ*muslim
label variable middle_educ_muslim "Middle Educated Muslim"
gen post_secondary_educ = 0
replace post_secondary_educ = 1 if HHEDUC > 12
label variable post_secondary_educ "Post Secondary Educated"
gen primary_educ_sc_st = primary_educ * sc_st
label variable primary_educ_sc_st "Primary Educated x SC/ST"
gen primary_educ_obc = primary_educ*obc
label variable primary_educ_obc "Primary Educated x OBC"
gen primary_educ_muslim = primary_educ*muslim
label variable primary_educ_muslim "Primary Educated x Muslim"

replace AN2E = 0 if AN2E == .
replace FM13B = 0 if FM13B == .
replace IN4A = 0 if IN4A == .
replace RSUNEARN = 0 if RSUNEARN == .
replace INCEARN = 0 if INCEARN == .
replace INCREMIT = 0 if INCREMIT == .
replace INCNREGA = 0 if INCNREGA == .
replace INCNONNREGA = 0 if INCNONNREGA == .
gen asset = AN2E + FM13B + IN4A + IN4B + RSUNEARN + INCEARN + INCREMIT + INCNREGA + INCNONNREGA
replace asset = . if asset == 0

xtile asset_quintile = asset[aw=WT], n(5)

gen bottom_asset = 0
replace bottom_asset = 1 if asset_quintile == 1
label variable bottom_asset "Bottom 20 Percentile (Assets)"

gen top_asset = 0
replace top_asset = 1 if asset_quintile == 5

gen bottom2_asset = 0
replace bottom2_asset = 1 if asset_quintile ==1 | asset_quintile ==2 
label variable bottom2_asset "Bottom 40 Percentile (Assets)"
gen bottom3_asset = 0
replace bottom3_asset = 1 if asset_quintile ==1 | asset_quintile ==2 | asset_quintile ==3
label variable bottom3_asset "Bottom 60 Percentile (Assets)"
gen bottom4_asset = 0 
replace bottom4_asset = 1 if asset_quintile != 5

gen bottom_asset_sc_st = bottom_asset*sc_st
label variable bottom_asset_sc_st "Bottom 20 Percentile x SC/ST"

gen bottom_asset_muslim = bottom_asset*muslim
label variable bottom_asset_muslim "Bottom 20 Percentile x Muslim"

gen bottom_asset_obc = bottom_asset*obc
label variable bottom_asset_obc "Bottom 20 Percentile x OBC"

gen bottom2_asset_sc_st = bottom2_asset*sc_st
label variable bottom2_asset_sc_st "Bottom 40 Percentile x SC/ST"

gen bottom2_asset_muslim = bottom2_asset*muslim
label variable bottom2_asset_muslim "Bottom 40 Percentile x Muslim"

gen bottom2_asset_obc = bottom2_asset*obc
label variable bottom2_asset_obc "Bottom 40 Percentile x OBC"

//Generating Rural and Poor States
gen rural = 0
replace rural = 1 if URBAN4_2011 > 1
label variable rural "Rural"

gen rural_sc_st = rural * sc_st
label variable rural_sc_st "Rural x SC/ST"

gen rural_obc = rural*obc
label variable rural_obc "Rural x OBC"

gen rural_muslim = rural*muslim
label variable rural_muslim "Rural x Muslim"

gen rural_female = rural*female
label variable rural_female "Rural x Female"

gen rural_female_sc_st = rural*female_sc_st
label variable rural_female_sc_st "Rural x Female x SC/ST"

gen rural_female_obc = rural*female_obc
label variable rural_female_obc "Rural x Female x OBC"

gen rural_female_muslim = rural*female_muslim
label variable rural_female_muslim "Rural x Female x Muslim"

gen poor_state = 0
replace poor_state = 1 if STATEID ==10 | STATEID==20 | STATEID==23 | STATEID==9 | STATEID==22
// Generating NREGA Variables 

replace IN19 = 0 if IN19 == .
replace IN21 = 0 if IN21 == . 

gen nrega = 0
replace nrega = 1 if IN19 > 0
label variable nrega "MGNREGA"

gen nregra_dur = IN21

gen nrega_inc = INCNREGA

gen ln_nregra_inc = ln(nrega_inc + 1)
label variable ln_nregra_inc "Ln(MGNREGA Income + 1)"

drop if RO5 > 60 | RO5 <18 
// Regressions 

**Model 1 Heterogeniety by education**

areg nrega st sc obc muslim if rural == 1 [aw=WT] , absorb(district_code) cluster(district_code) 
outreg2 using Model1_results, tex replace label dec(4)

areg nrega st sc obc muslim primary_educ middle_educ sec_educ post_secondary_educ if rural == 1 [aw=WT] , absorb(district_code) cluster(district_code) 
outreg2 using Model1_results, tex replace label dec(4)

areg nrega st sc  obc muslim  primary_educ_sc_st primary_educ_obc primary_educ_muslim  if rural == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model1_results, tex append label dec(4)

areg nrega st sc  obc muslim  middle_educ_sc_st middle_educ_obc middle_educ_muslim  if rural == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model1_results, tex append label dec(4)



**Model 2 Heterogeniety by Assets**


areg nrega st sc obc muslim bottom_asset bottom2_asset bottom3_asset if rural == 1 [aw=WT] , absorb(district_code) cluster(district_code) 
outreg2 using Model2_results, tex replace label dec(4)



areg nrega st sc  obc muslim  bottom_asset_sc_st bottom_asset_obc bottom_asset_muslim if rural == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model2_results, tex append label dec(4)

areg nrega st sc  obc muslim  bottom2_asset_sc_st bottom2_asset_obc bottom2_asset_muslim if rural == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model2_results, tex append label dec(4)


**Model 3 Heterogeniety by education**

areg ln_nregra_inc st sc obc muslim primary_educ middle_educ sec_educ post_secondary_educ if rural == 1 [aw=WT] , absorb(district_code) cluster(district_code) 
outreg2 using Model3_results, tex replace label dec(4)



areg ln_nregra_inc st sc  obc muslim  primary_educ_sc_st primary_educ_obc primary_educ_muslim if rural == 1  [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model3_results, tex append label dec(4)

areg ln_nregra_inc st sc  obc muslim  middle_educ_sc_st middle_educ_obc middle_educ_muslim  if rural == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model3_results, tex append label dec(4)



**Model 4 Heterogeniety by Assets**


areg ln_nregra_inc st sc obc muslim bottom_asset bottom2_asset bottom3_asset if rural == 1 [aw=WT] , absorb(district_code) cluster(district_code) 
outreg2 using Model4_results, tex replace label dec(4)



areg ln_nregra_inc st sc  obc muslim  bottom_asset_sc_st bottom_asset_obc bottom_asset_muslim if rural == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model4_results, tex append label dec(4)

areg ln_nregra_inc st sc  obc muslim  bottom2_asset_sc_st bottom2_asset_obc bottom2_asset_muslim if rural == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model4_results, tex append label dec(4)


**Model 5 Heterogeniety by education**

areg nrega st sc obc muslim primary_educ middle_educ sec_educ post_secondary_educ if rural == 1 & poor_state == 1 [aw=WT] , absorb(district_code) cluster(district_code) 
outreg2 using Model5_results, tex replace label dec(4)


areg nrega st sc  obc muslim  primary_educ_sc_st primary_educ_obc primary_educ_muslim  if rural == 1 & poor_state == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model5_results, tex append label dec(4)

areg nrega st sc  obc muslim  middle_educ_sc_st middle_educ_obc middle_educ_muslim  if rural == 1 & poor_state == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model5_results, tex append label dec(4)



**Model 6 Heterogeniety by Assets**


areg nrega st sc obc muslim bottom_asset bottom2_asset bottom3_asset if rural == 1 & poor_state == 1 [aw=WT] , absorb(district_code) cluster(district_code) 
outreg2 using Model6_results, tex replace label dec(4)



areg nrega st sc  obc muslim  bottom_asset_sc_st bottom_asset_obc bottom_asset_muslim if rural == 1 & poor_state == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model6_results, tex append label dec(4)

areg nrega st sc  obc muslim  bottom2_asset_sc_st bottom2_asset_obc bottom2_asset_muslim if rural == 1 & poor_state == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model6_results, tex append label dec(4)


**Model 7 Heterogeniety by education**

areg ln_nregra_inc st sc obc muslim primary_educ middle_educ sec_educ post_secondary_educ if rural == 1 & poor_state == 1  [aw=WT] , absorb(district_code) cluster(district_code) 
outreg2 using Model7_results, tex replace label dec(4)


areg ln_nregra_inc st sc  obc muslim  primary_educ_sc_st primary_educ_obc primary_educ_muslim if rural == 1 & poor_state == 1  [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model7_results, tex append label dec(4)

areg ln_nregra_inc st sc  obc muslim  middle_educ_sc_st middle_educ_obc middle_educ_muslim  if rural == 1 & poor_state == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model7_results, tex append label dec(4)



**Model 8 Heterogeniety by Assets**


areg ln_nregra_inc st sc obc muslim bottom_asset bottom2_asset bottom3_asset if rural == 1 & poor_state == 1 [aw=WT] , absorb(district_code) cluster(district_code) 
outreg2 using Model8_results, tex replace label dec(4)


areg ln_nregra_inc st sc  obc muslim  bottom_asset_sc_st bottom_asset_obc bottom_asset_muslim if rural == 1 & poor_state == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model8_results, tex append label dec(4)

areg ln_nregra_inc st sc  obc muslim  bottom2_asset_sc_st bottom2_asset_obc bottom2_asset_muslim if rural == 1 & poor_state == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model8_results, tex append label dec(4)

**Model 9 Heterogeniety by education**

areg nrega st sc obc muslim  if rural == 1 & poor_state == 1 [aw=WT] , absorb(district_code) cluster(district_code) 
outreg2 using Model9_results, tex replace label dec(4)



areg nrega st sc  obc muslim female_sc_st female_obc female_muslim  if rural == 1 & poor_state == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model9_results, tex append label dec(4)

areg ln_nregra_inc st sc obc muslim female if rural == 1 & poor_state == 1 [aw=WT] , absorb(district_code) cluster(district_code) 
outreg2 using Model9_results, tex replace label dec(4)



areg ln_nregra_inc st sc  obc muslim  female_sc_st female_obc female_muslim  if rural == 1 & poor_state == 1 [aw=WT], absorb(district_code) cluster(district_code) 
outreg2 using Model9_results, tex append label dec(4)




** Summary Stat
sutex st sc obc muslim female female_sc_st female_obc primary_educ primary_educ_sc_st primary_educ_obc middle_educ middle_educ_sc_st middle_educ_obc bottom_asset bottom_asset_sc_st bottom_asset_obc post_secondary_educ if rural == 1, lab nobs key(descstat) replace ///
file(descstat.tex) title("Summary statistics") minmax

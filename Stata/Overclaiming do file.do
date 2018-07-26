*06/26/18
*Yuqi Liao
*Overclaiming

clear
set more off


*read in PISA 2012 Student file
use "G:\Conference\2019\Data\PISA\2012_STATAFile\INT_STU12_DEC03.DTA", clear
*lower case for all variables
rename *, lower


***************************************************************************
*generate new variables
***************************************************************************

// I could recode the variables as described below in the tech report, but it shouldn't change anything
/*
All items were coded as (1=0), (2=1), (3=2), (4=3), (5=4). Total score
was calculated as a ratio of a sum of all questions over maximum
score of valid responses (questions with missing value did not
contribute to max score).
*/
/*
label define foil_label 0 "Never heard of it" 1 "Heard of it once or twice" 2 "Heard of it a few times" 3 "Heard of it often" 4 "Know it well,  understand the concept"
recode st62q* (1=0) (2=1) (3=2) (4=3) (5=4), pre(R) label(foil_label)
label values Rst62q* foil_label

tab st62q11, m nol
tab Rst62q11, m nol

//get avg
egen avg_13_recode = rmean(Rst62q*)

sum avg_13_recode
sum famcon if oecd == 1

egen avg_13_standardize = std(avg_13)
*/

*avg for the 13 real items
egen avg_13 = rmean(st62q01 st62q02 st62q03 st62q06 st62q07 st62q08 st62q09 st62q10 st62q12 st62q15 st62q16 st62q17 st62q19)
*avg for the 3 fake items
egen avg_3 = rmean(st62q04 st62q11 st62q13)
*avg for the 13 real items-adjusted
gen avg_13_adjusted = avg_13 - avg_3


br st62* famcon* avg_*
*descriptives - to make sure avg_13 avg_3 avg_13_adjusted are okay to use
sum st62* famcon* avg_*

***************************************************************************
*correlation - within country
***************************************************************************
//repest PISA, estimate(corr pv@math pv@read) by(cnt)
//repest PISA if cnt == "USA", estimate(corr pv@math famcon famconc)
//repest PISA if cnt == "USA", estimate(corr pv@math famcon famconc avg_13 avg_3 avg_13_adjusted)
//repest PISA, estimate(corr pv@math famcon famconc avg_13 avg_3 avg_13_adjusted) by(cnt)


levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(corr pv@math famcon famconc avg_13 avg_13_adjusted avg_3)
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[4,1], A[1,2] , A[4,2], A[1,3] , A[4,3], A[1,4] , A[4,4], A[1,5], A[4,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "c_pv_math_famcon" "c_pv_math_famcon p" "c_pv_math_famconc" "c_pv_math_famconc p" "c_pv_math_avg_13" "c_pv_math_avg_13 p" "c_pv_math_avg_13_adj" "c_pv_math_avg_13_adj p" "c_pv_math_avg_3"  "c_pv_math_avg_3 p"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Corr-within country", replace) 
putexcel B2 = matrix(analysis, names)



levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(corr pv@math famcon famconc avg_13 avg_13_adjusted avg_3) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[4,1], A[1,2] , A[4,2], A[1,3] , A[4,3], A[1,4] , A[4,4], A[1,5], A[4,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "c_pv_math_famcon" "c_pv_math_famcon p" "c_pv_math_famconc" "c_pv_math_famconc p" "c_pv_math_avg_13" "c_pv_math_avg_13 p" "c_pv_math_avg_13_adj" "c_pv_math_avg_13_adj p" "c_pv_math_avg_3"  "c_pv_math_avg_3 p"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Corr-within country-flag", replace) 
putexcel B2 = matrix(analysis, names)



***************************************************************************
*correlation - across country - need further test
***************************************************************************
levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize pv@math famcon famconc avg_13 avg_13_adjusted avg_3, stats(mean))
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4] ,A[1,5], A[2,5], A[1,6], A[2,6]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "pv_math_mean" "pv_math_mean se" "famcon_mean" "famcon_mean se" "famconc_mean" "famconc_mean se" "avg_13_mean" "avg_13_mean se" "avg_13_adjusted_mean"  "avg_13_adjusted_mean se" "avg_3_mean"  "avg_3_mean se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Corr-across country", replace) 
putexcel B2 = matrix(analysis, names)

levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize pv@math famcon famconc avg_13 avg_13_adjusted avg_3, stats(mean)) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4] ,A[1,5], A[2,5], A[1,6], A[2,6]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "pv_math_mean" "pv_math_mean se" "famcon_mean" "famcon_mean se" "famconc_mean" "famconc_mean se" "avg_13_mean" "avg_13_mean se" "avg_13_adjusted_mean"  "avg_13_adjusted_mean se" "avg_3_mean"  "avg_3_mean se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Corr-across country-flag", replace) 
putexcel B2 = matrix(analysis, names)





//the output suggests that, among the 64 countries (65 minus Norway that has missing values), in about 21 of them: corr math & avg_13_adjusted > math & avg_13. in about 42 of them: corr math & avg_13_adjusted < math & avg_13

***************************************************************************
*define national quartiles based on a few variables (ESCS, avg_13, avg_3)
***************************************************************************
//ESCS
*generate r_escs, using the see "5094" from the repest source code
set seed 5094
bysort cnt: gen r_escs = escs + 0.0001*runiform() if  escs != .

*use egen on r_escs to create r_escs_quartiles
*Josh's code is to use "bysort", however, it is not compatiable with xtile, so i changed it into egen
*bysort cntryid: xtile r_escs_quartiles = r_escs [aw=w_fstuwt], nquantiles(4)
egen r_escs_quartiles = xtile(r_escs), by(cnt) nq(4) weight(w_fstuwt)
egen r_escs_2cat = xtile(r_escs), by(cnt) nq(2) weight(w_fstuwt)
	
*This creates 4 equal sized groups (25% each)
bysort cnt: tab r_escs_quartiles [aw=w_fstuwt]
bysort cnt: tab r_escs_quartiles [aw=w_fstuwt], m

*This creates 2 equal sized groups (25% each)
bysort cnt: tab r_escs_2cat [aw=w_fstuwt]
bysort cnt: tab r_escs_2cat [aw=w_fstuwt], m


//AVG_13
*generate r_avg_13, using the see "5094" from the repest source code
set seed 5094
bysort cnt: gen r_avg_13 = avg_13 + 0.0001*runiform() if  avg_13 != .

*use egen on r_avg_13 to create avg_13_quartiles
egen avg_13_quartiles = xtile(r_avg_13), by(cnt) nq(4) weight(w_fstuwt)
egen avg_13_2cat = xtile(r_avg_13), by(cnt) nq(2) weight(w_fstuwt)
	
*This creates 4 equal sized groups (25% each)
bysort cnt: tab avg_13_quartiles [aw=w_fstuwt]
bysort cnt: tab avg_13_quartiles [aw=w_fstuwt], m

*This creates 2 equal sized groups (25% each)
bysort cnt: tab avg_13_2cat [aw=w_fstuwt]
bysort cnt: tab avg_13_2cat [aw=w_fstuwt], m


//AVG_3
*generate r_avg_3, using the see "5094" from the repest source code
set seed 5094
bysort cnt: gen r_avg_3 = avg_3 + 0.0001*runiform() if  avg_3 != .

*use egen on r_avg_3 to create avg_3_quartiles
egen avg_3_quartiles = xtile(r_avg_3), by(cnt) nq(4) weight(w_fstuwt)
egen avg_3_2cat = xtile(r_avg_3), by(cnt) nq(2) weight(w_fstuwt)
	
*This creates 4 equal sized groups (25% each)
bysort cnt: tab avg_3_quartiles [aw=w_fstuwt]
bysort cnt: tab avg_3_quartiles [aw=w_fstuwt], m

*This creates 2 equal sized groups (25% each)
bysort cnt: tab avg_3_2cat [aw=w_fstuwt]
bysort cnt: tab avg_3_2cat [aw=w_fstuwt], m

***************************************************************************
*label kids based on the national quartile/2-categories
***************************************************************************

label define student_group 0 "l_avg13_l_avg3" 1 "l_avg13_h_avg3" 2 "h_avg13_l_avg3" 3 "h_avg13_h_avg3"
label define student_group_v2 0 "non-overclaimers" 1 "overclaimers" 
label define student_group_v3 0 "l_avg13_l_avg3" 1 "l_avg13_h_avg3" 2 "h_avg13_l_avg3" 3 "h_avg13_h_avg3" 4 "other"
label define student_group_v4 0 "h_avg13_l_avg3" 1 "l_avg13_h_avg3" 2 "l_avg13_l_avg3" 3 "h_avg13_h_avg3"



gen group = .
replace group = 0 if avg_13_quartiles == 1 & avg_3_quartiles == 1 //l_avg13_l_avg3 (low claimers)
replace group = 1 if avg_13_quartiles == 1 & avg_3_quartiles == 4 //l_avg13_h_avg3 (crazy kids)
replace group = 2 if avg_13_quartiles == 4 & avg_3_quartiles == 1 //h_avg13_l_avg3 (ideal group)
replace group = 3 if avg_13_quartiles == 4 & avg_3_quartiles == 4 //h_avg13_h_avg3 (over claimers)
label values group student_group
tab group, m
tab group
bysort cnt: tab group


gen group_v2 = .
replace group_v2 = 0 if avg_13_quartiles == 1 & avg_3_quartiles == 1 //l_avg13_l_avg3 (low claimers)
replace group_v2 = 1 if avg_13_quartiles == 1 & avg_3_quartiles == 4 //l_avg13_h_avg3 (crazy kids)
replace group_v2 = 2 if avg_13_quartiles == 4 & avg_3_quartiles == 1 //h_avg13_l_avg3 (ideal group)
replace group_v2 = 3 if avg_13_quartiles == 4 & avg_3_quartiles == 4 //h_avg13_h_avg3 (over claimers)
replace group_v2 = 4 if avg_13_quartiles == 2 | avg_13_quartiles == 3 | avg_3_quartiles == 2 | avg_3_quartiles == 3 //other
label values group_v2 student_group_v3
tab group_v2, m
tab group_v2
bysort cnt: tab group_v2

gen group_v3 = .
replace group_v3 = 2 if avg_13_quartiles == 1 & avg_3_quartiles == 1 //l_avg13_l_avg3 (low claimers)
replace group_v3 = 1 if avg_13_quartiles == 1 & avg_3_quartiles == 4 //l_avg13_h_avg3 (crazy kids)
replace group_v3 = 0 if avg_13_quartiles == 4 & avg_3_quartiles == 1 //h_avg13_l_avg3 (ideal group)
replace group_v3 = 3 if avg_13_quartiles == 4 & avg_3_quartiles == 4 //h_avg13_h_avg3 (over claimers)
label values group_v3 student_group_v4
tab group_v3, m
tab group_v3
bysort cnt: tab group_v3


gen over_claimer = .
replace over_claimer = 1 if group == 3 //(over claimers)
replace over_claimer = 0 if group == 0 | group == 1 | group == 2 // (non_over claimers)
label values over_claimer student_group_v2
tab over_claimer, m
tab over_claimer
bysort cnt: tab over_claimer

gen group_2cat = .
replace group_2cat = 0 if avg_13_2cat == 1 & avg_3_2cat == 1 //l_avg13_l_avg3 (low claimers)
replace group_2cat = 1 if avg_13_2cat == 1 & avg_3_2cat == 2 //l_avg13_h_avg3 (crazy kids)
replace group_2cat = 2 if avg_13_2cat == 2 & avg_3_2cat == 1 //h_avg13_l_avg3 (ideal group)
replace group_2cat = 3 if avg_13_2cat == 2 & avg_3_2cat == 2 //h_avg13_h_avg3 (over claimers)
label values group_2cat student_group
tab avg_13_2cat avg_3_2cat, m
bysort cnt: tab avg_13_2cat avg_3_2cat, m
tab group_2cat, m
bysort cnt: tab group_2cat


***************************************************************************
*Start creating tables (could load the proceeded data set and start from here)
***************************************************************************
//save "G:\Conference\2019\Data\PISA\2012_STATAFile\INT_STU12_DEC03_processed.dta", replace
use "G:\Conference\2019\Data\PISA\2012_STATAFile\INT_STU12_DEC03_processed.dta", clear

***************************************************
***************************************************
*get outcome by group
***************************************************
***************************************************


***************************************************************************
*//Table 1-a, percentage
***************************************************************************
//repest PISA, estimate(freq group) by(cnt) flag
//repest PISA, estimate(freq group_2cat) by(cnt) flag

levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(freq group)
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "% group_0" "SE group_0" "% group_1" "SE group_1" "% group_2" "SE group_2" "% group_3" "SE group_3" 
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 1-a", replace) 
putexcel B2 = matrix(analysis, names)


levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(freq group) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "% group_0" "SE group_0" "% group_1" "SE group_1" "% group_2" "SE group_2" "% group_3" "SE group_3" 
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 1-a-flag", replace) 
putexcel B2 = matrix(analysis, names)



levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(freq group_v2)
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5] , A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "% group_0" "SE group_0" "% group_1" "SE group_1" "% group_2" "SE group_2" "% group_3" "SE group_3" "% group_4" "SE group_4" 
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 1-a-other", replace) 
putexcel B2 = matrix(analysis, names)


levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(freq group_v2) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5] , A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "% group_0" "SE group_0" "% group_1" "SE group_1" "% group_2" "SE group_2" "% group_3" "SE group_3" "% group_4" "SE group_4" 
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 1-a-other-flag", replace) 
putexcel B2 = matrix(analysis, names)


***************************************************************************
*//Table 1-b, percentage by gender
***************************************************************************
//repest PISA, estimate(freq group) over(st04q01, test) by(cnt) flag
//repest PISA, estimate(freq group_2cat) over(st04q01, test) by(cnt) flag


levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(freq group) over(st04q01, test)
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5] , A[1,6], A[2,6] , A[1,7], A[2,7] , A[1,8], A[2,8] , A[1,9] , A[4,9], A[1,10] , A[4,10], A[1,11] , A[4,11], A[1,12] , A[4,12]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "% st04q01=1: group_0" "SE st04q01=1: group_0" "% st04q01=1: group_1" "SE st04q01=1: group_1" "% st04q01=1: group_2" "SE st04q01=1: group_2" "% st04q01=1: group_3" "SE st04q01=1: group_3" "% st04q01=2: group_0" "SE st04q01=2: group_0" "% st04q01=2: group_1" "SE st04q01=2: group_1" "% st04q01=2: group_2" "SE st04q01=2: group_2" "% st04q01=2: group_3" "SE st04q01=2: group_3" "% st04q01=d: group_0" "P st04q01=d: group_0" "% st04q01=d: group_1" "P st04q01=d: group_1" "% st04q01=d: group_2" "P st04q01=d: group_2" "% st04q01=d: group_3" "P st04q01=d: group_3" 
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 

*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 1-b", replace) 
putexcel B2 = matrix(analysis, names)

levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(freq group) over(st04q01, test) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5] , A[1,6], A[2,6] , A[1,7], A[2,7] , A[1,8], A[2,8] , A[1,9] , A[4,9], A[1,10] , A[4,10], A[1,11] , A[4,11], A[1,12] , A[4,12]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "% st04q01=1: group_0" "SE st04q01=1: group_0" "% st04q01=1: group_1" "SE st04q01=1: group_1" "% st04q01=1: group_2" "SE st04q01=1: group_2" "% st04q01=1: group_3" "SE st04q01=1: group_3" "% st04q01=2: group_0" "SE st04q01=2: group_0" "% st04q01=2: group_1" "SE st04q01=2: group_1" "% st04q01=2: group_2" "SE st04q01=2: group_2" "% st04q01=2: group_3" "SE st04q01=2: group_3" "% st04q01=d: group_0" "P st04q01=d: group_0" "% st04q01=d: group_1" "P st04q01=d: group_1" "% st04q01=d: group_2" "P st04q01=d: group_2" "% st04q01=d: group_3" "P st04q01=d: group_3" 
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 

*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 1-b-flag", replace) 
putexcel B2 = matrix(analysis, names)




levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(freq group_v2) over(st04q01, test)
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5] , A[1,6], A[2,6] , A[1,7], A[2,7] , A[1,8], A[2,8] , A[1,9] , A[2,9], A[1,10] , A[2,10], A[1,11] , A[4,11], A[1,12] , A[4,12], A[1,13] , A[4,13], A[1,14] , A[4,14], A[1,15] , A[4,15]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "% st04q01=1: group_0" "SE st04q01=1: group_0" "% st04q01=1: group_1" "SE st04q01=1: group_1" "% st04q01=1: group_2" "SE st04q01=1: group_2" "% st04q01=1: group_3" "SE st04q01=1: group_3" "% st04q01=1: group_4" "SE st04q01=1: group_4" "% st04q01=2: group_0" "SE st04q01=2: group_0" "% st04q01=2: group_1" "SE st04q01=2: group_1" "% st04q01=2: group_2" "SE st04q01=2: group_2" "% st04q01=2: group_3" "SE st04q01=2: group_3" "% st04q01=2: group_4" "SE st04q01=2: group_4" "% st04q01=d: group_0" "P st04q01=d: group_0" "% st04q01=d: group_1" "P st04q01=d: group_1" "% st04q01=d: group_2" "P st04q01=d: group_2" "% st04q01=d: group_3" "P st04q01=d: group_3" "% st04q01=d: group_4" "P st04q01=d: group_4" 
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 

*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 1-b-other", replace) 
putexcel B2 = matrix(analysis, names)

levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(freq group_v2) over(st04q01, test) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5] , A[1,6], A[2,6] , A[1,7], A[2,7] , A[1,8], A[2,8] , A[1,9] , A[2,9], A[1,10] , A[2,10], A[1,11] , A[4,11], A[1,12] , A[4,12], A[1,13] , A[4,13], A[1,14] , A[4,14], A[1,15] , A[4,15]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "% st04q01=1: group_0" "SE st04q01=1: group_0" "% st04q01=1: group_1" "SE st04q01=1: group_1" "% st04q01=1: group_2" "SE st04q01=1: group_2" "% st04q01=1: group_3" "SE st04q01=1: group_3" "% st04q01=1: group_4" "SE st04q01=1: group_4" "% st04q01=2: group_0" "SE st04q01=2: group_0" "% st04q01=2: group_1" "SE st04q01=2: group_1" "% st04q01=2: group_2" "SE st04q01=2: group_2" "% st04q01=2: group_3" "SE st04q01=2: group_3" "% st04q01=2: group_4" "SE st04q01=2: group_4" "% st04q01=d: group_0" "P st04q01=d: group_0" "% st04q01=d: group_1" "P st04q01=d: group_1" "% st04q01=d: group_2" "P st04q01=d: group_2" "% st04q01=d: group_3" "P st04q01=d: group_3" "% st04q01=d: group_4" "P st04q01=d: group_4" 
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 

*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 1-b-other-flag", replace) 
putexcel B2 = matrix(analysis, names)



***************************************************************************
*//Table 1-c, percentage by ESCS
***************************************************************************
//repest PISA, estimate(freq group) over(r_escs_quartiles, test) by(cnt) flag
//repest PISA, estimate(freq group_2cat) over(r_escs_2cat, test) by(cnt) flag


levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(freq group) over(r_escs_2cat, test)
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5] , A[1,6], A[2,6] , A[1,7], A[2,7] , A[1,8], A[2,8] , A[1,9] , A[4,9], A[1,10] , A[4,10], A[1,11] , A[4,11], A[1,12] , A[4,12]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "% escs=1: group_0" "SE escs=1: group_0" "% escs=1: group_1" "SE escs=1: group_1" "% escs=1: group_2" "SE escs=1: group_2" "% escs=1: group_3" "SE escs=1: group_3" "% escs=2: group_0" "SE escs=2: group_0" "% escs=2: group_1" "SE escs=2: group_1" "% escs=2: group_2" "SE escs=2: group_2" "% escs=2: group_3" "SE escs=2: group_3" "% escs=d: group_0" "P escs=d: group_0" "% escs=d: group_1" "P escs=d: group_1" "% escs=d: group_2" "P escs=d: group_2" "% escs=d: group_3" "P escs=d: group_3" 
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 

*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 1-c", replace) 
putexcel B2 = matrix(analysis, names)

levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(freq group) over(r_escs_2cat, test) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5] , A[1,6], A[2,6] , A[1,7], A[2,7] , A[1,8], A[2,8] , A[1,9] , A[4,9], A[1,10] , A[4,10], A[1,11] , A[4,11], A[1,12] , A[4,12]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "% escs=1: group_0" "SE escs=1: group_0" "% escs=1: group_1" "SE escs=1: group_1" "% escs=1: group_2" "SE escs=1: group_2" "% escs=1: group_3" "SE escs=1: group_3" "% escs=2: group_0" "SE escs=2: group_0" "% escs=2: group_1" "SE escs=2: group_1" "% escs=2: group_2" "SE escs=2: group_2" "% escs=2: group_3" "SE escs=2: group_3" "% escs=d: group_0" "P escs=d: group_0" "% escs=d: group_1" "P escs=d: group_1" "% escs=d: group_2" "P escs=d: group_2" "% escs=d: group_3" "P escs=d: group_3" 
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 

*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 1-c-flag", replace) 
putexcel B2 = matrix(analysis, names)




levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(freq group_v2) over(r_escs_2cat, test)
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5] , A[1,6], A[2,6] , A[1,7], A[2,7] , A[1,8], A[2,8] , A[1,9] , A[2,9], A[1,10] , A[2,10], A[1,11] , A[4,11], A[1,12] , A[4,12], A[1,13] , A[4,13], A[1,14] , A[4,14], A[1,15] , A[4,15]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "% escs=1: group_0" "SE escs=1: group_0" "% escs=1: group_1" "SE escs=1: group_1" "% escs=1: group_2" "SE escs=1: group_2" "% escs=1: group_3" "SE escs=1: group_3" "% escs=1: group_4" "SE escs=1: group_4" "% escs=2: group_0" "SE escs=2: group_0" "% escs=2: group_1" "SE escs=2: group_1" "% escs=2: group_2" "SE escs=2: group_2" "% escs=2: group_3" "SE escs=2: group_3" "% escs=2: group_4" "SE escs=2: group_4" "% escs=d: group_0" "P escs=d: group_0" "% escs=d: group_1" "P escs=d: group_1" "% escs=d: group_2" "P escs=d: group_2" "% escs=d: group_3" "P escs=d: group_3" "% escs=d: group_4" "P escs=d: group_4" 
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 

*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 1-c-other", replace) 
putexcel B2 = matrix(analysis, names)

levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(freq group_v2) over(r_escs_2cat, test) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5] , A[1,6], A[2,6] , A[1,7], A[2,7] , A[1,8], A[2,8] , A[1,9] , A[2,9], A[1,10] , A[2,10], A[1,11] , A[4,11], A[1,12] , A[4,12], A[1,13] , A[4,13], A[1,14] , A[4,14], A[1,15] , A[4,15]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "% escs=1: group_0" "SE escs=1: group_0" "% escs=1: group_1" "SE escs=1: group_1" "% escs=1: group_2" "SE escs=1: group_2" "% escs=1: group_3" "SE escs=1: group_3" "% escs=1: group_4" "SE escs=1: group_4" "% escs=2: group_0" "SE escs=2: group_0" "% escs=2: group_1" "SE escs=2: group_1" "% escs=2: group_2" "SE escs=2: group_2" "% escs=2: group_3" "SE escs=2: group_3" "% escs=2: group_4" "SE escs=2: group_4" "% escs=d: group_0" "P escs=d: group_0" "% escs=d: group_1" "P escs=d: group_1" "% escs=d: group_2" "P escs=d: group_2" "% escs=d: group_3" "P escs=d: group_3" "% escs=d: group_4" "P escs=d: group_4" 
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 

*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 1-c-other-flag", replace) 
putexcel B2 = matrix(analysis, names)

***************************************************************************
*//Table 2-a, math score
***************************************************************************
//repest PISA, estimate(summarize pv@math, stats(mean)) over(group, test) by(cnt) flag //need to change the group order so the sig test is comparing ideal group with over claimers?
//repest PISA, estimate(summarize pv@math, stats(mean)) over(group_2cat, test) by(cnt) flag
levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize pv@math, stats(mean)) over(group, test) 
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=0: pv_math_mean" "group=0: se" "group=1: pv_math_mean" "group=1: se" "group=2: pv_math_mean" "group=2: se" "group=3: pv_math_mean" "group=3: se" "group=d: pv_math_mean"  "group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-a", replace) 
putexcel B2 = matrix(analysis, names)

levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize pv@math, stats(mean)) over(group, test) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=0: pv_math_mean" "group=0: se" "group=1: pv_math_mean" "group=1: se" "group=2: pv_math_mean" "group=2: se" "group=3: pv_math_mean" "group=3: se" "group=d: pv_math_mean"  "group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-a-flag", replace) 
putexcel B2 = matrix(analysis, names)


***************************************************************************
*//Table 2-a-idealVSover, math score
***************************************************************************
levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize pv@math, stats(mean)) over(group_v3, test) 
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[4,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=ideal: pv_math_mean" "group=ideal: se" "group=irrational: pv_math_mean" "group=irrational: se" "group=low: pv_math_mean" "group=low: se" "group=over: pv_math_mean" "group=over: se" "group=d: pv_math_mean"  "group=d: p"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-a-idealVSover", replace) 
putexcel B2 = matrix(analysis, names)

levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize pv@math, stats(mean)) over(group_v3, test) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[4,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=ideal: pv_math_mean" "group=ideal: se" "group=irrational: pv_math_mean" "group=irrational: se" "group=low: pv_math_mean" "group=low: se" "group=over: pv_math_mean" "group=over: se" "group=d: pv_math_mean"  "group=d: p"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-a-idealVSover-f", replace) 
putexcel B2 = matrix(analysis, names)




***************************************************************************
*//Table 2-a-regression, math score
***************************************************************************
//repest PISA if cnt == "USA", estimate(stata: reg pv@math over_claimer st04q01 escs) flag



***************************************************************************
*//Table 2-b, math score by gender
***************************************************************************
//repest PISA, estimate(summarize pv@math, stats(mean)) over(group, test) by(cnt) flag //need to change the group order so the sig test is comparing ideal group with over claimers?
//repest PISA, estimate(summarize pv@math, stats(mean)) over(group_2cat, test) by(cnt) flag
levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 
*getting A B (by gender)
	*st04q01 == 1 (female)
	repest PISA if cnt=="`l'" & st04q01 == 1, estimate(summarize pv@math, stats(mean)) over(group, test)
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 
	
	*st04q01 == 2 (male)
	repest PISA if cnt=="`l'" & st04q01 == 2, estimate(summarize pv@math, stats(mean)) over(group, test)
	*return list 
	cap mat list r(table)
	cap mat drop B 
	qui mat B = r(table) 
	
	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5], B[1,1] , B[2,1], B[1,2] , B[2,2], B[1,3] , B[2,3], B[1,4] , B[2,4], B[1,5], B[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "female: group=0: pv_math_mean" "female: group=0: se" "female: group=1: pv_math_mean" "female: group=1: se" "female: group=2: pv_math_mean" "female: group=2: se" "female: group=3: pv_math_mean" "female: group=3: se" "female: group=d: pv_math_mean"  "female: group=d: se" "male: group=0: pv_math_mean" "male: group=0: se" "male: group=1: pv_math_mean" "male: group=1: se" "male: group=2: pv_math_mean" "male: group=2: se" "male: group=3: pv_math_mean" "male: group=3: se" "male: group=d: pv_math_mean"  "male: group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-b", replace) 
putexcel B2 = matrix(analysis, names)


levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 
*getting A B (by gender)
	*st04q01 == 1 (female)
	repest PISA if cnt=="`l'" & st04q01 == 1, estimate(summarize pv@math, stats(mean)) over(group, test) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 
	
	*st04q01 == 2 (male)
	repest PISA if cnt=="`l'" & st04q01 == 2, estimate(summarize pv@math, stats(mean)) over(group, test) flag
	*return list 
	cap mat list r(table)
	cap mat drop B 
	qui mat B = r(table) 
	
	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5], B[1,1] , B[2,1], B[1,2] , B[2,2], B[1,3] , B[2,3], B[1,4] , B[2,4], B[1,5], B[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "female: group=0: pv_math_mean" "female: group=0: se" "female: group=1: pv_math_mean" "female: group=1: se" "female: group=2: pv_math_mean" "female: group=2: se" "female: group=3: pv_math_mean" "female: group=3: se" "female: group=d: pv_math_mean"  "female: group=d: se" "male: group=0: pv_math_mean" "male: group=0: se" "male: group=1: pv_math_mean" "male: group=1: se" "male: group=2: pv_math_mean" "male: group=2: se" "male: group=3: pv_math_mean" "male: group=3: se" "male: group=d: pv_math_mean"  "male: group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-b-flag", replace) 
putexcel B2 = matrix(analysis, names)



***************************************************************************
*//Table 2-c, science score
***************************************************************************
levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize pv@scie, stats(mean)) over(group, test) 
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=0: pv_scie_mean" "group=0: se" "group=1: pv_scie_mean" "group=1: se" "group=2: pv_scie_mean" "group=2: se" "group=3: pv_scie_mean" "group=3: se" "group=d: pv_scie_mean"  "group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-c", replace) 
putexcel B2 = matrix(analysis, names)

levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize pv@scie, stats(mean)) over(group, test) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=0: pv_scie_mean" "group=0: se" "group=1: pv_scie_mean" "group=1: se" "group=2: pv_scie_mean" "group=2: se" "group=3: pv_scie_mean" "group=3: se" "group=d: pv_scie_mean"  "group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-c-flag", replace) 
putexcel B2 = matrix(analysis, names)



***************************************************************************
*//Table 2-d, reading score
***************************************************************************
levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize pv@read, stats(mean)) over(group, test) 
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=0: pv_read_mean" "group=0: se" "group=1: pv_read_mean" "group=1: se" "group=2: pv_read_mean" "group=2: se" "group=3: pv_read_mean" "group=3: se" "group=d: pv_read_mean"  "group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-d", replace) 
putexcel B2 = matrix(analysis, names)

levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize pv@read, stats(mean)) over(group, test) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=0: pv_read_mean" "group=0: se" "group=1: pv_read_mean" "group=1: se" "group=2: pv_read_mean" "group=2: se" "group=3: pv_read_mean" "group=3: se" "group=d: pv_read_mean"  "group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-d-flag", replace) 
putexcel B2 = matrix(analysis, names)



***************************************************************************
*//Table 2-e, anxmat - Math Anxiety
***************************************************************************
levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize anxmat, stats(mean)) over(group, test) 
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=0: anxmat_mean" "group=0: se" "group=1: anxmat_mean" "group=1: se" "group=2: anxmat_mean" "group=2: se" "group=3: anxmat_mean" "group=3: se" "group=d: anxmat_mean"  "group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-e", replace) 
putexcel B2 = matrix(analysis, names)


levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize anxmat, stats(mean)) over(group, test) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=0: anxmat_mean" "group=0: se" "group=1: anxmat_mean" "group=1: se" "group=2: anxmat_mean" "group=2: se" "group=3: anxmat_mean" "group=3: se" "group=d: anxmat_mean"  "group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-e-flag", replace) 
putexcel B2 = matrix(analysis, names)


***************************************************************************
*//Table 2-f, matheff - math self-efficacy
***************************************************************************
levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize matheff, stats(mean)) over(group, test) 
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=0: matheff" "group=0: se" "group=1: matheff" "group=1: se" "group=2: matheff" "group=2: se" "group=3: matheff" "group=3: se" "group=d: matheff"  "group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-f", replace) 
putexcel B2 = matrix(analysis, names)


levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize matheff, stats(mean)) over(group, test) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=0: matheff" "group=0: se" "group=1: matheff" "group=1: se" "group=2: matheff" "group=2: se" "group=3: matheff" "group=3: se" "group=d: matheff"  "group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-f-flag", replace) 
putexcel B2 = matrix(analysis, names)



***************************************************************************
*//Table 2-g, scmat - math self-concept
***************************************************************************
levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize scmat, stats(mean)) over(group, test) 
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=0: scmat" "group=0: se" "group=1: scmat" "group=1: se" "group=2: scmat" "group=2: se" "group=3: scmat" "group=3: se" "group=d: scmat"  "group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-g", replace) 
putexcel B2 = matrix(analysis, names)

levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize scmat, stats(mean)) over(group, test) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=0: scmat" "group=0: se" "group=1: scmat" "group=1: se" "group=2: scmat" "group=2: se" "group=3: scmat" "group=3: se" "group=d: scmat"  "group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-g-flag", replace) 
putexcel B2 = matrix(analysis, names)

***************************************************************************
*//Table 2-h, ancscmat - math self-concept (anchored)
***************************************************************************
levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize ancscmat, stats(mean)) over(group, test) 
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=0: ancscmat" "group=0: se" "group=1: ancscmat" "group=1: se" "group=2: ancscmat" "group=2: se" "group=3: ancscmat" "group=3: se" "group=d: ancscmat"  "group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-h", replace) 
putexcel B2 = matrix(analysis, names)


levelsof cnt  , local(lvs) 
local num = 0 
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(summarize ancscmat, stats(mean)) over(group, test) flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5], A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "group=0: ancscmat" "group=0: se" "group=1: ancscmat" "group=1: se" "group=2: ancscmat" "group=2: se" "group=3: ancscmat" "group=3: se" "group=d: ancscmat"  "group=d: se"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("Table 2-h-flag", replace) 
putexcel B2 = matrix(analysis, names)




***************************************************************************
*//Table 3 series, percentage by items (st62q*)
*//st62q01 st62q02 st62q03 st62q04 st62q06 st62q07 st62q08 st62q09 st62q10 st62q11 st62q12 st62q13 st62q15 st62q16 st62q17 st62q19
***************************************************************************
//repest PISA, estimate(freq st62q01) by(cnt) flag

levelsof cnt  , local(lvs) 
local num = 0 
local item st62q01 st62q02 st62q03 st62q04 st62q06 st62q07 st62q08 st62q09 st62q10 st62q11 st62q12 st62q13 st62q15 st62q16 st62q17 st62q19
foreach var of local item {
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(freq `var')
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5] , A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "% group_1" "SE group_1" "% group_2" "SE group_2" "% group_3" "SE group_3" "% group_4" "SE group_4" "% group_5" "SE group_5"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("`var'", replace) 
putexcel B2 = matrix(analysis, names)
local num = 0 
}

levelsof cnt  , local(lvs) 
local num = 0 
local item st62q01 st62q02 st62q03 st62q04 st62q06 st62q07 st62q08 st62q09 st62q10 st62q11 st62q12 st62q13 st62q15 st62q16 st62q17 st62q19
foreach var of local item {
foreach l of local lvs { 

	repest PISA if cnt=="`l'"  , estimate(freq `var') flag
	*return list 
	cap mat list r(table)
	cap mat drop A 
	qui mat A = r(table) 

	* Getting the coeeficients
	cap mat drop b
	qui mat b = (A[1,1] , A[2,1], A[1,2] , A[2,2], A[1,3] , A[2,3], A[1,4] , A[2,4], A[1,5] , A[2,5]) 
	* mat list b 
	*local lb : label (cnt)`l'
	*display "`lb'"
	qui mat rown b  = `l'
	qui mat coln b = "% group_1" "SE group_1" "% group_2" "SE group_2" "% group_3" "SE group_3" "% group_4" "SE group_4" "% group_5" "SE group_5"
	
	qui if `num' == 0 { 
		cap drop mat analysis 
		mat analysis = b 
	} 
	
	qui else { 
		mat analysis = analysis \ b 
	} 
	
	*locla num = `num' + 1 
	local ++num
} 

mat list analysis 
*export
putexcel set "G:\Conference\2019\Git\InternationalAssessment_Overclaiming\Stata\Stata Output\Output_tables.xls", modify sheet("`var'-flag", replace) 
putexcel B2 = matrix(analysis, names)
local num = 0 
}


















////
gen overclaim_pn = .
replace overclaim_pn = 0 if st62q04 == 1
replace overclaim_pn = 1 if (st62q04 != 1 & st62q04 != .)
tab overclaim_pn, m

label define yes_no 0 "No" 1 "Yes"
label values overclaim_pn yes_no




repest PISA if cnt==68 , estimate(stata: reg st62q04 pv@math, robust)
repest PISA if cnt==68 , estimate(stata: reg overclaim_pn pv@math, robust) // USA

repest PISA if cnt==34 , estimate(stata: reg overclaim_pn pv@math, robust) //Korea
repest PISA if cnt==1 , estimate(stata: reg overclaim_pn pv@math st04q01 st25q01, robust) //Albania

repest PISA if cnt==34 , estimate(stata: logit overclaim_pn pv@math, robust) //Korea






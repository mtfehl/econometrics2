// Problem Set 2 //
pwd
cd "C:\Users\T14\7Programming\STATA\02Classes\01EconometricsII\Problem_Sets\PS02\"
use "soep_lebensz_en.dta", clear


// question 1 //
gen has_kids = (no_kids > 0 & no_kids != .) // has_kids dummy
label var has_kids "kids dummy"

sort id year
by id: gen obs_no = _n 
keep if obs_no <= 2  // only keep the first two obs for an individal

by id: gen total_obs = _N
keep if total_obs == 2 // only keep obs with exactly two obs

by id: gen year_gap = (year-year[_n-1])
by id: egen total_gap = max(year_gap)
keep if total_gap == 1 // only keep obs with exactly one year differences

sort id year
xtset id year

// first-diff 
reg d.satisf_std d.has_kids d.health_std d.education, noconstant
estimate store firstdiff1

// fixed effects
xtreg satisf_std has_kids health_std education, fe
estimate store fixed1

esttab firstdiff1 fixed1 using "fd_fe_T=2.tex", replace tex ///
	rename(D.has_kids has_kids D.health_std health_std D.education education) ///
    mtitles("First Difference" "Fixed Effects") ///
	main(b) se parentheses nonumbers ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Comparison of FE and FD Estimates when T=2")

	
// question 2 //
use "soep_lebensz_en.dta", clear
gen has_kids = (no_kids > 0 & no_kids != .)

xtset id year // run on full dataset

// first diff
reg d.satisf_std d.has_kids d.health_std d.education, noconstant
estimate store firstdiff2

// fixed effects
xtreg satisf_std has_kids health_std education, fe
estimate store fixed2

esttab firstdiff2 fixed2 using "fd_fe_T!=2.tex", replace tex ///
	rename(D.has_kids has_kids D.health_std health_std D.education education) ///
    mtitles("First Difference" "Fixed Effects") ///
	main(b) se parentheses nonumbers ///
	star(* 0.10 ** 0.05 *** 0.01) 
	
// first diff, clustered at individual lvl
reg d.satisf_std d.has_kids d.health_std d.education, noconstant vce(cluster id)
estimate store fdclustered

// fixed effects, clustered at individual lvl
xtreg satisf_std has_kids health_std education, fe vce(cluster id)
estimate store feclustered

esttab fdclustered feclustered using "fd_fe_T!=2_clustered.tex", replace tex ///
	rename(D.has_kids has_kids D.health_std health_std D.education education) ///
    mtitles("First Difference" "Fixed Effects") ///
	main(b) se parentheses nonumbers ///
	star(* 0.10 ** 0.05 *** 0.01)

	
// question 3 //

// fixed effects on dynamic model: single lag as a regressor
xtreg satisf_std l.satisf_std has_kids health_std education, fe
estimate store lagfixed

esttab lagfixed using "fe_dynamic_singlelag.tex", replace tex ///
	mtitles("Fixed Effects") ///
	main(b) se parentheses nonumbers ///
    star(* 0.10 ** 0.05 *** 0.01) 
	
	
// question 4 //

xtabond satisf_std has_kids health_std education, lags(1) maxldep(2) twostep //arellano-bond
estimate store bond

esttab lagfixed using "abond_1lag_2iv.tex", replace tex ///
	mtitles("Arellano-Bond") ///
	main(b) se parentheses nonumbers ///
    star(* 0.10 ** 0.05 *** 0.01)

// hypothesis testing for serial correlation 
estat abond

return list
matrix list r(arm)
matrix AB = J(2,2,.) // build table to export as latex
matrix list AB
matrix colnames AB = z p-value
matrix rownames AB = AR1 AR2

matrix AB[1,1] = round(r(arm)[1,2], 0.001)
matrix AB[1,2] = round(r(arm)[1,3], 0.001)
matrix AB[2,1] = round(r(arm)[2,2], 0.001)
matrix AB[2,2] = round(r(arm)[2,3], 0.001)

esttab matrix(AB) using "abond_serialcorr_results.tex", replace tex 


esttab lagfixed bond using "fe_abond_singlelag.tex", replace tex ///
	mtitles("Fixed Effects" "Arellano-Bond") ///
    main(b) se parentheses nonumbers ///
    star(* 0.10 ** 0.05 *** 0.01) 

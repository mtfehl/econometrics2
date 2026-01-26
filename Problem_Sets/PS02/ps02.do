// Problem Set 2 //
pwd
cd "C:\Users\T14\7Programming\STATA\02Classes\01EconometricsII\Problem_Sets\PS02\"
use "soep_lebensz_en.dta", clear



// question 1 //
gen has_kids = (no_kids > 0 & no_kids != .)

sort id year
by id: gen obs_no = _n
keep if obs_no <= 2

gen valid = !missing(satisf_std, has_kids, health_std, education)
bysort id: egen valid_count = sum(valid)
keep if valid_count == 2

sort id year
by id: gen time_seq = _n
xtset id time_seq

reg d.satisf_std d.has_kids d.health_std d.education, noconstant
estimate store firstdiff1
xtreg satisf_std has_kids health_std education, fe
estimate store fixed1





// question 2.
use "soep_lebensz_en.dta", clear
gen has_kids = 0
replace has_kids = 1 if (no_kids > 0 & no_kids != .)


xtset id year
reg d.satisf_std d.has_kids d.health_std d.education, noconstant
estimate store firstdiff2
xtreg satisf_std has_kids health_std education, fe
estimate store fixed2


reg d.satisf_std d.has_kids d.health_std d.education, noconstant vce(cluster id)
estimate store fdclustered
xtreg satisf_std has_kids health_std education, fe vce(cluster id)
estimate store feclustered

// question 3
xtset id year
xtreg satisf_std l.satisf_std has_kids health_std education, fe
estimate store lagfixed
predict res, ue
correlate d.res l.d.res

// question 4
xtabond satisf_std has_kids health_std education, lags(1) maxldep(2) twostep
estimate store bond

estat abond
outreg2 using "abond_serialcorr_results.tex", replace tex

esttab lagfixed bond using "results4.tex", replace ///
    tex label main(b) abs parentheses ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("Comparison of FE and Arellano-Bond Estimates") ///
    addnotes("Standard errors in parentheses")


























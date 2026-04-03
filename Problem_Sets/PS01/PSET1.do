* --- set working dir ---
cd "C:\Users\T14\7Programming\STATA\02Classes\01EconometricsII\Problem_Sets\PS01"

* --- set output dirs for tables and plot ---
capture mkdir "tables_tex"
capture mkdir "tables_html"
capture mkdir "plots"

* --- load data ---
use "soep_lebensz_en.dta", clear

* --- check of variables ---
describe id year sex education no_kids health_org satisf_org health_std satisf_std
summarize no_kids satisf_std education

* --- Declare Panel ---
xtset id year

label list sex
* 0 are males, 1 females

generate byte has_kids = (no_kids > 0)
label var has_kids "has any children in household (indicator)"


* --- Baseline: Pooled OLS (cluster SEs at id level) ---
regress satisf_std has_kids sex education health_org i.year, vce(cluster id)
estimates store pooled

* --- Fixed Effects ---
xtreg satisf_std sex has_kids education health_org i.year, fe vce(cluster id)
estimates store fe

// tex table
esttab pooled fe using "tables_tex/q4_pols_fe.tex", replace tex ///
	b(3) se(2) label ///
	title("Pooled OLS vs Fixed Effects") ///
	mtitles("Pooled OLS" "Fixed Effects") ///
	nonumbers star(* 0.10 ** 0.05 *** 0.01)

// html table
esttab pooled fe using "tables_html/q4_pols_fe.html", replace htm ///
	b(3) se(2) label ///
	title("Pooled OLS vs Fixed Effects") ///
	mtitles("Pooled OLS" "Fixed Effects") ///
	nonumbers star(* 0.10 ** 0.05 *** 0.01)


* --- Interaction: has_kids * sex ---
generate byte has_kids_sex = has_kids * sex
xtreg satisf_std sex has_kids has_kids_sex education health_org i.year, fe vce(cluster id)
estimates store fe_int

// tex table
esttab fe fe_int using "tables_tex/q4_fe_interaction.tex", replace tex ///
	b(3) se(2) label ///
	title("FE with Gender x Children Interaction") ///
	mtitles("Fixed Effects" "FE + Interaction") ///
	nonumbers star(* 0.10 ** 0.05 *** 0.01)

// html table
esttab fe fe_int using "tables_html/q4_fe_interaction.html", replace htm ///
	b(3) se(2) label ///
	title("FE with Gender x Children Interaction") ///
	mtitles("Fixed Effects" "FE + Interaction") ///
	nonumbers star(* 0.10 ** 0.05 *** 0.01)


* --- Random Effects ---
xtreg satisf_std sex has_kids_sex has_kids education health_org i.year, re vce(cluster id)
estimates store re

// tex table
esttab fe_int re using "tables_tex/q4_fe_re.tex", replace tex ///
	b(3) se(2) label ///
	title("Fixed Effects vs Random Effects") ///
	mtitles("Fixed Effects" "Random Effects") ///
	nonumbers star(* 0.10 ** 0.05 *** 0.01)

// html table
esttab fe_int re using "tables_html/q4_fe_re.html", replace htm ///
	b(3) se(2) label ///
	title("Fixed Effects vs Random Effects") ///
	mtitles("Fixed Effects" "Random Effects") ///
	nonumbers star(* 0.10 ** 0.05 *** 0.01)


* --- Hausman test (FE vs RE) ---
* Hausman does not work with clustered SE, so re-estimate without clustering

xtreg satisf_std has_kids has_kids_sex education health_org i.year, fe
estimates store fe_nocluster

xtreg satisf_std has_kids has_kids_sex education health_org i.year, re
estimates store re_nocluster

hausman fe_nocluster re_nocluster, sigmamore

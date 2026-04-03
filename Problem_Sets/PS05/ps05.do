// Problem Set 5: Selection & Matching //
cd "C:\Users\T14\7Programming\STATA\02Classes\01EconometricsII\Problem_Sets\PS05\"
use "dw.dta", clear

capture mkdir "tables_tex"
capture mkdir "tables_html"
capture mkdir "plots"


// question 1: summary stats across subgroups
quietly eststo estcps:    estpost summarize age ed black hisp married nodeg re74 re75 re78 if cps == 1
quietly eststo estpsid:   estpost summarize age ed black hisp married nodeg re74 re75 re78 if psid == 1
quietly eststo estshow:   estpost summarize age ed black hisp married nodeg re74 re75 re78 if treat == 1 & exp == 1
quietly eststo estnoshow: estpost summarize age ed black hisp married nodeg re74 re75 re78 if treat == 0 & exp == 1

esttab estcps estpsid estnoshow estshow using "tables_tex/q1_var_means.tex", replace tex ///
	cells("mean(fmt(2))") collabels(none) ///
	mtitles("CPS" "PSID" "NSW Control" "Treated") nonumbers

esttab estcps estpsid estnoshow estshow using "tables_html/q1_var_means.html", replace htm ///
	cells("mean(fmt(2))") collabels(none) ///
	mtitles("CPS" "PSID" "NSW Control" "Treated") nonumbers


// question 2: OLS — Dehejia & Wahba (1999) Table 2

// B1: no controls
eststo b1_1: quietly reg re78 treat if exp == 1
scalar exp_att = _b[treat]
scalar exp_se  = _se[treat]
eststo b1_2: quietly reg re78 treat if (treat == 1 | cps  == 1)
eststo b1_3: quietly reg re78 treat if (treat == 1 | psid == 1)

// B2: full controls
eststo b2_1: quietly reg re78 treat age ed black hisp married nodeg if exp == 1
eststo b2_2: quietly reg re78 treat age ed black hisp married nodeg if (treat == 1 | cps  == 1)
eststo b2_3: quietly reg re78 treat age ed black hisp married nodeg if (treat == 1 | psid == 1)

// B3: past earnings 1975
eststo b3_1: quietly reg re78 treat re75 if exp == 1
eststo b3_2: quietly reg re78 treat re75 if (treat == 1 | cps  == 1)
eststo b3_3: quietly reg re78 treat re75 if (treat == 1 | psid == 1)

// B4: 1975 earnings + full controls
eststo b4_1: quietly reg re78 treat re75 age ed black hisp married nodeg if exp == 1
eststo b4_2: quietly reg re78 treat re75 age ed black hisp married nodeg if (treat == 1 | cps  == 1)
eststo b4_3: quietly reg re78 treat re75 age ed black hisp married nodeg if (treat == 1 | psid == 1)

// C1: past earnings 1974
eststo c1_1: quietly reg re78 treat re74 if exp == 1
eststo c1_2: quietly reg re78 treat re74 if (treat == 1 | cps  == 1)
eststo c1_3: quietly reg re78 treat re74 if (treat == 1 | psid == 1)

// C2: 1974 earnings + full controls
eststo c2_1: quietly reg re78 treat re74 age ed black hisp married nodeg if exp == 1
eststo c2_2: quietly reg re78 treat re74 age ed black hisp married nodeg if (treat == 1 | cps  == 1)
eststo c2_3: quietly reg re78 treat re74 age ed black hisp married nodeg if (treat == 1 | psid == 1)

esttab b1_1 b2_1 b3_1 b4_1 c1_1 c2_1 using "tables_tex/q2_NSW.tex", replace tex ///
	keep(treat) cells(b(star fmt(2)) se(par fmt(2))) ///
	mtitles("No controls" "Full controls" "RE75" "RE75+ctrl" "RE74" "RE74+ctrl") ///
	collabels(none) nonumbers title("NSW Experiment")

esttab b1_1 b2_1 b3_1 b4_1 c1_1 c2_1 using "tables_html/q2_NSW.html", replace htm ///
	keep(treat) cells(b(star fmt(2)) se(par fmt(2))) ///
	mtitles("No controls" "Full controls" "RE75" "RE75+ctrl" "RE74" "RE74+ctrl") ///
	collabels(none) nonumbers title("NSW Experiment")

esttab b1_3 b2_3 b3_3 b4_3 c1_3 c2_3 using "tables_tex/q2_PSID.tex", replace tex ///
	keep(treat) cells(b(star fmt(2)) se(par fmt(2))) ///
	mtitles("No controls" "Full controls" "RE75" "RE75+ctrl" "RE74" "RE74+ctrl") ///
	collabels(none) nonumbers title("PSID")

esttab b1_3 b2_3 b3_3 b4_3 c1_3 c2_3 using "tables_html/q2_PSID.html", replace htm ///
	keep(treat) cells(b(star fmt(2)) se(par fmt(2))) ///
	mtitles("No controls" "Full controls" "RE75" "RE75+ctrl" "RE74" "RE74+ctrl") ///
	collabels(none) nonumbers title("PSID")

esttab b1_2 b2_2 b3_2 b4_2 c1_2 c2_2 using "tables_tex/q2_CPS.tex", replace tex ///
	keep(treat) cells(b(star fmt(2)) se(par fmt(2))) ///
	mtitles("No controls" "Full controls" "RE75" "RE75+ctrl" "RE74" "RE74+ctrl") ///
	collabels(none) nonumbers title("CPS")

esttab b1_2 b2_2 b3_2 b4_2 c1_2 c2_2 using "tables_html/q2_CPS.html", replace htm ///
	keep(treat) cells(b(star fmt(2)) se(par fmt(2))) ///
	mtitles("No controls" "Full controls" "RE75" "RE75+ctrl" "RE74" "RE74+ctrl") ///
	collabels(none) nonumbers title("CPS")


// question 3: propensity score — Becker-Ichino (2002) spec
// ssc install pscore, replace

gen ed2      = ed^2
gen re742    = re74^2
gen re752    = re75^2
gen U74      = re74 == 0
gen blackU74 = black * U74

log using "pscore_output.log", replace
pscore treat age age2 ed ed2 married black hisp re74 re75 re742 re752 blackU74 ///
	if treat == 1 | psid == 1, ///
	pscore(myscore) blockid(myblock) comsup numblo(5) level(0.005) logit
log close


// question 4: ATT — nearest-neighbor matching
set seed 1221
attnd re78 treat age age2 ed ed2 married black hisp re74 re75 re742 re752 blackU74 ///
	if treat == 1 | psid == 1, ///
	comsup boot reps(100) dots logit
scalar nn_att = r(attnd)
scalar nn_se  = r(bseattnd)


// question 5: alternative propensity score specification
gen re7475 = re74 * re75

pscore treat age ed2 married hisp re75 re7475 re752 blackU74 ///
	if treat == 1 | psid == 1, ///
	pscore(myscore2) blockid(myblock2) comsup numblo(5) level(0.005) logit

attnd re78 treat age ed2 married black hisp re74 re75 re752 blackU74 ///
	if treat == 1 | psid == 1, ///
	pscore(myscore2) comsup boot reps(100) dots
scalar nn_att2 = r(attnd)
scalar nn_se2  = r(bseattnd)


// question 6: propensity score histograms
twoway (histogram myscore if treat == 1, color(blue%30) density) ///
       (histogram myscore if psid  == 1, color(red%30)  density), ///
       legend(order(1 "Treated" 2 "Control")) ///
       title("Propensity Score Distribution (Spec 1)") ///
       xtitle("Propensity Score") ytitle("Density")
graph export "plots/propensity_hist.png", replace

twoway (histogram myscore2 if treat == 1, color(blue%30) density) ///
       (histogram myscore2 if psid  == 1, color(red%30)  density), ///
       legend(order(1 "Treated" 2 "Control")) ///
       title("Propensity Score Distribution (Spec 2)") ///
       xtitle("Propensity Score") ytitle("Density")
graph export "plots/propensity_hist_spec2.png", replace


// question 7: radius and kernel matching
attr re78 treat age age2 ed ed2 married black hisp re74 re75 re742 re752 blackU74 ///
	if treat == 1 | psid == 1, ///
	comsup boot reps(100) dots logit radius(0.0001)
scalar rad_att = r(attr)
scalar rad_se  = r(bseattr)

attk re78 treat age age2 ed ed2 married black hisp re74 re75 re742 re752 blackU74 ///
	if treat == 1 | psid == 1, ///
	comsup boot reps(100) dots logit
scalar kern_att = r(attk)
scalar kern_se  = r(bseattk)


// question 8: summary comparison table
matrix T = J(5, 2, .)
matrix T[1,1] = exp_att
matrix T[1,2] = exp_se
matrix T[2,1] = nn_att
matrix T[2,2] = nn_se
matrix T[3,1] = nn_att2
matrix T[3,2] = nn_se2
matrix T[4,1] = rad_att
matrix T[4,2] = rad_se
matrix T[5,1] = kern_att
matrix T[5,2] = kern_se

matrix rownames T = "Experimental" "NN Spec 1" "NN Spec 2" "Radius" "Kernel"
matrix colnames T = "ATT" "SE"

esttab matrix(T, fmt(2)) using "tables_tex/q8_ATT_table.tex", replace tex ///
	title("ATT Estimates Comparison") nomtitles

esttab matrix(T, fmt(2)) using "tables_html/q8_ATT_table.html", replace htm ///
	title("ATT Estimates Comparison") nomtitles

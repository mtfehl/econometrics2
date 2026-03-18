// Problem Set 5: Selection & Matching //
pwd
cd "C:\Users\T14\7Programming\STATA\02Classes\01EconometricsII\Problem_Sets\PS05\"
use "dw.dta", clear

// question 1
** summary: pre-estimation means across variables, by subgroup
quietly eststo estcps: estpost summarize age ed black hisp married nodeg re74 re75 re78 if cps == 1

quietly eststo estpsid: estpost summarize age ed black hisp married nodeg re74 re75 re78 if psid == 1

quietly eststo estshow: estpost summarize age ed black hisp married nodeg re74 re75 re78 if treat == 1 & exp == 1

quietly eststo estnoshow: estpost summarize age ed black hisp married nodeg re74 re75 re78 if treat == 0 & exp == 1

esttab using "q1_var_means.tex", replace tex cells("mean(fmt(2))") ///
	collabels(none) mtitles("CPS" "PSID" "NSW CONTROL" "TREATED") ///
	nonumbers
	
// gen nb=1  * alternative command
// collapse (mean) age ed black hisp married nodeg re74 re75 (sum) nb, by(treat exp cps psid)
// list

	
// question 2
** run ols on NWS, CPS, PSID samples, compare results (Dehejia and Wahba, 1999 Table 2)

** B1: no controls

* ols1: full experiment sample        ** BASELINE EXPERIMENTAL ESTIMATE OF ATT **
eststo b1_1: quietly reg re78 treat if exp == 1     // exp data
estimates store b1_1
scalar exp_att = _b[treat]    // experimental ATT!!
scalar exp_se = _se[treat] 

* ols2: treatment sample + constructed control groups
quietly reg re78 treat if (treat == 1 | cps == 1)   // cps data
estimates store b1_2

quietly reg re78 treat if (treat == 1 | psid == 1)  // psid data
estimates store b1_3

** B2: full controls

* ols1: full experiment sample
quietly reg re78 treat age ed black hisp married nodeg if exp == 1                  // exp data
estimates store b2_1

* ols2: treatment sample + constructed control groups
quietly reg re78 treat age ed black hisp married nodeg if (treat == 1 | cps == 1)   // cps data
estimates store b2_2

quietly reg re78 treat age ed black hisp married nodeg if (treat == 1 | psid == 1)  // psid data
estimates store b2_3

** B3: past earnings (1975) as a control

* ols1: full experiment sample
quietly reg re78 treat re75 if exp == 1                  // exp data
estimates store b3_1

* ols2: treatment sample + constructed control groups
quietly reg re78 treat re75 if (treat == 1 | cps == 1)   // cps data
estimates store b3_2

quietly reg re78 treat re75 if (treat == 1 | psid == 1)  // psid data
estimates store b3_3

** B4: past earnings (1975) + previous controls

* ols1: full experiment sample
quietly reg re78 treat re75 age ed black hisp married nodeg if exp == 1                  // exp data
estimates store b4_1

* ols2: treatment sample + constructed control groups
quietly reg re78 treat re75 age ed black hisp married nodeg if (treat == 1 | cps == 1)   // cps data
estimates store b4_2

quietly reg re78 treat re75 age ed black hisp married nodeg if (treat == 1 | psid == 1)  // psid data
estimates store b4_3

** C1: past earnings (1974) as a control

* ols1: full experiment sample
quietly reg re78 re74 treat if exp == 1                  // exp data
estimates store c1_1

* ols2: treatment sample + constructed control groups
quietly reg re78 treat re74 if (treat == 1 | cps == 1)   // cps data
estimates store c1_2

quietly reg re78 treat re74 if (treat == 1 | psid == 1)  // psid data
estimates store c1_3

** C2: past earnings (1974) + previous controls (not including 1975 earnings)

* ols1: full experiment sample
quietly reg re78 re74 treat age ed black hisp married nodeg if exp == 1                  // exp data
estimates store c2_1

* ols2: treatment sample + constructed control groups
quietly reg re78 treat re74 age ed black hisp married nodeg if (treat == 1 | cps == 1)   // cps data
estimates store c2_2

quietly reg re78 treat re74 age ed black hisp married nodeg if (treat == 1 | psid == 1)  // psid data
estimates store c2_3


// row 1: NSW
esttab b1_1 b2_1 b3_1 b4_1 c1_1 c2_1 using "q2_NSW.tex", replace tex ///
    keep(treat) cells(b(star fmt(2)) se(par fmt(2))) ///
    mtitles("No controls" "Full controls" "RE75" "RE75+ctrl" "RE74" "RE74+ctrl") ///
    collabels(none) nonumbers title("NSW Experiment")

// row 2: PSID
esttab b1_3 b2_3 b3_3 b4_3 c1_3 c2_3 using "q2_PSID.tex", replace tex ///
    keep(treat) cells(b(star fmt(2)) se(par fmt(2))) ///
    mtitles("No controls" "Full controls" "RE75" "RE75+ctrl" "RE74" "RE74+ctrl") ///
    collabels(none) nonumbers title("PSID")

// row 3: CPS
esttab b1_2 b2_2 b3_2 b4_2 c1_2 c2_2 using "q2_CPS.tex", replace tex ///
    keep(treat) cells(b(star fmt(2)) se(par fmt(2))) ///
    mtitles("No controls" "Full controls" "RE75" "RE75+ctrl" "RE74" "RE74+ctrl") ///
    collabels(none) nonumbers title("CPS")



// question 3
* perform matching on NSW-treated / PSID-control sample

pscore treat age age2 ed black hisp married re74 re75 if treat == 1 | psid == 1, ///
    pscore(myscore) blockid(myblock) comsup numblo(5) level(0.005) logit
**  fails: balancing hypothesis does not hold. should include more controls (feature engineer some interactions)
**  gen features to make output 6.1 from (Becker&Ichino, 2002 output6.1)

drop myblock myscore

gen ed2 = ed^2                   //  education sqrd
gen re742 = re74^2               // '74 earnings sqrd
gen re752 = re75^2               // '75 earnings sqrd
gen U74 = re74 == 0              //  unempl in '74 flag
gen blackU74 = black * U74       //  black * U74 interaction

log using "pscore_output.log", replace
pscore treat age age2 ed ed2 married black hisp re74 re75 re742 re752 blackU74 if treat == 1 | psid == 1, ///
    pscore(myscore) blockid(myblock) comsup numblo(5) level(0.005) logit // passes balancing check
	
log close 

	
// question 4
* nearest neighbor matching, estimate ATT (Becker & Ichino,2002 output 6.2)
set seed 1221
attnd re78 treat age age2 ed ed2 married black hisp re74 re75 re742 re752 blackU74 if treat == 1 | psid == 1, ///
	comsup boot reps(100) dots logit
scalar nn_att = r(attnd)
scalar nn_se = r(bseattnd)

// question 5
* alternative specification of propensity score (Lemma 1 B&I 2002)
* compare to q2
gen re7475 = re74*re75 // interaction bw '74 & '75 earnings

pscore treat age ed2 married hisp re75 re7475 re752 blackU74 if treat == 1 | psid == 1, ///
    pscore(myscore2) blockid(myblock2) comsup numblo(5) level (0.005) logit // alt spec: drop re74, re742, age2, ed, add re7475


// estimate ATT on alternative spec
attnd re78 treat age ed2 married black hisp re74 re75 re752 blackU74 if treat == 1 | psid == 1, ///
    pscore(myscore2) comsup boot reps(100) dots 
scalar nn_att2 = r(attnd)
scalar nn_se2 = r(bseattnd)

// question 6
* histograms of propenity scores for treat/control groups

twoway (histogram myscore if treat == 1, color(blue%30) density) ///
       (histogram myscore if psid == 1, color(red%30) density), ///
       legend(order(1 "Treated" 2 "Control")) ///
       title("Propensity Score Distribution") /// 
       xtitle("Propensity Score") ytitle("Density")     // first specification
	   
graph export "propensity_hist.pdf", replace
	   
twoway (histogram myscore2 if treat == 1, color(blue%30) density) ///
       (histogram myscore2 if psid == 1, color(red%30) density), ///
       legend(order(1 "Treated" 2 "Control")) ///
       title("Propensity Score Distribution") ///
       xtitle("Propensity Score") ytitle("Density")	   // second specification
	   

// summary stats of prop score
summarize myscore, detail
// dist across blocks
tab myblock treat

// mean propensity score by block and treatment status
bysort myblock: summarize myscore


// question 7
* radius + kernel matching (B&I 2002, output 6.3-6.4) compare w/ results from (d)
attr re78 treat age age2 ed ed2 married black hisp re74 re75 re742 re752 blackU74 if treat == 1 | psid == 1, ///
	comsup boot reps(100) dots logit radius(0.0001) // radius matching
scalar rad_att = r(attr)
scalar rad_se = r(bseattr)
	
attk re78 treat age age2 ed ed2 married black hisp re74 re75 re742 re752 blackU74 if treat == 1 | psid == 1, ///
    comsup boot reps(100) dots logit
scalar kern_att = r(attk)
scalar kern_se = r(bseattk)


// question 8
* compare results from q7 to results in (Dehejia & Wahba, 1999 Table 2A)

matrix T = J(5, 2, .)
matrix T[1,1] = exp_att
matrix T[1,2] = exp_se
matrix T[2,1] = nn_att          // manually build matrix to store results
matrix T[2,2] = nn_se
matrix T[3,1] = nn_att2
matrix T[3,2] = nn_se2
matrix T[4,1] = rad_att
matrix T[4,2] = rad_se
matrix T[5,1] = kern_att
matrix T[5,2] = kern_se

matrix rownames T = "Experimental" "NN 1st Specif" "NN 2nd Specif" "Radius" "Kernel"
matrix colnames T = "ATT" "SE"

esttab matrix(T, fmt(2)) using "q8_ATT_table.tex", replace tex ///        display as esttab
	title("ATT Estimates Comparison") nomtitles


// bonus: stratification method
atts re78 treat, ///
	pscore(myscore) blockid(myblock) comsup boot reps(100) dots
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
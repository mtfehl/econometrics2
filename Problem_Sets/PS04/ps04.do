// Problem Set 4: Randomized Controlled Trials //
cd "C:\Users\T14\7Programming\STATA\02Classes\01EconometricsII\Problem_Sets\PS04"
use "STAR.dta", clear

capture mkdir "tables_tex"
capture mkdir "tables_html"
capture mkdir "plots"


**************************************************************
*                           3.b
**************************************************************

eststo q3b: reg tscorek sck

scalar beta      = _b[sck]
scalar se_       = _se[sck]
quietly summ tscorek
scalar sd_score  = r(sd)
scalar effect_sd = beta / sd_score

display "Treatment effect (points): " beta
display "SE: "                         se_
display "Sample SD of scores: "        sd_score
display "Effect in SD units: "         effect_sd


**************************************************************
*                           3.c
**************************************************************

eststo q3c: areg tscorek sck, absorb(schidkn)

scalar beta_areg      = _b[sck]
scalar se_areg        = _se[sck]
scalar effect_sd_areg = beta_areg / sd_score

display "Treatment effect (points): " beta_areg
display "SE: "                         se_areg
display "Effect in SD units: "         effect_sd_areg


**************************************************************
*                           3.d
**************************************************************

eststo q3d: areg tscorek sck, absorb(schidkn) vce(cluster schidkn)

scalar beta_cluster    = _b[sck]
scalar se_cluster      = _se[sck]
scalar effect_sd_clust = beta_areg / sd_score

display "Treatment effect (points): " beta_cluster
display "SE (clustered): "             se_cluster
display "Effect in SD units: "         effect_sd_clust

// combined regression table: baseline, school FE, school FE + clustered
esttab q3b q3c q3d using "tables_tex/q3_regs_main.tex", replace tex ///
	mtitles("Baseline" "School FE" "School FE (Clustered)") ///
	main(b) se parentheses nonumbers ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Effect of Small Class Size on Test Scores")

esttab q3b q3c q3d using "tables_html/q3_regs_main.html", replace htm ///
	mtitles("Baseline" "School FE" "School FE (Clustered)") ///
	main(b) se parentheses nonumbers ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Effect of Small Class Size on Test Scores")


**************************************************************
*                           3.e
**************************************************************

ssc install ritest, replace

ritest sck _b[sck], reps(1000) seed(12345) strata(schidkn): ///
	areg tscorek sck, absorb(schidkn) vce(cluster schidkn)


**************************************************************
*                           3.f
**************************************************************

foreach var in boy freelunk totexpk {
	eststo balance_`var': reg `var' sck, vce(cluster schidkn)
}

esttab balance_boy balance_freelunk balance_totexpk ///
	using "tables_tex/q3_balance.tex", replace tex ///
	mtitles("boy" "freelunk" "totexpk") ///
	keep(sck) main(b) se parentheses nonumbers ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Balance Tests: Pre-Determined Characteristics on Treatment")

esttab balance_boy balance_freelunk balance_totexpk ///
	using "tables_html/q3_balance.html", replace htm ///
	mtitles("boy" "freelunk" "totexpk") ///
	keep(sck) main(b) se parentheses nonumbers ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Balance Tests: Pre-Determined Characteristics on Treatment")


**************************************************************
*                           3.g
**************************************************************

eststo q3g: areg tscorek sck boy freelunk totexpk, ///
	absorb(schidkn) vce(cluster schidkn)

scalar beta_cov = _b[sck]
scalar se_cov   = _se[sck]

display "Treatment effect with covariates: " beta_cov
display "SE with covariates: "               se_cov
display "Treatment effect without:          " beta_cluster
display "SE without:                        " se_cluster

esttab q3d q3g using "tables_tex/q3_covariates.tex", replace tex ///
	mtitles("School FE (Clustered)" "School FE + Covariates") ///
	main(b) se parentheses nonumbers ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Adding Covariates to the Regression")

esttab q3d q3g using "tables_html/q3_covariates.html", replace htm ///
	mtitles("School FE (Clustered)" "School FE + Covariates") ///
	main(b) se parentheses nonumbers ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Adding Covariates to the Regression")


**************************************************************
*                           3.h
**************************************************************

gen sck_boy      = sck * boy
gen sck_freelunk = sck * freelunk
gen sck_totexpk  = sck * totexpk

eststo q3h: areg tscorek ///
	sck sck_boy sck_freelunk sck_totexpk ///
	boy freelunk totexpk, ///
	absorb(schidkn) vce(cluster schidkn)

display "sck (baseline group): "             _b[sck]
display "sck_boy (differential for boys): "  _b[sck_boy]
display "sck_freelunk (diff for free lunch): " _b[sck_freelunk]
display "sck_totexpk (per yr experience): "  _b[sck_totexpk]

esttab q3h using "tables_tex/q3_interactions.tex", replace tex ///
	mtitles("Heterogeneous Effects") ///
	main(b) se parentheses nonumbers ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Treatment Effect Heterogeneity")

esttab q3h using "tables_html/q3_interactions.html", replace htm ///
	mtitles("Heterogeneous Effects") ///
	main(b) se parentheses nonumbers ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Treatment Effect Heterogeneity")


**************************************************************
*                           3.i
**************************************************************

test sck_boy sck_freelunk sck_totexpk

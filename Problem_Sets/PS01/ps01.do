					** PROBLEM SET 01 **
					* ---------------- *

cd "C:\Users\T14\7Programming\STATA\02Classes\01EconometricsII\Problem_Sets\PS01"
use "soep_lebensz_en.dta", clear
br

*** Q1. ***
* (a) Construct a binary variable has kids indicating if a person has any children at time t.
sort id year

gen has_kids = 0
replace has_kids = 1 if no_kids > 0 & no_kids !=.

xtset id year


* (b) Regress the standardized life satisfaction variable, satisf std, on this indicator. Include gender, education, categorical health, and year dummies. Cluster standard errors at the individual level.
tab year, gen(yr_) // create dummy vars manually since issues with i.year on hausman
regress satisf_std sex education health_org i.year has_kids, vce(cluster id)
estimates store pols

* (c) Compare the effect using Pooled OLS and Fixed Effects. What does the difference tell you about the unobserved effect fi and its covariance with has kids?
xtreg satisf_std sex education health_org i.year has_kids, fe vce(cluster id)
estimates store fixed1


esttab pols fixed1, ///
	b(3) se(2) aic(0) label ///
	title("Comparing Pooled OLS and Fixed Effects") ///
	mtitles("Pooled OLS" "Fixed Effects") ///
	nonumbers 
	
** The POLS estimator tends to overestimate the variance (therefore the SEs) -- since 
	
*** Q2. *** 
* (a) Why has the coefficient on gender disappeared in the fixed effects regression? 
** the gender variable is a time-invariant regressor: the fixed effects estimator 'demeans' our variables in the model. Since sex is time-invariant, the sample average of Z_i wrt t is equal to Z_i; thus, our 'demeaned' Z_i would be (Z_i - Z_i)'gamma, which is equal to 0.


* (b) Run the fixed effects regression again, interacting the gender indicator with the children indicator.
gen gender_kids_interaction = sex*has_kids

xtreg satisf_std sex education health_org yr_2 yr_3 yr_4 yr_5 has_kids gender_kids_interaction, fe vce(cluster id)

estimates store fixed

* (c) Are women and men affected differentially? How do you interpret the magnitudes of the estimated coefficients?
**


*** Q3. ***
* (a) Test the effect in a random effects model. 
xtreg satisf_std sex education health_org yr_2 yr_3 yr_4 yr_5 has_kids gender_kids_interaction, re vce(cluster id)

estimates store random

* (b) Does the has_kids coefficient differ from the fixed effects model?



* (c) What can you infer from this regarding the trust you place in RE assumptions? Why/Why not?



*** Q4. *** 
* (a) Perform a formal Hausman test to compare the fixed effects and random effects models. 

xtreg satisf_std sex education health_org yr_2 yr_3 yr_4 yr_5 has_kids gender_kids_interaction, re vce(cluster id)
xtoverid

hausman random fixed, force

* (b) Do you reject the null hypothesis, and what does this result tell you?
** Yes, with a p-value = 0.00, we reject at all standard significance levels. This result tells us that the H_0: RE is consistent is not true given our data structure.



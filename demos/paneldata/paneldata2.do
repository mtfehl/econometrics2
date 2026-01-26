/* This file runs smoothly on a Stata 14 version. */
/* This dofile shows a number of command lines used in panel data analysis. The corresponding data set demo.dta can be found on the course website. */
/* For the ivreg2 commands, you must have installed the latest update for ivreg2. If you do not have that, go to "Help" in Stata, "Search", type ivreg2, and install the latest package. */ 
clear
set matsize 4000

use demo.dta, clear

xtset id year		/* 596 workers over on average 4.7 years */

/* no unobserved effect & homoskedastic */
reg lnwage i.sex i.nation i.education age age2 i.year

/* no unobserved effect & heteroskedastic */
reg lnwage i.sex i.nation i.education age age2 i.year, robust

/* arbitrary heteroskedasticity and serial correlation */
reg lnwage i.sex i.nation i.education age age2 i.year, vce(cluster id)


/* unobserved random effect */
xtreg lnwage i.sex i.nation i.education age age2 i.year, re

/* unobserved random effect - clustered standard errors */
xtreg lnwage i.sex i.nation i.education age age2 i.year, re vce(cluster id)



/* unobserved fixed effect */
xtreg lnwage i.sex i.nation i.education age2 i.year, fe
/* age should drop out here */

set more on
reg lnwage i.nation i.education age2 i.year i.id
/* Watch out here - if you leave i.sex in the specification it is estimated but then one of the id dummies is dropped */
reg lnwage i.sex i.nation i.education age2 i.year i.id



/* unobserved fixed effect - clustered standard errors */
xtreg lnwage i.sex i.nation i.education age2 i.year, fe vce(cluster id)

xtreg lnwage i.sex i.nation i.education age2 i.year, fe robust
/* Current Stata versions (starting with version 10) automatically allow for clustering on id level once you specify the robust option. In older versions, the robust option only allows for heteroskedasticity but not serial correlation. */





/* Dynamic models */

/* Pooled OLS: typically inconsistent and upward biased because f_i in error term and positively correlated with lnwage in all periods.. */
reg lnwage l1.lnwage i.sex i.nation i.education age age2 i.year

/* Fixed Effects: inconsistent and downward biased because dynamic models violate by construction the strict exogeneity condition */
xtreg lnwage l1.lnwage i.sex i.nation i.education age2 i.year, fe

xi i.education i.year
foreach i in 2 3 4 5 6 7 9 {
gen diffedu`i'=d._Ieducation_`i'
}
foreach i in 2016 2017 2018 2019 2020 2021 {
gen diffyear`i'=d._Iyear_`i'
}


/* Anderson/Hsiao Estimator */
* ssc install avar
* ssc install weakivtest

/* Here I use 2SLS, instrumenting with the lag2 of lnwage. Below the robust version. Note that 2SLS (in the way specified here) does not use the GMM structure for the Z matrix. Here, 2 instruments mean 2 columns, whereas in the GMM each available instrument in each time period gets an extra column. */


/* Should always cluster in first differences model! Warning message due to the singleton dummy in diffedu9. One could partial this out but then one would have to rename all the variables with time operators. */
ivreg2 d.lnwage (l1d.lnwage=l2.lnwage) d.nation diffedu* d.age2 i.year, first cluster(id) 

/* Here I drop diffedu9 because it causes problems in the clustered version of the command. In principle, should be partialled out; see below. */
ivreg2 d.lnwage (l1d.lnwage=l2.lnwage) d.nation diffedu2-diffedu7 d.age2 i.year, first cluster(id)


/* Arellano Bond uses the lagged levels of the dependent variables as instruments in a GMM framework. Therefore estimates different than those from the previous ivreg2 which used 2SLS. */

/* One-step GMM */
xi: xtabond lnwage i.nation i.education age2 i.year, lags(1) maxldep(1)
xi: xtabond lnwage i.nation i.education age2 i.year, lags(1) maxldep(1) robust
xi: xtabond lnwage i.nation i.education age2 i.year, lags(1) maxldep(2) robust
estat abond

/* Two-step GMM */
xi: xtabond lnwage i.nation i.education age2 i.year, lags(1) maxldep(1) twostep robust


xi: xtabond lnwage i.nation i.education age2 i.year, lags(1) robust
xi: xtabond lnwage i.nation i.education age2 i.year, lags(1) twostep robust





/* Hausman test */

xtreg lnwage i.sex i.nation i.education age age2 i.year, fe vce(conventional)
estimates store fe
xtreg lnwage i.sex i.nation i.education age age2 i.year, re vce(conventional)
estimates store re
hausman fe re

/* reject the hypothesis that there are no fixed effects! */

/*
/* Seemingly unrelated estimation to allow for robust and clustered covariance matrices for a generalized hausman test */
suest fe re, vce(cluster id)
*/



/* Sargan Test Statistic - check at the bottom of the output. */

ivreg2 d.lnwage (l1d.lnwage=l2.lnwage l3.lnwage l4.lnwage) d.nation diffedu* d.age d.age2 i.year
/* Instruments appear to be valid - but again, you should cluster. */

ivreg2 d.lnwage (l1d.lnwage=l2.lnwage l3.lnwage l4.lnwage) d.nation diffedu* d.age d.age2 i.year, cluster(id)
/* No overidentification test reported - problem with single dummy variable. So I rename my variables after which I can partial the problematic diffedu9 variable out. */
gen l1dlnwage=l1d.lnwage
gen l2lnwage=l2.lnwage
gen l3lnwage=l3.lnwage
gen l4lnwage=l4.lnwage
gen dnation=d.nation
gen dage=d.age
gen dage2=d.age2
gen dlnwage=d.lnwage

/* Two stage least squares */
ivreg2 dlnwage (l1dlnwage=l2lnwage l3lnwage l4lnwage) dnation diffedu* dage dage2 i.year, partial(diffedu9) cluster(id)
/* Now it correctly reports the cluster-robust Hansen J statistic (instead of the Sargan statistic). */ 

/* GMM */
ivreg2 dlnwage (l1dlnwage=l2lnwage l3lnwage l4lnwage) dnation diffedu* dage dage2 i.year, partial(diffedu9) cluster(id) gmm2s

/* Testing the validity of a subset of instruments */
ivreg2 dlnwage (l1dlnwage=l2lnwage l3lnwage l4lnwage) dnation diffedu* dage dage2 i.year, partial(diffedu9) cluster(id) gmm2s orthog(l3lnwage)

/* Also note the reported weak identification tests at the bottom of the output with the Stock-Yogo weak identification critical values for the F statistic. */



/* Testing for weak instruments */

/* Here I drop diffedu9 because it causes problems in the clustered version of the command. In principle, should be partialled out; see below. */
ivreg2 d.lnwage (l1d.lnwage=l2.lnwage) d.nation diffedu2-diffedu7 d.age2 i.year, first cluster (id)

/* Obtaining correct critical values for testing for weak instruments, accounting for the clustering of the standard errors (Montiel Olea and Pflueger, 2013). */
weakivtest
/* Also note the Anderson-Rubin Wald test for weak-instrument-robust inference. */



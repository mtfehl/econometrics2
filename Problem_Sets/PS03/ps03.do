// Problem Set 3: Multinomal Models //
pwd
cd "C:\Users\T14\7Programming\STATA\02Classes\01EconometricsII\Problem_Sets\PS03\"
use "soep_lebensz_en.dta", clear

* note that our data is panel; to apply MLE, we keep only the first obs for each ind

sort id year
by id: gen obs_no = _n 
keep if obs_no == 1 // only keep first obs for each ind

// question 1
 
mlogit no_kids education

estimate store mlogit1

esttab mlogit1 using "mlogit1.tex", replace tex ///
	main(b) se parentheses nonumbers ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Multinomal Logit Regression on Education")

	
// question 2

* built-in method
quietly mlogit no_kids education, baseoutcome(0)
margins, dydx(education) post

esttab using "mlogit_ame.tex", replace tex ///
main(b) se parentheses star(* 0.10 ** 0.05 *** 0.01) ///
	rename (1._predict 0_kids 2._predict 1_kid 3._predict 2_kids 4._predict 3_kids) ///
    title("Average Marginal Effects of Education on No. of Kids") ///
	nonumbers

	
* manual method
quietly mlogit no_kids education, baseoutcome(0)
predict p1 p2 p3 p4
* compare to our sample probalities; really close
summarize p1 p2 p3 p4
tab no_kids

// avg marginal effect: what happens when we incremenent by a small amnt
replace education = education + 0.0001
predict p1new p2new p3new p4new
gen dp1dy = 10000*(p1new - p1)
gen dp2dy = 10000*(p2new - p2)
gen dp3dy = 10000*(p3new - p3)
gen dp4dy = 10000*(p4new - p4)

sum dp1dy dp2dy dp3dy dp4dy 


// question 3

oprobit no_kids education
estimate store oprobit1

esttab oprobit1 using "oprobit.tex", replace tex ///
    mtitles("Ordered Probit") ///
	main(b) se parentheses nonumbers ///
	star(* 0.10 ** 0.05 *** 0.01) 

// avg marginal effects across all educ levels
margins, dydx(*)

// avg marginal effects for individuals with educ=12
margins, dydx(education) at(education=12) post
estimate store oprobit_ame_12

esttab oprobit_ame_12 using "oprobit_ame.tex", replace tex ///
main(b) se parentheses star(* 0.10 ** 0.05 *** 0.01) ///
rename (1._predict 0_kids 2._predict 1_kid 3._predict 2_kids 4._predict 3_kids) ///
    title("Average Marginal Effects of Education at 12 Years") ///
	nonumbers

// to visualize:
marginsplot, horizontal recast(dot)
	
	
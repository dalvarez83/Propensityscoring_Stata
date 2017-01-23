/***U8220: PROGRAM EVALUATION***/
/***ASSIGNMENT 3***/
/***PROPENSITY SCORING***/

set mem 60m
use "C:\Documents and Settings\Daniel\My Documents\Danny's documents\SIPA\Courses\SemesterIV\Program Evaluation and Policy Research\Propensity_scoring\nlsy.hw3.dta", clear
describe

/* examine the benchmark treatment effect */
regress piatm56 first


*****
/*Question 2 */
/*examine the distribution of pre-treatment covariates across the two groups */
bysort first: sum b_marr lths hs ltcoll college momed pr0 lnfinc_a0 momage afqt brthwt brorddum preterm rmomwk 

/*generate variable for afqt if afqt >0 */
gen afqt2 = afqt if afqt > 0
drop afqt
gen afqt = afqt2
drop afqt2
*tab afqt

/*create squared terms for variables that vary with age*/
gen momagesq = momage*momage

/*retain variables that show most marked differences across the two groups*/
/*run psmatch2*/
psmatch2 first b_marr lths hs ltcoll college momed pr0 lnfinc_a0 momage momagesq afqt brthwt brorddum preterm rmomwk, comm 

/*run pstest and check for balance*/
pstest b_marr lths hs ltcoll college momed pr0 lnfinc_a0 momage momagesq afqt brthwt brorddum preterm rmomwk 

/*RETRY 1 psmatch2 and pstest for specification*/
psmatch2 first b_marr hs ltcoll momed pr0 momage brorddum, comm 
pstest b_marr hs ltcoll momed pr0 momage brorddum 

/*RETRY 2 psmatch2 and pstest for specification*/
/*drop b_marr and hs*/
psmatch2 first ltcoll momed pr0 momage brorddum
pstest ltcoll momed pr0 momage brorddum 

*****
/*Question 3*/
sum _pscore

/*histogram of propensity scores for each treatment group prior to matching*/
psgraph, title(Propensity scores prior to matching)

/*determine areas of incomplete overlap*/
sum _pscore if first==1
sum _pscore if first==0
count if _pscore>.9151596

/*histogram of comparing densities across treatment and control groups on the unmatched sample*/
twoway (histogram _pscore if first==1, fcolor(none) lcolor(red) lwidth(thin)) (histogram _pscore if first==0, fcolor(none) lcolor(teal) lwidth(thin))

/*histogram of comparing densities across treatment and control groups on the matched sample*/
twoway (histogram _pscore if first==0 & _weight~=., frequency fcolor(none) lcolor(teal) lwidth(thin) lpattern(solid)) (histogram _pscore if first==1 & _weight~=., frequency fcolor(none) lcolor(red) lwidth(vthin) lpattern(solid))

*****
/*Question 4*/
/*part a*/
/*determine the treatment effect estimate using the difference in means for the outcome variable across the matched groups*/
/*use bootstrapped standard errors for the estimate*/
/*set seed to reproduce results in future*/ 
set seed 12345

bs "psmatch2 first ltcoll momed pr0 momage brorddum, out(piatm56)" "r(att)" 
bs "psmatch2 first ltcoll momed pr0 momage brorddum, out(piatm56)" "r(att)" , reps(1000)
/*part b*/
/*estimate the regression-adjusted treatment effect estimate*/
/*first calculate the weight for the number of times each observation was used in the match*/
tab _weight
/*now determine the regression-adjusted propensity matched estimates*/
regress piatm56 first momrace female b_marr ltcoll momed pr0 lnfinc_a0 momage momagesq afqt brthwt brorddum preterm rmomwk [pw = _weight]

/*part c*/
/*histogram/response surface plot*/
twoway (lowess piatm56 _pscore if first==0) (lowess piatm56 _pscore if first==1, lcolor(red)) (histogram _pscore if first==0, lcolor(blue) fcolor(none)) (histogram _pscore if first==1, lcolor(red) fcolor(none))
/*response surface plot with scatter*/
twoway (lowess piatm56 _pscore if first==0, clpattern(solid)) || (lowess piatm56 _pscore if first==1, clpattern(dash) legend(label (1 "Lowess Control") label (2 "Lowess Treated")))

****
/*Question 5*/
psmatch2 notfirst b_marr lths hs ltcoll college momed pr0 lnfinc_a0 momage afqt brthwt brorddum preterm rmomwk , comm 
pstest b_marr lths hs ltcoll college momed pr0 lnfinc_a0 momage afqt brthwt brorddum preterm rmomwk 

****
/*Question 6*/
/*kernel matching*/
psmatch2 first b_marr lths hs ltcoll college momed pr0 lnfinc_a0 momage momagesq afqt brthwt brorddum preterm rmomwk, kernel out(piatm56)comm 
regress piatm56 first b_marr lths hs ltcoll college momed pr0 lnfinc_a0 momage momagesq afqt brthwt brorddum preterm rmomwk [pw= _weight]

****
/*Question 7*/
/*standard linear regression*/
regress piatm56 first b_marr lths hs ltcoll college momed pr0 lnfinc_a0 momage momagesq afqt brthwt brorddum preterm rmomwk

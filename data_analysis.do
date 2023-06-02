cap log close
clear
set more off


/*
Will Elliott, Michael Harrison, Chukwuka Odigbo, Carl Ufongene

Data analysis: did the Medicaid expansion drive more voter turnout? A triple DiD using times states adopted Medicaid

*/

cd "/Users/chukwuka/Desktop/Stata/Econ38Stata"
log using "data_analysis.log", replace
use "newest.dta"

save newest_copy.dta, replace

** To generate state level averages for different medicaid characteristics. **
clear
use newest_copy.dta

* dummy for having medicaid in the past year
generate hadmcaidly = 0
replace hadmcaidly = 1 if himcaidly == 2

* dummies for race
gen statewgts = 1
generate white = (race == 100)
generate black = (race == 200)
generate otherrace = (race > 200)
generate mcaidelig = (nchild==0 & educ<80 & 18<=age<=65)

* dummies for education status
generate hseduc = (educ==73)
generate colleduc = (educ==111|123<=educ & educ<125)
generate somecoll = (81<=educ & educ<=110|120<=educ & educ <=122)

* state averages weighted by observations
collapse (mean) hadmcaidly white black otherrace hseduc colleduc somecoll age mcaidelig (rawsum) nobs=statewgts [pw=asecwt], by (statefip year)
save medicaid, replace

** To generate the state level voter statistics using the voter dataset. **
clear
use newest_copy.dta
drop if age<18
drop if voted==.
gen votestatewgts = 1
gen didvote = 0
replace didvote = 1 if voted==2
generate mcaideligvote = (nchild==0 & educ<80 & 18<=age<=65)
regress didvote mcaideligvote
collapse (mean) didvote age mcaideligvote (rawsum) nobs2=votestatewgts, by (statefip year)
save voting, replace

** Merging both datasets **
merge 1:1 statefip year using medicaid
drop _merge
save first_voting_medicaid, replace


** Core data analysis **
use first_voting_medicaid

* Not interested in non-voting years
// drop if inlist(year, 2013, 2014, 2015, 2017, 2019)
save first_voting_medicaid, replace

use first_voting_medicaid

* Isolating states that adopted medicaid in 2014
egen group = group(statefip)
su group, meanonly
generate in2014 = 0
replace in2014=1 if (group >= 3 & group <= 9|group==12|group==14|group==16|group==18|group>=21&group<=24|29<=group&group<=33|group == 35|group == 36|group==38|group==40|group==46|group==48| group==49)

* Generating relevant variables
generate elec2016 = (year==2016)
generate mcaid_state2014 = hadmcaidly*in2014
generate mcaid_elec2016 = hadmcaidly*elec2016
generate elec2016_in2014 = elec2016*in2014
generate mcaid_in2014_elec2016 = hadmcaidly*in2014*elec2016

save working_copy_data, replace

use working_copy_data


*** Table of means to show a parallel trends assumption in states that adopted medicaid and in states that did not adopt medicaid in 2012 and in 2016 ***
local howout = "replace"
forval i = 2012(4)2016 {
	forval j = 0/1  {
	outsum didvote age hadmcaidly mcaidelig white black otherrace hseduc somecoll colleduc using tabMeansState.csv if year==`i' & in2014==`j', comma `howout' bracket ctitle (year==`i' & states2014=`j')
	local howout = "append"
	}
}


*** Balance test to test whether any other variables of interest affect our outcome variable, didvote ***
local howout = "replace"
foreach var in age white black otherrace hseduc somecoll colleduc {
	reg `var' hadmcaidly, robust
	testparm hadmcaidly
	outreg2 using initialbaltest, `howout' excel ctitle(`var') addstat (p-value, `r(p)')
	local howout = "append"
}


*** Key regressions ***
regress hadmcaidly year in2014 age white otherrace hseduc somecoll colleduc [aw=nobs], robust
outreg2 using testing, replace excel

* OLS regression
regress didvote elec2016 in2014 elec2016_in2014 age white otherrace hseduc somecoll colleduc [aw=nobs], robust
outreg2 using tableResults, ctitle(OLS, voter turnout) replace excel

* First stage regression
regress hadmcaidly elec2016 in2014 elec2016_in2014 age white otherrace hseduc somecoll colleduc [aw=nobs], robust
outreg2 using testing, replace excel
outreg2 using tableResults, ctitle(First Stage, had medicaid last year)append

* Reduced form regression
regress didvote hadmcaidly elec2016 in2014 age white otherrace hseduc somecoll colleduc[aw=nobs], robust
outreg2 using tableResults, ctitle(Reduced form, voter turnout) append

* IV Reg/2SLS
ivreg didvote (hadmcaidly=elec2016_in2014) elec2016 in2014 age white otherrace hseduc somecoll colleduc [aw=nobs], robust
outreg2 using tableResults, ctitle (2SLS, voter turnout) append


*** Key regressions using voter turnout of only people who were eligible for the medicaid expansion ***
* OLS regression
regress mcaideligvote elec2016 in2014 elec2016_in2014 age white otherrace hseduc somecoll colleduc [aw=nobs], robust
outreg2 using tableResults2, ctitle(OLS, voter turnout) replace excel

* First stage regression
regress hadmcaidly elec2016 in2014 elec2016_in2014 age white otherrace hseduc somecoll colleduc [aw=nobs], robust
outreg2 using tableResults2, ctitle(First Stage, had medicaid last year)append

* Reduced form regression
regress mcaideligvote hadmcaidly elec2016 in2014 age white otherrace hseduc somecoll colleduc[aw=nobs], robust
outreg2 using tableResults2, ctitle(Reduced form, voter turnout) append

* IV Reg/2SLS
ivreg mcaideligvote (hadmcaidly=elec2016_in2014) elec2016 in2014 age white otherrace hseduc somecoll colleduc [aw=nobs], robust
outreg2 using tableResults2, ctitle (2SLS, voter turnout) append










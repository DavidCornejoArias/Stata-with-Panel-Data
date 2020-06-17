* RA test
cd "C:\Users\david\Dropbox\work opportunities\peruFirstOpportunity\RA David Cornejo\for_candidates"
log using "raTest.smcl", replace
* Importing the dataset
import delimited using "BERD_input.csv", clear 
preserve
*Cleaning the dataset
drop if regexm(indu_main_act, "^[A-Z][0-9][0-9][0-9]$")
drop if regexm(indu_main_act, "^M[0-9]+")
drop if regexm(indu_main_act, "^[A-Z][A-Z][A-Z]$")
drop if indu_main_act == "L"
drop if indu_main_act =="C25T30"
drop if indu_main_act =="GTN"
drop if indu_main_act =="J62"
drop if indu_main_act =="C"
drop if indu_main_act =="_T"
drop if indu_main_act =="_T"
export delimited using "resultsTotal", replace
save "step5.dta", replace
* Just keeping R&D given by bussines or goverments or dollars
keep if measure == "DF6"
keep if sectfund == "BES" | sectfund == "GOV"
preserve
* Keep only the useful variables
keep ïcountry unitcode country indu_main_act industrymainactivity sectfund year value
save "step9.dta", replace
* Leaving one with only bes values
keep if sectfund == "BES"
rename value busrd
save "busrd.dta", replace
* Recalling the one we left behing
use "step9.dta", replace
keep if sectfund == "GOV"
rename value govrd
* Joining the datasets
merge 1:1 country indu_main_act year using "other.dta"
save "combinationTotal.dta"
* Renaming variables for the dataset
rename indu_main_act industrycode
rename industrycode indcode
gen industry2 = substr(industry,1,30)
drop industry
rename industry2 industry
rename ïcountry cocode
keep cocode country indcode year govrd busrd industry
save "results.dta"
* Creating highgov and govrd
mean govrd
gen highgov = 1 if govrd >60.25
replace highgov = 0 if govrd <=60.25
* Creating log and identification variables
gen loggovrd = log(govrd)
gen logbusrd = log(busrd)
gen coAndInd = cocode + indcode
egen coAndIndNum = group( coAndInd )
*Package for installing estout
*ssc install estout, replace
*Regressions
eststo: regress logbusrd loggovrd
* adding one year log
eststo:xtreg logbusrd L.loggovrd loggovrd
* Adding year fix effects
eststo:xtreg logbusrd L.loggovrd i.year, fe
* adding more than individual and time effects
xtset coAndIndNumber year, yearly
eststo:xtreg logbusrd loggovrd, fe
esttab
* Different variables
eststo clear
gen difLogBusrd = d.logbusrd
gen difLoggovrd = d.loggovrd
gen difYear = d.year
* Regress
eststo: regress difLogBusrd difLoggovrd
* adding one year log
eststo:xtreg difLogBusrd L.difLoggovrd difLoggovrd
* Adding year fix effects
eststo:xtreg difLogBusrd L.difLoggovrd i.year, fe
* adding more than individual and time effects
xtset coAndIndNumber year, yearly
eststo:xtreg logbusrd loggovrd, fe
esttab
save "resultsLaterOnTheTest.dta", replace
* sum govrd by country and Industry
statsby sum=r(sum) n=r(N), by(coAndIndNumber) subsets nodots: summarize govrd
* Generating the average
gen average = sum / n
save "other.dta", replace
* Reading the other dta
use "C:\Users\david\Desktop\Stata14\resultsLaterOnTheTest.dta", replace
* Adding an average
merge m:1 coAndIndNumber using "other.dta"
save "secondMerge.dta", replace
use "secondMerge.dta", replace
*adding an IV variable
gen IV = average* difLoggovrd
* Regressions with an IV variable
eststo clear
xtset coAndIndNumber year, yearly
eststo: xtivreg difLogBusrd (difLoggovrd = IV), fe
eststo:xtivreg difLogBusrd (difLoggovrd = IV), fe
esttab

*


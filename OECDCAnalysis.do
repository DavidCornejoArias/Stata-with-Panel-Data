/* packages to install
ssc install ivreg2
ssc install ranktest
*/
* Set the direction in where you will have the files
cd "workpath"
/*
CLEANING
*/
clear all
log using "testPractice.scml", append
import delimited "BERD_input.csv"
*cleaning according to instructions
tab indu_main_act
tab sectfund
drop if indu_main_act=="_T"
drop if indu_main_act=="C303"|indu_main_act=="J582"|indu_main_act=="J631"
drop if indu_main_act=="M71"|indu_main_act=="M72"
drop if indu_main_act=="GTN"|indu_main_act=="C25730"
* Selecing only the ones founded by government or private sector
drop if sectfund == "HES"
drop if sectfund == "PNP"|sectfund == "ROW"|sectfund == "_T"
* Selecting just the ones in constant prices and purchasing power parity
drop if measure != "DC6"
* describing the data
describe year
sum year
* Selecing all the variables to use
keep ïcountry country indu_main_act industrymainactivity sectfund sourceoffunds year value
drop sourceoffunds
* Transforming from long form to wide form
reshape wide value, i(ïcountry country indu_main_act year) j(sectfund) string
* Renaming the variables according to the instructions
rename ïcountry isocode
rename indu_main_act indcode
rename industrymainactivity  industry
rename valueGOV govrd
rename valueBES busrd
/*
DESCRIPTIVE ANALYSIS
*/
* Creating a variable if gov founding is greater than the average
gen highgov = 1 if govrd < 54.99
 replace highgov = 1 if govrd > 54.99
replace highgov = 0 if govrd < 54.99
* Tabulating founging
tab highgov
* Showing the mean business funding for the high government founding and low government founding sectors
mean busrd, over(highgov)
* Preparing for statitical analysis
* Generating the log value of business and government
gen logbusrd = log( busrd )
gen loggovrd = log( govrd )
gen yearlaggedloggovrd = loggovrd[_n -1]
gen coAndID = isocode+ indcode
tostring year, generate(yearString)
gen yearStringAndCountry = yearString+country
egen countryYear = group(yearStringAndCountry)
egen coAndIdNum = group( coAndID )
/*
MODELING
*/
eststo clear
eststo: quietly regress logbusrd loggovrd
estadd local yearFE "No"
estadd local countryAndIndustryFE "No"
xtset coAndIdNum year, yearly
gen laggedLogGovrd = L.loggovrd
label variable laggedLogGovrd "GOV R&D t-1"
label variable loggovrd "GOV R&D"
label variable logbusrd "BUS R&D"
eststo:quietly regress logbusrd laggedLogGovrd
estadd local yearFE "No"
estadd local countryAndIndustryFE "No"
* Adding year fix effects
eststo:quietly reg logbusrd laggedLogGovrd i.year
estadd local yearFE "Yes"
estadd local countryAndIndustryFE "No"
* Adding country-industry fix effects
eststo:quietly xtreg logbusrd laggedLogGovrd i.coAndIdNum i.year, fe
estadd local yearFE "Yes"
estadd local countryAndIndustryFE "Yes"
esttab, se ar2 keep(laggedLogGovrd loggovrd _cons) scalars("yearFE yearFixedEffects" "countryAndIndustryFE countryAndIndustryFE") title("Table 1:Fixed effects") label
* Replicating last table with robust standar errors at year country level
gen yearCountryStringLevel = countryYear
eststo clear
eststo: quietly regress logbusrd loggovrd, vce(robust)
estadd local yearFE "No"
estadd local countryAndIndustryFE "No"
xtset coAndIdNum year, yearly
eststo:quietly regress logbusrd laggedLogGovrd, vce(robust)
estadd local yearFE "No"
estadd local countryAndIndustryFE "No"
* Adding year fix effects
eststo:quietly reg logbusrd laggedLogGovrd i.year, vce(robust)
estadd local yearFE "Yes"
estadd local countryAndIndustryFE "No"
* Adding country and industry fixed effects
eststo:quietly xtreg logbusrd laggedLogGovrd i.coAndIdNum i.year, fe vce(robust)
estadd local yearFE "Yes"
estadd local countryAndIndustryFE "Yes"
esttab, se ar2 keep(laggedLogGovrd loggovrd _cons) scalars("yearFE yearFixedEffects" "countryAndIndustryFE countryAndIndustryFE") title("Table 2:Fixed effects robust STD ERROR") label
* Running regression analysis with differences
eststo clear
xtset coAndIdNum year, yearly
gen difLoggovrd = D.loggovrd
label variable difLoggovrd "Dif GOV R&D"
gen difLogBusrd = D.logbusrd
label variable difLogBusrd "Dif BUS R&D"
eststo:quietly regress difLogBusrd difLoggovrd, vce(robust)
estadd local yearFE "No"
* Adding year fix effects
eststo:quietly reg difLogBusrd difLoggovrd i.year, vce(robust)
estadd local yearFE "Yes"
esttab, se ar2 keep(difLoggovrd _cons) scalars("yearFE yearFixedEffects") title("Table 3:Fixed effects with Differences") label
* Running analysis with instrumental variable
* Creating instrumental variable: 
*iv = indshareic * ∆ln(govrdct)
*where indshareic denotes the average share of government funded R&D that
*each industry gets in a country across all years; and govrdct denotes the total
*government funded R&D that is spent in a given country and year
sort indcode country
by indcode country: egen avgCountryTotal = mean(govrd)
sort country year
by country year: egen sumCountryTotal = total(govrd)
gen logSumCountryTotal = log( sumCountryTotal )
egen countryLevel = group(isocode)
sort country year
gen covernmentLogCountryOnYearLess = logSumCountryTotal[_n-1] if country[_n]==country[_n-1] & year[_n]!=year[_n-1]
by country year: egen sumCountryTotalLogYearLess = total(covernmentLogCountryOnYearLess)
gen difLogGovrdIV = logSumCountryTotal - sumCountryTotalLogYearLess if sumCountryTotalLogYearLess !=0
gen iv = avgCountryTotal * difLogGovrdIV
* Running the differences regressions with the instrumental variables
xtset coAndIdNum year, yearly
eststo: quietly ivreg2 difLogBusrd (difLoggovrd = iv), robust
estadd local yearFE "No"
eststo: quietly ivreg2 difLogBusrd (difLoggovrd = iv), robust
estadd local yearFE "Yes"
esttab, se ar2 keep(difLoggovrd _cons) scalars(idp "yearFE yearFixedEffects") title("Table 4:Fixed effects with IV") label
log close
/* Note: I made this excercise as a task for a research assistant position
*/

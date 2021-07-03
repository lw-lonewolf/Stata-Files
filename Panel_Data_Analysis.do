**** Panel Data Analysis 

cd "~/Desktop/stata/STATA"

***** Panel Data Structure 
use Data\Euro_Banks.dta, clear 
encode name, gen(bank)
order bank
cap xtset bank year	
cap drop rmlr 
*cap drop --> drops if rmlr exists and if it does not exist then command is skipped
*** Addressing the issue of repeated time series 
duplicates report bank year
duplicates list bank year
duplicates list roa size nim bank year
duplicates tag bank year, gen(isdup)
order isdup
tabulate isdup
drop if isdup != 0 
gen fcdummy = 0
replace fcdummy = 1 if year > 2006 & year < 2009


gen sdc = 0 
replace sdc =1 if year == 2009
gen crd = 0
replace crd = 1 if year > 2010
xtset bank year 
gen fcsize = fcdummy * size
order  fcdummy fcsize size year
order country
drop if missing(size) 
outsheet using Data\Euro_Banks_Panel.csv, comma replace 
save Data\Data_Panel.dta, replace 

keep name bank country year rwata roa roe nim size liq loanhhi lr 
save Data\Data_Panel_Reduced.dta, replace 

save, replace 
sort country bank
keep if country == 1
drop in 90/2852
xtset bank year 
xtline rwata
egen bankid = group(bank)
xtset bankid year
xtline rwata

xtline rwata, overlay 

bysort bank: egen mean_size = mean(size)
twoway scatter size bank
twoway scatter mean_size bank
twoway scatter size bankid, msymbol(circle_hollow) || connected mean_size bankid, msymbol(diamond)
bysort year: egen mean_size2 = mean(size)
twoway scatter size year, msymbol(circle_hollow) || connected mean_size2 year, msymbol(diamond)
** OLS is not suitable for panel data. Why? let's run the ols to see this. 
xtset bank year 
reg nim size 
twoway scatter (nim size), mlabel(bankid) || lfit nim size 

*** LSDV Model 
*** ---> As coefficient is 1.31 ( positive) and t is greater than 2.0 therefore  impact is positive and significant.
reg nim l.size 
* ---> controls bank effect ((It will change i.e. impact will be negative now))
reg nim l.size i.bankid 
** ---> Black line is OLS line while other lines are individual bank lines 
predict yhat
separate size, by(bankid)
separate yhat, by(bankid)

twoway (scatter nim l.size) (lfit nim l.size)

twoway connected yhat1-yhat11 l.size, mlabel(bankid) //////
       msymbol(none diamond_hollow triangle_hollow ///
	   square_hollow + circle_hollow x square circle) ///
	   msize(medium) || lfit nim l.size, ///
	   clwidth(thick) clcolor(black)

xtset bank year 

reg nim l.size
estimates store ols
reg nim l.size i.bank
estimates store lsdv
xtreg nim l.size, fe
estimates store fixed 

estimates table ols lsdv fixed, star stats(N)

areg nim l.size, absorb(bank)
estimates store aregg
estimates table ols lsdv fixed aregg, star  stats(N r2 r2_a)

*** Caution about Fixed Effect: Do not include time invariant dummies like gender etc.
** if you beleive that differences among the entities have an effect on the DV, use REM. You may include time invariant dummies as well in the random effect model. 
xtreg nim l.size, re

estimates store random 
estimates table ols lsdv fixed aregg random, star  stats(N r2 r2_a)

xtreg nim l.size, fe 
estimates store fixed 
*** Hausman Test 
hausman fixed random 
save, replace 

*** using full data
use Data\Data_Panel.dta, clear 
xtset bank year 
global xlist liq roa lr size 
eststo: xtreg rwata $xlist, fe
eststo: xtreg rwata $xlist fcdummy, fe
eststo: xtreg rwata $xlist fcdummy sdc, fe
eststo: xtreg rwata $xlist i.year, fe
eststo: xtreg rwata $xlist i.year i.country, fe
esttab using Output/results_fixed.csv, se stats(r2_w) replace 
eststo clear 
est clear 
**** Comparison of Random and Fixed 
eststo: xtreg rwata $xlist, fe
eststo: xtreg rwata $xlist, re
eststo: xtreg rwata $xlist i.year, fe
eststo: xtreg rwata $xlist i.year, re
esttab using Output/results_fixed_random.csv, se stats(r2_w) replace 
eststo clear

xtreg rwata $xlist, fe
estimates store fixed
xtreg rwata $xlist, re
estimates store random 
hausman fixed random 
save, replace 

**** Panel Data Diagnostics 
xtreg rwata $xlist i.year, fe 
*** testing for time effect 
testparm i.year

**** testing for random effect
xtreg roa $xlist, re 
**** Random vs OLS 
*** H0: OLS is better 
xttest0

**** Correlation across entities 
xtreg rwata $xlist, fe 
xtcd2 rwata
** If correlation accross entities found use Driscoll-Kroy Standard Errors 
xtreg rwata $xlist, fe vce(cluster bank)
xtscc rwata $xlist, fe 

*** Checking for heteroscedasticity 
xtreg rwata $xlist, fe 
xttest3
xtreg rwata $xlist, fe robust 

*** auto correlation in panel 
xtserial rwata $xlist 
*** if auto correlation found use Auto Regression Panel Model 
xtregar rwata $xlist, fe

*** if auto correlation and heteroscedasticity both found 
xtreg rwata $xlist, fe vce(cluster bank)

clear all 

cd "G:\My Drive\Work\Teaching\University_Courses\FAST_NUCES\Spring_2021\Applied_Econometrics\Stata_Practice"

insheet using https://stats.idre.ucla.edu/wp-content/uploads/2016/02/sample.csv, clear 

twoway (scatter hon math) (lfit hon math)
tabulate hon
logit hon
gen prs = 49/200
gen prf = 151/200
gen odds = prs/prf
gen lodds = log(odds)

logistic hon

logistic hon female 
tabulate hon female 

gen prm = 17/74
gen prfe = 32/77
gen oddsf = prfe/prm 
gen loddsf = log(oddsf)

logit hon female 
logistic hon female

logit hon female math 
use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear
tabulate honors 
gen prs = 53/200
gen prf = 147/200
gen oddsf = prs/prf 
gen loddsf = log(oddsf)

logit honors
logit honors i.female
margins female

webuse nhanes2f.dta, clear 
keep diabetes black female age sex race

drop if missing(diabetes, black, female, age, sex, race)

sum diabetes black female age

logit diabete black female age 

adjust age = 20 black female, pr
adjust age = 70 black female, pr

margins, at(age=(20 70)) atmeans 

margins, at(age=(20(10)70)) atmeans post 
marginsplot 
marginsplot, recast(line) recastci(rarea)

logit diabete i.race i.sex age 
margins race 
margins sex 

insheet using Data\Panel_Reduced.csv, clear 
replace treatment =0 if missing(treatment)
drop if missing(roa, roe, nim, liq)
format bank %-20s
drop name 
encode country, gen(co)

logit abs
logit abs i.co
margins co
marginsplot 

logit abs i.co liq 

margins, at(liq=(.01(.1)1.08)) atmeans post 
marginsplot
tabulate treatment 
drop if treatment==3

label define treat 0 "NONE" 1 "ABS Issuer" 2 "CB Issuer"
label values treatment treat 

mlogit treatment i.co 
margins co
marginsplot 
mlogit treatment liq size roa 
mlogit treatment i.co, base(2)
encode bank, gen(bankid)
xtset bankid year 
femlogit treatment liq roa 
***** tobit regression
insheet using Data\Euro_Banks.csv, clear 
sum abs_vol 
encode name, gen(bankid)
gen year = substr(closdate,7,10)
order year 
destring year, replace 
cap xtset bankid year 
duplicates report bankid year
duplicates list bankid year
duplicates list roa size bankid year
duplicates tag bankid year, gen(isdup)
order isdup
tabulate isdup
drop if isdup != 0 
xtset bankid year 
gen absta = abs_vol / l.tas 
sum absta, detail 
gen size = ln(tas)

tobit absta roa liq size, ll(0)
tobit absta roa liq size, ul(.000000259)

insheet using Data\sim.csv, clear 

label define prg  1 "General" 2 "Acadmic" 3 "Vocational"
label values prog prg 
tabstat num_awards, by(prog) stats(mean sd n)
poisson num_awards i.prog math
poisson num_awards i.prog math, vce(robust)
predict c 
separate c, by(prog)
twoway scatter c1 c2 c3 math, connect(l l l) sort ///
       ytitle("Predicted Count") xtitle("Math Score") legend(rows(3)) ///
       legend(ring(0) position(10)) scheme(s2mono)
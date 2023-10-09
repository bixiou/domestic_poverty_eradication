version 12
***To be run from the Povcal Master File
clear
set more off, permanently
capture log close
set type double
tempfile pov missing temp complete gdp income income_alt temp_MNA temp_EAP temp_ECA temp_LCN temp_SAR temp_AFR

***Imputation
*** Instances where z increases by $.01 or  >$.01 and headcount increases by >0.01. We replace the missing z with the value of z at the next higher headcount value where z is present
	*Loop over regions one at a time due to size of dataset
	
local regions EAP ECA LCN MNA SAR AFR
	
foreach region of local regions { 

	use "$output/Temp/`region'.dta", clear 
	
	*Create a master dataset with 10,000 observations 
	duplicates drop country year survey, force
	keep country year survey
	expand 10000
	bys country year survey: gen headcount=_n
	replace headcount = headcount/100
	save `complete', replace
	
	*Find and fill in holes: tag observations that fit description, and replace z with the z value of the next higher headcount value. 
	use "$output/Temp/`region'.dta", clear
	sort country year survey headcount
	foreach var of varlist headcount z {
		bysort country year survey (headcount): gen d_`var' = `var' - `var'[_n-1]
		replace d_`var'=round(d_`var',.01)
	}
	mmerge country year survey headcount using `complete'	

	gsort country year survey -headcount	
	by country year survey: carryforward z region, replace 
	drop if headcount == 0
	drop d_*  _merge
	
	gen year_povcal = year
	replace year = round(year,1)
	
	*Add in WB data on exchange rates, cpi and ppp conversion factors
		
	preserve
		tempfile ppp_icp ppp		
		insheet using "$input/Consumption/ppp_2005_icp.csv", clear
		renvars v3  v7 / countryname  ppp_lcu_icp
		keep countryname ppp_lcu_icp
		save `ppp_icp'
	
		insheet using "$input/Consumption/exchange rate_icp.csv", names clear
		renvars  v7 /  exchangerate_icp
		keep countryname exchangerate_icp

		mmerge countryname  using `ppp_icp'
		renvars countryname  / country 
		
		la var exchangerate_icp	"Exchange rate, LCU per USD, 2005, individual consumption"
		la var ppp_lcu_icp	 	"PPP conversion factor, private consumption (LCU per international 2005$) - ICP"
		save `ppp', replace
	restore
	
mmerge country using `ppp', unmatched(master)

*Create z at market exchange rate $ 
*gen z_mer = z * ppp_lcu_icp / exchangerate_icp
replace year = year_povcal 
drop year_povcal _merge ppp_lcu* exchangerate*  
		
	*Label variables
	la var country 			"Country Name"
	la var region 			"Region - World Bank Classification"
	la var year		 		"Year from PovcalNet"
	la var survey 			"Welfare Measure (Income or Consumption)"
	la var z 				"Poverty line, per person per month, 2005 consumption PPP$"
	la var headcount 		"% population living in hhlds with income/consumption per person below poverty line"
*	la var z_mer  			"Poverty line, per person per month, 2005 consumption (official exchange rate $)"
	
saveold "$output/povcal_full_`region'.dta", replace

****Prepare alternate dataset for syncronized years calculations
	cap drop z_mer		
	drop if regexm(country, "Urban") | regexm(country, "Rural")	 & country != "Argentina--Urban" & country != "Uruguay--Urban"	
	*Id the two closest existing years below and above 1990 and 2000 
	foreach x of numlist 2010 2000 1990 {
		bys country survey: egen below_`x'    = max(year) if year<`x'	
		bys country survey: egen altbelow_`x' = max(year) if year<`x' & year<below_`x'
		qui gen cond = 1 if year<altbelow_`x' & altbelow_`x'!=.
		bys country survey:  egen altaltbelow_`x' = max(year) if cond==1	
		bys country survey:  egen above_`x'    = min(year) if year>`x'
		bys country survey:  egen altabove_`x' = min(year) if year>`x' & year>above_`x'			
		qui replace below_`x' = round(below_`x', .1)
		qui replace altaltbelow_`x' = round(altaltbelow_`x', .1)
		bys country  survey (year):  carryforward below_`x' altbelow_`x' altaltbelow_`x', replace
		gsort country survey -year
		by country survey:  carryforward above_`x' altabove_`x', replace
		drop cond
	}
	
	gen     year_dec = year
	replace year = round(year)	
	
	tempfile gdp income income_alt
	*Add in GDP data for all survey years and 1990 and 2000
	preserve
		clear
		wbopendata, indicator(ny.gdp.pcap.kd)long 
		keep if year>1977
		keep countryname year ny*
		renvars countryname ny* / country gdp
		replace country = subinstr(country, ".", "",.) if country!="St. Lucia"
		replace country = "Congo, Dem. Rep" if country=="Congo, Dem Rep" 
		replace country = subinstr(country, "o", char(244), 1) if country=="Cote d'Ivoire"
		label var gdp "GDP per capita (constant 2005 US$)"
		reshape wide gdp , i(country) j(year)
		/*use "$output/wdi_na.dta", clear		// Check using alternate national accounts figure to extrapolate. Doesn't work well. 
		drop if countryn=="North America" | regexm(countryn, "CIS")
		gen gdp = C_h_CN/POP
		keep if year>1977
		keep countryname year gdp
		replace country = subinstr(country, ".", "",.) if countryn!="St. Lucia"
		loc p = char(130)
		loc e = char(233)
		loc a = char(227)
		replace country = subinstr(country, "Tome", "Tom`e'", 2) 
		replace country = subinstr(country, "Sao", "S`a'o", 2)
		replace country = "Moldova, Rep" if country=="Moldova"
		replace country = "Congo, Dem. Rep" if country=="Congo, Dem Rep" 
		replace country = subinstr(country, "o", char(244), 1) if country=="Cote d'Ivoire"
		reshape wide gdp , i(country) j(year) */
		save `gdp', replace
	restore
	
	*Sum Zs for each survey year		
	preserve
		collapse (sum) share = z, by(country year_dec survey)
		save `income', replace
	restore
	
	
	*Sum Zs for each survey year, with dropped bottom and top 1%	
	preserve
		replace z = . if headcount<=1 | headcount>=99
		collapse (sum) share_alt = z, by(country year_dec survey)
		save `income_alt', replace
	restore
	
	mmerge country survey year_dec using `income'
		replace share = z/share					        //Calculate share of income held by each 0.01% of pop
		
	mmerge country survey year_dec using `income_alt'
		replace share_alt = z/share_alt					//Calculate share of income held by each 0.01% of pop
		
		
	mmerge country using `gdp'
	bys country: carryforward region, replace
	drop if _merge==2
	gsort country -year
	bys country: carryforward region, replace
			
	if year_dec!=. {
		replace year = year_dec*10
	}
	else{
		replace year = year*10
	}
	drop year_dec _merge

	*Reshape wide for calculations
	*Drop bottom and top 1%
	qui replace z = . if headcount<=1 | headcount>99
	reshape wide z share share_alt, i(country survey headcount) j(year)
	cap gen z19900       = . 
	cap gen z20000       = .
	cap gen z20100       = .
	cap gen estimate1990 = 3
	cap gen estimate2000 = 3
	cap gen estimate2010 = 3
	lab def estimate   0 "Present in Povcal" 1 "Interpolated from 2 years of data" 2 "Extrapolated using GDP data" 3 "Insufficient data" 4 "Extrapolated using single survey + GDP"
	lab val estimate* estimate
	cap gen nodata   = 4 
	lab def nodata     0 "Data available" 1 "Too few surveys" 2 "No GDP data for interpolation" 3 "No GDP data for extrapolation" 4 " " 
	lab val nodata nodata
	cap drop _*
	order region country survey headcount below* altbe* above* altab* altalt* z* share* gdp*
		
saveold "$output/Temp/`region'.dta", replace
}

exit

****** Povcal metadata table ****** 
tempfile temp
use "H:\DFAD (Justin Sandefur)\DFAD-OUTPUT\povcal_sync_AFR.dta", clear
duplicates drop country survey year estimate , force
save `temp', replace
local regions MNA  SAR  EAP ECA LCN 
foreach region of local regions {
	use "H:\DFAD (Justin Sandefur)\DFAD-OUTPUT\povcal_sync_`region'.dta", clear
	duplicates drop country survey year estimate , force
	cap append using `temp'
	save `temp', replace
	}
	
duplicates tag country survey year, gen(dup)
drop if estimate==. & dup==1
replace estimate=3 if estimate==. 
drop z below3 headcount 
cap label drop estimate
cap lab def estimate   0 "Present in PovcalNet" 1 "Interpolated" 2 "Extrapolated using two years of data" 4 "Extrapolated using single survey" 3 "No GDP data available" 
cap lab val estimate* estimate

*Show spread of data
gen distance = 0 if estimate == 0 							//Data present in PovcalNet
replace distance = above1-below1 if estimate==1				// If interpolated, count # of years between surveys
replace distance = above1-year   if estimate==2 & above1!=. // If extrapolated, count # of years between target year and closest survey
replace distance = year-below1   if estimate==2 & below1!=. // 
replace distance = above1-year   if estimate==4 & above1!=. // If extrapolated, count # of years between target year and closest survey
replace distance = year-below1   if estimate==4 & below1!=. // 

tabstat distance, by(estimate) s(me co sd)

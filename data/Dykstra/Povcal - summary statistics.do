* Last revision
version 12
global date "2013-08-27"
* *****************************************************************************
* Install some programs, capture if already installed
foreach p in kdens sxpose fre spineplot wbopendata carryforward tabout cdfplot sumdist ineqdeco {
	cap which `p'
	if _rc==111 cap noi ssc install `p'
}
* *****************************************************************************
clear
set more off, permanently
capture log close

*Loop over regions one at a time due to size of dataset
tempfile sumstats collapse1 collapse2 tempAFR tempEAP tempECA tempLCN tempMNA tempSAR 
local regions EAP ECA LCN MNA SAR AFR
	
foreach region of local regions { 

use "$output\povcal_full_`region'.dta", clear 

di "Replace means with monthly consumption / income. Then run"

*Break region files into groups of 10 to speed up the process
ren survey measure	
gen d = z*12/365
egen group = group(country measure year)
sum group 
loc rounds = ceil(r(max)/10)
replace group = ceil(group/10)

forval a = 1/`rounds' {

preserve 
	keep if group == `a'

	* Generate identifier for country-year pairs 
	egen cy = concat(country year measure), p(-)
	encode(cy), gen(cyid)
	sum cyid
	local n = r(max)	

************************ Calculate summary statistics

	* Estimate means for each country-pair
	gen z_mean_est = .
	forval i = 1/`n' {
		quietly sum z if cyid==`i' 
		replace z_mean_est = r(mean) if cyid==`i'
	}
	gen d_mean_est = z_mean_est*12/365

	* Read out medians for each country-pair
	sort country year headcount 
	by country year: gen diff = abs(headcount-50)
	* ** Right now we just select the closest value to the median, no interpolation involved...
	bys country year (diff): gen z_p50_est = z if _n==1
	gen p50_est_marker = 1 if diff==0
	bys country year: carryforward z_p50_est, replace
	gen d_p50_est = z_p50_est*12/365
	drop diff

	* Calculate Gini and p90/p10 percentile ratios, using Jenkins' ineqdec from SSC
	gen gini   		= .
	gen p90p10 		= .
	gen p75p25 		= .
	gen sen    		= .
	gen palma  		= .
	gen share20 	= .
	gen share40 	= .
	forval i = 1/`n' {
		di "Currently working on country-year number `i'"
		ineqdeco d if cyid==`i', welfare
		replace sen = r(w2)  if cyid==`i'
		replace gini = r(gini)  if cyid==`i'
		replace p90p10 = r(p90p10) if cyid==`i'
		replace p75p25 = r(p75p25) if cyid==`i'			
		cap sumdist  d  if cyid==`i', n(10)
		cap replace palma      = r(sh10)/(r(sh4) + r(sh3) + r(sh2) + r(sh1)) if cyid==`i'
		cap replace share20   = r(sh2) + r(sh1)  if cyid==`i'
		cap replace share40   = r(sh4) + r(sh3) + r(sh2) + r(sh1)  if cyid==`i'

	}	
	save `sumstats', replace
	
	* Collapse means, medians, and inequality statistics into CSV sheet
		#delimit ;
		collapse (firstnm) region 
		(mean) d_mean_est d_p50_est gini p90p10 p75p25 sen palma share_20 share_40, 
		by(country year measure) fast;
		#delimit cr		
	save `collapse1', replace
	
	*Calculate the income of bottom 40% + shared prosperity indicator
	use `sumstats', clear
	drop if headcount>40.00
	collapse (sum) z40 = z, by(country year measure)
	save `collapse2', replace
	
	use `sumstats', clear
	drop if headcount>20.00
	collapse (sum) z20 = z , by(country year measure)
	
	mmerge country year measure using `collapse1'
	mmerge country year measure using `collapse2'
	
	tempvar dyear 
	bys country measure: gen `dyear'	= year - year[_n-1]	
	bys country measure: gen shared 	= (z40/z40[_n-1])^(1/`dyear') - 1


	* Match with World Bank country codes to merge with other data
	* Using codes_masterlist from N:\Public\Research Data
	mmerge country using "codes_masterlist.dta", unmatched(master) ukeep(country wb_region wb_ccode)
	drop _merge

	* Identify urban/rural (to avoid duplicates)
	gen whole = 1
	replace whole = 0 if country=="Argentina--Urban" || country=="China--Rural" || country=="China--Urban" || country=="Colombia--Urban" || country=="Ecuador--Urban" || country=="Honduras--Urban" || country=="India--Urban" || country=="India--Rural" || country=="Indonesia--Urban" || country=="Indonesia--Rural" || country=="Micronesia, Fed. Sts.--Urban" || country=="Uruguay--Urban"
	gen subsample = 0
	replace subsample = 1 if country=="Argentina--Urban" || country=="China--Urban" || country=="Colombia--Urban" || country=="Ecuador--Urban" || country=="Honduras--Urban" || country=="India--Urban" || country=="Indonesia--Urban" || country=="Micronesia, Fed. Sts.--Urban" || country=="Uruguay--Urban"
	replace subsample = 2 if country=="China--Rural" || country=="India--Rural" || country=="Indonesia--Rural"
	la de subsample 0 "whole" 1 "urban" 2 "rural"
	la var subsample subsample

	* Create new country code based on WB, but with rural-urban
	clonevar countrycode = wb_ccode
	replace  countrycode = wb_ccode + "-U" if subsample==1
	replace  countrycode = wb_ccode + "-R" if subsample==2

	* Round half-years to full years
	ren      year   year_povcal
	clonevar year = year_povcal
	replace  year = round(year,1)
	 
	* Generate id for merging
	egen mergeid = concat(wb_ccode year)
cap append using `temp`region''
save `temp`region'', replace
restore
}

use `temp`region'', clear
	la var d_mean_est 		"Mean Inc/Cons per capita per day (2005 PPP)"
	la var d_p50_est 		"Median Inc/Cons per capita per day (2005 PPP)"
	la var gini 			"Gini coefficient"
	la var sen 				"Sen's measure of welfare"
	la var share20			"Share of income of bottom 20%"
	la var share40			"Share of income of bottom 40%"
	la var shared		    "Growth in share of income of bottom 40%"
	la var p90p10 			"p90 / p10 ratio"
	la var p75p25 			"p75 / p25 ratio"
	la var wb_region 		"World Bank Region"
	la var whole 			"Survey coverage (whole country or region)"
	la var subsample 		"Survey coverage (0=whole, 1=urban, 2=rural)"	
	la var wb_ccode 		"World Bank 3-letter code"
	la var countrycode 		"World Bank 3-letter code with indicator for urban/rural"
	la var year_povcal 		"Year from PovcalNet"
	la var year 			"Year, rounded to nearest integer"
	la var z20	 			"Income of bottom quintile"
	la var z40	 			"Income of bottom two quintiles"
	order country countrycode wb_ccode wb_region year year_povcal mergeid whole subsample shared, first
	
*Output two - one to work with, one to put online
drop _*
cap drop firstnm* 
saveold "$output\PovcalExport_SummaryStatistics_`region'", replace
export excel country countrycode wb_region year_povcal measure subsample d_mean_est d_p50_est gini p90p10 p75p25 sen palma share_20 share_40 /// 
	   using "$output\PovcalExport_SummaryStatistics_`region'.xlsx", first(varl) replace

}

tempfile append
foreach x in EAP ECA LCN MNA SAR AFR {
	use "$output/PovcalExport_SummaryStatistics_`x'.dta",  clear
	cap append using `append'
	save `append', replace
}
outsheet using "$output\PovcalExport_SummaryStatistics.csv", comma names replace

exit

*Create summary statistics table 
unique country
	putexcel A2=("Number of surveys") C2=(r(N)) ///
			 A3=("Countries represented") C3=(r(sum)) ///	
		     using "$output/summary table.xlsx", sh("Estimates") modify		 

tab measure, matcell(measure) 
	putexcel A10=("Consumption surveys") A11=("Income surveys") C10=matrix(measure) ///
			 using "$output/summary table.xlsx", sh("Estimates") modify	

recode region 1=6 6=4 4=3 3=2 2=1 
lab de region 1 "Europe & Central Asia" 2 "Latin America & Caribbean"  3 "Middle East & North Africa"  4 "Sub-Saharan Africa" 5 "South Asia" 6 "East Asia & Pacific"
lab val region region

local forlab: value label region
	forvalues n = 1(1)6 {
		local x = `n' + 3
		local label: label `forlab' `n'
		putexcel A`x'=("`label'") using "$output/summary table.xlsx", sh("Estimates") modify		 
	}

foreach n of numlist 1/6 {
			local x = `n' + 3
			sum year if region==`n' , d 
			putexcel C`x'=(r(N))     ///
			using "$output/summary table.xlsx", sh("Estimates") modify	
	}

 
local regions EAP ECA LCN MNA SAR AFR	
	tempfile sum	
	foreach region of local regions { 
	use "$output\povcal_`region'.dta", clear 
		cap append using `sum'
		save `sum', replace
	}

bys country year survey: gen count = _N
duplicates drop country survey year, force
sum count, d
	putexcel A12=("Total unique headcount observations") C12=(r(sum)) ///
		     A13=("Mean unique headcount observations per survey") C13=(r(mean)) C14=("(`r(sd)')") ///
			 A15=("Total queries to PovcalNet") ///
			 using "$output/summary table.xlsx", sh("Estimates") modify	

		 	

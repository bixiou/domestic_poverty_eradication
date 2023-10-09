*Must be run from Povcal Master do file. 
cd 	"$input/Consumption/Povcal"

tempfile temp_1 temp_2 temp_3 temp_4 temp_5 temp_6 temp_7 temp_8 temp_9 temp_10 EAP ECA LCN MNA SAR AFR

*Get a listing of all files in output directory (and subdirectories)
	if "`c(username)'" == "sdykstra" {
    ! dir *.csv /s /b >..\listing.txt
}
/*Different command for Macs 
	if "`c(username)'" == "Justin" {
    ! ls *.csv /s /b >..\listing.txt
}*/

*Read in text file and edit problematic Cote d'Ivoire + Sao Tome obs
	infix str list 1-200 using "$input\Consumption\listing.txt", clear
		ascii
		replace list = subinstr(list, char(147), char(244), 2)
		loc p = char(130)
		loc e = char(233)
		loc a = char(227)
		replace list = subinstr(list, "Tom`p'", "Tom`e'", 2) 
		replace list = subinstr(list, "Sao",    "S`a'o",  2)
		unique list
		loc panel = r(sum)
	outfile using "$output\listing.txt", replace noquote wide	

*Read through directory listing to import and append each file		
	clear
	local n = 1
	file open list using "$output\listing.txt", read 
	file read list line
		while r(eof) == 0 {
		set type double
	/*	if `n'<799 | `n'>850 {			//Uncomment to run a portion of csvs (spot checks)
			file read list line
			local n = `n' + 1
			continue
			}*/
			di "`line'"
			local name "`line'"
			insheet z headcount using "`line'", comma clear
			*Assert that there are observations in file, else exit and continue from top of loop + record country - years with empty datasets in a separate file
			count
			if r(N) == 0 {
				set obs 1
				gen file = "`name'"
				append using "$output\povlines_empty.dta"
				save "$output\povlines_empty.dta", replace
				file read list line
				continue
			}
			*Create country, survey type and year variables by parsing filename
			gen file =        reverse("`name'")
				split file,   parse(\)
				gen country = reverse(file2)
				split file1,  parse(-)
				split file12, parse(" ")
				gen survey  = substr(file11, 5, 1)
				gen year    = reverse(file121)    
				drop file*
			*Identify files where there's 'na' in headcount, resave as double
			loc id country survey
			ds, has(type string)
				local varlist `r(varlist)'
				local vars : list varlist - id
				foreach var in `vars' {
				replace `var' = "." if `var' =="na"
				destring `var', replace
			}
			*Append dataset to earlier file -- break into 10 groups to decrease file size
			di "File `n'"
			if `n'==1 save "`temp_1'", replace
			foreach num of numlist 1(1)10 {
				local base    = `num'*100 - 100
				local ceiling = `num'*100
				if `n'==`base' save "`temp_`num''", replace
				else if `n' >1 & `n'>`base' & `n'<`ceiling' {
					append using "`temp_`num''"
					save "`temp_`num''", replace 
					di "Round `num' "
					}
				}
				
		file read list line
		local n = `n' + 1
		}
	file close list

*Append all and clean up

	use "`temp_1'", clear
		foreach n of numlist 2(1)10 {
		append using "`temp_`n''"
		}
		duplicates drop
		drop if headcount==.
		sort country year
		order year country z headcount
		mmerge country using "N:\Public\Research Data\codes_masterlist.dta", unmatched(master) ukeep(country wb_region)
		drop _merge
		sort country year survey z
		
		*Duplicates checks
		duplicates tag country year survey headcount, gen(dup_hc)
		tab dup_hc
		duplicates tag country year survey z, gen(dup_z)
		tab dup_z
		
		*Data issues:
		*1) Instances where there are duplicate headcounts at different z values. Keep obs with lowest z
			*Note this seems to happen at very high and very low headcount values, 99.97 and 0.02 (there may be some issue with Povcalnet calculations). 
			*It also occurs in runs - for example, values between 24.00 - 24.09 will be repeated. I can replicate these results on Povcalnet. 
			bys country year headcount survey: egen min = min(z)
			bys country year headcount survey: gen n = _n
			drop if dup_hc>0 & z!=min
			
		*2) There are instances where where headcount decreases as z increases (see: Turkey - 2009, for example). This is a problem with Povcalnet's calculations - I can replicate all of these instances
			bysort country year survey (z): gen change_hc = headcount - headcount[_n-1]
			gen error = 0
			replace error = 1 if change_hc<0
			sum change_hc
			
		bys country year survey: gen count = _N
		sum count, d
		 
		*Assert that we have all 952 observations (except for the country-years with empty datasets)
		preserve
		 use "H:\DFAD (Justin Sandefur)\DFAD-OUTPUT\povlines_empty.dta", clear
		 unique file
		 local empty = r(sum)
		restore
			 
		unique country year survey
		local obs = r(sum) + `empty'
		di `obs'			//Should be 952, but should also be consistent with the panel provided by the text file listing all files. 
		assert `obs'==`panel'
		
		**Record count as of 7/22 is 7,183,221 (duplicates removed), as of 8/1 7,184,166, 10/22 7,199,274
									 
*Use region to split dataset and output as csvs
		encode wb_region, gen(region)
		drop wb_region min n dup* change_hc error count
		order region country year survey 
		foreach n of numlist 1(1)6 {
			preserve
			keep if region==`n'
			if `n'==1 saveold "$output/Temp/EAP.dta",  replace
			if `n'==2 saveold "$output/Temp/ECA.dta",  replace
			if `n'==3 saveold "$output/Temp/LCN.dta",  replace
			if `n'==4 saveold "$output/Temp/MNA.dta",  replace
			if `n'==5 saveold "$output/Temp/SAR.dta",  replace
			if `n'==6 saveold "$output/Temp/AFR.dta",  replace
			restore
			}

log close

exit

***Check for missing countries / years

	duplicates drop country year survey, force
	gen source="Master"

	preserve
		infix str list 1-200 using "$input\Consumption\listing.txt", clear
		gen file = reverse(list)
			split file, parse(\)
			gen country = reverse(file2)
			split file1, parse(-)
			split file12, parse(" ")
			gen survey  = substr(file11, 5, 1)
			gen year    = reverse(file121)    
			destring year, replace
			drop file*
		duplicates drop country year survey, force
		gen source="Listing"
		tempfile import
		save `import'
	restore
		
	preserve
		use "$output\povlines_empty.dta", clear
			replace file = reverse(file)
			split file, parse(\)
			gen country = reverse(file2)
			split file1, parse(-)
			split file12, parse(" ")
			gen survey  = substr(file11, 5, 1)
			gen year    = reverse(file121)    
			destring year, replace
			drop file*		
		keep country year survey 
		duplicates drop
		gen source="Empty"
		tempfile empty
		save `empty'
	restore
		
	append using `import'
	append using `empty'
	duplicates tag country year survey, gen(dup)
	list country year survey source if dup==0
	
	
	
	
	
	
	

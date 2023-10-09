***Povcal Master File
*Note: if using a Mac, must modify lines 6-13 in Povcal-import.do 
*Last revision
global date "2014-04-09"

clear
set more off, permanently
capture log close
capture file close list
tempfile pov

*Set folder structure - set global for root directory ($DFAD) by entering folder location in quotes
global DFAD "H:/DFAD" 

*Organize files in folders according to the structure below
global dofiles  "$DFAD/DFAD-DOFILES"
global input 	"$DFAD/DFAD-INPUT"		//Put output from Unloader.py in DFAD-INPUT/Consumption/Povcal"
global output 	"$DFAD/DFAD-OUTPUT"
global paper 	"$DFAD/DFAD-PAPER/Latex"

***Import information from raw Povcal csvs, clean, append and run checks.
  *Note: Stata will occasionally skip a file and error at the assert command. Re-run. 

	do "$dofiles/Povcal/Povcal - import.do"
	
***Create a dataset where each country-year contains 10,000 headcount observations. 

	do "$dofiles/Povcal/Povcal - imputation.do"

***Delete temporary datasets 

local datafiles: dir "$output/Temp" files "*.dta"
foreach datafile of local datafiles {
        rm "$output/Temp/`datafile'"
}

***Calculate summary statistics (inequality statistics, means and medians) and income distribution graphs

	do "$dofiles/Povcal/Povcal - summary statistics.do"  

***Replicate stats from Growth is Good
	
	do "$dofiles/Povcal/Povcal - replicate growth stats.do"

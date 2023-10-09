
*We Just Ran Twenty-Three Million Queries of the World Bank's Web Site - CGD Working Paper
*See Appendix 1 for more information on running the Python code

*4/28/2014

***FILES***

// DATA - csv
*SummaryStatistics.csv
*   Mean, Median, Gini, etc.
*povcal_abbrev.csv
*	100 point income/consumption distribution datasets
*povcal_parameters
*	Lorenz curve parameters from general quadratic (a, b, c) and Beta (theta, gamma, delta) models
*	best_curve specifies which Lorenz curve Povcal uses to estimate headcounts
*	curve_validity reports which specifications are valid
*povcal_full_MNA#.csv
*povcal_full_AFR#.csv
*povcal_full_SAR#.csv
*povcal_full_ECA#.csv
*povcal_full_EAP#.csv
*povcal_full_LCN#.csv
*	10,000 point income/consumption distribution datasets by region, split into multiple files to accomodate Excel's 1,000,000 observation limit


// DATA - dta
*povcal_abbrev.dta
*	100 point income/consumption distribution datasets
*povcal_parameters
*	Lorenz curve parameters from general quadratic (a, b, c) and Beta (theta, gamma, delta) models
*	best_curve specifies which Lorenz curve Povcal uses to estimate headcounts
*	curve_validity reports which specifications are valid
*povcal_full_MNA.dta
*povcal_full_AFR.dta
*povcal_full_SAR.dta
*povcal_full_ECA.dta
*povcal_full_EAP.dta
*povcal_full_LCN.dta
*	10,000 point income/consumption distribution datasets by region


//PYTHON SCRIPTS
*Harvester.py
*	Navigates to the PovcalNet website and issues queries in a series of passes which are laid out in lines 443-457 
*	Stores query results in a Mongo database, povcal 
*
*Unloader.py
*	Searches Mongo database and outputs lowest threshold income/consumption value for each incremental
*	increase in the poverty headcount, stored in a series of csvs by country-year
*
*Harvester_parameters.py
*	Navigates to Povcal website and downloads 'Detail output' files for all country-years 
*	Parses and outputs parameters for GQ and Beta Lorenz curves, 
*	Specifies which curve is used for each country-year and the validity of each curve



// CODE
*Povcal Master.do 
*	Code used to replicate import and clean data produced by Python scripts and create full distributions 
*	Reads in Povcal - import.do
*		Imports raw data produced by the Python program, cleans, and appends by region 
*	Reads in Povcal - imputation.do
*		Creates full distributions such that each country-year has 10,000 observations 
*		For instances where the headcount increases by >0.01%, we expand the 
*		dataset and replace the missing consumption/income value (z) with value 
*		of z at the next higher headcount value where z is present
*	Reads in Povcal - summary statistics.do
*		Replicates calculation for all summary statistics, including inequality measures, means and medians
	

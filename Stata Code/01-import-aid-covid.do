// -------------------------------------------------------------------------- //
// Import totals of federal COVID response programs
// -------------------------------------------------------------------------- //

cd "/Users/peterwilliams/Desktop/The Effects of Emergency Rental Assistance During the Pandemic/stata-rerun"

// Monthly data
import excel "rawdata/covid-aid-data/covid-aid-data.xlsx", sheet("COVID (monthly)") firstrow clear
replace covid_relief = 1e9*covid_relief
replace ppp_proprietors = 1e9*ppp_proprietors
save "01-import-aid-covid/covid-aid-monthly.dta", replace

// Quarterly data
import excel "rawdata/covid-aid-data/covid-aid-data.xlsx", sheet("COVID (quarterly)") firstrow clear
replace covid_subsidies = 1e9*covid_subsidies
replace covid_ppp = 1e9*covid_ppp
save "01-import-aid-covid/covid-aid-quarterly.dta", replace

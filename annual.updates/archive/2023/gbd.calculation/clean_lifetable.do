local region = "India" //"Global" or a country name

cd "C:\Users\Aarsh\Desktop\aqli-epic\annual.updates\september.2023\gbd.calculation\GBDComparisons\WHO life tables"

import delimited "`region'_raw.csv", varnames(2) rowrange(3) colrange(:4) clear
gen lt_var = substr(indicator, 1, strpos(indicator, " ")-1)

export delimited "`region'.csv"

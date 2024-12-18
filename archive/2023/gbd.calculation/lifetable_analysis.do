/* 
 * Purpose: Construct the estimates for life expectancy lost for each disease
		from GBD, using the cohort life table method. We use actual life expectancy,
		compared to constructed actual life expectancy, compared to counterfactual
		no-disease life expectancy (from lowering the mortality rate), then see
		the difference for the contribution of a disease to life expectancy lost.
 * Documentation: https://www.statsdirect.com/help/survival_analysis/abridged_life_table.htm
 * Actual life expectancy: http://apps.who.int/gho/data/view.main.LIFEREGIONGLOBAL?lang=en
 *
 * Note: This code assumes that the terminal age bin is "80+" for both the life tables data
 * and mortality rates data for each cause/risk of death. If "80+" is missing but there are
 * 80-85, 85-90, etc. categories, those can be turned into 80+ through weighted average. If 
 * no data exists beyond 75-80, then this code treats the mortality rate for 80+ as 0, which 
 * can lead to potentially severe underestimates of life expectancy lost if the actual mortality
 * rate for 80+ is high.
 */
 
pause on

* Change this to your home directory
local ROOT "C:\Users\Aarsh\Desktop\aqli-epic\annual.updates\september.2023\gbd.calculation\GBDComparisons"

// Load the CSV file
import delimited "`ROOT'/sr_country_wise_cleaned.csv", clear

local region = "" //e.g. Global, China, India, etc.
local year = "" //leave empty for default, or "_[year]" for other year

forval i = 178/`=_N' {
	// Load the CSV file
    import delimited "`ROOT'/sr_country_wise_cleaned.csv", clear
	local region = region[`i']
	di `region_local'
	local mf_sexratio = sr[`i']
	di `sr_local'
	

//local mf_sexratio = 1.045 //male births per female births, from https://data.worldbank.org/indicator/SP.POP.BRTH.MF

/* Binary switch: Do we adjust M or q? The standard is to adjust M, but the GBD documentation 
		is ambiguous. M is annual mortality rate, q is mortality rate within an age group */
local adjust_M = 1 

*** Part 1: Set up the actual life table
tempfile actual_life_table
/* First obtain the actual mortality rates, etc. Downloaded from 
		http://apps.who.int/gho/data/view.main.LIFEREGIONGLOBAL?lang=en */
insheet using "`ROOT'/WHO life tables/`region'.csv", names clear
cap drop indicator
replace agegroup = subinstr(agegroup, "years", "", .)
replace agegroup = subinstr(agegroup, "year", "", .)
replace agegroup = subinstr(agegroup, " ", "", .)
/* Since the WHO's data on global mortality rates ends at 85+, but the GBD
		categories either end at 80+ or 95+, we decided to truncate the WHO
		at 80+. This leads to a slightly different estimate of life expectancy 
		at birth from the WHO table, recalculated here, but leads to consistent 
		comparisons across diseases from GBD. To recalculate with the 80+ as the
		max category, it is sufficient to just have the lx, nLx, ndx, and nqx 
		variables. These are, for an artificial cohort of 100k births:
			lx = number of life-years remaining at the beginning of the age group
			nLx = life-years lived within the age group
			ndx = deaths within the age group
			nqx = probability that individual will die in the interval
			nMx = annual death rate, as a probability, in the interval */
keep if inlist(lt_var, "lx", "nLx", "ndx", "nqx", "nMx")
* Generate the age category variable
gen age_n = substr(agegroup, 1, strpos(agegroup, "-") - 1)
destring age_n, replace
gen age_cat = .
replace age_cat = 1 if agegroup == "<1"
replace age_cat = 2 if agegroup == "1-4"
replace age_cat = (age_n/5) + 2 if age_cat == . & agegroup != "85+"
qui summ age_cat
replace age_cat = `r(max)' + 1 if agegroup == "85+"
drop age_n
* Prepare for the reshape
rename male _male
rename female _female
reshape wide @_male @_female, i(agegroup age_cat) j(lt_var) string
sort age_cat
* Convert from separate 80-84 and 85+ obs to just 80+ for comparison with GBD
local new_num_obs = _N + 1
set obs `new_num_obs'
replace agegroup = "80+" if _n == `new_num_obs'
* For 80+, the lx variable should have the value at age 80, i.e. from 80-84
foreach v in lx_male lx_female {
	qui summ `v' if agegroup == "80-84"
	replace `v' = `r(mean)' if agegroup == "80+"
}
* For 80+, the nqx value should be 1 b/c everyone in the 80+ group dies in that group.
foreach v in nqx_male nqx_female {
	replace `v' = 1 if agegroup == "80+"
}
* For 80+, the ndx and nLx values are the sum of the values from 80-84 and 85+
foreach v in ndx_male ndx_female nLx_male nLx_female {
	qui summ `v' if agegroup == "80-84"
	local val1 = `r(mean)'
	qui summ `v' if agegroup == "85+"
	replace `v' = `r(mean)' + `val1' if agegroup == "80+"
}
* For 80+, the nMx value is simply the ratio of ndx and nLx, its general definition
foreach s in "male" "female" {
	replace nMx_`s' = ndx_`s' / nLx_`s' if agegroup == "80+"
	qui summ nMx_`s' if agegroup == "80+"
	local terminal_nMx_`s' = `r(mean)'
}
* Now remove the rows for 80-84 and 85+
drop if inlist(agegroup, "80-84", "85+")
qui summ age_cat
local new_max_age_cat = `r(max)' + 1
replace age_cat = `new_max_age_cat' if agegroup == "80+"
gen agegap = 5 // def'd to be number of yrs, inclusive, in the age category
replace agegap = 1 if age_cat == 1
replace agegap = 4 if age_cat == 2
replace agegap = . if age_cat == `new_max_age_cat'
foreach sex in "male" "female" {
	/* One major issue that I noticed: there's a lot of rounding in nqx and nMx, which leads
			to inaccuracies when using nqx later on. For a more accurate value of nqx,
			back it out from lx and ndx using the identity nqx = ndx / lx */
	replace nqx_`sex' = ndx_`sex' / lx_`sex'
	/* Similarly, to cure the rounding issue, for all but the terminal age group,
			back out nMx from the identity 
			q = n * M / (1 + (1 - a) * n * M),
			where n is the agegap and a is defined below. For the terminal,
			M = l / L */
	/* Using the L_i formula of L = n * (l - d) + a * n * d, then solving for a */
	gen ax_`sex' = (nLx_`sex' - agegap * (lx_`sex' - ndx_`sex')) / (agegap * ndx_`sex')
	replace nMx_`sex' = nqx_`sex' / (agegap * (1 - nqx_`sex' * (1 - ax_`sex'))) if age_cat !=`new_max_age_cat'
	replace nMx_`sex' = lx_`sex' / nLx_`sex' if age_cat ==`new_max_age_cat'
	/* Now re-calculate the life expectancy at birth given the new truncated set of
			age groups */
	gen nTx_`sex'=nLx_`sex' if age_cat ==`new_max_age_cat'
	/* Recursively calculate the total life-years lived for people who survive
			to the beginning of the age group - this is how nTx is defined */
	local penultimate = `new_max_age_cat' - 1
	forvalues i=1/`penultimate' {
		replace nTx_`sex'=nLx_`sex'[_n] + nTx_`sex'[_n+1] if age_cat ==`new_max_age_cat'-`i'
	}
	* Now calculate the remaining life expectancy conditional on living to the age group 
	gen ex_`sex'=nTx_`sex'/lx_`sex'
	local ex_birth_`sex' = ex_`sex'[1] // life expectancy at birth
}
save `"`actual_life_table'"', replace
di "Global estimated life expectancy at birth, women: `ex_birth_female'"
di "Global estimated life expectancy at birth, men: `ex_birth_male'"
* Save age groups in WHO life table data to match with GBD data below
levelsof(agegroup), local(agegroups)
	
	
*** Part 2: Determine the life expectancy were a disease not to exist
tempfile gbd
/* The difference between the counterfactual and actual is the life expectancy
		lost due to the disease */
* First, read in the GBD death rates files, appending mortality causes and risk factors together
insheet using "`ROOT'/GBD/gbdrates_`region'_causes`year'.csv", names clear
tempfile causes 
save `"`causes'"'
insheet using "`ROOT'/GBD/gbdrates_`region'_risks`year'.csv", names clear
drop cause 
rename rei cause 
append using "`causes'"
drop if age == "95 plus"

keep sex age cause val
replace sex = lower(sex)
rename val shifter_ // mortality rate shifter
replace shifter_ = shifter_ / 100000 // to convert from rate to a probability
* Create a merge-able age category
gen age_n = substr(age, 1, strpos(age, "to") - 2)
destring age_n, replace
gen age_cat = .
replace age_cat = 1 if age == "<1 year"
replace age_cat = 2 if age == "1 to 4"
replace age_cat = (age_n/5) + 2 if age_cat == . & age != "80 plus"
qui summ age_cat
replace age_cat = `r(max)' + 1 if age == "80 plus"
drop age_n
reshape wide shifter_@, i(age cause age_cat) j(sex) string
save `"`gbd'"', replace
* Now loop over the causes and compute the counterfactual life expectancy
levelsof(cause), local(the_causes)
foreach c of local the_causes {
	use "`gbd'", clear
	di "Currently considered cause of death: `c'"
	keep if cause == `"`c'"'
	merge 1:1 age_cat using "`actual_life_table'", assert(match using) nogen
	sort age_cat
	/* There are some unmatched using b/c we don't have mortality shifters for all 
			age categories. If GBD did not report any, we will assume are zero */
	foreach s in "male" "female" {
		replace shifter_`s' = 0 if shifter_`s' == .
	}
	/* Shift the mortality value for each sex - consider world w/o cause of death
		 This requires us to recalculate lx and ndx for each interval, not just the 
			last interval as in part 1. */
	drop nqx* lx* nLx* ndx* nTx* ex*
	foreach s in "male" "female" {
		/* It is not immediately clear whether the GBD documentation tells us to 
				adjust M or q, but I interpret it as adjusting M, the standard. */
		if `adjust_M' == 1 {
			replace nMx_`s' = nMx_`s' - shifter_`s'
			* Recalculate q, the age group probability of death
			gen nqx_`s' = agegap * nMx_`s' / (1 + (1 - ax_`s') * agegap * nMx_`s')
			replace nqx_`s' = 1 if age_cat == `new_max_age_cat'
		} 
		else {
			/* From the GBD documentation, see "rate per 100k". One interpretation is that the
			shifter is over the years in an age group, not the annual mortality.
			In other words, we shift q, not M. Note that the typical situation is
			we shift M. Thus I had to derive M as a function of q, see below. 
			http://www.healthdata.org/terms-defined
			*/
			replace nqx_`s' = nqx_`s' - shifter_`s'
			replace nqx_`s' = 1 if age_cat == `new_max_age_cat' // everyone must die in 80+
			/* Derivation: Let n be the age gap. q is defined as n*M/(1 + (1 - a)*n*M).
					Rearranging, q + q*(1-a)*n*M = n*M.
					Thus, n*M = q/(1 - q*(1 - a)) */
			gen nMx_`s' = nqx_`s' / (agegap * (1 - nqx_`s' * (1 - ax_`s')))
			/* Assume the same terminal mortality rate as before - we standardize q to
					1, so we must standardize M as well */
			replace nMx_`s' = `terminal_nMx_`s'' if age_cat == `new_max_age_cat'
		}
		gen lx_`s' = 100000
		/* Deaths per interval */
		forvalues i=2/`new_max_age_cat' {
			replace lx_`s' = lx_`s'[_n-1]*(1 - nqx_`s'[_n-1]) if _n == `i'
		}
		gen ndx_`s' = nqx_`s' * lx_`s'
		/* The following line of code is equivalent to 
				( 1 * lx(1) ) + ( a(0) * d(0) ) since 
				lx(1) = lx(0) - d(0), so d(0) = lx(0) - lx(1)
			 The 1, 4, 5 is the number of years in each age bin
		*/
		gen nLx_`s' = 1*(lx_`s' - ndx_`s') + ax_`s'*1*ndx_`s' if _n==1	
		replace nLx_`s' = 4*(lx_`s' - ndx_`s') + ax_`s'*4*ndx_`s' if _n==2	
		replace nLx_`s' = 5*(lx_`s' - ndx_`s') + ax_`s'*5*ndx_`s' if _n>=3 & _n<=`penultimate'
		replace nLx_`s' = ndx_`s'/nMx_`s' if _n==`new_max_age_cat'	// terminal bin

		* Now continue to follow the derivation from part 1
		gen nTx_`s'=nLx_`s' if _n==`new_max_age_cat'
		forvalues i=1/`penultimate' {
			replace nTx_`s'=nLx_`s'[_n] + nTx_`s'[_n+1] if _n==`new_max_age_cat'-`i'
		}
		gen ex_`s'=nTx_`s'/lx_`s'
		
		* Prepare and store the life expectancy difference
		local c_abbrev = substr("`c'", 1, 12)
		local c_abbrev = subinstr("`c_abbrev'", "/", "", .)
		local c_abbrev = subinstr("`c_abbrev'", "-", "", .)
		local c_abbrev = subinstr("`c_abbrev'", " ", "", .)
		* Here is the ounterfactual life expectancy at birth
		local ex_cf_birth_`s'_`c_abbrev' = ex_`s'[1] 
		local ex_diff_`s'_`c_abbrev' = round(`ex_cf_birth_`s'_`c_abbrev'' - `ex_birth_`s'', 0.001)
		* Due to issues with rounding errors, only use 3 digits after the decimal place
		di "The difference in life expectancy for `s's is `ex_diff_`s'_`c_abbrev''"
		assert `ex_diff_`s'_`c_abbrev'' >= 0
	} // end loop over birth sex
}


*** Part 3: Write results to file
/* As weights for aggregating across the sexes for a total life expectancy
	 difference, use sex ratio at birth
   Source: World Bank https://data.worldbank.org/indicator/SP.POP.BRTH.MF
*/
putexcel set "`ROOT'/results/estimated_life_expectancy_differences_`region'`year'.xlsx", modify
putexcel B2 = "Cause of death"
putexcel C2 = "Estimated life expectancy difference vs. actual: women"
putexcel D2 = "Estimated life expectancy difference vs. actual: men"
putexcel E2 = "Estimated life expectancy difference vs. actual: aggregate"
local iterator = 3
foreach c of local the_causes {
	local c_abbrev = substr("`c'", 1, 12)
	local c_abbrev = subinstr("`c_abbrev'", "/", "", .)
	local c_abbrev = subinstr("`c_abbrev'", "-", "", .)
	local c_abbrev = subinstr("`c_abbrev'", " ", "", .)
	di "Cause is `c' and its abbreviation is `c_abbrev'"
	putexcel B`iterator' = "`c'"
	putexcel C`iterator' = `ex_diff_female_`c_abbrev''
	putexcel D`iterator' = `ex_diff_male_`c_abbrev''
	putexcel E`iterator' = ((`mf_sexratio'*`ex_diff_male_`c_abbrev''+`ex_diff_female_`c_abbrev'')/(`mf_sexratio'+1))
	local iterator = `iterator' + 1
}

}
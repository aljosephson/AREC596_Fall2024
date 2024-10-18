/* BEGIN MAIN*/	

* Project: IRRI_Central_Luzon_Loop
* Created on: Jan 2021
* Created by: Ek
* Revised by: alj
* Revised on: 2 March 2023
* Stata v.16

* does
	* this file is the regression analysis for the market completeness tests
	* the regressions have been edited from lafave and thomas

* assumes
	* access to lafavethomasreplicate.dta 
	* demographics do not change within a "year", so seasons have same demographic traits

* TO DO:
	* complete
	
* **********************************************************************
* 0 - setup
* **********************************************************************

* define paths	

	loc		root	=		"$data/clean"
	loc		export	=		"$data/clean"
	loc		logout	= 		"$data/logs"
	loc 	tables 	= 		"$data/regression/tables"

* open log	
	cap log close
	log using "`logout'/lafavethomasreplicate", append
	
	use "`root'/lafavethomasreplicate.dta", clear
	
	set varabbrev off

************************************************************************* 
* 1 - determine recontact rate
************************************************************************* 
	
	gen 			year1970 = year if year == 1970	
	egen 			presentin1970 = max(year1970), by(hhid)
	
	gen 			year1979 = year if year == 1979
	egen 			presentin1979 = max(year1979), by(hhid)
	
* to determine the recontact rate in each year tab presentin1970 if year= a particular year to get the number of households who were present in 1970 and also present in a particluar year
	
	tab 			presentin1970 if year == 1986

	tab 			presentin1970 if year == 1999

	tab 			presentin1970 if year == 2016

	tab 			presentin1979 if year == 1990
		
	sum
	
************************************************************************* 
* 2 - count singleton observations
************************************************************************* 

* counting singleton observations

	sort 			hhid
	by 				hhid: generate nhhid = _N

	sort 			hhcommtime
	by 				hhcommtime: gen nhhcommtime = _N

************************************************************************* 
* 3 - local list of variables to include on RHS (Including TFSIZE)
************************************************************************* 

* household comp - preferred linear model and Deaton shares model

	local 			hhcomp 	m0014 m1519 m2034 m3549 m5064 mge65 f0014 f1519 f2034 f3549 f5064 fge65
	
	local 			hhshares hhsize_log	sm1519	sm2034 sm3549 sm5064 smge65 sf0014 sf1519 sf2034 sf3549 sf5064 sfge65 

* RHS control variables - all models include community-time FE (farmhhcommtime)
* panel models also include household-farm FE (farmhhid) 5456
	local 			rhs m_age m_educ f_age f_educ bottom15 mid70 top50 irrigated tfsize

************************************************************************* 
* 4 - select data and sample
************************************************************************* 

* properly xtset for farm-household FE;

	xtset 			hhid 
	
	est clear

************************************************************************* 
* 5 - TABLE 5 REGRESSIONS
************************************************************************* 

* estimates regressions and joint tests of demographics for all years 

************************************************************************* 
* 5a - pooled cross-sections
************************************************************************* 
* ----------------------------------------------------------------------*
* T5-1 (in L&T). Col 1 - linear age-gender bins specification 
* ----------------------------------------------------------------------*
*** table 5
		
	eststo:	  		reghdfe labor_d_log `hhcomp' `rhs', absorb(hhcommtime) vce(cluster hhid)  
	
*************************************************************************
*************************************************************************

/*
vce(cluster farmhhid) is the robust standard error command where standard errors are clustered at the farmhhid level. 
the observations are not completely independent at the farmhhid level, the farmhhid is the cluster, the observations may be independent accross clusters. 
the robust estimator weights the contribution to \delta L / \delta \beta from each n observation, assuming the observations are independent. 
When the observations are dependent within k clusters then the robust standard error forumla weights the contribution to \delta L / \delta \beta from each k cluster.

absorb(varname) specifies the categorical variable, which is to be included in the regression as if it were specified by dummy variable. 
we include community-time fixed effects.
*/
		
	test 			`hhcomp' 
	estadd 			scalar  F   = r(F), replace
	estadd 			scalar pv   = r(p), replace
	
	test 			m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 			scalar F_m = r(F), replace
	estadd 			scalar pv_m = r(p), replace
	
	test 			f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 			scalar F_f = r(F), replace
	estadd 			scalar pv_f = r(p), replace 
	
	test 			m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 			scalar F_p = r(F), replace
	estadd 			scalar pv_p = r(p), replace
				
*outreg2 using "`tables'/tab2.xls", keep(`hhcomp') adds( "F_m", `F_m', "pv-m", `pv_m', "F_f", `F_f', "pv_f", `pv_f', "F_page", `F_p', "pv-page", `pv_p', nobs, _N) ctitle(n. hh mem) dec(2) noaster noobs nor2 replace 

* ----------------------------------------------------------------------*			
* T5-2 (in L&t). Column 2 - Deaton/Benj shares specification 
* ----------------------------------------------------------------------*
*** WHAT IS OUR TABLE NUMBER? 
		
	eststo: 		reghdfe labor_d_log `hhshares' `rhs', absorb(hhcommtime) vce(cluster hhid)  			
			
	test 			`hhshares' 
	estadd 			scalar  F   = r(F), replace
	estadd 			scalar pv   = r(p), replace
	
	test 			sm1519 sm2034 sm3549 sm5064 smge65 
	estadd 			scalar F_m = r(F), replace
	estadd 			scalar pv_m = r(p), replace
	
	test 			sf0014 sf1519 sf2034 sf3549 sf5064 sfge65 
	estadd 			scalar F_f = r(F), replace
	estadd 			scalar pv_f = r(p), replace 
	
	test 			sm1519 sm2034 sm3549 sf1519 sf2034 sf3549 
	estadd 			scalar F_p = r(F), replace
	estadd 			scalar pv_p = r(p), replace

*outreg2 using "`tables'/tab2.xls", keep(`hhshares	') addstat( "F-dems", `F',   "pval", `pv', "F_m", `F_m', "pv-m", `pv_m', "F_f", `F_f', "pv_f", `pv_f', "F_page", `F_p', "pv-page", `pv_p', nobs, _N) ctitle(hhs size + shares) dec(2)  noaster noobs nor2 
						
************************************************************************* 
* 5b - including farm-level household fixed effects
************************************************************************* 

* ----------------------------------------------------------------------*
*T5-3 (in L&T). Column 3 - baseline panel model with farm-household fixed effects 
* ----------------------------------------------------------------------*

	eststo: 		reghdfe labor_d_log `hhcomp' `rhs', absorb(hhid hhcommtime) vce(cluster hhid) 
			 
	test 			`hhcomp' 
	estadd 			scalar  F   = r(F), replace
	estadd 			scalar pv   = r(p), replace
	
	test 			m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 			scalar F_m = r(F), replace
	estadd 			scalar pv_m = r(p), replace
	
	test 			f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 			scalar F_f = r(F), replace
	estadd 			scalar pv_f = r(p), replace 
	
	test 			m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 			scalar F_p = r(F), replace
	estadd 			scalar pv_p = r(p), replace
								
	*outreg2 		using "`tables'/tab2.xls", keep(`hhcomp') ///
	*					addstat("F-dems", `F',  "pval", `pv', "F_m", `F_m', "pv-m", `pv_m', "F_f", `F_f', "pv_f", `pv_f', "F_page", `F_p', "pv-page", `pv_p', nobs, _N) ///
	*					ctitle(n. hh mem) dec(2) noaster noobs nor2    
		
*  ************************************************************************* 
*  estout
*  *************************************************************************


	esttab, 	rename(sm1519 m1519 sm2034 m2034 sm3549 m3549 sm5064 m5064 smge65 mge65 sf0014 f0014 sf1519 f1519 sf2034 f2034 sf3549 f3549 sf5064 f5064 sfge65 fge65)	b(3) se(3) nomtitle star(* 0.10 ** 0.05 *** 0.01) ///
						drop(`rhs' _cons) stats(F pv F_m pv_m F_f pv_f F_p pv_p N,label("All Groups" "p-value" "Males" "p-value" "Females" "p-value" "Prime-age adults" "p-value" "N. observations")) ///
						label mgroups("Pooled Cross-Section" "Household Fixed Effects" , ///
						pattern(1 0 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
						
			
	#delimit ;

	esttab 			using "`tables'/lafavethomasreplicate.tex", replace
					rename(sm1519 m1519 sm2034 m2034 sm3549 m3549 sm5064 m5064 smge65
					mge65 sf0014 f0014 sf1519 f1519 sf2034 f2034 sf3549 f3549 sf5064 f5064 
					sfge65 fge65) 
					b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) nomtitle drop(`rhs' _cons)
					stats(F pv F_m pv_m F_f pv_f F_p pv_p N,label("All Groups" "p-value" 					"Males" "p-value" "Females" "p-value" "Prime-age adults" "p-value" 						"N. observations")) label booktabs

					prehead(
					\begin{table}[htbp] 
					\centering 
					\caption{Labor Demand (Log of Person Days Per Season) And Household Composition \label{lafavethomasreplicate-postwinsor}} 
					\scalebox{.85}
					{ \setlength{\linewidth}{.1cm}\newcommand{\contents}
					{\begin{tabular}{l*{3}{D{.}{.}{-1}}} \\
					[-1.8ex]\hline 
					\hline \\[-1.8ex])

					mgroups("Pooled Cross-Section" "Household Fixed Effects" ,
					pattern(1 0 1) prefix(\multicolumn{@span}{c}{) suffix(}) span 
					erepeat(\cmidrule(lr){@span}))
					
					postfoot(\bottomrule 

\multicolumn{4}{p{\linewidth}}{\footnotesize Note: Table replicates \cite{lafavethomas16}. Columns 1 and 3 measure demographic variables as counts of household members in the category. Column 2 measures household demographics as a share of household size in each age-sex category relative to share of household made up of males aged 0-14 years. Standard errors are in parentheses. Results of hypothesis testing with joint F-tests are presented at the bottom of the table. Prime-age adults are aged between 15-49. ( \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)).}\\
					\end{tabular}}                                          
					\setbox0=\hbox{\contents}                     
					\setlength{\linewidth}{\wd0-2\tabcolsep-.25em}                      
					\contents} 
					\label{tab:lafavethomasreplicate-postwinsor}
					\end{table});

	#delimit cr
	
**************************************************************************
* 6 - Split all households 
**************************************************************************
** EMIL'S HYPO, re: 1994	
************************************************************************** 
* 6a - before 1994 farm-household fixed effects
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 1991, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6b - after 1994 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 1991, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace	

	
**************************************************************************
* 6 - table code
**************************************************************************


/*
	
	esttab, 			b(3) se(3) nomtitle star(* 0.10 ** 0.05 *** 0.01) ///
							keep(`hhcomp') stats(F pv F_m pv_m F_f pv_f F_p pv_p N) ///
							label mgroups("1970-1991" "1994-2016", ///
							pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))

	esttab 				using "`tables'/split_reg_ownedland.tex", replace b(3) se(3) ///
							star(* 0.10 ** 0.05 *** 0.01) nomtitle keep(`hhcomp') ///
							stats(F pv F_m pv_m F_f pv_f F_p pv_p N) label booktabs alignment(D{.}{.}{-1}) ///
							title(Market Completeness in Distinct Periods \label{split_reg}) ///
							addnotes("The split occurs after 50% of land is owned for at least ten years") ///
							mgroups("1970-1991" "1994-2016", pattern(1 1) ///
							prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))			
	
*/	


	
		esttab, 	rename(sm1519 m1519 sm2034 m2034 sm3549 m3549 sm5064 m5064 smge65 mge65 sf0014 f0014 sf1519 f1519 sf2034 f2034 sf3549 f3549 sf5064 f5064 sfge65 fge65)	b(3) se(3) nomtitle star(* 0.10 ** 0.05 *** 0.01) ///
						drop(`rhs' _cons) stats(F pv F_m pv_m F_f pv_f F_p pv_p N,label("All Groups" "p-value" "Males" "p-value" "Females" "p-value" "Prime-age adults" "p-value" "N. observations")) ///
						label mgroups("Pooled Cross-Section" "Household Fixed Effects" "1970-1991" "1994-2016", ///
						pattern(1 0 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
						
			
			
	#delimit ;

	esttab 			using "`tables'/lafavethomasreplicate_sample_split.tex", replace
					rename(sm1519 m1519 sm2034 m2034 sm3549 m3549 sm5064 m5064 smge65
					mge65 sf0014 f0014 sf1519 f1519 sf2034 f2034 sf3549 f3549 sf5064 f5064 
					sfge65 fge65) 
					b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) nomtitle drop(`rhs' _cons)
					stats(F pv F_m pv_m F_f pv_f F_p pv_p N,label("All Groups" "p-value" 					"Males" "p-value" "Females" "p-value" "Prime-age adults" "p-value" 						"N. observations")) label booktabs

					prehead(
					\begin{table}[htbp] 
					\centering 
					\caption{Labor Demand (Log of Person Days Per Season) And Household Composition \label{lafavethomasreplicate-postwinsor}} 
					\scalebox{.85}
					{ \setlength{\linewidth}{.1cm}\newcommand{\contents}
					{\begin{tabular}{l*{5}{D{.}{.}{-1}}} \\
					[-1.8ex]\hline 
					\hline \\[-1.8ex]
					& & & &\multicolumn{2}{c}{Household Fixed Effects} \\\cmidrule(lr){5-6})
					
					mgroups("Pooled Cross-Section" "Household Fixed Effects" "1970-1991" "1994-2016",
					pattern(1 0 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span 
					erepeat(\cmidrule(lr){@span}))
					
					postfoot(\bottomrule 

\multicolumn{6}{p{\linewidth}}{\footnotesize Note: Table replicates \cite{lafavethomas16}. Columns 1 and 3 measure demographic variables as counts of household members in the category. Column 2 measures household demographics as a share of household size in each age-sex category relative to share of household made up of males aged 0-14 years. Standard errors are in parentheses. Results of hypothesis testing with joint F-tests are presented at the bottom of the table. Prime-age adults are aged between 15-49. ( \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)).}\\
					\end{tabular}}                                          
					\setbox0=\hbox{\contents}                     
					\setlength{\linewidth}{\wd0-2\tabcolsep-.25em}                      
					\contents} 
					\label{tab:lafavethomasreplicate_sample_split}
					\end{table});

	#delimit cr
	
**************************************************************************	
*** MORE TESTS BREAKING ALL YEAR PERIODS 		
************************************************************************** 
* 6c - before 1971 farm-household fixed effects
/* NOT POSSIBLE c and d - not enough observations (1970 has only 57, 1971 has only 11)
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 1971, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6d - after 1971 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 1971, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace		
	*/
	
************************************************************************** 
* 6e - before 1974 farm-household fixed effects
/* NOT POSSIBLE e and f not enough observations (1974 has only 49, 1975 has only 11)
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 1975, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6f - after 1974 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 1975, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace		
	*/
	
	
************************************************************************** 
* 6g - before 1980 farm-household fixed effects
/* RUNS BUT ONLY 74 obs, too small for any poewr 
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 1980, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6h - after 1980 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 1980, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace		
	*/	
		
************************************************************************** 
* 6i - before 1982 farm-household fixed effects
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 1982, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6j - after 1982 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 1982, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace		
				
************************************************************************** 
* 6k - before 1986 farm-household fixed effects
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 1986, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6l - after 1986 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 1986, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace			
	
				
************************************************************************** 
* 6m - before 1987 farm-household fixed effects
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 1987, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6n - after 1987 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 1987, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace			
	
************************************************************************** 
* 6o - before 1990 farm-household fixed effects
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 1990, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6p - after 1990 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 1990, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace		
	
*** 1991 (relative to 94?) not here - included at top 	
		
************************************************************************** 
* 6q - before 1994 farm-household fixed effects
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 1994, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6r - after 1994 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 1994, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace		
	
	
************************************************************************** 
* 6s - before 1995 farm-household fixed effects
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 1995, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6t - after 1995 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 1995, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace		
	
************************************************************************** 
* 6u - before 1998 farm-household fixed effects
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 1998, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6v - after 1998 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 1998, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace			
	
************************************************************************** 
* 6w - before 1999 farm-household fixed effects
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 1999, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6x - after 1999 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 1999, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace		
	
************************************************************************** 
* 6y - before 2003 farm-household fixed effects
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 2003, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6z - after 2003 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 2003, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace		
	
************************************************************************** 
* 6aa - before 2004 farm-household fixed effects
*** still runs from this point, some skept re: power
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 2004, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				
************************************************************************** 
* 6bb - after 2004 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 2004, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace			
	
************************************************************************** 
* 6cc - before 2007 farm-household fixed effects
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 2007, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6dd - after 2007 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 2007, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace				
	
************************************************************************** 
* 6ee - before 2008 farm-household fixed effects
/* insufficient obserations from here to end
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 2008, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6ff - after 2008 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 2008, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace				
		
************************************************************************** 
* 6gg - before 2011 farm-household fixed effects
**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 2011, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6hh - after 2011 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 2011, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace				

		
************************************************************************** 
* 6ii - before 2015 farm-household fixed effects
*** DOES NOT RUN INSUFFICIENT OBSERVATIONS

**************************************************************************
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 2015, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 6jj - after 2015 farm-household fixed effects
**************************************************************************
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 2015, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace			
*/
*** WON'T HAVE ENOUGH OBS POST 2016 ALONE	

* results of this included in GDRIVE: "f-test_over-time.xls"
* not going to include in final paper 
	
* ************************************************************************* 
* 7 - Table N/A Labor demand by task was not included in the thesis 
* *************************************************************************

* properly xtset for farm-household FE;

	xtset 			hhid 
	
	est clear
				
/*

vce(cluster farmhhid) is the robust standard error command where standard errors are clustered at the farmhhid level
the observations are not completely independent at the farmhhid level, the farmhhid is the cluster, the observations may be independent accross clusters 
independent and identically distributed is the condition for the unbiased standard error of the estimate 
the robust estimator weights the contribution to \delta L / \delta \beta from each n observation, assuming the observations are independent 
when the observations are dependent within k clusters then the robust standard error formula weights the contribution to \delta L / \delta \beta from each k cluster.

absorb(varname) specifies the categorical variable, which is to be included in the regression as if it were specified by dummy variable. We include community-time fixed effects

*/			
			
************************************************************************* 
* 7a - dependent variable: log of total labor demand BY TASK
************************************************************************* 

* ----------------------------------------------------------------------*
* I. labor d for weeding planting and fertilizing
* ----------------------------------------------------------------------*
* TABLE NUMBER?
* This regression was not included in the thesis


	eststo: 		reghdfe labor_d_wpf_log `hhcomp' `rhs', absorb(hhid hhcommtime) vce(cluster hhid)  
			 
	test 			`hhcomp' 
	estadd 			scalar  F   = r(F), replace
	estadd 			scalar pv   = r(p), replace
	
	test 			m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 			scalar F_m = r(F), replace
	estadd 			scalar pv_m = r(p), replace
	
	test 			f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 			scalar F_f = r(F), replace
	estadd 			scalar pv_f = r(p), replace 
	
	test 			m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 			scalar F_p = r(F), replace
	estadd 			scalar pv_p = r(p), replace		
													
* ----------------------------------------------------------------------*
* II. labor d for harvesting
* ----------------------------------------------------------------------*	
* TABLE NUMBER ?	
* this regression was not included in the thesis


	eststo: 		reghdfe labor_d_h_log `hhcomp' `rhs', absorb(hhid hhcommtime) vce(cluster hhid)  
			 
	test 			`hhcomp' 
	estadd 			scalar  F   = r(F), replace
	estadd 			scalar pv   = r(p), replace
	
	test 			m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 			scalar F_m = r(F), replace
	estadd 			scalar pv_m = r(p), replace
	
	test 			f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 			scalar F_f = r(F), replace
	estadd 			scalar pv_f = r(p), replace 
	
	test 			m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 			scalar F_p = r(F), replace
	estadd 			scalar pv_p = r(p), replace

* ----------------------------------------------------------------------*
* III. labor d for land prep
* ----------------------------------------------------------------------*	
* TABLE NUMBER?	
* This regression was not included in the thesis


	eststo: 		reghdfe labor_d_llvd_log `hhcomp' `rhs', absorb(hhid hhcommtime) vce(cluster hhid)  
			 
	test 			`hhcomp' 
	estadd 			scalar  F   = r(F), replace
	estadd 			scalar pv   = r(p), replace
	
	test 			m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 			scalar F_m = r(F), replace
	estadd 			scalar pv_m = r(p), replace
	
	test 			f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 			scalar F_f = r(F), replace
	estadd 			scalar pv_f = r(p), replace 
	
	test 			m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 			scalar F_p = r(F), replace
	estadd 			scalar pv_p = r(p), replace
	
**************************************************************************
* 7 - table code
**************************************************************************	

	
	
			esttab, 	rename(sm1519 m1519 sm2034 m2034 sm3549 m3549 sm5064 m5064 smge65 mge65 sf0014 f0014 sf1519 f1519 sf2034 f2034 sf3549 f3549 sf5064 f5064 sfge65 fge65)	b(3) se(3) nomtitle star(* 0.10 ** 0.05 *** 0.01) ///
						drop(`rhs' _cons) stats(F pv F_m pv_m F_f pv_f F_p pv_p N,label("All Groups" "p-value" "Males" "p-value" "Females" "p-value" "Prime-age adults" "p-value" "N. observations")) ///
						label mgroups("Weeding, Planting & Fertilizer" "Harvesting" "Land Preparation", ///
						pattern(1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
	
	
	
	
		#delimit ;

	esttab 			using "`tables'/labor_demand_by_task.tex", replace
					rename(sm1519 m1519 sm2034 m2034 sm3549 m3549 sm5064 m5064 smge65
					mge65 sf0014 f0014 sf1519 f1519 sf2034 f2034 sf3549 f3549 sf5064 f5064 
					sfge65 fge65) 
					b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) nomtitle drop(`rhs' _cons)
					stats(F pv F_m pv_m F_f pv_f F_p pv_p N,label("All Groups" "p-value" 					"Males" "p-value" "Females" "p-value" "Prime-age adults" "p-value" 						"N. observations")) label booktabs

					prehead(
					\begin{table}[htbp] 
					\centering 
					\caption{Labor Demand by Task (Log of Person Days Per Task Per Season) And Household Composition \label{lab_d_task}} 
					\scalebox{.93}
					{ \setlength{\linewidth}{.1cm}\newcommand{\contents}
					{\begin{tabular}{l*{4}{D{.}{.}{-1}}} \\
					[-1.8ex]\hline 
					\hline \\[-1.8ex])

					mgroups("Weeding, Planting \& Fertilizer" "Harvesting" "Land Preparation",
					pattern(1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span 
					erepeat(\cmidrule(lr){@span}))
					
					postfoot(\bottomrule 

\multicolumn{4}{p{\linewidth}}{\footnotesize Note: All columns measure demographic variables as counts of household members in the category.  Standard errors are in parentheses. Results of hypothesis testing with joint F-tests are presented at the bottom of the table. Prime-age adults are aged between 15-49. ( \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)).}\\
					\end{tabular}}                                          
					\setbox0=\hbox{\contents}                     
					\setlength{\linewidth}{\wd0-2\tabcolsep-.25em}                      
					\contents} 
					\label{tab:lab_d_task}
					\end{table});

	#delimit cr
	
************************************************************************* 
* 8 - labor demand by task over time 
************************************************************************* 	
* TESTING IF ACCESS TO MACHINES CHANGES MARKET COMPLETENESS
* 1982 is a good candidate for a cutoff point due to an increase in capital in that year
* SEE merge mkt completenss line 182 for the capital use graphs and data
		
	xtset 			hhid 
	
	est clear	
	
* ----------------------------------------------------------------------*
* I. labor d for weeding planting and fertilizing before 1991
* ----------------------------------------------------------------------*
* TABLE NUMBER?
* This regression was not included in the thesis


	eststo: 		reghdfe labor_d_wpf_log `hhcomp' `rhs' if year< 1982, absorb(hhid hhcommtime) vce(cluster hhid)  
			 
	test 			`hhcomp' 
	estadd 			scalar  F   = r(F), replace
	estadd 			scalar pv   = r(p), replace
	
	test 			m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 			scalar F_m = r(F), replace
	estadd 			scalar pv_m = r(p), replace
	
	test 			f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 			scalar F_f = r(F), replace
	estadd 			scalar pv_f = r(p), replace 
	
	test 			m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 			scalar F_p = r(F), replace
	estadd 			scalar pv_p = r(p), replace		
	
* ----------------------------------------------------------------------*
* II. labor d for weeding planting and fertilizing after 1991
* ----------------------------------------------------------------------*
* TABLE NUMBER?
* This regression was not included in the thesis


	eststo: 		reghdfe labor_d_wpf_log `hhcomp' `rhs' if year >= 1982, absorb(hhid hhcommtime) vce(cluster hhid)  
			 
	test 			`hhcomp' 
	estadd 			scalar  F   = r(F), replace
	estadd 			scalar pv   = r(p), replace
	
	test 			m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 			scalar F_m = r(F), replace
	estadd 			scalar pv_m = r(p), replace
	
	test 			f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 			scalar F_f = r(F), replace
	estadd 			scalar pv_f = r(p), replace 
	
	test 			m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 			scalar F_p = r(F), replace
	estadd 			scalar pv_p = r(p), replace		
													
* ----------------------------------------------------------------------*
* III. labor d for harvesting before 1991
* ----------------------------------------------------------------------*	
* TABLE NUMBER?	
* this regression was not included in the thesis


	eststo: 		reghdfe labor_d_h_log `hhcomp' `rhs' if year< 1982, absorb(hhid hhcommtime) vce(cluster hhid)  
			 
	test 			`hhcomp' 
	estadd 			scalar  F   = r(F), replace
	estadd 			scalar pv   = r(p), replace
	
	test 			m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 			scalar F_m = r(F), replace
	estadd 			scalar pv_m = r(p), replace
	
	test 			f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 			scalar F_f = r(F), replace
	estadd 			scalar pv_f = r(p), replace 
	
	test 			m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 			scalar F_p = r(F), replace
	estadd 			scalar pv_p = r(p), replace

* ----------------------------------------------------------------------*
* IV. labor d for harvesting after 1991
* ----------------------------------------------------------------------*	
* TABLE NUMBER?	
* this regression was not included in the thesis


	eststo: 		reghdfe labor_d_h_log `hhcomp' `rhs' if year>= 1982, absorb(hhid hhcommtime) vce(cluster hhid)  
			 
	test 			`hhcomp' 
	estadd 			scalar  F   = r(F), replace
	estadd 			scalar pv   = r(p), replace
	
	test 			m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 			scalar F_m = r(F), replace
	estadd 			scalar pv_m = r(p), replace
	
	test 			f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 			scalar F_f = r(F), replace
	estadd 			scalar pv_f = r(p), replace 
	
	test 			m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 			scalar F_p = r(F), replace
	estadd 			scalar pv_p = r(p), replace

* ----------------------------------------------------------------------*
* V. labor d for land prep before 1991
* ----------------------------------------------------------------------*	
* TABLE NUMBER?	
* This regression was not included in the thesis


	eststo: 		reghdfe labor_d_llvd_log `hhcomp' `rhs' if year< 1982, absorb(hhid hhcommtime) vce(cluster hhid)  
			 
	test 			`hhcomp' 
	estadd 			scalar  F   = r(F), replace
	estadd 			scalar pv   = r(p), replace
	
	test 			m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 			scalar F_m = r(F), replace
	estadd 			scalar pv_m = r(p), replace
	
	test 			f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 			scalar F_f = r(F), replace
	estadd 			scalar pv_f = r(p), replace 
	
	test 			m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 			scalar F_p = r(F), replace
	estadd 			scalar pv_p = r(p), replace
	
* ----------------------------------------------------------------------*
* VI. labor d for land prep
* ----------------------------------------------------------------------*	
* TABLE NUMBER?	
* This regression was not included in the thesis


	eststo: 		reghdfe labor_d_llvd_log `hhcomp' `rhs' if year>= 1982, absorb(hhid hhcommtime) vce(cluster hhid)  
			 
	test 			`hhcomp' 
	estadd 			scalar  F   = r(F), replace
	estadd 			scalar pv   = r(p), replace
	
	test 			m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 			scalar F_m = r(F), replace
	estadd 			scalar pv_m = r(p), replace
	
	test 			f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 			scalar F_f = r(F), replace
	estadd 			scalar pv_f = r(p), replace 
	
	test 			m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 			scalar F_p = r(F), replace
	estadd 			scalar pv_p = r(p), replace
	
	
**************************************************************************
* 8 - table code
**************************************************************************	
	
			esttab, 	rename(sm1519 m1519 sm2034 m2034 sm3549 m3549 sm5064 m5064 smge65 mge65 sf0014 f0014 sf1519 f1519 sf2034 f2034 sf3549 f3549 sf5064 f5064 sfge65 fge65)	b(3) se(3) nomtitle star(* 0.10 ** 0.05 *** 0.01) ///
						drop(`rhs' _cons) stats(F pv F_m pv_m F_f pv_f F_p pv_p N,label("All Groups" "p-value" "Males" "p-value" "Females" "p-value" "Prime-age adults" "p-value" "N. observations")) ///
						label mgroups("Weeding, Planting & Fertilizer" "Harvesting" "Land Preparation", ///
						pattern(1 1 1 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
	
		
		#delimit ;

	esttab 			using "`tables'/labor_demand_by_task.tex", replace
					rename(sm1519 m1519 sm2034 m2034 sm3549 m3549 sm5064 m5064 smge65
					mge65 sf0014 f0014 sf1519 f1519 sf2034 f2034 sf3549 f3549 sf5064 f5064 
					sfge65 fge65) 
					b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) nomtitle drop(`rhs' _cons)
					stats(F pv F_m pv_m F_f pv_f F_p pv_p N,label("All Groups" "p-value" 					"Males" "p-value" "Females" "p-value" "Prime-age adults" "p-value" 						"N. observations")) label booktabs

					prehead(
					\newgeometry{margin=1cm} 
					\begin{landscape}
					\begin{table}[htbp] 
					\centering 
					\caption{Labor Demand by Task (Log of Person Days Per Task Per Season) Over Time \label{lab_d_task_time}} 
					\scalebox{.81}
					{ \setlength{\linewidth}{.1cm}\newcommand{\contents}
					{\begin{tabular}{l*{6}{D{.}{.}{-1}}} \\
					[-1.8ex]\hline 
					\hline \\[-1.8ex]
					& \multicolumn{2}{c}{Weeding, Planting \& Fertilizer} & \multicolumn{2}{c}{Harvesting} & \multicolumn{2}{c}{Land Preparation} \\\cmidrule(lr){2-7})

					mgroups("1970-1980" "1982-2016" "1970-1980" "1982-2016" "1970-1980" "1982-2016",
					pattern(1 1 1 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span 
					erepeat(\cmidrule(lr){@span}))
					
					postfoot(\bottomrule 

\multicolumn{7}{p{\linewidth}}{\footnotesize Note: All columns measure demographic variables as counts of household members in the category.  Standard errors are in parentheses. Results of hypothesis testing with joint F-tests are presented at the bottom of the table. Prime-age adults are aged between 15-49. ( \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)).}\\
					\end{tabular}}                                          
					\setbox0=\hbox{\contents}                     
					\setlength{\linewidth}{\wd0-2\tabcolsep-.25em}                      
					\contents} 
					\label{tab:lab_d_task_time}
					\end{table}
					\end{landscape}
					\restoregeometry);

	#delimit cr	
	
************************************************************************* 
* 9 - empirical search for completeness in different year windows 
************************************************************************* 	
*** 1970 to 1982 is the first time interval where there are enough observations to run a regression. 1970 to any year less than 1982 does not have enough obervations to run a regression.

	loc		root	=		"$data/clean"
	loc		export	=		"$data/clean"
	loc		logout	= 		"$data/logs"
	loc 	tables 	= 		"$data/regression/tables"
	
	xtset 			hhid 
	
	est clear	
	
* household comp - preferred linear model and Deaton shares model

	local 			hhcomp 	m0014 m1519 m2034 m3549 m5064 mge65 f0014 f1519 f2034 f3549 f5064 fge65
	
	local 			hhshares hhsize_log	sm1519	sm2034 sm3549 sm5064 smge65 sf0014 sf1519 sf2034 sf3549 sf5064 sfge65 

* RHS control variables - all models include community-time FE (farmhhcommtime)
* panel models also include household-farm FE (farmhhid) 5456
	local 			rhs m_age m_educ f_age f_educ bottom15 mid70 top50 irrigated tfsize
	
*****************************************************************************			
	*regression loop
*****************************************************************************
	
	*** locals for men, women, working age
	local men				m0014 m1519 m2034 m3549 m5064 mge65 
	local women				f0014 f1519 f2034 f3549 f5064 fge65 
	local workingage		m1519 m2034 m3549 f1519 f2034 f3549 
	
	
	***name the temporary file
	tempfile reg_results_loop
	
	*** postfile is the command. Name the file, list the names of the variables in the dataset, and then where it will be stored
	postutil clear
	postfile reg_results_loop startyr endyear hhcomp_p men_p women_p workingage_p observations using "`tables'/reg_results_loop.dta", replace	
	

	*** labeled the years with a number. Forval loops through each number between 1 and 14. where (1) means between/to
	forval j =1982(1)2016 {
		
				*** do the regression. Note we go from year 1 to year j
				reghdfe labor_d_llvd_log `hhcomp' `rhs' if year >= 1970 & year <= `j', absorb(hhid hhcommtime) vce(cluster hhid)  
					local obs = e(N)
					*** f-test the variable groupings (pmen, pwomen, phhcomp) for their significance
					test `men'
					*** last f-test is stored in r(p). Copy observation in r(p) into a local called pmen, phhcomp etc
					local pmen = r(p)
					
					test `women'
					local pwomen = r(p)
					test  `workingage'
					local pworkingage = r(p)
					test `hhcomp'
					local phhcomp =r(p)
				
			*** post command adds a new observation to dataset		
			post reg_results_loop (1970) (`j') (`phhcomp') (`pmen') (`pwomen') (`pworkingage') (`obs')
			
	*** close loop
	}
	
	forval j =2015(-1)1970 {
			* the first interval that has sufficient observations is 2016-2015

					reghdfe labor_d_llvd_log `hhcomp' `rhs' if year >= `j' & year <= 2016, absorb(hhid hhcommtime) vce(cluster hhid)  
					
					local obs = e(N)
					
					*** f-test the variable groupings (pmen, pwomen, phhcomp) for their significance
					test `men'
					*** last f-test is stored in r(p). Copy observation in r(p) into a local called pmen, phhcomp etc
					local pmen = r(p)
					
					test `women'
					local pwomen = r(p)
					test  `workingage'
					local pworkingage = r(p)
					test `hhcomp'
					local phhcomp =r(p)
				
			*** post command adds a new observation to dataset		
			post reg_results_loop (`j') (2016) (`phhcomp') (`pmen') (`pwomen') (`pworkingage') (`obs')
	}
	

	*** close post
	postclose reg_results_loop		
	
	use "`tables'\reg_results_loop.dta", clear
	
	duplicates drop hhcomp_p men_p women_p workingage_p, force
	
	drop if hhcomp_p ==. & men_p == . & women_p== . & workingage_p== .
	
************************************************************************* 
* 10 - explore land ownership split sample 
************************************************************************* 

************************************************************************* 
* 10a - create and examine land ownership variables 
* table 3 and table 4
************************************************************************* 

use "`root'/lafavethomasreplicate.dta", clear
	

* create a variable showing which households own land

	sort 			hhid 
	
	replace 		ownedland = 0 if ownedland == .

	by 				hhid: gen ownedland10 = 1 if year < 1984 & ownedland > 0
	
	sort 			hhid
	
	egen 			ownedland10allhh = max(ownedland10), by(hhid)
	
	replace 		ownedland10allhh = 0 if ownedland10allhh == .


***********************************************************************
* Mann Whitney test on households before 1994
***********************************************************************
* table 3
***********************************************************************
* examine differences between households with and without ownership 

	gen 			ownedland10allhh1994 = 1 if ownedland10allhh == 1 & year < 1994
	replace 		ownedland10allhh1994 = 0 if ownedland10allhh1994 == . & year < 1994

	local i = 0

	foreach 		v of var m0014 m1519 m2034 m3549 m5064 mge65 ///
								f0014 f1519 f2034 f3549 f5064 fge65 ///
								hhsize_log labor_d_log tfsize m_age m_educ f_age f_educ { 
    local ++i
    quietly 		ranksum `v', by(ownedland10allhh1994) porder 
    scalar 			pval = 2*normprob(-abs(r(z)))
    
    if `i' == 1 {
      display %20s "P-Value", %5s "Obs. Without Unrestricted Title", %5s "Obs. With Unrestricted Title"
      display ""
    }

    display "`v'{col 14}" %05.3f pval ///
    "   " %05.3f r(N_1)  "   " %05.3f r(N_2)
} 

***********************************************************************
* Mann Whitney test on households after 1994
***********************************************************************
* table 4
***********************************************************************

	gen 			ownedland10allhhpost1994 = 1 if ownedland10allhh == 1 & year >= 1994
	replace 		ownedland10allhhpost1994 = 0 if ownedland10allhhpost1994 == . & year >= 1994


	local i = 0

	foreach 		v of var m0014 m1519 m2034 m3549 m5064 mge65 ///
								f0014 f1519 f2034 f3549 f5064 fge65 ///
								hhsize_log labor_d_log tfsize m_age m_educ f_age f_educ { 
    local ++i
    quietly 		ranksum `v', by(ownedland10allhhpost1994) porder 
    scalar 			pval = 2*normprob(-abs(r(z)))
    
    if `i' == 1 {
      display %20s "P-Value", %5s "Obs. Without Unrestricted Title", %5s "Obs. With Unrestricted Title"
      display ""
    }

    display "`v'{col 14}" %05.3f pval ///
    "   " %05.3f r(N_1)  "   " %05.3f r(N_2)
} 

************************************************************************** 
* 11 - set locals and data
************************************************************************** 

* household comp - preferred linear model and Deaton shares model

	local 			hhcomp 	m0014 m1519 m2034 m3549 m5064 mge65 f0014 f1519 f2034 f3549 f5064 fge65
	
	local 			hhshares hhsize_log		 sm1519	sm2034 sm3549 sm5064 smge65 sf0014 sf1519 sf2034 sf3549 sf5064 sfge65 

* RHS control variables - all models include community-time FE (farmhhcommtime)
* panel models also include household-farm FE (farmhhid) 

	local 			rhs m_age m_educ f_age f_educ bottom15 mid70  top50  tfsize irrigated

* SPLIT 1: 1970 to 1991 
* SPLIT 2: after 1991

* properly xtset for farm-household FE;
	xtset hhid 
	
* clear stored estimates
	est clear
	
**************************************************************************
* 11 - split sample regressions
************************************************************************** 
*Table 6

**************************************************************************
* 11a - no owned land before 1994 farm-household fixed effects 
**************************************************************************
*T6-1
	
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 1991 & ownedland10allhh == 0 , absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
	
************************************************************************** 
* 11b - no one owned land after 1994 farm-household fixed effects
**************************************************************************
* T6-2
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 1991 & ownedland10allhh == 0, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace

************************************************************************** 
* 11c - owned land before 1994 farm-household fixed effects
**************************************************************************
*T6-3
		
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year <= 1991 & ownedland10allhh == 1, absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace
				

************************************************************************** 
* 11d - owned land after 1994 farm-household fixed effects
**************************************************************************
*T6-4
			
	eststo: 			reghdfe labor_d_log `hhcomp' `rhs' if year > 1991 & ownedland10allhh == 1 , absorb(hhid hhcommtime) vce(cluster hhid) 
			
	test 				`hhcomp' 
	estadd 				scalar  F   = r(F), replace
	estadd 				scalar pv   = r(p), replace
	
	test 				m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 				scalar F_m = r(F), replace
	estadd 				scalar pv_m = r(p), replace
	
	test 				f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 				scalar F_f = r(F), replace
	estadd 				scalar pv_f = r(p), replace 
	
	test 				m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 				scalar F_p = r(F), replace
	estadd 				scalar pv_p = r(p), replace			
	

**************************************************************************
* 11 - table code
**************************************************************************		
	
	#delimit ;
	
	
	esttab 			using "`tables'/split_sample_by_ownership.tex", replace
					rename(sm1519 m1519 sm2034 m2034 sm3549 m3549 sm5064 m5064 smge65
					mge65 sf0014 f0014 sf1519 f1519 sf2034 f2034 sf3549 f3549 sf5064 f5064 
					sfge65 fge65) 
					b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) nomtitle drop(`rhs' _cons)
					stats(F pv F_m pv_m F_f pv_f F_p pv_p N,label("All Groups" "p-value" 					"Males" "p-value" "Females" "p-value" "Prime-age adults" "p-value" 						"N. observations")) label booktabs

					prehead(
					\begin{table}[htbp] 
					\centering 
					\caption{Split regressions between land owners and non-landowners over time \label{split_sample_ownership}} 
					\scalebox{.82}
					{ \setlength{\linewidth}{.1cm}\newcommand{\contents}
					{\begin{tabular}{l*{4}{D{.}{.}{-1}}} \\
					[-1.8ex]\hline 
					\hline \\[-1.8ex]
					&\multicolumn{2}{c}{Households Without Unrestricted Title in 1994}&\multicolumn{2}{c}{Households With Unrestricted Title in 1994}\\\cmidrule(lr){2-3}\cmidrule(lr){4-5})

					mgroups("1970-1991" "1994-2016" "1970-1991" "1994-2016",
					pattern(1 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span 
					erepeat(\cmidrule(lr){@span}))
					
					postfoot(\bottomrule 

\multicolumn{5}{p{\linewidth}}{\footnotesize Note: The split occurs after 50\% of the land is owned for at least ten years. Column 1 is estimated on data from 1970 to 1991. Column 2 is estimated on all data from 1994 to 2016. All columns are estimated with the full specification from \ref{eq:empiricalmodel}. Standard errors are clustered at the household level and are reported parentheses. The demographic variables in both columns are counts of the number of household members. F tests are presented at the bottom of the table. Prime-age adults are 15-49 years old.( \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)).}\\
					\end{tabular}}                                          
					\setbox0=\hbox{\contents}                     
					\setlength{\linewidth}{\wd0-2\tabcolsep-.25em}                      
					\contents} 
					\label{tab:split_sample_ownership}
					\end{table});

	#delimit cr

	
			
**************************************************************************
* 12 - credit share as dependent variable 
**************************************************************************
* Table 7
************************************************************************** 

	est clear

	replace 			ownedland = 1 if ownedland >0
	gen 				after1991 = 1 if year >=1994
	replace 			after1991 = 0 if after1991 != 1
	replace 			ownedland = 0 if ownedland != 1
			

	reghdfe 			creditshareofexp tfsize irrigated i.ownedland10allhh##i.after1991, absorb(hhcommtime) vce(cluster hhid)

	
	
	
				esttab,	b(3) se(3) nomtitle star(* 0.10 ** 0.05 *** 0.01) label
					
					
					
					
					
		#delimit ;
	
	
	esttab 			using "`tables'/credit_share_exp.tex", replace
					b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) nomtitle
					 label booktabs

					prehead(
					\begin{table}[htbp] 
					\centering 
					\caption{Split regressions between land owners and non-landowners over time \label{split_sample_ownership}} 
					\scalebox{.82}
					{ \setlength{\linewidth}{.1cm}\newcommand{\contents}
					{\begin{tabular}{l*{1}{D{.}{.}{-1}}} \\
					[-1.8ex]\hline 
					\hline \\[-1.8ex])
					

					
					postfoot(\bottomrule 

\multicolumn{5}{p{\linewidth}}{\footnotesize Note: \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\).}\\
					\end{tabular}}                                          
					\setbox0=\hbox{\contents}                     
					\setlength{\linewidth}{\wd0-2\tabcolsep-.25em}                      
					\contents} 
					\label{tab:credit_share_exp}
					\end{table});

	#delimit cr
	
/* END MAIN */

************************************************************************** 
************************************************************************** 
************************************************************************** 
************************************************************************** 
									
/* BEGIN APPENDIX */		
	
************************************************************************** 
* APPENDIX  
************************************************************************** 

* testing if the results are sensitive to matching demographics to farm characteristics during seasons where farm characteristics were collected but demographics were not			
			
* precisely match demographic data 

	clear all 


	use "`root'/lafavethomasreplicate_match_demographics"
	
************************************************************************** 
* AP1 - local list of variables to include on RHS
************************************************************************** 
 
* household comp - preferred linear model and Deaton shares model;
	local 			hhcomp m0014 m1519 m2034 m3549 m5064 mge65 f0014 f1519 f2034 f3549 f5064 fge65
	
	local 			hhshares hhsize_log sm1519	sm2034 sm3549 sm5064 smge65 sf0014 sf1519 sf2034 sf3549 sf5064 sfge65 

* RHS control variables - all models include community-time FE (farmhhcommtime)
* panel models also include household-farm FE (farmhhid) 
	local 			rhs m_age m_educ f_age f_educ bottom15 mid70  top50 irrigated tfsize

*************************************************************************
*************************************************************************	
	
	*** not include irrigated? or tfsize? 
	*** local from above: 	local rhs m_age m_educ f_age f_educ bottom15 mid70 top50 irrigated tfsize
	*** YES we should include tfsize and irrigated
	*** It was a mistake to not include them before
					
************************************************************************** 
* AP2 - select data and sample
************************************************************************** 
 
* properly xtset for farm-household FE 
	xtset hhid 
			
************************************************************************** 
* AP3 - TABLE 2 REGS
************************************************************************** 

* estimates regressions and joint tests of demographics
					
************************************************************************** 
* AP2a - pooled cross sections 
************************************************************************** 
  
* ----------------------------------------------------------------------*
* TA1-1. Col 1 - linear age-gender bins specification 
* ----------------------------------------------------------------------*

/*
* ORIGINAL CODE - NOT WORKING 
	reghdfe 		labor_d_log `hhcomp' `rhs', absorb(hhcommtime) vce(cluster hhid)  ;


	test 			`hhcomp' ; 	 				
	local 			F   = r(F) ; local pv   = r(p) ;
	test 			m0014 m1519 m2034 m3549 m5064 mge65 ; local F_m = r(F) ; local pv_m = r(p) ;
	test 			f0014 f1519 f2034 f3549 f5064 fge65 ; local F_f = r(F) ; local pv_f = r(p) ;
	test 			m1519 m2034 m3549 f1519 f2034 f3549 ; local F_p = r(F) ; local pv_p = r(p) ; 
*/
	
*************************************************************************
*************************************************************************

	eststo: 		reghdfe labor_d_log `hhcomp' `rhs', absorb(hhcommtime) vce(cluster hhid)
			 
	test 			`hhcomp' 
	estadd 			scalar  F   = r(F), replace
	estadd 			scalar pv   = r(p), replace
	
	test 			m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 			scalar F_m = r(F), replace
	estadd 			scalar pv_m = r(p), replace
	
	test 			f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 			scalar F_f = r(F), replace
	estadd 			scalar pv_f = r(p), replace 
	
	test 			m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 			scalar F_p = r(F), replace
	estadd 			scalar pv_p = r(p), replace
	
	*outreg2 		using "`tables'/appendixtab2.xls", keep(`hhcomp') ///
	*					addstat( "F_m", `F_m', "pv-m", `pv_m', "F_f", `F_f', "pv_f", `pv_f', "F_page", `F_p', "pv-page", `pv_p') ///
	*					ctitle(n. hh mem) dec(2) noaster noobs nor2 replace ;
	 
* ----------------------------------------------------------------------*
* TA1-2. Column 2 - Deaton/Benj shares specification  
* ----------------------------------------------------------------------*

/*
* ORIGINAL CODE - NOT WORKING 
	reghdfe 		labor_d_log `hhshares' `rhs', absorb(hhcommtime) vce(cluster hhid)  ;

	test 			`hhshares' ; 	 							 local F   = r(F) ; local pv   = r(p) ;
	test 			sm1519 sm2034 sm3549 sm5064 smge65 ; local F_m = r(F) ; local pv_m = r(p) ;
	test 			sf0014 sf1519 sf2034 sf3549 sf5064 sfge65 ; local F_f = r(F) ; local pv_f = r(p) ;
	test 			sm1519 sm2034 sm3549 sf1519 sf2034 sf3549 ; local F_p = r(F) ; local pv_p = r(p) ; 
*/ 
	
*************************************************************************
*************************************************************************

	eststo: 		reghdfe labor_d_log `hhshares' `rhs', absorb(hhcommtime) vce(cluster hhid)  
			 
	test 			`hhshares' 
	estadd 			scalar  F   = r(F), replace
	estadd 			scalar pv   = r(p), replace
	
	test 			sm1519 sm2034 sm3549 sm5064 smge65 
	estadd 			scalar F_m = r(F), replace
	estadd 			scalar pv_m = r(p), replace
	
	test 			sf0014 sf1519 sf2034 sf3549 sf5064 sfge65 
	estadd 			scalar F_f = r(F), replace
	estadd 			scalar pv_f = r(p), replace 
	
	test 			sm1519 sm2034 sm3549 sf1519 sf2034 sf3549 
	estadd 			scalar F_p = r(F), replace
	estadd 			scalar pv_p = r(p), replace	
	

	*outreg2 		using "`tables'/appendixtab2.xls", keep(`hhshares') ///
	*					addstat( "F-dems", `F', "pval",	`pv', "F_m", `F_m', "pv-m", `pv_m', "F_f", `F_f', "pv_f", `pv_f', "F_page", `F_p', "pv-page", `pv_p', nobs, _N) ///
	*					ctitle(hhs size + shares) dec(2)  noaster noobs nor2 ;

************************************************************************** 
* AP2a - including farm-household fixed effects
************************************************************************** 

* ----------------------------------------------------------------------*
* TA1-3. Column 3 - baseline panel model with farm-household fixed effects
* ----------------------------------------------------------------------*

/*
* ORIGINAL CODE - NOT WORKING 
	reghdfe 		labor_d_log `hhcomp' `rhs', absorb(hhid hhcommtime) vce(cluster hhid) ;

	test 			`hhcomp' ; 	local F   = r(F) ; local pv   = r(p) ;
	test 			m0014 m1519 m2034 m3549 m5064 mge65 ; local F_m = r(F) ; local pv_m = r(p) ;
	test 			f0014 f1519 f2034 f3549 f5064 fge65 ; local F_f = r(F) ; local pv_f = r(p) ;
	test 			m1519 m2034 m3549 f1519 f2034 f3549 ; local F_p = r(F) ; local pv_p = r(p) ; 
*/
	
*************************************************************************
*************************************************************************

	eststo: 		reghdfe labor_d_log `hhcomp' `rhs', absorb(hhid hhcommtime) vce(cluster hhid)  
			 
	test 			`hhcomp' 
	estadd 			scalar  F   = r(F), replace
	estadd 			scalar pv   = r(p), replace
	
	test 			m0014 m1519 m2034 m3549 m5064 mge65 
	estadd 			scalar F_m = r(F), replace
	estadd 			scalar pv_m = r(p), replace
	
	test 			f0014 f1519 f2034 f3549 f5064 fge65 
	estadd 			scalar F_f = r(F), replace
	estadd 			scalar pv_f = r(p), replace 
	
	test 			m1519 m2034 m3549 f1519 f2034 f3549 
	estadd 			scalar F_p = r(F), replace
	estadd 			scalar pv_p = r(p), replace

	*outreg2 		using "`tables'/appendixtab2.xls", keep(`hhcomp') ///
	*					addstat( "F-dems", `F', "pval", `pv', "F_m", `F_m', "pv-m", `pv_m', "F_f", `F_f', "pv_f", `pv_f', "F_page", `F_p', "pv-page", `pv_p', nobs, _N) ///
	*					ctitle(n. hh mem) dec(2) noaster noobs nor2   ;
										
/* END APPENDIX */						
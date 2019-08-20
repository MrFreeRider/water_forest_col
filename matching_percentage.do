*****************************************************
** Matching Percentage
*****************************************************

cd "/Users/mrfreerider/Documents/Research/Natural Capital & Water/more tree better water"

use data_forest_merge.dta, clear
label var arm_group "# Armed-groups attacks"

*******************************************************
** Summary statistics for Forest/WSS information
// Summary Forest/WSS

preserve
tab org_type, gen(orgtype)
label var orgtype1 "Public utility"
label var orgtype2 "Municipality"
label var orgtype3 "Community"
label var orgtype4 "Private utility"

eststo clear
estpost su  id_wss id_mpio id_dpto id_szh  ///
user_res_year orgtype1 orgtype2 orgtype3 orgtype4  vc fc pc n_cap /// 
y_pc arm_group ///
stableforest1 deforested1 reforested1 noforest1 p_deforest1 per_sa_1km stableforest3 deforested3 reforested3 noforest3 p_deforest3 per_sa_3km stableforest5 deforested5 reforested5 noforest5 p_deforest5 per_sa_5km  
esttab using sum_data.tex, label replace cells("mean(label(Mean) fmt(a1)) sd(label(Std. Dev.)) min(label(Min.)) max(label(Max.)) count(label(Obs.))") refcat(user_res_year "\textbf{\emph{WSS}}" turbacued "\textbf{\emph{Municipality }}" stableforest1 "\textbf{\emph{WCA 1km}}" stableforest3 "\textbf{\emph{WCA 3km}}" stableforest5 "\textbf{\emph{WCA 5km}}" id_wss "\textbf{\emph{General}}"  , nolabel) noobs nonumber booktabs 

export delimited "/Users/mrfreerider/Documents/Research/Natural Capital & Water/more tree better water/data_forest_merge.csv", nolabel replace
restore

*******************************************************
** Cleaning the data
** Variables of forest cover averaged by period / Catchment

drop stableforest1-cachtment1 stableforest3 stableforest3-cachtment5
sort id_wss period

forv i=1(2)5{
bys period: egen forest_period`i'=mean(stforest_wss`i')
label var forest_period`i' "Average Forest Cover `i'km (ha)"
bys id_wss: egen forest_mpre`i'=mean(stforest_wss`i') if period <=6
label var forest_mpre`i' "Average Forest Cover `i'km (ha) Baseline"
bys id_wss: egen catchment`i'=mean(cachtment`i') 
label var catchment`i' "WCA `i' km (ha)"
}

**  Generate the treatment variable. We consider a WCA treated if the 10% of ist area is covered by natural forest.
forv j=1(2)5{
gen ftreat`j'=1 if (forest_mpre`j'/catchment`j')>0.1 & forest_mpre`j'!=.
replace ftreat`j'=0 if ftreat`j'==.
label var ftreat`j' ">% forested WCA"
}




** Averaging covariates of interest  at WSS level
gen vc_report=0  
replace vc_report=1 if vc==.

global mean_var vc_report vc fc pc user_res_year res_inv y_total y_pc mean_prec_wss mean_temp_wss arm_group
foreach x in $mean_var{
bys id_wss: egen `x'_mpre=mean(`x') if period <=6
}

bys id_wss: egen vc_mpost=mean(vc) if period>6
replace vc_mpost=vc_mpost[_n+3] if vc_mpost==.
replace vc_mpost=vc_mpost[_n+3] if vc_mpost==.

drop 





save "/Users/mrfreerider/Documents/Research/Natural Capital & Water/more tree better water/gen_matching/panel.forest.cost.dta", replace


** Keep the variable of interest and only the last time period at the baseline.

keep catchment1 catchment3 catchment5 mean_prec_wss_mpre mean_temp_wss_mpre ///
forest_mpre1 forest_mpre3 forest_mpre5 ///
special_area_1km special_area_3km special_area_5km  dist_reaa_mean prot_area ///
slope1km slope3km slope5km  eleva1km eleva3km eleva5km ///
vc vc_report vc_report_mpre vc_mpre vc_mpost fc_mpre pc_mpre  org_type org_loc n_cap  ///
dist_road_mean dist_min_mean dist_coca_mean dist_capital_mean areaoficialkm2 y_pc_mpre  arm_group_mpre id_wss id_dpto id_mpio id_ah id_zh id_szh period    


** Reshape wide
sort id_wss period

global vardist dist_road_mean dist_min_mean dist_coca_mean dist_capital_mean dist_reaa_mean forest_mpre1 forest_mpre3 forest_mpre5 vc_report_mpre vc_mpre fc_mpre pc_mpre y_pc_mpre mean_prec_wss_mpre mean_temp_wss_mpre arm_group_mpre
foreach x in $vardist{
bys id_wss: replace `x'=`x'[_n-1] if `x'==.
}
reshape wide vc vc_report, i(id_wss) j(period)



forv j=1(2)5{
gen streat`j'=1 if (special_area_`j'km/catchment`j')>0.05
replace streat`j'=0 if streat`j'==.
label var streat`j' "% Special WCA"
}

** Defining the covariate variables of interest. We include: 1) variables that are related with the possible disturbances; in Colombia we observe the economic activities such as agriculture, cattle, mining. Also we include in this group the distance to capital, under the arguent that the farthest the inteakes, the more conservated it is.  2) Protected area indicators: We use three variable, if the intakes is in a protected area (at different administrative level); the categories of the protected areas; and the distance to ecosystems considered as strategically important to maintain ecosystems services. Finally, 3) Characteristics of the water supply systems: the tyoe of organization and the fixed cost,as a proxy of the level of complexity.


** First we analyze the differences in the covariates by treatment status:

rename prot_area pa
rename vc_mpre vc_pre
rename vc_mpost vc_pos
rename fc_mpre fc_pre
rename dist_reaa_mean d_se
rename dist_road_mean d_road
rename dist_coca_mean d_coke
rename dist_capital_mean d_cap
rename dist_min_mean d_min
rename mean_prec_wss_mpre rain_pre
rename mean_temp_wss_mpre temp_pre
rename y_pc_mpre y_pc
rename areaoficialkm2 area_mp
rename pc_mpre pc_pre
renam arm_group_mpre arm_att

label var vc1 "Variable Cost WSS 1990-2000"
label var vc2 "Variable Cost WSS 2000-2005"
label var vc3 "Variable Cost WSS 2005-2010"
label var vc4 "Variable Cost WSS 2010-2012"
label var vc5 "Variable Cost WSS 2012-2013"
label var vc6 "Variable Cost WSS 2013-2014"
label var vc7 "Variable Cost WSS 2014-2015"
label var vc8 "Variable Cost WSS 2015-2016"
label var vc9 "Variable Cost WSS 2016-2017"
label var vc_pre "Cost (2004-2014)"
label var vc_pos"Cost (2014-2017)"
label var fc_pre "Management Cost (2004-2014)"
label var pc_pre "Production Cost (2004-2014)"
label var y_pc "Municipal revenue (COP/percapita)"

label var pa "Protected Area"

label var d_se "D. Strategic Ecosystems (km)"
label var d_road "D. main roads (km)"
label var d_coke "D. Coke Crops (Km)"
label var d_min "D. mine projects (Km)"
label var d_cap "D. capital city (Km)"

label var rain_pre "Avg. Rainfall (mm/yr)(2004-2014)"

label var temp_pre "Avg. Temperature (mm/yr)(2004-2014)"
label var area_mp "Municipal Area (km2)"
label var arm_att "Violent attacks (\#)"
label var slope1km "Avg. Slope WCA1km"
label var slope3km "Avg. Slope WCA3km"
label var slope5km "Avg. Slope WCA5km"
label var eleva1km "Avg. Elevation WCA1km (m)"
label var eleva3km "Avg. Elevation WCA3km (m)"
label var eleva5km "Avg. Elevation WCA5km (m)"


label var ftreat1 "Has >\% in Natural forest 1km"
label var ftreat3 "Has >\% in Natural forest 3km"
label var ftreat5 "Has >\% in Natural forest 5km"



global cov_var vc_pre vc2 vc3 vc4 vc5 pa d_se d_road d_coke d_min d_cap rain_pre temp_pre area_mp arm_att y_pc n_cap 


// Summary statistics Covariates/treatment/DV
eststo clear // summary statistics of the covariates 
estpost su ftreat1 ftreat3 ftreat5 $cov_var slope1km eleva1km slope3km eleva3km slope5km eleva5km 
esttab using sum_covar_interest.tex, label replace nonumber booktabs noobs  ///
cells("mean(label(Mean) fmt(a2)) sd(label(Std. Dev.)) min(label(Min.)) max(label(Max.)) count(label(Obs.))")


// Differences in means by treatment status
eststo clear 
forv j=1(2)5{
qui eststo test`j': estpost ttest $cov_var slope`j'km eleva`j'km, by(ftreat`j')
esttab using diff_mean`j'km.tex, label replace nomtitle nonumbers noobs ///
 cell("mu_1(label(Mean t) fmt(a1)) mu_2(label(Mean C) fmt(a1)) t(fmt(a2)) b(label(Diff.) star fmt(a2))  ") mgroups("WCA 1km" "WCA 3km" "WCA 5km", pattern( 1 0  0 0 1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
star(* 0.10 ** 0.05 *** 0.01)


stddiff $cov_var slope`j'km eleva`j'km , by(ftreat`j') 
matrix sta_ttest`j'=r(output)
esttab matrix(sta_ttest`j', fmt(2)) using std_test`j'km.tex, replace label

}

preserve
keep $cov_var id_wss ftreat1 ftreat3 ftreat5 streat1 streat3 streat5 eleva1km eleva3km eleva5km slope1km slope3km slope5km 
**saving data to use R
export delimited using "/Users/mrfreerider/Documents/Research/Natural Capital & Water/more tree better water/data_baseline.csv", nolabel replace
restore
***************************************************************
** Pair id
import delimited "/Users/mrfreerider/Documents/Research/Natural Capital & Water/more tree better water/gen_matching/pair.nn.1km.cvs",delimiter(comma)  varnames(1)  clear
rename v1 id_wss
rename v2 nn

save "/Users/mrfreerider/Documents/Research/Natural Capital & Water/more tree better water/gen_matching/pair.nn.1km.dta", replace
** Matching output
import delimited "/Users/mrfreerider/Documents/Research/Natural Capital & Water/more tree better water/gen_matching/nn.1km.cvs",delimiter(comma)  varnames(1)  clear

rename v1 id_wss

merge 1:1 id_wss using "/Users/mrfreerider/Documents/Research/Natural Capital & Water/more tree better water/gen_matching/pair.nn.1km.dta"
drop _merge

save "/Users/mrfreerider/Documents/Research/Natural Capital & Water/more tree better water/gen_matching/nn.1km.dta", replace



** Panel data
use "/Users/mrfreerider/Documents/Research/Natural Capital & Water/more tree better water/gen_matching/panel.forest.cost.dta", clear

merge m:1 id_wss using "/Users/mrfreerider/Documents/Research/Natural Capital & Water/more tree better water/gen_matching/nn.1km.dta", gen(mergenn1km)

gen pair = id_wss if ftreat1==0
replace pair = nn if ftreat1==1
bysort pair: egen paircount = count(pair)
drop if paircount !=2

gen d=vc_mpost- vc_mpre
reg d ftreat1 



coefplot, vertical

global sq_var dist_reaa_mean mean_prec_wss_mpre mean_temp_wss_mpre ///
dist_road_mean dist_min_mean dist_coca_mean dist_capital_mean 

foreach x in $sq_var{
gen `x'_2=`x'^2
codebook `x'
}


** Pscore estimation
forv j=1(2)5{
global cov_var`j' prot_area dist_reaa_mean mean_prec_wss_mpre slope`j'km eleva`j'km dist_road_mean dist_min_mean dist_coca_mean dist_capital_mean mean_prec_wss_mpre_2 mean_temp_wss_mpre_2 dist_road_mean_2 dist_min_mean_2 dist_coca_mean_2 dist_capital_mean_2 
}


pscore  forest_treat1 $cov_var, pscore(pscore1)


twoway (kdensity pscore1 if forest_treat1==1)  (kdensity pscore1 if forest_treat1==0)

pscore forest_treat1 $cov_var1, pscore(pscore1) 
save pscore1km.dta,replace
pscore forest_treat3 $cov_var3, pscore(pscore3) 
save pscore3km.dta,replace
pscore forest_treat5 $cov_var5, pscore(pscore5) 
save pscore5km.dta,replace




*Before using the matching with the nearest neighbor, you must do the following which ensure the sort order is random
set seed 100
drawnorm orden
sort orden

psmatch2 forest_treat1, pscore(pscore1) mahalanobis($cov_var ) common logit

//mahalanobis($cov_var) logit

forv j=1(2)5{
use pscore`j'km.dta, clear

** Estimating the PSCORE. We use Mahalanobis. It seems that derive the best reduction in bias. However the rule of the thumbs is a reduction of <5%. Our estimations are above this, in 5 percentage points.

psmatch2 forest_treat`j', pscore(pscore`j'km) n(1) trim(20)

**Std differences in means BEFORE Matching
stddiff $cov_var, by(forest_treat`j') cohensd abs
matrix bef_stdiff`j'km=r(output) // save the ouput
esttab matrix(bef_stdiff`j'km) using bef_stdiff`j'km.tex, replace

**Std differences in means AFTER Matching
stddiff $cov_var if _support, by(_treated)  cohensd abs
matrix aft_stdiff`j'km=r(output)
esttab matrix(aft_stdiff`j'km) using aft_stdiff`j'km.tex, label replace

eststo clear // Plots the bias after and before matching
eststo psttest`j': pstest $cov_var, both graph
graph export pstest`j'km.png, replace
window manage close graph

// Plots the histogramas of PSCORE by treatment
psgraph, treated(_treated) pscore(pscore`j'km) bin(75) xline(10) 
graph export pscore_mat`j'km.png, replace
window manage close graph

rename _treated treated`j'km
rename _support support`j'km
rename _weight weight`j'km
rename _id id`j'km
rename  _n1  n1_`j'km
rename _nn nn`j'km

bootstrap, reps(100) seed(1): reg treated`j'km $cov_var

preserve
keep id_wss  pscore`j'km treated`j'km support`j'km weight`j'km id`j'km n1_`j'km nn`j'km $cov_var
save pscore`j'km_mat.dta,replace // data for each buffer
restore

}






** Comments about Matching methods: Mahalanobis is an ecuclidean distance. It performs better to our data. Remember Caliper optimal size 0.2*SD of the logit of the propensity score





*** Merge the information with pscores and treatment after matching
/*
////////////////////////////////////////////////////////
use data_forest_merge.dta, clear
drop _merge
merge m:1 id_wss using pscore1km_mat.dta
drop _merge
merge m:1 id_wss using pscore3km_mat.dta
drop _merge
merge m:1 id_wss using pscore5km_mat.dta
drop _merge


**** Regressions Diff-Diff for all the periods. We only care about periods 7 and 8.

gen post=0
replace post=1 if period>6

xtreg vc i.period##i._treated1km if _support1km, fe vce(robust)
xtreg vc i.period##i._treated3km if _support3km, fe vce(robust)
xtreg vc i.period##i._treated5km if _support5km, fe vce(robust)

*** You have to check how to estimate this. Directly or using weights.
*/

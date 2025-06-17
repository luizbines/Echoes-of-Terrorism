
clear
set more off

use terror_voting_data

xtset local year

***Table 3 (summary stats):***
sort min_range_p
drop if min_range_p==.
by min_range_p : sum hs_matricul1999 academic1995 pop1995 cluster1995 wage1999 total_killed med_age1999 male2female1999 asia1995 africa1995 former_ussr1995 jews_share95 turnout1999 right_share1999 reg_capital closest_brdr


***Table 4 (main results):*** 
xtreg right_share min_range_p, fe cluster(local) robust 
xtreg right_share min_range_p total_killed wage hs_matricul pop med_age male2female migration jews_share asia1995 africa1995  former reg_capital closest, fe cluster(local) robust 
xtreg right_share min_range_p total_killed wage hs_matricul pop med_age male2female migration jews_share asia1995 africa1995  former reg_capital closest i.year, fe cluster(local) robust 
reg right_share right_lag min_range_p total_killed wage hs_matricul pop med_age male2female migration jews_share asia1995 africa1995  former reg_capital closest, cluster(local) robust 

***Table 5 (main results for subroups of right wing bloc):***
xtreg national min_range_p total_killed wage hs_matricul pop med_age male2female migration jews_share i.year, fe cluster(local) robust 
xtreg religious min_range_p total_killed wage hs_matricul pop med_age male2female migration jews_share i.year, fe cluster(local) robust 
xtreg russian min_range_p total_killed wage hs_matricul pop med_age male2female migration jews_share i.year, fe cluster(local) robust 

***Table 6 (conditional on incumbent):***

*Kadima incumbent:
gen kadimapm=0
replace kadimapm=1 if year>=2006
*Right wing incumbent (always Likud)
gen right_pm=0
replace right_pm=1 if year==2003 
*Interaction term: in range during right wing incumbent:
gen inter1=min_range_p*right_pm

xtreg right_share min_range_p right_pm inter1 total_killed wage hs_matricul pop med_age male2female migration jews_share i.year, fe cluster(local) robust  
xtreg likud min_range_p right_pm inter1 total_killed wage hs_matricul pop med_age male2female migration jews_share i.year, fe cluster(local) robust  
xtreg kadima min_range_p total_killed wage hs_matricul pop med_age male2female migration jews_share i.year, fe cluster(local) robust  

***Table 7 (robustness tests -- dropping obs outside common support & alternative definitions of range):***

*Reestimating without obs outside common support based on 1995, 1999 data
drop if wage1999<742 | wage1999>4341
drop if pop1995<3.1 | pop1995>11.91
drop if med_age1999<13.1 | med_age1999>37.6
drop if male2female1999 <0.9 
drop if hs_matricul1999<0.08 | hs_matricul1999>0.76
drop if asia1995>0.46
drop if former>0.52
drop if turnout1999 <0.63 | turnout >0.87

xtreg right_share min_range_p total_killed wage hs_matricul pop med_age male2female migration jews_share i.year, fe cluster(local) robust 
xtreg national min_range_p total_killed wage hs_matricul pop med_age male2female migration jews_share i.year, fe cluster(local) robust 

*Reloading data (because of the dropped ons)
clear
use terror_voting_data

xtset local year

*Alternative definitions of being in range (different measures of distance)
*Reducing range by 5km to address possibility of launching from within Gaza Strip
gen new_range=0
replace new_range=1 if dist_perimeter_min <=5 & year==2003
replace new_range=1 if dist_perimeter_min <=15.4 & year==2006
replace new_range=1 if dist_perimeter_min <=38 & year==2009

xtreg right_share min_range_c total_killed wage hs_matricul pop med_age male2female migration jews_share i.year, fe cluster(local) robust 
xtreg right_share new_range total_killed wage hs_matricul pop med_age male2female migration jews_share i.year, fe cluster(local) robust 

***Table 8 (ruling out turnout and migration explanations):***

*Turnout percentiles:
centile turnout, centile (0 25 50 75)  // 0.69 is the median
xtreg turnout min_range_p total_killed wage hs_matricul pop med_age male2female migration jews_share i.year, fe cluster(local) robust 
xtreg right_share min_range_p total_killed wage hs_matricul pop med_age male2female migration jews_share i.year  if turnout>0.69, fe cluster(local) robust 

*Incoming and outgoing residents percentiles
centile incoming outgoing, centile (0 25 36 50) // incoming median=291, outgoing median=256

*Running the model only on localities with low incoming and outgoing movement of residents (lowest 50%, below median)
xtreg right_share min_range_p total_killed wage hs_matricul migration pop med_age male2female jews_share i.year if incoming< 291    & outgoing<256, fe cluster(local) robust 

*Creating a new, conservative estimate of right-wing vote share, assuming all incoming residents are right-leaning and all outgoing residents are left-leaning
replace right_num=right_num-incoming
replace right_num=0 if right_num<0
replace tot_votes=tot_votes+outgoing
gen new_right=right_num/tot_votes

xtreg new_right min_range_p total_killed wage hs_matricul pop med_age male2female migration jews_share i.year, fe cluster(local) robust 

gen range=0
replace range=1 if dist_perimeter_min<=10 & year==2003
replace range=1 if dist_perimeter_min<=20.4 & year==2006
replace range=1 if dist_perimeter_min<=43 & year==2009


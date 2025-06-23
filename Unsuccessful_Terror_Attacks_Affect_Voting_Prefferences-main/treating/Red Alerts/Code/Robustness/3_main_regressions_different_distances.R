# This script runs the regressions only for localities between 85km and 140km from Gaza
# Luiz Bines
# 2024

# Library
library(tidyr)
library(dplyr)
library(readxl)
library(kableExtra)
library(fixest)
library(modelsummary)
library(knitr)
library(broom)


# Directory
wd = 'C:/Users/luizb/Desktop/Echoes-of-Terrorism/Unsuccessful_Terror_Attacks_Affect_Voting_Prefferences-main/'
setwd(wd);

##### IMPORTING #####
election_percentages = read.csv('treating/Red Alerts/Output/3_election_percentages.csv')

# filtering years
election_percentages = election_percentages %>% 
  filter(year <= 2015)

# filtering distances
election_percentages = election_percentages %>% 
  filter((distance >= 85 & distance <= 140))


####### REGRESSIONS #########


#### LIKUD ####

# REG 1

reg_1_likud =
  
  feols(likud_percentage ~ i(year_election, temporal_group,
                             ref = '2013', 
                             ref2 = 'no_red_alert')| 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        se = 'standard')



# CONTROL VARIABLES + CITY FIXED EFFECTS
reg_2_likud = 
  
  feols(likud_percentage ~ i(year_election, temporal_group,
                             ref = '2013', 
                             ref2 = 'no_red_alert') +
          density + Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        se = 'standard')



#### RIGHT WING ####

# REG 1

reg_1_right_wing =
  
  feols(right_wing_percentage ~ i(year_election, temporal_group,
                                  ref = '2013', 
                                  ref2 = 'no_red_alert') | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        se = 'standard')



# CONTROL VARIABLES + CITY FIXED EFFECTS
reg_2_right_wing = 
  
  feols(right_wing_percentage ~ i(year_election, temporal_group,
                                  ref = '2013', 
                                  ref2 = 'no_red_alert') +
          density + Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        se = 'standard')


#### TURNOUT ####

# REG 1

reg_1_turnout =
  
  feols(turnout_percentage ~ i(year_election, temporal_group,
                               ref = '2013', 
                               ref2 = 'no_red_alert') | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        se = 'standard')



# CONTROL VARIABLES + CITY FIXED EFFECTS
reg_2_turnout = 
  
  feols(turnout_percentage ~ i(year_election, temporal_group,
                               ref = '2013', 
                               ref2 = 'no_red_alert') +
          density + Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        se = 'standard')




# Likud, Right Wing, turnout
modelsummary(
  list(
    reg_1_likud, 
    reg_2_likud,
    reg_1_right_wing, 
    reg_2_right_wing,
    reg_1_turnout,
    reg_2_turnout
  ),
  # output = 'latex_tabular',
  output = 'treating/Red Alerts/Output/Figures/Robustness/DIFFERENT_DISTANCE_regressions.tex',
  coef_map = c('year_election::2015:temporal_group::temporal_distance == 6' = 'Red Alert 6 Days Before * 2015 Election',
               'year_election::2015:temporal_group::temporal_distance > 149' = 'Red Alert 149+ Days Before * 2015 Election',
               'year_election::2009:temporal_group::temporal_distance == 6' = 'Red Alert 6 Days Before * 2009 Election',
               'year_election::2009:temporal_group::temporal_distance > 149' = 'Red Alert 149+ Days Before * 2009 Election',
               'year_election::2006:temporal_group::temporal_distance == 6' = 'Red Alert 6 Days Before * 2006 Election',
               'year_election::2006:temporal_group::temporal_distance > 149' = 'Red Alert 149+ Days Before * 2006 Election'),
  stars = T, 
  notes = 'NRA 2015 stands for No Red Alerts until the 2015 Election.',
  gof_omit= '',
  add_rows = data.frame('VARIABLES' = c('Dependent Variable',
                                        'Control Variables',
                                        'Locality Fixed Effects',
                                        'Control Group',
                                        'Observations'
  ),
  '1' = c(
    'Likud',
    'No',
    'Yes',
    'NRA 2015',
    summary(reg_1_likud)$nobs
  ),
  
  '2' = c(
    'Likud',
    'Yes',
    'Yes',
    'NRA 2015',
    summary(reg_1_likud)$nobs
  ),
  
  '3' = c(
    'Right Wing (excluding Likud)',
    'No',
    'Yes',
    'NRA 2015',
    summary(reg_1_right_wing)$nobs
  ),
  
  '4' = c(
    'Right Wing (excluding Likud)',
    'Yes',
    'Yes',
    'NRA 2015',
    summary(reg_1_right_wing)$nobs
  ),
  
  '5' = c(
    'Turnout',
    'No',
    'Yes',
    'NRA 2015',
    summary(reg_1_turnout)$nobs
  ),
  
  '6' = c(
    'Turnout',
    'Yes',
    'Yes',
    'NRA 2015',
    summary(reg_1_turnout)$nobs
  )
  )
)



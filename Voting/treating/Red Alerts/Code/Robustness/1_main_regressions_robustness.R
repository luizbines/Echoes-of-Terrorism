# This script runs the ALTERNATIVE regressions, that include Arab cities
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
library(tidyverse)


# Directory
wd = '/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Voting/'
setwd(wd);


# Function to save tables in correct latex format:
save_tabular <- function(table, filename) {
  tex <- as.character(table)
  
  # Clean table wrappers
  tex <- gsub("\\\\begin\\{table\\}.*?\n", "", tex)
  tex <- gsub("\\\\end\\{table\\}.*?", "", tex)
  tex <- gsub("\\\\centering\n?", "", tex)
  
  # Store the corrected file
  cat(tex, file = filename)
}

##### IMPORTING #####
parties_percentages_panel = read_csv('treating/Red Alerts/Output/Datasets/2_parties_percentages_panel.csv')


### FILTERING ###

# filtering years
parties_percentages_panel = parties_percentages_panel %>% 
  filter(year <= 2015) %>% 
  mutate(year = as.integer(year)) %>% 
  rename(year_election = year)

# Removing observations with missing voting data (only 28 observations, from 2006 and 2009 elections)
parties_percentages_panel = parties_percentages_panel %>% filter(!is.na(likud_percentage))


# 1. Custom dataset where we keep arab cities
parties_percentages_panel_ARAB = parties_percentages_panel

# 2. Custom dataset filtering for a different distance from the Gaza Strip (85-140km)
parties_percentages_panel_DISTANCES = parties_percentages_panel %>% 
  filter(distance > 85 & distance < 140
         # we exclude all majority-arab municipalities 
         & Religion_yishuv_Code != 2)


##### REGRESSIONS #####

#### 1. Without excluding Arab cities ####

#### LIKUD ####

# FIXED EFFECTS

reg_1_likud_ARAB =
  
  feols(likud_percentage ~ i(year_election, temporal_group,
                             ref = '2013', 
                             ref2 = 'no_red_alert')| 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_ARAB,
        cluster = 'SEMEL_YISHUV')



# CONTROL VARIABLES + FIXED EFFECTS
reg_2_likud_ARAB = 
  
  feols(likud_percentage ~ i(year_election, temporal_group,
                             ref = '2013', 
                             ref2 = 'no_red_alert') +
          Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_ARAB,
        cluster = 'SEMEL_YISHUV')



#### RIGHT WING ####

# FIXED EFFECTS

reg_1_right_wing_ARAB =
  
  feols(right_wing_percentage ~ i(year_election, temporal_group,
                                  ref = '2013', 
                                  ref2 = 'no_red_alert') | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_ARAB,
        cluster = 'SEMEL_YISHUV')



# CONTROL VARIABLES + FIXED EFFECTS
reg_2_right_wing_ARAB = 
  
  feols(right_wing_percentage ~ i(year_election, temporal_group,
                                  ref = '2013', 
                                  ref2 = 'no_red_alert') +
          Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_ARAB,
        cluster = 'SEMEL_YISHUV')


#### TURNOUT ####

# FIXED EFFECTS

reg_1_turnout_ARAB =
  
  feols(turnout_percentage ~ i(year_election, temporal_group,
                               ref = '2013', 
                               ref2 = 'no_red_alert') | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_ARAB,
        cluster = 'SEMEL_YISHUV')



# CONTROL VARIABLES + FIXED EFFECTS
reg_2_turnout_ARAB = 
  
  feols(turnout_percentage ~ i(year_election, temporal_group,
                               ref = '2013', 
                               ref2 = 'no_red_alert') +
          Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_ARAB,
        cluster = 'SEMEL_YISHUV')


#### COALITION 2015 ####

# FIXED EFFECTS
reg_1_coalition_ARAB =
  feols(coalition_2015_percentage ~ i(year_election, temporal_group,
                                      ref = '2013', 
                                      ref2 = 'no_red_alert')| 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_ARAB,
        cluster = 'SEMEL_YISHUV')

# CONTROL VARIABLES + FIXED EFFECTS
reg_2_coalition_ARAB = 
  feols(coalition_2015_percentage ~ i(year_election, temporal_group,
                                      ref = '2013', 
                                      ref2 = 'no_red_alert') +
          Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_ARAB,
        cluster = 'SEMEL_YISHUV')



#### 2. With different distances from Gaza ####

#### LIKUD ####

# FIXED EFFECTS

reg_1_likud_DISTANCE =
  
  feols(likud_percentage ~ i(year_election, temporal_group,
                             ref = '2013', 
                             ref2 = 'no_red_alert')| 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_DISTANCES,
        cluster = 'SEMEL_YISHUV')


# CONTROL VARIABLES + FIXED EFFECTS
reg_2_likud_DISTANCE = 
  
  feols(likud_percentage ~ i(year_election, temporal_group,
                             ref = '2013', 
                             ref2 = 'no_red_alert') +
          Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_DISTANCES,
        cluster = 'SEMEL_YISHUV')



#### RIGHT WING ####

# FIXED EFFECTS

reg_1_right_wing_DISTANCE =
  
  feols(right_wing_percentage ~ i(year_election, temporal_group,
                                  ref = '2013', 
                                  ref2 = 'no_red_alert') | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_DISTANCES,
        cluster = 'SEMEL_YISHUV')



# CONTROL VARIABLES + FIXED EFFECTS
reg_2_right_wing_DISTANCE = 
  
  feols(right_wing_percentage ~ i(year_election, temporal_group,
                                  ref = '2013', 
                                  ref2 = 'no_red_alert') +
          Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_DISTANCES,
        cluster = 'SEMEL_YISHUV')


#### TURNOUT ####

# FIXED EFFECTS

reg_1_turnout_DISTANCE =
  
  feols(turnout_percentage ~ i(year_election, temporal_group,
                               ref = '2013', 
                               ref2 = 'no_red_alert') | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_DISTANCES,
        cluster = 'SEMEL_YISHUV')



# CONTROL VARIABLES + FIXED EFFECTS
reg_2_turnout_DISTANCE = 
  
  feols(turnout_percentage ~ i(year_election, temporal_group,
                               ref = '2013', 
                               ref2 = 'no_red_alert') +
          Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_DISTANCES,
        cluster = 'SEMEL_YISHUV')


#### COALITION 2015 ####

# FIXED EFFECTS
reg_1_coalition_DISTANCE =
  feols(coalition_2015_percentage ~ i(year_election, temporal_group,
                                      ref = '2013', 
                                      ref2 = 'no_red_alert')| 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_DISTANCES,
        cluster = 'SEMEL_YISHUV')

# CONTROL VARIABLES + FIXED EFFECTS
reg_2_coalition_DISTANCE = 
  feols(coalition_2015_percentage ~ i(year_election, temporal_group,
                                      ref = '2013', 
                                      ref2 = 'no_red_alert') +
          Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_DISTANCES,
        cluster = 'SEMEL_YISHUV')



##### CREATING TABLES #####


# 1. For ARAB CITIES robustness
# Likud, Right Wing, Coalition, turnout
arab_model = modelsummary(
  list(
    reg_1_likud_ARAB, 
    reg_2_likud_ARAB,
    reg_1_coalition_ARAB,
    reg_2_coalition_ARAB,
    reg_1_right_wing_ARAB, 
    reg_2_right_wing_ARAB,
    reg_1_turnout_ARAB,
    reg_2_turnout_ARAB
  ),
  # output = 'treating/Red Alerts/Output/Tables/Robustness/Robustness_ARAB_CITIES_regressions.tex',
  output = 'latex',
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
    summary(reg_1_likud_ARAB)$nobs
  ),
  
  '2' = c(
    'Likud',
    'Yes',
    'Yes',
    'NRA 2015',
    summary(reg_1_likud_ARAB)$nobs
  ),
  
  '3' = c(
    '2015 Coalition',
    'No',
    'Yes',
    'NRA 2015',
    summary(reg_1_coalition_ARAB)$nobs
  ),
  
  '4' = c(
    '2015 Coalition',
    'Yes',
    'Yes',
    'NRA 2015',
    summary(reg_2_coalition_ARAB)$nobs
  ),
  
  '5' = c(
    'Right Wing',
    'No',
    'Yes',
    'NRA 2015',
    summary(reg_1_right_wing_ARAB)$nobs
  ),
  
  '6' = c(
    'Right Wing',
    'Yes',
    'Yes',
    'NRA 2015',
    summary(reg_1_right_wing_ARAB)$nobs
  ),
  
  '7' = c(
    'Turnout',
    'No',
    'Yes',
    'NRA 2015',
    summary(reg_1_turnout_ARAB)$nobs
  ),
  
  '8' = c(
    'Turnout',
    'Yes',
    'Yes',
    'NRA 2015',
    summary(reg_1_turnout_ARAB)$nobs
  )
  )
)

save_tabular(arab_model, 'treating/Red Alerts/Output/Tables/Robustness/Robustness_ARAB_CITIES_regressions.tex')

# 2. For CUSTOM DISTANCE robustness
# Likud, Right Wing, Coalition, turnout
distance_model = modelsummary(
  list(
    reg_1_likud_DISTANCE, 
    reg_2_likud_DISTANCE,
    reg_1_coalition_DISTANCE,
    reg_2_coalition_DISTANCE,
    reg_1_right_wing_DISTANCE, 
    reg_2_right_wing_DISTANCE,
    reg_1_turnout_DISTANCE,
    reg_2_turnout_DISTANCE
  ),
  # output = 'treating/Red Alerts/Output/Tables/Robustness/Robustness_DIFFERENT_DISTANCE_regressions.tex',
  output = 'latex',
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
    summary(reg_1_likud_DISTANCE)$nobs
  ),
  
  '2' = c(
    'Likud',
    'Yes',
    'Yes',
    'NRA 2015',
    summary(reg_1_likud_DISTANCE)$nobs
  ),
  
  '3' = c(
    '2015 Coalition',
    'No',
    'Yes',
    'NRA 2015',
    summary(reg_1_coalition_DISTANCE)$nobs
  ),
  
  '4' = c(
    '2015 Coalition',
    'Yes',
    'Yes',
    'NRA 2015',
    summary(reg_2_coalition_DISTANCE)$nobs
  ),
  
  '5' = c(
    'Right Wing',
    'No',
    'Yes',
    'NRA 2015',
    summary(reg_1_right_wing_DISTANCE)$nobs
  ),
  
  '6' = c(
    'Right Wing',
    'Yes',
    'Yes',
    'NRA 2015',
    summary(reg_1_right_wing_DISTANCE)$nobs
  ),
  
  '7' = c(
    'Turnout',
    'No',
    'Yes',
    'NRA 2015',
    summary(reg_1_turnout_DISTANCE)$nobs
  ),
  
  '8' = c(
    'Turnout',
    'Yes',
    'Yes',
    'NRA 2015',
    summary(reg_1_turnout_DISTANCE)$nobs
  )
  )
)

# Saving
save_tabular(distance_model, 'treating/Red Alerts/Output/Tables/Robustness/Robustness_DIFFERENT_DISTANCE_regressions.tex')

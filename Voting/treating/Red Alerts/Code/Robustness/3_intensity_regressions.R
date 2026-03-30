# This script runs the regressions using number of red alerts as independent variable
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
library(panelsummary)
library(knitr)
library(tidyverse)


# Directory
wd = '/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Voting/'
setwd(wd);

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


##### REGRESSIONS #####

# If no red alerts, substitute NA for 0
parties_percentages_panel = parties_percentages_panel %>%
  mutate(
    alerts_6_days = ifelse(is.na(alerts_6_days),0,alerts_6_days),
    alerts_149_plus = ifelse(is.na(alerts_149_plus),0,alerts_149_plus)
  )

#### LIKUD ####

# FIXED EFFECTS

reg_1_likud =
  
  feols(likud_percentage ~ i(year_election, alerts_6_days,
                             ref = '2013'
                             ) +
                           i(year_election, alerts_149_plus,
                             ref = '2013'
                             )  | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
        cluster = 'SEMEL_YISHUV')

summary(reg_1_likud)

# CONTROL VARIABLES + FIXED EFFECTS
reg_2_likud = 
  
  feols(likud_percentage ~ i(year_election, alerts_6_days,
                             ref = '2013'
                             ) +
                           i(year_election, alerts_149_plus,
                             ref = '2013'
                             )  +
          Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
        cluster = 'SEMEL_YISHUV')



#### RIGHT WING ####

# FIXED EFFECTS

reg_1_right_wing =
  
  feols(right_wing_percentage ~ i(year_election, alerts_6_days,
                             ref = '2013'
                             ) +
                           i(year_election, alerts_149_plus,
                             ref = '2013'
                             )  | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
        cluster = 'SEMEL_YISHUV')



# CONTROL VARIABLES + FIXED EFFECTS
reg_2_right_wing = 
  
  feols(right_wing_percentage ~ i(year_election, alerts_6_days,
                             ref = '2013'
                             ) +
                           i(year_election, alerts_149_plus,
                             ref = '2013'
                             ) +
          Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
        cluster = 'SEMEL_YISHUV')


#### TURNOUT ####

# FIXED EFFECTS

reg_1_turnout =
  
  feols(turnout_percentage ~ i(year_election, alerts_6_days,
                             ref = '2013'
                             ) +
                           i(year_election, alerts_149_plus,
                             ref = '2013'
                             )  | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
        cluster = 'SEMEL_YISHUV')



# CONTROL VARIABLES + FIXED EFFECTS
reg_2_turnout = 
  
  feols(turnout_percentage ~ i(year_election, alerts_6_days,
                             ref = '2013'
                             ) +
                           i(year_election, alerts_149_plus,
                             ref = '2013'
                             ) +
          Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
        cluster = 'SEMEL_YISHUV')


#### COALITION 2015 ####

# FIXED EFFECTS
reg_1_coalition =
  feols(coalition_2015_percentage ~ i(year_election, alerts_6_days,
                             ref = '2013'
                             ) +
                           i(year_election, alerts_149_plus,
                             ref = '2013'
                             ) | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
        cluster = 'SEMEL_YISHUV')

# CONTROL VARIABLES + FIXED EFFECTS
reg_2_coalition = 
  feols(coalition_2015_percentage ~ i(year_election, alerts_6_days,
                             ref = '2013'
                             ) +
                           i(year_election, alerts_149_plus,
                             ref = '2013'
                             ) +
          Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
        cluster = 'SEMEL_YISHUV')

# Likud, Right Wing, turnout, coalition
modelsummary(
  list(
    reg_1_likud, 
    reg_2_likud,
    reg_1_coalition,
    reg_2_coalition,
    reg_1_right_wing, 
    reg_2_right_wing,
    reg_1_turnout,
    reg_2_turnout
    ),
            output = 'treating/Red Alerts/Output/Figures/Robustness/Robustness_intensity_regressions.tex',
            #  output = 'latex_tabular',
             coef_map = c('year_election::2015:alerts_6_days' = 'Red Alert 6 Days Before * 2015 Election',
                          'year_election::2015:alerts_149_plus' = 'Red Alert 149+ Days Before * 2015 Election',
                          'year_election::2009:alerts_6_days' = 'Red Alert 6 Days Before * 2009 Election',
                          'year_election::2009:alerts_149_plus' = 'Red Alert 149+ Days Before * 2009 Election',
                          'year_election::2006:alerts_6_days' = 'Red Alert 6 Days Before * 2006 Election',
                          'year_election::2006:alerts_149_plus' = 'Red Alert 149+ Days Before * 2006 Election'),
             stars = T, 
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
                                     summary(reg_2_likud)$nobs
                                   ),
                                   
                                   '3' = c(
                                           '2015 Coalition (excluding Likud)',
                                           'No',
                                           'Yes',
                                           'NRA 2015',
                                           summary(reg_1_coalition)$nobs
                                           ),
                                   
                                   '4' = c(
                                     '2015 Coalition (excluding Likud)',
                                     'Yes',
                                     'Yes',
                                     'NRA 2015',
                                     summary(reg_2_coalition)$nobs
                                   ),
                                   
                                   '5' = c(
                                     'Right Wing (excluding Likud)',
                                     'No',
                                     'Yes',
                                     'NRA 2015',
                                     summary(reg_1_right_wing)$nobs
                                   ),
                                   
                                   '6' = c(
                                     'Right Wing (excluding Likud)',
                                     'Yes',
                                     'Yes',
                                     'NRA 2015',
                                     summary(reg_2_right_wing)$nobs
                                   ),
                                   
                                   '7' = c(
                                           'Turnout',
                                           'No',
                                           'Yes',
                                           'NRA 2015',
                                           summary(reg_1_turnout)$nobs
                                           ),
                                   
                                   '8' = c(
                                     'Turnout',
                                     'Yes',
                                     'Yes',
                                     'NRA 2015',
                                     summary(reg_2_turnout)$nobs
                                   )
             )
)

# This script runs the main regressions with clustered SE
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

# Directory
wd = '/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Unsuccessful_Terror_Attacks_Affect_Voting_Prefferences-main/'
setwd(wd);

election_percentages = read.csv('treating/Red Alerts/Output/3_election_percentages.csv')

# filtering years
election_percentages = election_percentages %>% filter(year <= 2015)


####### REGRESSIONS #########


#### LIKUD ####

# FIXED EFFECTS

reg_1_likud =
  
  feols(likud_percentage ~ i(year_election, temporal_group,
                             ref = '2013', 
                             ref2 = 'no_red_alert')| 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        cluster = 'SEMEL_YISHUV')


# calculating average effect
coef_likud <-  reg_1_likud$coefficients[5] %>% as.numeric()

mean_likud_2013 <- mean(election_percentages$likud_percentage[election_percentages$time_to_treatment == -1 & 
                                                                election_percentages$temporal_group == "temporal_distance == 6"])
effect_percent_likud <- (coef_likud / mean_likud_2013) * 100

effect_percent_likud

# CONTROL VARIABLES + FIXED EFFECTS
reg_2_likud = 
  
  feols(likud_percentage ~ i(year_election, temporal_group,
                             ref = '2013', 
                             ref2 = 'no_red_alert') +
          density + Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        cluster = 'SEMEL_YISHUV')



#### RIGHT WING ####

# FIXED EFFECTS

reg_1_right_wing =
  
  feols(right_wing_percentage ~ i(year_election, temporal_group,
                                  ref = '2013', 
                                  ref2 = 'no_red_alert') | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        cluster = 'SEMEL_YISHUV')



# CONTROL VARIABLES + FIXED EFFECTS
reg_2_right_wing = 
  
  feols(right_wing_percentage ~ i(year_election, temporal_group,
                                  ref = '2013', 
                                  ref2 = 'no_red_alert') +
          density + Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        cluster = 'SEMEL_YISHUV')


#### TURNOUT ####

# FIXED EFFECTS

reg_1_turnout =
  
  feols(turnout_percentage ~ i(year_election, temporal_group,
                               ref = '2013', 
                               ref2 = 'no_red_alert') | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        cluster = 'SEMEL_YISHUV')



# CONTROL VARIABLES + FIXED EFFECTS
reg_2_turnout = 
  
  feols(turnout_percentage ~ i(year_election, temporal_group,
                               ref = '2013', 
                               ref2 = 'no_red_alert') +
          density + Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        cluster = 'SEMEL_YISHUV')




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
  # output = 'treating/Red Alerts/Output/Figures/3_likud_right_wing_turnout_temporal_results_CLUSTERED.tex',
  # output = 'latex_tabular',
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




#### HETEROGENEITIES ####

#### by likud vote share in 2013 ####

# Create quintile variable based on Likud share in 2013
quintiles <- quantile(
  election_percentages$likud_percentage[election_percentages$year == 2013 & election_percentages$Religion_yishuv_Code != 2],
  probs = seq(0, 1, 0.2),
  na.rm = TRUE
)


# Assigning specific quintile to each locality
election_percentages <- election_percentages %>%
  group_by(SEMEL_YISHUV) %>%
  mutate(
    quintile2013 = cut(
      likud_percentage * (year == 2013),
      breaks = quintiles,
      labels = paste0("Q", 1:5)
    )
  ) %>%
  fill(quintile2013, .direction = "downup") %>%
  ungroup()


# Models for likud_percentage
mod_likud_q1 <- feols(likud_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                      data = election_percentages %>% filter(quintile2013 == 'Q1') %>% filter(Religion_yishuv_Code != 2),
                      cluster = 'SEMEL_YISHUV')
mod_likud_q2 <- feols(likud_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                      data = election_percentages %>% filter(quintile2013 == 'Q2') %>% filter(Religion_yishuv_Code != 2),
                      cluster = 'SEMEL_YISHUV')
mod_likud_q3 <- feols(likud_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                      data = election_percentages %>% filter(quintile2013 == 'Q3') %>% filter(Religion_yishuv_Code != 2),
                      cluster = 'SEMEL_YISHUV')
mod_likud_q4 <- feols(likud_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                      data = election_percentages %>% filter(quintile2013 == 'Q4') %>% filter(Religion_yishuv_Code != 2),
                      cluster = 'SEMEL_YISHUV')
mod_likud_q5 <- feols(likud_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                      data = election_percentages %>% filter(quintile2013 == 'Q5') %>% filter(Religion_yishuv_Code != 2),
                      cluster = 'SEMEL_YISHUV')


# Models for right_wing_percentage
mod_right_wing_q1 <- feols(right_wing_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                           data = election_percentages %>% filter(quintile2013 == 'Q1') %>% filter(Religion_yishuv_Code != 2),
                           cluster = 'SEMEL_YISHUV')
mod_right_wing_q2 <- feols(right_wing_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                           data = election_percentages %>% filter(quintile2013 == 'Q2') %>% filter(Religion_yishuv_Code != 2),
                           cluster = 'SEMEL_YISHUV')
mod_right_wing_q3 <- feols(right_wing_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                           data = election_percentages %>% filter(quintile2013 == 'Q3') %>% filter(Religion_yishuv_Code != 2),
                           cluster = 'SEMEL_YISHUV')
mod_right_wing_q4 <- feols(right_wing_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                           data = election_percentages %>% filter(quintile2013 == 'Q4') %>% filter(Religion_yishuv_Code != 2),
                           cluster = 'SEMEL_YISHUV')
mod_right_wing_q5 <- feols(right_wing_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                           data = election_percentages %>% filter(quintile2013 == 'Q5') %>% filter(Religion_yishuv_Code != 2),
                           cluster = 'SEMEL_YISHUV')


# Models for turnout_percentage
mod_turnout_q1 <- feols(turnout_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                        data = election_percentages %>% filter(quintile2013 == 'Q1') %>% filter(Religion_yishuv_Code != 2),
                        cluster = 'SEMEL_YISHUV')
mod_turnout_q2 <- feols(turnout_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                        data = election_percentages %>% filter(quintile2013 == 'Q2') %>% filter(Religion_yishuv_Code != 2),
                        cluster = 'SEMEL_YISHUV')
mod_turnout_q3 <- feols(turnout_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                        data = election_percentages %>% filter(quintile2013 == 'Q3') %>% filter(Religion_yishuv_Code != 2),
                        cluster = 'SEMEL_YISHUV')
mod_turnout_q4 <- feols(turnout_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                        data = election_percentages %>% filter(quintile2013 == 'Q4') %>% filter(Religion_yishuv_Code != 2),
                        cluster = 'SEMEL_YISHUV')
mod_turnout_q5 <- feols(turnout_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                        data = election_percentages %>% filter(quintile2013 == 'Q5') %>% filter(Religion_yishuv_Code != 2),
                        cluster = 'SEMEL_YISHUV')



# Creating table
polarization_quintiles = panelsummary(
  list(mod_likud_q1, mod_likud_q2, mod_likud_q3, mod_likud_q4, mod_likud_q5),
  list(mod_right_wing_q1, mod_right_wing_q2, mod_right_wing_q3, mod_right_wing_q4, mod_right_wing_q5),
  list(mod_turnout_q1, mod_turnout_q2, mod_turnout_q3, mod_turnout_q4, mod_turnout_q5),
  panel_labels = c("Panel A: Likud", "Panel B: Right-Wing", "Panel C: Turnout"),
  coef_map = c(
    'year_election::2015:temporal_group::temporal_distance == 6' = '6 days before elections',
    'year_election::2015:temporal_group::temporal_distance > 149' = '149+ days before elections'
  ),
  gof_map = c('nobs','r.squared'),
  colnames = c('','Q1','Q2','Q3','Q4','Q5'),
  format = 'latex',
  stars = TRUE)


# Saving
writeLines(polarization_quintiles, 'treating/Red Alerts/Output/Figures/3_quintiles_results_CLUSTERED.tex')

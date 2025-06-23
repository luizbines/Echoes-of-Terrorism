# This script runs the main regressions
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
wd = 'C:/Users/luizb/Desktop/Echoes-of-Terrorism/Unsuccessful_Terror_Attacks_Affect_Voting_Prefferences-main/'
setwd(wd);

##### IMPORTING #####

likud_percentage = read.csv('treating/Red Alerts/Output/2_likud_percentage_panel.csv') 

right_wing_percentage = read.csv('treating/Red Alerts/Output/2_right_wing_percentage_panel.csv') 

turnout_percentage = read.csv('treating/Red Alerts/Output/2_turnout_percentage_panel.csv') 


### MERGING ###

election_percentages =
  merge(likud_percentage,
        right_wing_percentage %>% 
          select('SEMEL_YISHUV','right_wing_percentage','year'),
        by = c('SEMEL_YISHUV','year')) %>% 
  merge(turnout_percentage %>% 
          select('SEMEL_YISHUV', 'turnout_percentage','year'),
        by = c('SEMEL_YISHUV','year'),
        all.x = T) %>% 
  mutate(
    number_of_red_alerts = ifelse(is.na(number_of_red_alerts),0,number_of_red_alerts),
    number_of_red_alerts = ifelse(year < 2015, 0, number_of_red_alerts),
    red_alert_number_category = cut(number_of_red_alerts,
                                    breaks = c(-Inf, 0, 2, 5, Inf), 
                                    labels = c("No Alerts", "1-2 Alerts", "3-5 Alerts", "6+ Alerts"))
    
  ) %>%
  group_by(SEMEL_YISHUV) %>%
  mutate(Religion_yishuv_Code = ifelse(is.na(Religion_yishuv_Code),
                                       first(Religion_yishuv_Code[!is.na(Religion_yishuv_Code)]),
                                       Religion_yishuv_Code),

  ) %>%
  ungroup()


rm(likud_percentage,right_wing_percentage,turnout_percentage)



##### REGRESSIONS #####
# creating time to treatment variable
election_percentages = election_percentages %>% 
  mutate(
    
    time_to_treatment = case_when(
      year == 2006 ~ -3,
      year == 2009 ~ -2,
      year == 2013 ~ -1,
      year == 2015 ~ 0,
      TRUE ~ NA_real_ 
    )
    
  )



# excluding localities that did not exist in 2006 or 2009
election_percentages <- election_percentages %>%
  group_by(loc) %>%                
  filter(!any(is.na(likud_percentage))) %>% 
  ungroup() %>% 
  mutate(
    year_election = as.factor(year_election)
  )






# exporting elections dataset
write.csv(election_percentages, 'treating/Red Alerts/Output/3_election_percentages.csv', row.names = F)


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
        se = 'standard')


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
        se = 'standard')



#### RIGHT WING ####

# FIXED EFFECTS

reg_1_right_wing =
  
  feols(right_wing_percentage ~ i(year_election, temporal_group,
                             ref = '2013', 
                             ref2 = 'no_red_alert') | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        se = 'standard')



# CONTROL VARIABLES + FIXED EFFECTS
reg_2_right_wing = 
  
  feols(right_wing_percentage ~ i(year_election, temporal_group,
                             ref = '2013', 
                             ref2 = 'no_red_alert') +
          density + Pop_Total + ntl | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        se = 'standard')


#### TURNOUT ####

# FIXED EFFECTS

reg_1_turnout =
  
  feols(turnout_percentage ~ i(year_election, temporal_group,
                                  ref = '2013', 
                                  ref2 = 'no_red_alert') | 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        se = 'standard')



# CONTROL VARIABLES + FIXED EFFECTS
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
             # output = 'treating/Red Alerts/Output/Figures/3_likud_right_wing_turnout_temporal_results.tex',
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
                      se = "standard")
mod_likud_q2 <- feols(likud_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                      data = election_percentages %>% filter(quintile2013 == 'Q2') %>% filter(Religion_yishuv_Code != 2),
                      se = "standard")
mod_likud_q3 <- feols(likud_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                      data = election_percentages %>% filter(quintile2013 == 'Q3') %>% filter(Religion_yishuv_Code != 2),
                      se = "standard")
mod_likud_q4 <- feols(likud_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                      data = election_percentages %>% filter(quintile2013 == 'Q4') %>% filter(Religion_yishuv_Code != 2),
                      se = "standard")
mod_likud_q5 <- feols(likud_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                      data = election_percentages %>% filter(quintile2013 == 'Q5') %>% filter(Religion_yishuv_Code != 2),
                      se = "standard")


# Models for right_wing_percentage
mod_right_wing_q1 <- feols(right_wing_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                           data = election_percentages %>% filter(quintile2013 == 'Q1') %>% filter(Religion_yishuv_Code != 2),
                           se = "standard")
mod_right_wing_q2 <- feols(right_wing_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                           data = election_percentages %>% filter(quintile2013 == 'Q2') %>% filter(Religion_yishuv_Code != 2),
                           se = "standard")
mod_right_wing_q3 <- feols(right_wing_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                           data = election_percentages %>% filter(quintile2013 == 'Q3') %>% filter(Religion_yishuv_Code != 2),
                           se = "standard")
mod_right_wing_q4 <- feols(right_wing_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                           data = election_percentages %>% filter(quintile2013 == 'Q4') %>% filter(Religion_yishuv_Code != 2),
                           se = "standard")
mod_right_wing_q5 <- feols(right_wing_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                           data = election_percentages %>% filter(quintile2013 == 'Q5') %>% filter(Religion_yishuv_Code != 2),
                           se = "standard")


# Models for turnout_percentage
mod_turnout_q1 <- feols(turnout_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                        data = election_percentages %>% filter(quintile2013 == 'Q1') %>% filter(Religion_yishuv_Code != 2),
                        se = "standard")
mod_turnout_q2 <- feols(turnout_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                        data = election_percentages %>% filter(quintile2013 == 'Q2') %>% filter(Religion_yishuv_Code != 2),
                        se = "standard")
mod_turnout_q3 <- feols(turnout_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                        data = election_percentages %>% filter(quintile2013 == 'Q3') %>% filter(Religion_yishuv_Code != 2),
                        se = "standard")
mod_turnout_q4 <- feols(turnout_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                        data = election_percentages %>% filter(quintile2013 == 'Q4') %>% filter(Religion_yishuv_Code != 2),
                        se = "standard")
mod_turnout_q5 <- feols(turnout_percentage ~ i(year_election, temporal_group, ref = "2013", ref2 = "no_red_alert") | as.factor(SEMEL_YISHUV) + as.factor(year_election),
                        data = election_percentages %>% filter(quintile2013 == 'Q5') %>% filter(Religion_yishuv_Code != 2),
                        se = "standard")



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
writeLines(polarization_quintiles, 'treating/Red Alerts/Output/Figures/3_quintiles_results.tex')

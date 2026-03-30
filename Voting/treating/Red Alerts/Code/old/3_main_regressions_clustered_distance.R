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


# Directory
wd = 'C:/Users/luizb/Desktop/Dissertation/Red-Alerts-and-Votes/'
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
                               breaks = c(-Inf, 0, 2, 5, Inf), # Definindo os intervalos
                               labels = c("No Alerts", "1-2 Alerts", "3-5 Alerts", "6+ Alerts"))

) %>%
  group_by(SEMEL_YISHUV) %>%
  mutate(Religion_yishuv_Code = ifelse(is.na(Religion_yishuv_Code),
                                       first(Religion_yishuv_Code[!is.na(Religion_yishuv_Code)]),
                                       Religion_yishuv_Code),
         # area = as.factor(area),
         # # ,
         # area = ifelse(temporal_group == 'no_red_alert', 200000000000, area),
         # # area = ifelse(is.na(area),100000000,area)
         ) %>%
  ungroup()


rm(likud_percentage,right_wing_percentage,turnout_percentage)



##### REGRESSIONS #####

# CLUSTERING BY DISTANCE FROM GAZA
max_distance <- max(election_percentages$distance)

# Number of clusters
breaks <- seq(0, max_distance + 10, by = 10)

# Creating distance clusters
election_percentages$distance_Gaza <- cut(election_percentages$distance,
                                            breaks = breaks,
                                            include.lowest = TRUE,
                                            right = FALSE,
                                            labels = FALSE)

election_percentages$distance_Gaza = election_percentages$distance_Gaza %>% as.factor()



# exporting elections dataset
write.csv(election_percentages, 'treating/Red Alerts/Output/3_election_percentages.csv', row.names = F)



# filtering years
election_percentages = election_percentages %>% filter(year == 2013 | year == 2015)




###  ### LIKUD PERCENTAGE ### ###

# CLUSTERED ERRORS
reg_1_likud = feols(likud_percentage ~  temporal_group*post,
                    data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                    cluster = 'distance_Gaza')


# CLUSTERED ERRORS + CONTROL VARIABLES + CITY FIXED EFFECTS
reg_2_likud = feols(likud_percentage ~ temporal_group*post +
                      distance + density + Shape_Area +Pop_Total + ntl | as.factor(distance_Gaza),
                    data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                    cluster = 'distance_Gaza')


# # CLUSTERED ERRORS + CONTROL VARIABLES + CITY FIXED EFFECTS + EXCLUDING ARAB CITIES
# reg_3_likud = feols(likud_percentage ~ temporal_group*post +
#                       distance + density + Shape_Area + Pop_Total + ntl + as.factor(distance_Gaza),
#       data = election_percentages %>% filter(Religion_yishuv_Code != 2),
#       cluster = 'distance_Gaza',
#       weights = ~ Pop_Total)



### ### RIGHT WING PERCENTAGE ### ###

# CLUSTERED ERRORS

reg_1_right_wing = feols(right_wing_percentage ~  temporal_group*post,
                         data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                         cluster = 'distance_Gaza')


# CLUSTERED ERRORS + CONTROL VARIABLES + CITY FIXED EFFECTS + EXCLUDING ARAB CITIES
reg_2_right_wing = feols(right_wing_percentage ~  temporal_group*post +
                           distance + density + Shape_Area +  ntl  +  Pop_Total | as.factor(distance_Gaza),
                         data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                         cluster = 'distance_Gaza')


# # CLUSTERED ERRORS + CONTROL VARIABLES + CITY FIXED EFFECTS + EXCLUDING ARAB CITIES
# reg_3_right_wing = feols(right_wing_percentage ~ temporal_group*post +
#                            distance + density + Shape_Area + Pop_Total + ntl  + as.factor(distance_Gaza),
#                     data = election_percentages %>% filter(Religion_yishuv_Code != 2),
#                     cluster = 'distance_Gaza',
#                     weights = ~ Pop_Total)


### ### Turnout ### ###

# CLUSTERED ERRORS

reg_1_turnout = feols(turnout_percentage ~  temporal_group*post,
                      data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                      cluster = 'distance_Gaza')


# CLUSTERED ERRORS + CONTROL VARIABLES + CITY FIXED EFFECTS
reg_2_turnout = feols(turnout_percentage ~  temporal_group*post +
                        density + Shape_Area + ntl + Pop_Total | as.factor(distance_Gaza),
                      data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                      cluster = 'distance_Gaza')


# # CLUSTERED ERRORS + CONTROL VARIABLES + CITY FIXED EFFECTS + EXCLUDING ARAB CITIES
# reg_3_turnout = feols(turnout_percentage ~ temporal_group*post +
#                         distance + density + Shape_Area + Pop_Total + ntl  + as.factor(distance_Gaza),
#                     data = election_percentages %>% filter(Religion_yishuv_Code != 2),
#                     cluster = 'distance_Gaza',
#                     weights = ~ Pop_Total)



# Likud, Right Wing, turnout
modelsummary(list(reg_1_likud, reg_2_likud, reg_1_right_wing, reg_2_right_wing, reg_1_turnout, reg_2_turnout),
             # output = 'treating/Red Alerts/Output/Figures/likud_right_wing_turnout_temporal_results.tex',
             output = 'latex_tabular',
             coef_map = c('temporal_grouptemporal_distance == 6:postTRUE' = 'Red Alert 6 Days Before * Post',
                          'temporal_grouptemporal_distance > 200:postTRUE'= 'Red Alert 200+ Days Before * Post',
                          'temporal_grouptemporal_distance == 6' = 'Red Alert 6 Days Before Election',
                          'temporal_grouptemporal_distance > 200' = 'Red Alert 200+ Days Before Election',
                          'postTRUE' = 'Post',
                          '(Intercept)' = 'Intercept'),
             stars = T, 
             notes = 'NRA 2015 stands for No Red Alerts until the 2015 Election.',
             gof_omit= '',
             add_rows = data.frame('VARIABLES' = c('Observations',
                                                   'Control Variables',
                                                   'Locality Fixed Effects',
                                                   'Clustered Errors',
                                                   'Control Group',
                                                   'Dependent Variable'),
                                   '1' = c(round(summary(reg_1_likud)$nobs/2,0),
                                           'No',
                                           'No',
                                           'Yes',
                                           'NRA 2015',
                                           'Likud'),
                                   '2' = c(round(summary(reg_2_likud)$nobs/2,0),
                                           'Yes',
                                           'Yes',
                                           'Yes',
                                           'NRA 2015',
                                           'Likud'),
                                   '3' = c(round(summary(reg_1_right_wing)$nobs/2,0),
                                           'No',
                                           'No',
                                           'Yes',
                                           'NRA 2015',
                                           'Right Wing (excluding Likud)'),
                                   '4' = c(round(summary(reg_2_right_wing)$nobs/2,0),
                                           'Yes',
                                           'Yes',
                                           'Yes',
                                           'NRA 2015',
                                           'Right Wing (excluding Likud)'),
                                   '5' = c(round(summary(reg_1_turnout)$nobs/2,0),
                                           'No',
                                           'No',
                                           'Yes',
                                           'NRA 2015',
                                           'Turnout'),
                                   '6' = c(round(summary(reg_2_turnout)$nobs/2,0),
                                           'Yes',
                                           'Yes',
                                           'Yes',
                                           'NRA 2015',
                                           'Turnout')
             ),
             latex_options = c("hold_position", "striped", "adjustbox")
)






#### HETEROGENEITIES ####

#### by likud vote share in 2013 ####
percentiles_likud_2013 <- quantile(election_percentages$likud_percentage[election_percentages$year == 2013], 
                                   probs = c(0.60, 0.70, 0.80), na.rm = TRUE)

# Create 'pro_likud_2013' indicator variables for each percentile
election_percentages <- election_percentages %>%
  group_by(SEMEL_YISHUV) %>%
  mutate(pro_likud_60 = ifelse(year == 2013, 
                               ifelse(likud_percentage >= percentiles_likud_2013[1], 1, 0), 
                               NA),
         pro_likud_70 = ifelse(year == 2013, 
                               ifelse(likud_percentage >= percentiles_likud_2013[2], 1, 0), 
                               NA),
         pro_likud_80 = ifelse(year == 2013, 
                               ifelse(likud_percentage >= percentiles_likud_2013[3], 1, 0), 
                               NA)
  )

# Copying 2013 group value (pro or against Likud) to 2015
election_percentages <- election_percentages %>%
  group_by(SEMEL_YISHUV) %>%
  mutate(pro_likud_60 = ifelse(year == 2015, 
                               first(pro_likud_60[year == 2013], default = NA), 
                               pro_likud_60),
         pro_likud_70 = ifelse(year == 2015, 
                               first(pro_likud_70[year == 2013], default = NA), 
                               pro_likud_70),
         pro_likud_80 = ifelse(year == 2015, 
                               first(pro_likud_80[year == 2013], default = NA), 
                               pro_likud_80)
  ) %>%
  ungroup()



# Regressions for each percentile
# Likud
reg_likud_60 <- feols(likud_percentage ~ temporal_group * post * pro_likud_60,
                      data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                      cluster = 'distance_Gaza')

reg_likud_70 <- feols(likud_percentage ~ temporal_group * post * pro_likud_70,
                      data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                      cluster = 'distance_Gaza')

reg_likud_80 <- feols(likud_percentage ~ temporal_group * post * pro_likud_80,
                      data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                      cluster = 'distance_Gaza')




# Right Wing
reg_right_wing_60 <- feols(right_wing_percentage ~ temporal_group * post * pro_likud_60,
                           data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                           cluster = 'distance_Gaza')

reg_right_wing_70 <- feols(right_wing_percentage ~ temporal_group * post * pro_likud_70,
                           data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                           cluster = 'distance_Gaza')

reg_right_wing_80 <- feols(right_wing_percentage ~ temporal_group * post * pro_likud_80,
                           data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                           cluster = 'distance_Gaza')



# Turnout
reg_turnout_60 <- feols(turnout_percentage ~ temporal_group * post * pro_likud_60,
                        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                        cluster = 'distance_Gaza')

reg_turnout_70 <- feols(turnout_percentage ~ temporal_group * post * pro_likud_70,
                        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                        cluster = 'distance_Gaza')

reg_turnout_80 <- feols(turnout_percentage ~ temporal_group * post * pro_likud_80,
                        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                        cluster = 'distance_Gaza')



modelsummary(list(reg_likud_60, reg_likud_70, reg_likud_80,
                  reg_right_wing_60, reg_right_wing_70, reg_right_wing_80,
                  reg_turnout_60, reg_turnout_70, reg_turnout_80),
             # output = 'latex_tabular',
             booktabs = TRUE,
             coef_map = c('temporal_grouptemporal_distance == 6:postTRUE:pro_likud_60' = '60th percentile - 6 days',
                          'temporal_grouptemporal_distance > 200:postTRUE:pro_likud_60' = '60th percentile - 200+ days',
                          'temporal_grouptemporal_distance == 6:postTRUE:pro_likud_70' = '70th percentile - 6 days',
                          'temporal_grouptemporal_distance > 200:postTRUE:pro_likud_70' = '70th percentile - 200+ days',
                          'temporal_grouptemporal_distance == 6:postTRUE:pro_likud_80' = '80th percentile - 6 days',
                          'temporal_grouptemporal_distance > 200:postTRUE:pro_likud_80' = '80th percentile - 200+ days'
             ),
             stars = T, 
             gof_omit = "^(?!.*Num. Obs.)",
             add_rows = data.frame('VARIABLES' = 'Dependent Variable',
                                   '1' = 'Likud',
                                   '2' = 'Likud',
                                   '3' = 'Likud',
                                   '4' = 'Right Wing (excluding Likud)',
                                   '5' = 'Right Wing (excluding Likud)',
                                   '6' = 'Right Wing (excluding Likud)',
                                   '7' = 'Turnout',
                                   '8' = 'Turnout',
                                   '9' = 'Turnout')
             
)






# LIKUD
election_percentages$pro_likud_2013 = election_percentages$pro_likud_60
# Clustered errors with interaction based on Likud vote share in 2013
reg_1_likud_het = feols(likud_percentage ~ temporal_group * post * pro_likud_2013,
                        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                        cluster = 'distance_Gaza')

# Clustered errors + controls + city fixed effects with interaction for pro/against Likud in 2013
reg_2_likud_het = feols(likud_percentage ~ temporal_group * post * pro_likud_2013 +
                          Pop_Total + density + Shape_Area + distance + ntl | as.factor(distance_Gaza),
                        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                        cluster = 'distance_Gaza')

# RIGHT WING
# Clustered errors with interaction based on Right Wing vote share in 2013
reg_1_right_wing_het = feols(right_wing_percentage ~ temporal_group * post * pro_likud_2013,
                             data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                             cluster = 'distance_Gaza')

# Clustered errors + controls + city fixed effects with interaction for pro/against Likud in 2013
reg_2_right_wing_het = feols(right_wing_percentage ~ temporal_group * post * pro_likud_2013 +
                               distance + density + Shape_Area + Pop_Total + ntl | as.factor(distance_Gaza),
                             data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                             cluster = 'distance_Gaza')




# TURNOUT
# Clustered errors with interaction based on Turnout in 2013
reg_1_turnout_het = feols(turnout_percentage ~ temporal_group * post * pro_likud_2013,
                          data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                          cluster = 'distance_Gaza')

# Clustered errors + controls + city fixed effects with interaction for Turnout in 2013
reg_2_turnout_het = feols(turnout_percentage ~ temporal_group * post * pro_likud_2013 +
                            distance + density + Shape_Area + Pop_Total + ntl | as.factor(distance_Gaza),
                          data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                          cluster = 'distance_Gaza')

# table: pro-likud
modelsummary(list(reg_1_likud_het, reg_2_likud_het, reg_1_right_wing_het, reg_2_right_wing_het, reg_1_turnout_het, reg_2_turnout_het),
             # output = 'latex_tabular',
             booktabs = TRUE,
             coef_map = c('temporal_grouptemporal_distance == 6:postTRUE:pro_likud_2013' = 'Red Alert 6 Days Before * Pro Likud * Post',
                          'temporal_grouptemporal_distance > 200:postTRUE:pro_likud_2013' = 'Red Alert 200+ Days Before * Pro Likud * Post',
                          'temporal_grouptemporal_distance == 6:postTRUE' = 'Red Alert 6 Days Before * Post',
                          'temporal_grouptemporal_distance > 200:postTRUE' = 'Red Alert 200+ Days Before * Post',
                          'temporal_grouptemporal_distance == 6'  = 'Red Alert 6 Days Before',
                          'temporal_grouptemporal_distance > 200' = 'Red Alert 200+ Days Before',
                          'pro_likud_2013' = 'Pro Likud',
                          '(Intercept)' = 'Intercept'),
             stars = T, 
             notes = 'NRA 2015 stands for No Red Alerts until the 2015 Election.',
             gof_omit= '',
             add_rows = data.frame('VARIABLES' = c('Observations',
                                                   'Control Variables',
                                                   'Locality Fixed Effects',
                                                   'Clustered Errors',
                                                   'Control Group',
                                                   'Dependent Variable'),
                                   '1' = c(round(summary(reg_1_likud_het)$nobs/2,0),
                                           'No',
                                           'No',
                                           'Yes',
                                           'NRA 2015',
                                           'Likud'),
                                   '2' = c(round(summary(reg_2_likud_het)$nobs/2,0),
                                           'Yes',
                                           'Yes',
                                           'Yes',
                                           'NRA 2015',
                                           'Likud'),
                                   '3' = c(round(summary(reg_1_right_wing_het)$nobs/2,0),
                                           'No',
                                           'No',
                                           'Yes',
                                           'NRA 2015',
                                           'Right Wing (excluding Likud)'),
                                   '4' = c(round(summary(reg_2_right_wing_het)$nobs/2,0),
                                           'Yes',
                                           'Yes',
                                           'Yes',
                                           'NRA 2015',
                                           'Right Wing (excluding Likud)'),
                                   '5' = c(round(summary(reg_1_turnout_het)$nobs/2,0),
                                           'No',
                                           'No',
                                           'Yes',
                                           'NRA 2015',
                                           'Turnout'),
                                   '6' = c(round(summary(reg_2_turnout_het)$nobs/2,0),
                                           'Yes',
                                           'Yes',
                                           'Yes',
                                           'NRA 2015',
                                           'Turnout')
             ))























#### by number of Red Alerts ####

# LIKUD
# Clustered errors with interaction based on Likud vote share in 2013
reg_1_likud_het_number = feols(likud_percentage ~ temporal_group * post * number_of_red_alerts,
                               data = election_percentages ,
                               cluster = 'distance_Gaza')

# Clustered errors + controls + city fixed effects with interaction for pro/against Likud in 2013
reg_2_likud_het_number = feols(likud_percentage ~ temporal_group * post * number_of_red_alerts +
                                 distance + density + Shape_Area + Pop_Total + ntl | as.factor(distance_Gaza),
                               data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                               cluster = 'distance_Gaza')



# RIGHT WING
# Clustered errors with interaction based on Right Wing vote share in 2013
reg_1_right_wing_het_number = feols(right_wing_percentage ~ temporal_group * post * number_of_red_alerts,
                                    data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                                    cluster = 'distance_Gaza')

# Clustered errors + controls + city fixed effects with interaction for pro/against Likud in 2013
reg_2_right_wing_het_number = feols(right_wing_percentage ~ temporal_group * post * number_of_red_alerts +
                                      distance + density + Shape_Area + Pop_Total + ntl | as.factor(distance_Gaza),
                                    data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                                    cluster = 'distance_Gaza')



# TURNOUT
# Clustered errors with interaction based on Turnout in 2013
reg_1_turnout_het_number = feols(turnout_percentage ~ temporal_group * post * number_of_red_alerts,
                                 data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                                 cluster = 'distance_Gaza')

# Clustered errors + controls + city fixed effects with interaction for Turnout in 2013
reg_2_turnout_het_number = feols(turnout_percentage ~ temporal_group * post * number_of_red_alerts +
                                   distance + density + Shape_Area + Pop_Total + ntl | as.factor(distance_Gaza),
                                 data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                                 cluster = 'distance_Gaza')



# table: pro-likud, pro-right-wing, high turnout
modelsummary(list(reg_1_likud_het_number, reg_2_likud_het_number, reg_1_right_wing_het_number,
                  reg_2_right_wing_het_number, reg_1_turnout_het_number, reg_2_turnout_het_number),
             # coef_map = c('temporal_grouptemporal_distance == 6:postTRUE:number_of_red_alerts' = 'Red Alert 6 Days Before * Number of Red Alerts * Post',
             #              'temporal_grouptemporal_distance > 200:postTRUE:number_of_red_alerts' = 'Red Alert 200+ Days Before * Number of Red Alerts * Post',
             #              '(Intercept)' = 'Intercept'),
             stars = T, 
             notes = 'NRA 2015 stands for No Red Alerts until the 2015 Election.',
             gof_omit= '',
             add_rows = data.frame('VARIABLES' = c('Observations',
                                                   'Control Variables',
                                                   'Locality Fixed Effects',
                                                   'Clustered Errors',
                                                   'Control Group',
                                                   'Dependent Variable'),
                                   '1' = c(round(summary(reg_1_likud_het_number)$nobs/2,0),
                                           'No',
                                           'No',
                                           'Yes',
                                           'NRA 2015',
                                           'Likud'),
                                   '2' = c(round(summary(reg_2_likud_het_number)$nobs/2,0),
                                           'Yes',
                                           'Yes',
                                           'Yes',
                                           'NRA 2015',
                                           'Likud'),
                                   '3' = c(round(summary(reg_1_right_wing_het_number)$nobs/2,0),
                                           'No',
                                           'No',
                                           'Yes',
                                           'NRA 2015',
                                           'Right Wing'),
                                   '4' = c(round(summary(reg_2_right_wing_het_number)$nobs/2,0),
                                           'Yes',
                                           'Yes',
                                           'Yes',
                                           'NRA 2015',
                                           'Right Wing'),
                                   '5' = c(round(summary(reg_1_turnout_het_number)$nobs/2,0),
                                           'No',
                                           'No',
                                           'Yes',
                                           'NRA 2015',
                                           'Turnout'),
                                   '6' = c(round(summary(reg_2_turnout_het_number)$nobs/2,0),
                                           'Yes',
                                           'Yes',
                                           'Yes',
                                           'NRA 2015',
                                           'Turnout')
             ))



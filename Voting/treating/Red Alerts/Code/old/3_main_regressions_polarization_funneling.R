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
wd = 'C:/Users/luizb/Desktop/Dissertation/Dissertation/Red-Alerts-and-Votes/'
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

# Copying 2013 group value (pro or against Likud) to other years
election_percentages <- election_percentages %>%
  group_by(SEMEL_YISHUV) %>%
  mutate(pro_likud_60 = ifelse(year != 2013, 
                               first(pro_likud_60[year == 2013], default = NA), 
                               pro_likud_60),
         pro_likud_70 = ifelse(year != 2013, 
                               first(pro_likud_70[year == 2013], default = NA), 
                               pro_likud_70),
         pro_likud_80 = ifelse(year != 2013, 
                               first(pro_likud_80[year == 2013], default = NA), 
                               pro_likud_80)
  ) %>%
  ungroup()



# Regressions for each percentile
# Likud

# 60th percentile and above
reg_likud_60 <- feols(likud_percentage ~ i(year_election, temporal_group,
                                           ref = '2013',
                                           ref2 = 'no_red_alert') * pro_likud_60 |
                        as.factor(year_election) + as.factor(SEMEL_YISHUV),
                      data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                      se = 'standard')


# CALCULATING MEAN EFFECT
# pro_likud_60:time_to_treatment::0:temporal_group::temporal_distance == 6 coefficient
# pro_likud_60:year_election::2015:temporal_group::temporal_distance == 6 coefficient
coef_likud_60 <- reg_likud_60$coefficients[11] %>% as.numeric()

# likud_percentage mean for pro_likud_60:time_to_treatment::0:temporal_group::temporal_distance == 6 in 2013
mean_likud_2013_pro_60 <- mean(election_percentages[
  election_percentages$time_to_treatment == -1 & 
    election_percentages$temporal_group == "temporal_distance == 6" & 
    election_percentages$pro_likud_60 == 1,]$likud_percentage) 


effect_percent_pro_likud_60 <- (coef_likud_60 / mean_likud_2013_pro_60) * 100
effect_percent_pro_likud_60



# 70th percentile and above

reg_likud_70 <- feols(likud_percentage ~ i(year_election, temporal_group,
                                           ref = '2013', 
                                           ref2 = 'no_red_alert') * pro_likud_70 | 
                        as.factor(SEMEL_YISHUV) + as.factor(year_election),
                      data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                      se = 'standard')


# CALCULATING MEAN EFFECT

# pro_likud_70:time_to_treatment::0:temporal_group::temporal_distance == 6 coefficient
coef_likud_70 <- reg_likud_70$coefficients[11] %>% as.numeric()

# likud_percentage mean for pro_likud_70:time_to_treatment::0:temporal_group::temporal_distance == 6 in 2013
mean_likud_2013_pro_70 <- mean(election_percentages[
  election_percentages$time_to_treatment == -1 & 
    election_percentages$temporal_group == "temporal_distance == 6" & 
    election_percentages$pro_likud_70 == 1,]$likud_percentage) 


effect_percent_pro_likud_70 <- (coef_likud_70 / mean_likud_2013_pro_70) * 100
effect_percent_pro_likud_70


# 80th percentile and above

reg_likud_80 <- feols(likud_percentage ~ i(year_election, temporal_group,
                                           ref = '2013', 
                                           ref2 = 'no_red_alert') * pro_likud_80 | 
                        as.factor(SEMEL_YISHUV) + as.factor(year_election),
                      data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                      se = 'standard')


# CALCULATING MEAN EFFECT

# pro_likud_70:time_to_treatment::0:temporal_group::temporal_distance == 6 coefficient
coef_likud_80 <- reg_likud_80$coefficients[11] %>% as.numeric()

# likud_percentage mean for pro_likud_70:time_to_treatment::0:temporal_group::temporal_distance == 6 in 2013
mean_likud_2013_pro_80 <- mean(election_percentages[
  election_percentages$time_to_treatment == -1 & 
    election_percentages$temporal_group == "temporal_distance == 6" & 
    election_percentages$pro_likud_80 == 1,]$likud_percentage) 

effect_percent_pro_likud_80 <- (coef_likud_80 / mean_likud_2013_pro_80) * 100
effect_percent_pro_likud_80




# Right Wing
# 60th percentile and above
reg_right_wing_60 <- feols(right_wing_percentage ~ i(year_election, temporal_group,
                                                     ref = '2013', 
                                                     ref2 = 'no_red_alert') * pro_likud_60| 
                             as.factor(SEMEL_YISHUV) + as.factor(year_election),
                           data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                           se = 'standard')



coef_right_wing_60 <- reg_right_wing_60$coefficients[11] %>% as.numeric()

mean_right_wing_2013_pro_60 = mean(election_percentages[
  election_percentages$time_to_treatment == -1 & 
    election_percentages$temporal_group == "temporal_distance == 6" & 
    election_percentages$pro_likud_60 == 1,]$right_wing_percentage)


effect_percent_pro_right_wing_60 = coef_right_wing_60/mean_right_wing_2013_pro_60
effect_percent_pro_right_wing_60


# 70th and above
reg_right_wing_70 <- feols(right_wing_percentage ~ i(year_election, temporal_group,
                                                     ref = '2013', 
                                                     ref2 = 'no_red_alert') * pro_likud_70| 
                             as.factor(SEMEL_YISHUV) + as.factor(year_election),
                           data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                           se = 'standard')



coef_right_wing_70 <- reg_right_wing_70$coefficients[11] %>% as.numeric()

mean_right_wing_2013_pro_70 = mean(election_percentages[
  election_percentages$time_to_treatment == -1 & 
    election_percentages$temporal_group == "temporal_distance == 6" & 
    election_percentages$pro_likud_70 == 1,]$right_wing_percentage)


effect_percent_pro_right_wing_70 = coef_right_wing_70/mean_right_wing_2013_pro_70
effect_percent_pro_right_wing_70



# 80th and above

reg_right_wing_80 <- feols(right_wing_percentage ~ i(year_election, temporal_group,
                                                     ref = '2013', 
                                                     ref2 = 'no_red_alert') * pro_likud_80| 
                             as.factor(SEMEL_YISHUV) + as.factor(year_election),
                           data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                           se = 'standard')


coef_right_wing_80 <- reg_right_wing_80$coefficients[11] %>% as.numeric()

mean_right_wing_2013_pro_80 = mean(election_percentages[
  election_percentages$time_to_treatment == -1 & 
    election_percentages$temporal_group == "temporal_distance == 6" & 
    election_percentages$pro_likud_80 == 1,]$right_wing_percentage)


effect_percent_pro_right_wing_80 = coef_right_wing_80/mean_right_wing_2013_pro_80
effect_percent_pro_right_wing_80




# Turnout

# 60th percentile and above
reg_turnout_60 <- feols(turnout_percentage ~ i(year_election, temporal_group,
                                               ref = '2013', 
                                               ref2 = 'no_red_alert') * pro_likud_60| 
                          as.factor(SEMEL_YISHUV) + as.factor(year_election),
                        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                        se = 'standard')


coef_turnout_60 <- reg_turnout_60$coefficients[11] %>% as.numeric()

mean_turnout_2013_pro_60 = mean(election_percentages[
  election_percentages$time_to_treatment == -1 & 
    election_percentages$temporal_group == "temporal_distance == 6" & 
    election_percentages$pro_likud_60 == 1,]$turnout_percentage)


effect_percent_pro_turnout_60 = coef_turnout_60/mean_turnout_2013_pro_60
effect_percent_pro_turnout_60



# 70th and above
reg_turnout_70 <- feols(turnout_percentage ~ i(year_election, temporal_group,
                                               ref = '2013', 
                                               ref2 = 'no_red_alert') * pro_likud_70| 
                          as.factor(SEMEL_YISHUV) + as.factor(year_election),
                        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                        se = 'standard')


coef_turnout_70 <- reg_turnout_70$coefficients[11] %>% as.numeric()

mean_turnout_2013_pro_70 = mean(election_percentages[
  election_percentages$time_to_treatment == -1 & 
    election_percentages$temporal_group == "temporal_distance == 6" & 
    election_percentages$pro_likud_70 == 1,]$turnout_percentage)


effect_percent_pro_turnout_70 = coef_turnout_70/mean_turnout_2013_pro_70
effect_percent_pro_turnout_70



# 80th and above
reg_turnout_80 <- feols(turnout_percentage ~ i(year_election, temporal_group,
                                               ref = '2013', 
                                               ref2 = 'no_red_alert') * pro_likud_80| 
                          as.factor(SEMEL_YISHUV) + as.factor(year_election),
                        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                        se = 'standard')



coef_turnout_80 <- reg_turnout_80$coefficients[11] %>% as.numeric()

mean_turnout_2013_pro_80 = mean(election_percentages[
  election_percentages$time_to_treatment == -1 & 
    election_percentages$temporal_group == "temporal_distance == 6" & 
    election_percentages$pro_likud_80 == 1,]$turnout_percentage)


effect_percent_pro_turnout_80 = coef_turnout_80/mean_turnout_2013_pro_80
effect_percent_pro_turnout_80



modelsummary(list(reg_likud_60, reg_likud_70, reg_likud_80,
                  reg_right_wing_60, reg_right_wing_70, reg_right_wing_80,
                  reg_turnout_60, reg_turnout_70, reg_turnout_80),
             output = 'latex_tabular',
             # output = 'treating/Red Alerts/Output/Figures/3_polarization_results.tex',
             booktabs = TRUE,
             coef_map = c('pro_likud_60:year_election::2015:temporal_group::temporal_distance == 6' = '60th percentile - 6 days - 2015',
                          'pro_likud_60:year_election::2015:temporal_group::temporal_distance > 149' = '60th percentile - 149+ days - 2015',
                          'pro_likud_70:year_election::2015:temporal_group::temporal_distance == 6' = '70th percentile - 6 days - 2015',
                          'pro_likud_70:year_election::2015:temporal_group::temporal_distance > 149' = '70th percentile - 149+ days - 2015',
                          'pro_likud_80:year_election::2015:temporal_group::temporal_distance == 6' = '80th percentile - 6 days - 2015',
                          'pro_likud_80:year_election::2015:temporal_group::temporal_distance > 149' = '80th percentile - 149+ days - 2015'
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


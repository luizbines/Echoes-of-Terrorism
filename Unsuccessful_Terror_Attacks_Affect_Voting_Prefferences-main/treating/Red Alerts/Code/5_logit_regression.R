# This script runs the logit regressions to check whether 
# experiencing a red alert affects the probability of future alerts
# Luiz Bines
# 2024

# Library
library(dplyr)
library(lubridate)
library(zoo)
library(tidyverse)
library(glmnet)
library(modelsummary)

# Directory
wd = 'C:/Users/luizb/Desktop/Echoes-of-Terrorism/Unsuccessful_Terror_Attacks_Affect_Voting_Prefferences-main/'
setwd(wd);

# Importing
all_years_cities_grid = read.csv('treating/Red Alerts/Output/2_all_years_cities_grid.csv')



# Cleaning
all_years_cities_grid <- all_years_cities_grid %>%
  mutate(date = as.Date(date)) 
  
# Calculating cumulative sum of attacks
all_years_cities_grid <- all_years_cities_grid %>%
  arrange(SEMEL_YISHUV, date) %>%
  group_by(SEMEL_YISHUV) %>% 
  mutate(quantity_of_alarms = cumsum(lag(alert, default = 0))) %>%
  ungroup()
 
# Creating dummy that indicates whether the locality was attacked in the future
all_years_cities_grid <- all_years_cities_grid %>%
  arrange(SEMEL_YISHUV, date) %>%
  group_by(SEMEL_YISHUV) %>%
  mutate(future_red_alert = ifelse(cumsum(alert) < max(cumsum(alert)), 1, 0)) %>%
  ungroup()


# Creating dummies for years
all_years_cities_grid <- all_years_cities_grid %>%
  mutate(year = year(date))

# Create a dummy variable for future red alert within 1 year
all_years_cities_grid <- all_years_cities_grid %>%
  arrange(SEMEL_YISHUV, date) %>%
  group_by(SEMEL_YISHUV) %>%
  mutate(future_red_alert_within_year = ifelse(
    sapply(date, function(x) any(alert[date > x & date <= x + 365] == 1)),
    1,
    0
  )) %>%
  ungroup()




# Logit model: 
# Probability of future Red Alert considering the number of previous Red Alerts
model_1 <- glm(future_red_alert ~ quantity_of_alarms, 
             family = binomial(link = "logit"), 
             data = all_years_cities_grid)


### Alternative models ###


# Filtering 2021 and 2022
model_2 <- glm(future_red_alert ~ quantity_of_alarms,
               family = binomial(link = "logit"),
               data = all_years_cities_grid %>% filter(year<2022))



# Model with interaction
model_3 <- glm(future_red_alert ~ quantity_of_alarms * factor(year),
               family = binomial(link = "logit"),
               data = all_years_cities_grid)


# Probability of Red Alert within 1 year considering the number of previous Red Alerts
# excluding 2022
model_4 <- glm(future_red_alert_within_year ~ quantity_of_alarms + factor(year),
               family = binomial(link = "logit"),
               data = all_years_cities_grid %>% filter(year < 2022))


# Results Table
modelsummary(list(model_1, model_2, model_3, model_4), 
             # output = 'latex_tabular',
             output = 'treating/Red Alerts/Output/Figures/5_logit_probability_of_alert.tex',
             # coef_rename = c("quantity_of_alarms" = "Quantity of Previous Red Alerts")
             coef_map = c('quantity_of_alarms' = 'Quantity of Previous Red Alerts',
                          'quantity_of_alarms:factor(year)2014' = 'Quantity of Previous Red Alerts * 2014',
                          'quantity_of_alarms:factor(year)2015' = 'Quantity of Previous Red Alerts * 2015',
                          'quantity_of_alarms:factor(year)2016' = 'Quantity of Previous Red Alerts * 2016',
                          'quantity_of_alarms:factor(year)2017' = 'Quantity of Previous Red Alerts * 2017',
                          'quantity_of_alarms:factor(year)2018' = 'Quantity of Previous Red Alerts * 2018',
                          'quantity_of_alarms:factor(year)2019' = 'Quantity of Previous Red Alerts * 2019',
                          'quantity_of_alarms:factor(year)2020' = 'Quantity of Previous Red Alerts * 2020',
                          'quantity_of_alarms:factor(year)2021' = 'Quantity of Previous Red Alerts * 2021',
                          'quantity_of_alarms:factor(year)2022' = 'Quantity of Previous Red Alerts * 2022'
                          ),
             gof_omit= '',
             stars = TRUE, 
             add_rows = 
               data.frame('VARIABLES' = c('Period',
                                          'Year Fixed Effects',
                                          'Observations'
               ),
               '1' = c(
                 '2014-2022',
                 'No',
                 nrow(model_1$data)
               ),
               '2' = c(
                 '2014-2021',
                 'No',
                 nrow(model_2$data)
               ),
               '3' = c(
                 '2014-2022',
                 'Yes',
                 nrow(model_3$data)
               ),
               '4' = c(
                 '2014-2021',
                 'Yes',
                 nrow(model_4$data)
               )
               )
)





#### Plotting ####

### MODEL 1 ###

# X axis (number of Previous Red Alerts)
pred_data <- data.frame(quantity_of_alarms = seq(0, 13, by = 1))

# Predicting
pred_data$future_red_alert_prob <- predict(model_1, newdata = pred_data, type = "response")

# Creating Graph
ggplot(pred_data, aes(x = quantity_of_alarms, y = future_red_alert_prob)) +
  geom_line(color = "blue") +
  labs(
    x = "Number of Previous Red Alerts",
    y = "Probability of New Red Alert in the Future") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)
ggsave('treating/Red Alerts/Output/Figures/5_logit_probability_of_alert.pdf')


# X axis (number of Previous Red Alerts)
pred_data_within_year <- data.frame(quantity_of_alarms = seq(0, 13, by = 1))

# Predicting
pred_data_within_year$future_red_alert_within_year_prob <- predict(model_2, newdata = pred_data_within_year, type = "response")

# Creating Graph
ggplot(pred_data_within_year, aes(x = quantity_of_alarms, y = future_red_alert_within_year_prob)) +
  geom_line(color = "red") +
  labs(
    x = "Number of Previous Red Alerts",
    y = "Probability of Red Alert within 1 Year") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)
ggsave('treating/Red Alerts/Output/Figures/5_logit_probability_of_alert_1_year.pdf')



# Old model
# ### ALTERNATIVE MODEL ###
# # Probability of Red Alert in up to 1 year considering the number of previous Red Alerts
#
# # Create a dummy variable for future red alert within 1 year
# all_years_cities_grid <- all_years_cities_grid %>%
#   arrange(SEMEL_YISHUV, date) %>%
#   group_by(SEMEL_YISHUV) %>%
#   mutate(future_red_alert_within_year = ifelse(
#     sapply(date, function(x) any(alert[date > x & date <= x + 365] == 1)),
#     1,
#     0
#   )) %>%
#   ungroup()


# # Logit model: Probability of Red Alert within 1 year considering the number of previous Red Alerts
# # excluding 2022
# model_2 <- glm(future_red_alert_within_year ~ quantity_of_alarms,
#                family = binomial(link = "logit"),
#                data = all_years_cities_grid %>% filter(year < 2022))
# 



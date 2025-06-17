# This script prepares the datasets for the regressions,
# merges the datasets with israel demographics
# and creates descriptive statistics
# Luiz Bines
# 2024

# Library
library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
library(ggplot2)
library(sf)
library(fixest)
library(modelsummary)
library(geosphere)
library(lubridate)
library(scales)
library(units)
library(stringi)
library(stringdist)
library(kableExtra)

# Directory
wd = 'C:/Users/luizb/Desktop/Dissertation/Dissertation/Red-Alerts-and-Votes/'
setwd(wd);


##### IMPORTING #####
red_alerts = read.csv('treating/Red Alerts/Output/1_red_alerts_with_coordinates_and_electoral_localities.csv')
likud_percentage = read.csv('treating/Red Alerts/Output/1_likud_percentage_with_coordinates.csv')
right_wing_percentage = read.csv('treating/Red Alerts/Output/1_right_wing_percentage_with_coordinates.csv')
turnout_percentage = read.csv('treating/Red Alerts/Output/1_turnout_percentage_with_coordinates.csv')


israel_panel = read.csv('cleaning/Israel/Output/2_israel_panel_lights.csv')

all_red_alerts = read.csv('treating/Red Alerts/Output/1_ALL_red_alerts_with_coordinates_and_electoral_localities.csv')

##### PREPARING FOR REGRESSIONS #####


# RESTRICTING: 75-150KM FROM GAZA
likud_percentage = likud_percentage %>% filter(distance > 75 & distance < 150)
right_wing_percentage = right_wing_percentage %>% filter(distance > 75 & distance < 150)
turnout_percentage = turnout_percentage %>% filter(distance > 75 & distance < 150)



# Temporal Distance to 2015 election
red_alerts = red_alerts %>% 
  mutate(
    date = as.Date(date),
    temporal_distance = (ymd('2015-03-17') - date) %>% as.numeric(),
    alert = 1
  )

all_red_alerts = all_red_alerts %>% 
  mutate(
    date = as.Date(date),
    temporal_distance = (ymd('2015-03-17') - date) %>% as.numeric(),
    alert = 1
  )



# Counting red alerts at each temporal groups
red_alerts_grouped <- red_alerts %>%
  mutate(
    temporal_group = cut(temporal_distance,
                         breaks = c(-Inf, 6, Inf),
                         labels = c("number_of_alerts_6_days", "number_of_alerts_149_days"),
                         include.lowest = TRUE) %>% as.character(),
    temporal_group = ifelse(is.na(temporal_group), 'no_red_alert', temporal_group)
  ) %>%
  group_by(SEMEL_YISHUV, temporal_group) %>%
  summarise(number_of_red_alerts = n(), .groups = 'drop') %>%

  # Creating one variable for each temporal group
  pivot_wider(names_from = temporal_group,
              values_from = number_of_red_alerts,
              values_fill = list(number_of_red_alerts = 0))



# adding treatment dummy
likud_percentage = likud_percentage %>% 
  mutate(
    treated = ifelse(SEMEL_YISHUV %in% red_alerts[red_alerts$alarm == 1,]$SEMEL_YISHUV,
                     1,0)) %>% 
  left_join(
    red_alerts %>%
      group_by(SEMEL_YISHUV) %>%
      summarise(
        # adding temporal distance variable
        temporal_distance = min(temporal_distance),
        # # adding number of red alerts variable
        number_of_red_alerts = length(SEMEL_YISHUV),
        # area = first(area)
      ),
    by = "SEMEL_YISHUV"
  ) %>% 
  mutate(
    temporal_group = cut(temporal_distance, 
                         breaks = c(-Inf, 6, Inf), 
                         labels = c("temporal_distance == 6", "temporal_distance > 149"),
                         include.lowest = TRUE) %>% as.character(),
    temporal_group = ifelse(is.na(temporal_group),'no_red_alert',temporal_group)
  )



  # same thing for right_wing dataset
right_wing_percentage = right_wing_percentage %>% 
  mutate(
    treated = ifelse(SEMEL_YISHUV %in% red_alerts$SEMEL_YISHUV,
                     1,0)) %>% 
  left_join(
    red_alerts %>%
      group_by(SEMEL_YISHUV) %>%
      summarise(
        # adding temporal distance variable
        temporal_distance = min(temporal_distance),
        # adding number of red alerts variable
        number_of_red_alerts = length(SEMEL_YISHUV),
        area = first(area)
      ),
    by = "SEMEL_YISHUV"
  ) %>% 
  mutate(
    temporal_group = cut(temporal_distance, 
                         breaks = c(-Inf, 6, Inf), 
                         labels = c("temporal_distance == 6", "temporal_distance > 149"),
                         include.lowest = TRUE) %>% as.character(),
    temporal_group = ifelse(is.na(temporal_group),'no_red_alert',temporal_group)
  )

# adding number of red alerts at each temporal group
right_wing_percentage <- right_wing_percentage %>%
  left_join(red_alerts_grouped, by = "SEMEL_YISHUV") %>%
  # Substituting NA for 0 in case of No Red Alerts
  mutate(number_of_alerts_6_days = replace_na(number_of_alerts_6_days, 0),
         number_of_alerts_149_days = replace_na(number_of_alerts_149_days, 0))


# same thing for turnout dataset
turnout_percentage = turnout_percentage %>% 
  mutate(
    treated = ifelse(SEMEL_YISHUV %in% red_alerts$SEMEL_YISHUV,
                     1,0)) %>% 
  left_join(
    red_alerts %>%
      group_by(SEMEL_YISHUV) %>%
      summarise(
        # adding temporal distance variable
        temporal_distance = min(temporal_distance),
        # adding number of red alerts variable
        number_of_red_alerts = length(SEMEL_YISHUV),
        area = first(area)
      ),
    by = "SEMEL_YISHUV"
  ) %>% 
  mutate(
    temporal_group = cut(temporal_distance, 
                         breaks = c(-Inf, 6, Inf), 
                         labels = c("temporal_distance == 6", "temporal_distance > 149"),
                         include.lowest = TRUE) %>% as.character(),
    temporal_group = ifelse(is.na(temporal_group),'no_red_alert',temporal_group)
  )

# adding number of red alerts at each temporal group
turnout_percentage <- turnout_percentage %>%
  left_join(red_alerts_grouped, by = "SEMEL_YISHUV") %>%
  # Substituting NA for 0 in case of No Red Alerts
  mutate(number_of_alerts_6_days = replace_na(number_of_alerts_6_days, 0),
         number_of_alerts_149_days = replace_na(number_of_alerts_149_days, 0))


# pivoting likud_percentage
likud_percentage_prep = likud_percentage %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year", 
               values_to = "likud_percentage") %>%
  mutate(year = as.numeric(str_replace_all(year, c("X" = "", "_2" = ".2"))),
         post = ifelse(year > 2014, T, F))


  # same thing for right_wing dataset
right_wing_percentage_prep = right_wing_percentage %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year", 
               values_to = "right_wing_percentage") %>%
  mutate(year = as.numeric(str_replace_all(year, c("X" = "", "_2" = ".2"))),
         post = ifelse(year > 2014, T, F))


# same thing for right_wing dataset
turnout_percentage_prep = turnout_percentage %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year", 
               values_to = "turnout_percentage") %>%
  mutate(year = as.numeric(str_replace_all(year, c("X" = "", "_2" = ".2"))),
         post = ifelse(year > 2014, T, F))


# removing X before years
likud_percentage = likud_percentage %>% 
  rename_with(~ str_replace_all(., '^X', ''), starts_with('X'))





##### PREPARING FOR PROBABILITY OF RED ALERT REGRESSIONS #####

start_date <- as.Date("2014-07-24")
end_date <- as.Date("2015-03-17")
last_date <- as.Date("2022-12-31")


dates <- seq.Date(start_date, end_date, by = "day")
all_dates <- seq.Date(start_date, last_date, by = "day")

# one observation for each city-day
cities_grid <- expand.grid(SEMEL_YISHUV = unique(likud_percentage$SEMEL_YISHUV),
                           date = dates) %>% 
  merge(likud_percentage %>% dplyr::select(SEMEL_YISHUV,distance), by = "SEMEL_YISHUV", all.x = TRUE)





cities_grid <- merge(cities_grid,
                     # area
                     red_alerts %>% dplyr::select(SEMEL_YISHUV, date, alert) %>% distinct(),
                     by = c("SEMEL_YISHUV", "date"),
                     all.x = T,
                     all.y = F) %>% 
  mutate(
    alert = ifelse(is.na(alert), 0, alert)
  )




all_years_cities_grid <- expand.grid(SEMEL_YISHUV = unique(likud_percentage$SEMEL_YISHUV),
                               date = all_dates) %>%
  merge(likud_percentage %>% select(SEMEL_YISHUV,distance), by = "SEMEL_YISHUV", all.x = TRUE)
  


all_years_cities_grid <- merge(all_years_cities_grid,
                     all_red_alerts %>% dplyr::select(SEMEL_YISHUV, date, alert) %>% distinct(),
                     by = c("SEMEL_YISHUV", "date"),
                     all.x = T,
                     all.y = F) %>% 
  mutate(
    alert = ifelse(is.na(alert), 0, alert)
  )




##### MERGING LIKUD PERCENTAGE WITH ISRAEL DEMOGRAPHICS #####
likud_percentage_regs = likud_percentage_prep %>% 
  mutate(
    year_election = year,
    year = as.integer(year)
    ) %>%
  merge(israel_panel,
        by = c('SEMEL_YISHUV','year'),
        all.x = T,
        all.y = F)


right_wing_percentage_regs = right_wing_percentage_prep %>% 
  mutate(
    year_election = year,
    year = as.integer(year)
    ) %>% 
  merge(israel_panel,
        by = c('SEMEL_YISHUV','year'),
        all.x = T,
        all.y = F)


turnout_percentage_regs = turnout_percentage_prep %>% 
  mutate(
    year_election = year,
    year = as.integer(year)
    ) %>% 
  merge(israel_panel,
        by = c('SEMEL_YISHUV','year'),
        all.x = T,
        all.y = F)



#### DESCRIPTIVES #####

descriptive_statistics <- likud_percentage_regs %>%

  # filtering 2013 and 2015
  filter(year == 2013 | year == 2015) %>% 
  # filtering arab cities
  filter(Religion_yishuv_Code != 2) %>% 

  # merging with right wing percentage dataset
  merge(right_wing_percentage_regs %>% 
          filter(Religion_yishuv_Code !=2) %>% 
          select(SEMEL_YISHUV,year,right_wing_percentage),
        by = c('SEMEL_YISHUV','year')) %>% 
  merge(turnout_percentage_regs %>% 
          filter(Religion_yishuv_Code != 2) %>% 
          select(SEMEL_YISHUV,year,turnout_percentage),
        by = c('SEMEL_YISHUV','year')) %>% 
  # defining groups
  mutate(group = case_when(
    temporal_group == 'no_red_alert' ~ 'No Red Alert',
    temporal_group == 'temporal_distance > 149' ~ 'Last Red Alert 149+ Days Before 2015 Election',
    temporal_group == 'temporal_distance == 6' ~ 'Last Red Alert 6 Days Before 2015 Election',
    TRUE ~ NA_character_
  )) %>%
  mutate(Election = year) %>%
  group_by(group, Election) %>%
  # Statistics:
  summarise(
    'Likud % (Average)' = mean(likud_percentage * 100, na.rm = TRUE),
    'Likud % (SD)' = sd(likud_percentage * 100, na.rm = TRUE),
    'Right Wing % (Average)' = mean(right_wing_percentage * 100, na.rm = TRUE),
    'Right Wing % (SD)' = sd(right_wing_percentage * 100, na.rm = TRUE),
    'Turnout % (Average)' = mean(turnout_percentage * 100, na.rm = TRUE),
    'Turnout % (SD)' = sd(turnout_percentage * 100, na.rm = TRUE),    
    'Night Lights (0-63) (Average)' = mean(ntl, na.rm = TRUE),
    'Night Lights (0-63) (SD)' = sd(ntl, na.rm = TRUE),
    'Population Size (Average)' = mean(Pop_Total, na.rm = TRUE),
    'Population Size (SD)' = sd(Pop_Total, na.rm = TRUE),
    'Population Density (per km2) (Average)' = mean(density, na.rm = TRUE),
    'Population Density (per km2) (SD)' = sd(density, na.rm = TRUE),
    'Area (km2) (Average)' = mean(Shape_Area, na.rm = TRUE),
    'Area (km2) (SD)' = sd(Shape_Area, na.rm = TRUE),
    'Distance to Gaza (km) (Average)' = mean(distance, na.rm = TRUE),
    'Distance to Gaza (km) (SD)' = sd(distance, na.rm = TRUE),
    'N' = n()

  ) %>%
  arrange(group, Election)


# Filtering control group "No Red Alert"
control_group <- descriptive_statistics %>%
  filter(group == "No Red Alert")

# Function to calculate difference and SE
calculate_diff_se <- function(avg1, sd1, n1, avg2, sd2, n2) {
  diff <- avg2 - avg1
  se_diff <- sqrt((sd1^2 / n1) + (sd2^2 / n2))
  return(c(diff, se_diff))
}

# Creating results list
results <- list()

# Interest variables
variables <- c("Likud % (Average)", "Right Wing % (Average)", "Turnout % (Average)",
               "Night Lights (0-63) (Average)", "Population Size (Average)",
               "Population Density (per km2) (Average)", "Area (km2) (Average)",
               "Distance to Gaza (km) (Average)")

# Iterating for treatment groups
for (group_name in c("Last Red Alert 149+ Days Before 2015 Election", 
                     "Last Red Alert 6 Days Before 2015 Election")) {
  
  # Filtering treatment group data
  group_data <- descriptive_statistics %>%
    filter(group == group_name)
  
  
  # Sample size for control and treatment group
  n1 <- control_group$N[1]
  n2 <- group_data$N[1]
  
  
  for (var in variables) {
    # Correcting variable' SD
    var_sd <- gsub(" \\(Average\\)", " (SD)", var)
    
    # Calculating differences in mean and SD
    diff_se <- calculate_diff_se(
      control_group[[var]][1], control_group[[var_sd]][1], n1,
      group_data[[var]][1], group_data[[var_sd]][1], n2
    )
    
    # Storing results
    results[[length(results) + 1]] <- c(group_name, var, diff_se[1], diff_se[2])
  }
}

# Creating results dataframe
results_df <- do.call(rbind, results)
results_df <- as.data.frame(results_df)
colnames(results_df) <- c("Group", "Variable", "Difference", "SE")


# Function to determine significance level
get_significance_level <- function(diff, se) {
  z_value <- abs(diff / se)
  
  # Significance levels
  if (z_value > 3.291) {
    return("0.1%")  # p < 0.001
  } else if (z_value > 2.576) {
    return("1%")    # p < 0.01
  } else if (z_value > 1.96) {
    return("5%")    # p < 0.05
  } else if (z_value > 1.645) {
    return("10%")   # p < 0.1
  } else {
    return("")      # Not significant
  }
}

# Adding significance column
results_df$Significance <- mapply(get_significance_level, as.numeric(results_df$Difference), as.numeric(results_df$SE))


# Adjusting dataframe
results_df = results_df %>% pivot_wider(
  names_from = Group,
  values_from = c(Difference, SE, Significance),
  names_sep = "_"
)

no_red_alerts_descriptives = descriptive_statistics %>% 
  filter(Election == 2013 & group == 'No Red Alert')


#### LATEX DESCRIPTIVE STATISTICS TABLE ####
# HTML descriptive statistics table
statistics_table <- descriptive_statistics %>% filter(Election==2013) %>%
  pivot_longer(cols = -c(group, Election), names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = group, values_from = Value) %>%
  mutate(
    `Diff (vs No Red Alerts)` = `Last Red Alert 149+ Days Before 2015 Election` - `No Red Alert`,
    `Diff (vs No Red Alerts)_6days` = `Last Red Alert 6 Days Before 2015 Election` - `No Red Alert`
  ) %>%
  select(
    Statistic,
    `No Red Alert`,
    `Last Red Alert 149+ Days Before 2015 Election`,
    `Diff (vs No Red Alerts)`,
    `Last Red Alert 6 Days Before 2015 Election`,
    `Diff (vs No Red Alerts)_6days`
  )

# Adjusting names
colnames(statistics_table) <- c(
  "Statistic",
  "No Red Alerts (2013)",
  "Last Red Alert 149+ Days Before (2013)",
  "Diff (vs No Red Alerts)",
  "Last Red Alert 6 Days Before (2013)",
  "Diff (vs No Red Alerts)"
)

# Exhibiting 
statistics_table %>%
  kable("html", digits = 2, align = 'c', caption = "Table 1: Descriptive Statistics by Groups of Interest for 2013") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


##### EXPORTING #####

write.csv(likud_percentage_regs, 'treating/Red Alerts/Output/2_likud_percentage_panel.csv',
          row.names = F)
write.csv(right_wing_percentage_regs, 'treating/Red Alerts/Output/2_right_wing_percentage_panel.csv',
          row.names = F)
write.csv(turnout_percentage_regs, 'treating/Red Alerts/Output/2_turnout_percentage_panel.csv',
          row.names = F)

write.csv(cities_grid, 'treating/Red Alerts/Output/2_cities_grid.csv', row.names = F)
write.csv(all_years_cities_grid, 'treating/Red Alerts/Output/2_all_years_cities_grid.csv', row.names = F)


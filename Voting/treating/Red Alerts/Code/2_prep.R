# This script merges the election datasets with red alerts
# Additionally, it creates the datasets used for the Logit regressions
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
library(purrr)
library(kableExtra)
library(tidyverse)

# Directory
wd = '/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Voting/'
setwd(wd);


##### IMPORTING #####

parties_percentages = read_csv('treating/Red Alerts/Output/1_parties_percentages_with_coordinates.csv')

israel_panel = read.csv('cleaning/Israel/Output/2_israel_panel_lights.csv')

red_alerts = read.csv('treating/Red Alerts/Output/1_red_alerts_with_coordinates_and_electoral_localities.csv')
all_red_alerts = read.csv('treating/Red Alerts/Output/1_ALL_red_alerts_with_coordinates_and_electoral_localities.csv')

##### PROCESSING #####


# Adding Temporal Distance to 2015 election
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



# --- MERGE VOTING DATA WITH RED ALERTS ---
# This section identifies "treated" locations and calculates the intensity/timing of alerts.
parties_percentages = parties_percentages %>% 
  # Create a binary 'treated' flag: 1 if the location (SEMEL_YISHUV) appears in the red alerts list
  mutate(
    treated = ifelse(SEMEL_YISHUV %in% red_alerts[red_alerts$alarm == 1,]$SEMEL_YISHUV, 1, 0)
  ) %>% 
  # Join with aggregated alert statistics
  left_join(
    red_alerts %>%
      group_by(SEMEL_YISHUV) %>%
      summarise(
        # Capture the closest alert in time to the election
        temporal_distance = min(temporal_distance),
        # Count total alert occurrences per location
        number_of_red_alerts = length(SEMEL_YISHUV),
        # Count alerts within 6 days of the election
        alerts_6_days = sum(temporal_distance <= 6, na.rm = TRUE),
        # Count alerts more than 149 days before the election
        alerts_149_plus = sum(temporal_distance > 149, na.rm = TRUE),
      ),
    by = "SEMEL_YISHUV"
  ) %>% 
  # Categorize temporal distance into groups (Near-election vs. Far vs. None)
  mutate(
    temporal_group = cut(temporal_distance, 
                         breaks = c(-Inf, 6, Inf), 
                         labels = c("temporal_distance == 6", "temporal_distance > 149"),
                         include.lowest = TRUE) %>% as.character(),
    # Assign a specific label for locations with no alerts
    temporal_group = ifelse(is.na(temporal_group), 'no_red_alert', temporal_group)
  )



# Checking whether any location had alerts in both temporal groups (6 days and 149+ days)
parties_percentages %>% filter(alerts_149_plus != 0 & alerts_6_days != 0) %>% nrow() 
# 0 locations had alerts in both groups, so we can treat them as mutually exclusive categories

# --- INTEGRATE DEMOGRAPHIC CONTROL VARIABLES ---
# Merging with the 'israel_panel' dataset to add socioeconomic and geographic indicators.
parties_percentages = parties_percentages %>%
  mutate(
    # Create 'year_x' to handle duplicate election cycles in a single year (April/Sept 2019)
    year_x = substr(year, 1, 4) %>% as.integer()
  ) %>%
  # Perform a left join to bring in control variables
  merge(
    israel_panel %>% select('SEMEL_YISHUV',
                            'year',
                            'Religion_yishuv_Code',
                            'ntl',
                            'sci_index_value',
                            'sci_index_cluster',
                            'density',
                            'Shape_Area',
                            'Pop_Total'
    ),
    by.x = c('year_x', 'SEMEL_YISHUV'),
    by.y = c('year', 'SEMEL_YISHUV'),
    all.x = T,
    all.y = F
  ) %>% 
  # --- DATA CLEANING & CATEGORIZATION ---
  mutate(
    # Replace NA counts with 0 and zero-out alerts for years before the study period (pre-2015)
    number_of_red_alerts = ifelse(is.na(number_of_red_alerts), 0, number_of_red_alerts),
    number_of_red_alerts = ifelse(year < 2015, 0, number_of_red_alerts),
    
    # Bucket the number of alerts into ordinal categories for non-linear analysis
    red_alert_number_category = cut(number_of_red_alerts,
                                    breaks = c(-Inf, 0, 2, 5, Inf), 
                                    labels = c("No Alerts", "1-2 Alerts", "3-5 Alerts", "6+ Alerts"))
  ) %>%
  # --- IMPUTING MISSING VALUES ---
  # Fill missing Religion codes using the first available value for that specific location
  group_by(SEMEL_YISHUV) %>%
  mutate(Religion_yishuv_Code = ifelse(is.na(Religion_yishuv_Code),
                                       first(Religion_yishuv_Code[!is.na(Religion_yishuv_Code)]),
                                       Religion_yishuv_Code),
         
  ) %>%
  ungroup() %>% 
  # creating time to treatment variable
  mutate(
    
    time_to_treatment = case_when(
      year == 2006 ~ -3,
      year == 2009 ~ -2,
      year == 2013 ~ -1,
      year == 2015 ~ 0,
      TRUE ~ NA_real_ 
    )
    
  )


##### PREPARING FOR PROBABILITY OF RED ALERT REGRESSIONS #####

start_date <- as.Date("2014-07-24")
end_date <- as.Date("2015-03-17")
# just until 2021 to avoid censorship (as explained in the appendix)
last_date <- as.Date("2021-12-31")


dates <- seq.Date(start_date, end_date, by = "day")
all_dates <- seq.Date(start_date, last_date, by = "day")

# one observation for each city-day
cities_grid <- expand.grid(SEMEL_YISHUV = unique(parties_percentages$SEMEL_YISHUV),
                           date = dates) %>%
  # merging with red alerts dataset to get a city-day-alert dummy
  merge(
    red_alerts %>% dplyr::select(SEMEL_YISHUV, date, alert) %>% distinct(),
    by = c("SEMEL_YISHUV", "date"),
    all.x = T,
    all.y = F) %>% 
  mutate(
    alert = ifelse(is.na(alert), 0, alert)
  )


all_years_cities_grid <- expand.grid(SEMEL_YISHUV = unique(parties_percentages$SEMEL_YISHUV),
                                     date = all_dates) %>%
  merge(all_red_alerts %>% dplyr::select(SEMEL_YISHUV, date, alert) %>% distinct(),
                               by = c("SEMEL_YISHUV", "date"),
                               all.x = T,
                               all.y = F) %>% 
  mutate(
    alert = ifelse(is.na(alert), 0, alert)
  )



##### EXPORTING #####

write.csv(parties_percentages, 'treating/Red Alerts/Output/2_parties_percentages_panel.csv',
          row.names = F)

write.csv(cities_grid, 'treating/Red Alerts/Output/2_cities_grid.csv', row.names = F)
write.csv(all_years_cities_grid, 'treating/Red Alerts/Output/2_all_years_cities_grid.csv', row.names = F)


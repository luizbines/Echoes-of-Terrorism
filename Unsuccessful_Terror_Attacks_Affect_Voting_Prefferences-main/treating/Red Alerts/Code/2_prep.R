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

# Directory
wd = '/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Unsuccessful_Terror_Attacks_Affect_Voting_Prefferences-main/'
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



# Counting red alerts at each temporal group
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



# Merging voting dataset with red alerts dataset
# Then, adding treatment dummy
parties_percentages = parties_percentages %>% 
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

# Merging with israel_panel
parties_percentages = parties_percentages %>%
  mutate(
    # since there were 2 elections in 2019 (2019, 2019_2), we need to create a new
    # year variable for merging
    year_x = substr(year, 1, 4) %>% as.integer()
  ) %>%
  merge(
    israel_panel %>% select('SEMEL_YISHUV',
                            'year',
                            'Religion_yishuv_Code',
                            'ntl',
                            'density',
                            'Shape_Area',
                            'Pop_Total'
                            ),
    by.x = c('year_x', 'SEMEL_YISHUV'),
    by.y = c('year', 'SEMEL_YISHUV'),
    all.x = T,
    all.y = F
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


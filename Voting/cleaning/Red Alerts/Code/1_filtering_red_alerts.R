# FILTERING ROCKET ATTACKS FROM RED ALERTS DATASET
# LUIZ BINES - luizbines@gmail.com
# 2023


# Library
library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
library(sf)
library(modelsummary)
library(lubridate)

# Get the base path from environment or parent script
if (!exists("base_path")) {
  base_path <- Sys.getenv("R_PROJECT_DIR")
  if (base_path == "") {
    base_path <- getwd()
  }
}
setwd(base_path)


###### IMPORTING #####

# Separating rows so each line is only one observation
terror_attacks = read.csv('raw/Red Alerts/Output/red_alerts.csv')


# Area-to-city dictionary
# אזור ההתגוננות 
alarm_areas = read.csv('raw/Red Alerts/area_codes.csv', header = F, skip = 1)[,-2] 




###### CLEANING ######

all_rocket_attacks = terror_attacks %>% 
  mutate(year = as.integer(sub(".+\\.(\\d{4})", "\\1", date))) %>%
  separate_rows(data, sep = ',') %>%
  mutate(area = as.integer(gsub('[^0-9]','', data)),
         date = as.Date(date, format = "%d.%m.%Y"),
         # Correcting Ashdod localities
         data = data %>% str_replace_all("^אשדוד.*$", "אשדוד")) %>% 
  # Filtering only rocket attacks
  filter(category == 1)

rocket_attacks = all_rocket_attacks %>%
  filter(
    # Filtering only years of interest
    date <= '2015-03-17'
  )


alarm_areas = alarm_areas %>%
  rename(locality = V1, area = V3) %>%
  mutate(area = as.integer(gsub('[^0-9]','', area))) %>%
  distinct();

###### MERGING ######

rocket_attacks = merge(rocket_attacks,
                       alarm_areas,
                       by = 'area',
                       all.x = T,
                       #T
                       all.y = F) %>%   
  mutate(
    locality = coalesce(locality,
                        data),
    alarm = ifelse(is.na(data),0,1),
    
    # simplifying locality's writing
    loc = gsub("[^א-ת]", "",
               locality,
               perl = TRUE)) %>%
  subset(nchar(loc) >= 3)

all_rocket_attacks = merge(all_rocket_attacks,
                       alarm_areas,
                       by = 'area',
                       all.x = T,
                       #T
                       all.y = F) %>%   
  mutate(
    locality = coalesce(locality,
                        data),
    alarm = ifelse(is.na(data),0,1),
    
    # simplifying locality's writing
    loc = gsub("[^א-ת]", "",
               locality,
               perl = TRUE)) %>%
  subset(nchar(loc) >= 3)



##### EXPORTING #####
write.csv(rocket_attacks, 'cleaning/Red Alerts/Output/rocket_alerts.csv', row.names = F)
write.csv(all_rocket_attacks, 'cleaning/Red Alerts/Output/all.rocket_alerts.csv', row.names = F)

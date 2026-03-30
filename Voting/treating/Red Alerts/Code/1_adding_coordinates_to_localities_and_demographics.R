# This script adds geographical coordinates and polygons to each locality (both red alerts and electoral)

  # 1: adds geographical points to mark each red alert 
# this way, we are able to properly calculate the city/locality of each of them using the israeli shapefile 
# we then filter them based on distance to gaza

  # 2: until now, observations were geographical points. Here, they are merged with Israel's SF (polygons).
# Then, based on localities names and coordinates, electoral localities are added to each red alert observation

# Luiz Bines
# 2024

# Library
library(tidyr)
library(dplyr)
library(stringr)
library(sp)
library(readxl)
library(readr)
library(sf)
library(fixest)
library(modelsummary)
library(geosphere)
library(lubridate)
library(scales)
library(stargazer)
library(units)
library(lwgeom)
library(stringi)
library(stringdist)

# Directory
wd = '/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Voting/'
setwd(wd);



##### Importing #####
cities_coordinates = read.csv('raw/Israel/all_cities_coordinates.csv', row.names = 'X') %>% 
  mutate(
    loc = gsub("[^א-ת]", "", location, perl = TRUE)
  )

rocket_alerts = read.csv('cleaning/Red Alerts/Output/rocket_alerts.csv')
all_rocket_alerts = read.csv('cleaning/Red Alerts/Output/all.rocket_alerts.csv')

parties_percentages = read_csv('cleaning/Elections/Output/parties_percentages.csv')

### SFs ###
gaza_sf = read_sf('raw/Israel/Gaza/gaza.shp') %>% 
  summarise(geometry = st_union(geometry))
st_crs(gaza_sf) = 4326


israel = read_sf('raw/Israel/Demographics/statisticalareas_demography2013.gdb/') 

israel_demographics <- read.csv('cleaning/Israel/Output/1_israel_panel.csv') %>% 
  select(SEMEL_YISHUV,Pop_Total, year, SHEM_YISHUV) %>% 
  mutate(
    loc = gsub("[^א-ת]", "", SHEM_YISHUV, perl = TRUE)
  )



##### CLEANING #####
remove_parentheses <- function(text) {
  str_remove_all(text, "\\s*\\([^\\)]+\\)")
}

rocket_alerts = rocket_alerts %>% 
  mutate(
    loc = remove_parentheses(loc)
  )

parties_percentages = parties_percentages %>% 
  mutate(
    loc = remove_parentheses(loc)
  )


all_rocket_alerts = all_rocket_alerts %>% 
  mutate(
    loc = remove_parentheses(loc)
  )

israel_demographics = israel_demographics %>% 
  mutate(
    loc = remove_parentheses(loc)
  )


# Cleaning Israel Shapefile
israel <- israel %>%
  group_by(SEMEL_YISHUV) %>%
  summarise(
    Shape = st_union(Shape),
    Shape_Area = sum(Shape_Area, na.rm = TRUE),
    Pop_Total = sum(Pop_Total, na.rm = TRUE),
    SHEM_YISHUV = first(SHEM_YISHUV),
    SHEM_YISHUV_ENGLISH = first(SHEM_YISHUV_ENGLISH),
    .groups = "drop"
  )

#### Assigning distances from Israel Shapefile to the Gaza Strip ####
israel = israel %>%
  mutate(
    Shape = st_transform(Shape, st_crs(gaza_sf)),
  )
st_crs(israel) = st_crs(gaza_sf)
israel <- st_make_valid(israel)

# DISTANCE TO GAZA
distance = st_distance(israel, gaza_sf) %>% 
  set_units("km")
# adding to dataset
israel = israel %>%
  mutate(
    distance = as.numeric(distance)
  )



##### MERGING #####

# adding coordinates to red alerts
rocket_alerts = rocket_alerts %>% 
  merge(cities_coordinates %>% select(-location),
        by = 'loc',
        all.x = T,
        all.y = F
  )

# adding coordinates to all red alerts
all_rocket_alerts = all_rocket_alerts %>% 
  merge(cities_coordinates %>% select(-location),
        by = 'loc',
        all.x = T,
        all.y = F
  )


# adding coordinates to all localities
israel_demographics = israel_demographics %>% 
  merge(cities_coordinates %>% select(-location),
        by = 'loc',
        all.x = T,
        all.y = F)


# NAs
# rocket_alerts
is.na(rocket_alerts$lat) %>% sum
# 95 out of 20159
# 0.47%

# excluding NA coordinates
rocket_alerts = rocket_alerts[!is.na(rocket_alerts$lat),]
all_rocket_alerts = all_rocket_alerts[!is.na(all_rocket_alerts$lat),]


# Getting centroids of the shapefile to assign coordinates to parties_percentages
israel <- israel %>% 
  mutate(
    centroid = st_centroid(Shape),
    long = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  ) %>% 
  select(-centroid)

# Merging electoral localities with Israel dataset (shapefile)
parties_percentages = parties_percentages %>% 
  merge(israel %>% select(SEMEL_YISHUV,Shape,distance,SHEM_YISHUV, SHEM_YISHUV_ENGLISH, long, lat),
        by = 'SEMEL_YISHUV',
        all.x = T,
        all.y = F)

# checking number of localities that we don't know the distance to Gaza
parties_percentages %>%
  filter(is.na(distance)) %>%
  distinct(SEMEL_YISHUV) %>%
  nrow()
# 30 out of 1183 unique localities (parties_percentages %>% distinct(SEMEL_YISHUV) %>% nrow)
# have no match ~= 2.5%

# Removing NAs
parties_percentages = parties_percentages %>% filter(!is.na(parties_percentages$distance))

# Keeping only localities located 75km to 150km from Gaza
parties_percentages = parties_percentages %>% 
  filter(distance >= 75) %>% 
  filter(distance <= 150)


# Keeping only relevant variables
rocket_alerts = rocket_alerts %>% 
  select(
    c(
      area, alarm,
      locality,loc,date,lat,long)
  )


all_rocket_alerts = all_rocket_alerts %>% 
  select(
    c(
      area, alarm,
      locality, loc, date, lat, long)
  )


#### Distance to Gaza ####

### Red Alerts Coordinates ###
rocket_alerts_sf = st_as_sf(rocket_alerts, coords = c("long", "lat"), crs = 4326, remove = FALSE)
st_crs(rocket_alerts_sf) = st_crs(gaza_sf)


# Assigning a SEMEL_YISHUV and distance from the israel dataset
joined_data_rockets <- st_join(rocket_alerts_sf,
                               israel,
                               join = st_nearest_feature,
                               left = T)

rocket_alerts$SEMEL_YISHUV <- joined_data_rockets$SEMEL_YISHUV
rocket_alerts$distance <- joined_data_rockets$distance


### (All Years) Red Alerts Coordinates ###
all_rocket_alerts_sf = st_as_sf(all_rocket_alerts, coords = c("long", "lat"), crs = 4326, remove = FALSE)
st_crs(all_rocket_alerts_sf) = st_crs(gaza_sf)


# Assigning a SEMEL_YISHUV and distance from the israel dataset
joined_data_all_rockets <- st_join(all_rocket_alerts_sf,
                                   israel,
                                   join = st_nearest_feature,
                                   left = T)

all_rocket_alerts$SEMEL_YISHUV <- joined_data_all_rockets$SEMEL_YISHUV
all_rocket_alerts$distance <- joined_data_all_rockets$distance



# filtering Red Alerts based on distance from the Gaza Strip
rocket_alerts = rocket_alerts %>%
  filter(distance >= 75) %>%
  filter(distance <= 150)



##### EXPORTING #####
write.csv(rocket_alerts, 'treating/Red Alerts/Output/Datasets/1_red_alerts_with_coordinates_and_electoral_localities.csv',
          row.names = F)
write.csv(all_rocket_alerts, 'treating/Red Alerts/Output/Datasets/1_ALL_red_alerts_with_coordinates_and_electoral_localities.csv',
          row.names = F)
write_csv(parties_percentages, 'treating/Red Alerts/Output/Datasets/1_parties_percentages_with_coordinates.csv')
write.csv(israel_demographics, 'treating/Red Alerts/Output/Datasets/1_israel_demographics.csv')

# This script adds geographical coordinates and polygons to each locality (both red alerts and electoral)
  # 1: adds geographical points to mark where each red alert and electoral locality are
  # 2: merges red alerts and likud_percentage observations based on where they are
  # 3: until now, observations were geographical points. Here, they are merged with israel's SF.
# this way, we are able to properly calculate the distance between each city (polygon) and Gaza.


# Then, based on localities names and coordinates, electoral localities are added to each red alert observation
# Luiz Bines
# 2024

# Library
library(tidyr)
library(dplyr)
library(stringr)
library(sp)
library(readxl)
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
wd = 'C:/Users/luizb/Desktop/Dissertation/Dissertation/Red-Alerts-and-Votes/'
setwd(wd);



##### Importing #####
cities_coordinates = read.csv('raw/Israel/all_cities_coordinates.csv', row.names = 'X') %>% 
  mutate(
    loc = gsub("[^א-ת]", "", location, perl = TRUE)
  )

rocket_alerts = read.csv('cleaning/Red Alerts/Output/rocket_alerts.csv')
all_rocket_alerts = read.csv('cleaning/Red Alerts/Output/all.rocket_alerts.csv')

likud_percentage = read.csv('cleaning/Elections/Output/likud_percentage.csv')
right_wing_percentage = read.csv('cleaning/Elections/Output/right_wing_percentage.csv')
turnout_percentage = read.csv('cleaning/Elections/Output/turnout_percentage.csv')

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

likud_percentage = likud_percentage %>% 
  mutate(
    loc = remove_parentheses(loc)
  )

right_wing_percentage = right_wing_percentage %>% 
  mutate(
    loc = remove_parentheses(loc)
  )

turnout_percentage = turnout_percentage %>% 
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

##### MERGING #####
# adding coordinates to red alerts
rocket_alerts = rocket_alerts %>% 
  merge(cities_coordinates %>% select(-location),
        by = 'loc',
        all.x = T,
        all.y = F
        )


# adding coordinates to electoral localities
likud_percentage = likud_percentage %>% 
  merge(cities_coordinates,
        by = 'loc',
        all.x = T,
        all.y = F)

right_wing_percentage = right_wing_percentage %>% 
  merge(cities_coordinates,
        by = 'loc',
        all.x = T,
        all.y = F)

turnout_percentage = turnout_percentage %>% 
  merge(cities_coordinates,
        by = 'loc',
        all.x = T,
        all.y = F)


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
        all.y = F) %>% select(-SHEM_YISHUV)


##### FILTERING #####

# excluding NA coordinates
rocket_alerts = rocket_alerts[!is.na(rocket_alerts$lat),]
likud_percentage = likud_percentage[!is.na(likud_percentage$lat),]
right_wing_percentage = right_wing_percentage[!is.na(right_wing_percentage$lat),]
turnout_percentage = turnout_percentage[!is.na(turnout_percentage$lat),]
all_rocket_alerts = all_rocket_alerts[!is.na(all_rocket_alerts$lat),]


# Keeping only relevant variables
rocket_alerts = rocket_alerts %>% 
  select(
    c(
      area, alarm,
      locality,loc,date,lat,long)
  )

likud_percentage = likud_percentage %>% 
  select(-location)

all_rocket_alerts = all_rocket_alerts %>% 
  select(
    c(
      area, alarm,
      locality, loc, date, lat, long)
  )

##### ASSIGNING EACH RED ALERT OBSERVATION TO AN ELECTORAL LOCALITY #####


  # First: by perfect merge
rocket_alerts = rocket_alerts %>%
  merge(
    likud_percentage %>% select(loc,SEMEL_YISHUV),
    by = 'loc',
    all.x = T,
    all.y = F
  )

all_rocket_alerts = all_rocket_alerts %>%
  merge(
    likud_percentage %>% select(loc,SEMEL_YISHUV),
    by = 'loc',
    all.x = T,
    all.y = F
  )


  # Second: by smallest distance to electoral locality
rocket_alerts_sf <- st_as_sf(rocket_alerts, coords = c("long", "lat"), crs = 4326, remove = FALSE)
likud_percentage_sf <- st_as_sf(likud_percentage, coords = c("long", "lat"), crs = 4326, remove = FALSE)
all_rocket_alerts_sf <- st_as_sf(all_rocket_alerts, coords = c("long", "lat"), crs = 4326, remove = FALSE)


nearest <- st_nearest_feature(rocket_alerts_sf, likud_percentage_sf)
distances <- st_distance(rocket_alerts_sf, likud_percentage_sf[nearest, ], by_element = TRUE)

nearest_all <- st_nearest_feature(all_rocket_alerts_sf, likud_percentage_sf)
distances_all <- st_distance(all_rocket_alerts_sf, likud_percentage_sf[nearest_all, ], by_element = TRUE)

# # Max distance between red alert and electoral locality
# max_distance <- units::set_units(2000, "m")
# 
# rocket_alerts$nearest_SEMEL_YISHUV <- ifelse(distances < max_distance, likud_percentage_sf$SEMEL_YISHUV[nearest], NA)
# all_rocket_alerts$nearest_SEMEL_YISHUV <- ifelse(distances_all < max_distance, likud_percentage_sf$SEMEL_YISHUV[nearest_all], NA)
# 
# # If SEMEL_YISHUV == NA, SEMEL_YISHUV == nearest_SEMEL_YISHUV
# rocket_alerts <- rocket_alerts %>%
#   mutate(
#     SEMEL_YISHUV = ifelse(is.na(SEMEL_YISHUV) & !is.na(nearest_SEMEL_YISHUV),
#                           nearest_SEMEL_YISHUV,
#                           SEMEL_YISHUV)
#   ) %>%
#   select(-nearest_SEMEL_YISHUV)
# 
# all_rocket_alerts <- all_rocket_alerts %>%
#   mutate(
#     SEMEL_YISHUV = ifelse(is.na(SEMEL_YISHUV) & !is.na(nearest_SEMEL_YISHUV),
#                           nearest_SEMEL_YISHUV,
#                           SEMEL_YISHUV),
#     date = as.Date(date)
#   ) %>%
#   select(-nearest_SEMEL_YISHUV) 




#### Assigning each observation to a city in the map ####
israel = israel %>%

mutate(
  # centroid = st_transform(centroid, st_crs(gaza_sf)),
  Shape = st_transform(Shape, st_crs(gaza_sf)),
)

st_crs(israel) = st_crs(gaza_sf)
# israel = st_transform(israel, crs = st_crs(gaza_sf))

israel <- st_make_valid(israel)
likud_percentage_sf <- st_make_valid(likud_percentage_sf)


# DISTANCE TO GAZA


distance = st_distance(israel, gaza_sf) %>% 
  set_units("km")



israel = israel %>%
  mutate(
    distance = as.numeric(distance)
  )





# Joining by polygon
likud_percentage_sf <- st_transform(likud_percentage_sf, st_crs(israel))

joined_data <- st_join(likud_percentage_sf,
                       israel %>% st_as_sf(),
                       # join = st_nearest_feature,
                       left = T)

likud_percentage$distance <- joined_data$distance
likud_percentage$semel_map <- joined_data$SEMEL_YISHUV.y
  


  # adding distances to right_wing_percentage dataset
right_wing_percentage = right_wing_percentage %>%
  select(-location) %>% 
  merge(likud_percentage %>%
          select(SEMEL_YISHUV,
                 distance,
                 semel_map),
        by = 'SEMEL_YISHUV')

# adding distances to turnout_percentage dataset
turnout_percentage = turnout_percentage %>%
  select(-location) %>% 
  merge(likud_percentage %>%
          select(SEMEL_YISHUV,
                 distance,
                 semel_map),
        by = 'SEMEL_YISHUV')



# Assigning polygon distances to each red alert observation
# rocket_alerts_sf <- st_transform(rocket_alerts_sf, st_crs(israel))

joined_data_rockets <- st_join(rocket_alerts_sf,
                              israel,
                              join = st_nearest_feature,
                              left = T)

rocket_alerts$distance <- joined_data_rockets$distance
rocket_alerts$semel_map <- joined_data_rockets$SEMEL_YISHUV.y





##### EXPORTING #####
write.csv(rocket_alerts, 'treating/Red Alerts/Output/1_red_alerts_with_coordinates_and_electoral_localities.csv',
          row.names = F)
write.csv(likud_percentage, 'treating/Red Alerts/Output/1_likud_percentage_with_coordinates.csv',
          row.names = F)
write.csv(right_wing_percentage, 'treating/Red Alerts/Output/1_right_wing_percentage_with_coordinates.csv',
          row.names = F)
write.csv(turnout_percentage, 'treating/Red Alerts/Output/1_turnout_percentage_with_coordinates.csv',
          row.names = F)
write.csv(israel_demographics, 'treating/Red Alerts/Output/1_israel_demographics.csv')

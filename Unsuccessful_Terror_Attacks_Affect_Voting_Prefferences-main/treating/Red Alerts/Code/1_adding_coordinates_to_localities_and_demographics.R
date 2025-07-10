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
library(fuzzyjoin)

# Directory
wd = 'C:/Users/luizb/Desktop/Echoes-of-Terrorism/Unsuccessful_Terror_Attacks_Affect_Voting_Prefferences-main/'
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
# israel = st_transform(israel, crs = st_crs(gaza_sf))
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
        all.y = F) %>% select(-SHEM_YISHUV)


# NAs
# rocket_alerts
is.na(rocket_alerts$lat) %>% sum
# 95 out of 20159
# 0.47%

# excluding NA coordinates
rocket_alerts = rocket_alerts[!is.na(rocket_alerts$lat),]
all_rocket_alerts = all_rocket_alerts[!is.na(all_rocket_alerts$lat),]


### MERGING ELECTORAL LOCALITIES ###

#   # first adding coordinate points (manually collected)
# likud_percentage = likud_percentage %>%
#   merge(cities_coordinates,
#         by = 'loc',
#         all.x = T,
#         all.y = F)
# 
# 
# # Checking how many localities have no coordinates
# likud_percentage$lat %>% is.na %>% sum
# # 10 out of 1183
# # 0.88%
# 
# # Removing these localities
# likud_percentage = likud_percentage[!is.na(likud_percentage$lat),]
# 
# 
# 
# # Adding distances from the Gaza Strip
# likud_percentage_sf = st_as_sf(likud_percentage, coords = c("long", "lat"), crs = 4326, remove = FALSE)
# st_crs(likud_percentage_sf) = st_crs(gaza_sf)
# 
# distance = st_distance(likud_percentage_sf, gaza_sf) %>%
#   set_units("km")
# 
# 
# likud_percentage_sf = likud_percentage_sf %>%
#   mutate(
#     distance = as.numeric(distance)
#   )
# 
# likud_percentage$distance_gaza = likud_percentage_sf$distance
# 


# Merging electoral localities with israel dataset (shapefile)
likud_percentage = likud_percentage %>% 
  merge(israel %>% select(SEMEL_YISHUV,Shape,distance,SHEM_YISHUV, SHEM_YISHUV_ENGLISH),
        by = 'SEMEL_YISHUV',
        all.x = T,
        all.y = F)

# checking number of NAs
is.na(likud_percentage$distance) %>% sum
# 30 out of 2984 have no match ~= 1%

# Removing NAs
likud_percentage = likud_percentage %>% filter(!is.na(likud_percentage$distance))

# filtering Red Alerts based on distance from the Gaza Strip
likud_percentage = likud_percentage %>% 
  filter(distance >= 75) %>% 
  filter(distance <= 150)


# Same thing for right_wing and turnout datasets
right_wing_percentage = right_wing_percentage %>%
  filter(SEMEL_YISHUV %in% likud_percentage$SEMEL_YISHUV) %>% 
  merge(likud_percentage %>% select(SEMEL_YISHUV,Shape,distance,SHEM_YISHUV),
        by = 'SEMEL_YISHUV',
        all.x = T,
        all.y = F)

turnout_percentage = turnout_percentage %>%
  filter(SEMEL_YISHUV %in% likud_percentage$SEMEL_YISHUV) %>% 
  merge(likud_percentage %>% select(SEMEL_YISHUV,Shape,distance,SHEM_YISHUV),
        by = 'SEMEL_YISHUV',
        all.x = T,
        all.y = F)





##### FILTERING #####

# NAs
  # rocket_alerts
# is.na(rocket_alerts$lat) %>% sum
  # 95 out of 20159
  # 0.47%

# 
#   # electoral results
# is.na(likud_percentage$lat) %>% sum
#   # 10 out of 1183
#   # 0.88%

# excluding NA coordinates
# rocket_alerts = rocket_alerts[!is.na(rocket_alerts$lat),]
# likud_percentage = likud_percentage[!is.na(likud_percentage$lat),]
# right_wing_percentage = right_wing_percentage[!is.na(right_wing_percentage$lat),]
# turnout_percentage = turnout_percentage[!is.na(turnout_percentage$lat),]
# all_rocket_alerts = all_rocket_alerts[!is.na(all_rocket_alerts$lat),]


# Keeping only relevant variables
rocket_alerts = rocket_alerts %>% 
  select(
    c(
      area, alarm,
      locality,loc,date,lat,long)
  )

# likud_percentage = likud_percentage %>% 
#   select(-location)

all_rocket_alerts = all_rocket_alerts %>% 
  select(
    c(
      area, alarm,
      locality, loc, date, lat, long)
  )


#### Distance to Gaza ####

# # Electoral Localities
# likud_percentage_sf = st_as_sf(likud_percentage, coords = c("long", "lat"), crs = 4326, remove = FALSE)
# st_crs(likud_percentage_sf) = st_crs(gaza_sf)
# 
# distance = st_distance(likud_percentage_sf, gaza_sf) %>% 
#   set_units("km")
# 
# 
# likud_percentage_sf = likud_percentage_sf %>%
#   mutate(
#     distance = as.numeric(distance)
#   )
# 
# likud_percentage$distance = likud_percentage_sf$distance
# 
# # filtering Red Alerts based on distance from the Gaza Strip
# likud_percentage = likud_percentage %>% 
#   filter(distance >= 75) %>% 
#   filter(distance <= 150)
# 
# 
# # Same thing for right wing dataset
# right_wing_percentage_sf = st_as_sf(right_wing_percentage, coords = c("long", "lat"), crs = 4326, remove = FALSE)
# st_crs(right_wing_percentage_sf) = st_crs(gaza_sf)
# 
# distance = st_distance(right_wing_percentage_sf, gaza_sf) %>% 
#   set_units("km")
# 
# 
# right_wing_percentage_sf = right_wing_percentage_sf %>%
#   mutate(
#     distance = as.numeric(distance)
#   )
# 
# right_wing_percentage$distance = right_wing_percentage_sf$distance
# 
# # filtering Red Alerts based on distance from the Gaza Strip
# right_wing_percentage = right_wing_percentage %>% 
#   filter(distance >= 75) %>% 
#   filter(distance <= 150)
# 
# 
# 
# # Same thing for turnout dataset
# turnout_percentage_sf = st_as_sf(turnout_percentage, coords = c("long", "lat"), crs = 4326, remove = FALSE)
# st_crs(turnout_percentage_sf) = st_crs(gaza_sf)
# 
# distance = st_distance(turnout_percentage_sf, gaza_sf) %>% 
#   set_units("km")
# 
# 
# turnout_percentage_sf = turnout_percentage_sf %>%
#   mutate(
#     distance = as.numeric(distance)
#   )
# 
# turnout_percentage$distance = turnout_percentage_sf$distance
# 
# # filtering Red Alerts based on distance from the Gaza Strip
# turnout_percentage = turnout_percentage %>% 
#   filter(distance >= 75) %>% 
#   filter(distance <= 150)


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


# distance = st_distance(rocket_alerts_sf, gaza_sf) %>%
#   set_units("km")
# 
# 
# rocket_alerts_sf = rocket_alerts_sf %>%
#   mutate(
#     distance = as.numeric(distance)
#   )
# 
# rocket_alerts$distance = rocket_alerts_sf$distance

# filtering Red Alerts based on distance from the Gaza Strip
rocket_alerts = rocket_alerts %>%
  filter(distance >= 75) %>%
  filter(distance <= 150)


# ##### ASSIGNING EACH RED ALERT OBSERVATION TO AN ELECTORAL LOCALITY #####
# 
#   # First: by perfect merge
# rocket_alerts = rocket_alerts %>%
#   merge(
#     likud_percentage %>% select(loc,SEMEL_YISHUV),
#     by = 'loc',
#     all.x = T,
#     all.y = F
#   )
# 
# # Initial number of observations missing SEMEL_YISHUV:
# rocket_alerts$SEMEL_YISHUV %>% is.na %>% sum
#   # 116 out of 561 = 21%
# 
# all_rocket_alerts = all_rocket_alerts %>%
#   merge(
#     likud_percentage %>% select(loc,SEMEL_YISHUV),
#     by = 'loc',
#     all.x = T,
#     all.y = F
#   )
# 
# 
# ### FUZZY JOINING ROCKET ALERTS AND ELECTORAL LOCALITIES ###
# # Fuzzy join between rocket_alerts and likud_percentage by locality name
# rocket_alerts_fuzzy <- stringdist_left_join(
#   rocket_alerts %>% filter(is.na(SEMEL_YISHUV)) %>% select(-SEMEL_YISHUV),
#   likud_percentage %>% select(loc, SEMEL_YISHUV),
#   by = "loc",
#   method = "jw",       
#   max_dist = 0.15,     
#   distance_col = "dist"
# )
# 
# 
# # Keep only best match
# rocket_alerts_fuzzy <- rocket_alerts_fuzzy %>%
#   group_by(loc.x) %>%
#   slice_min(order_by = dist, n = 1) %>%
#   ungroup() %>%
#   rename(loc = loc.x)
# 
# 
# # Fuzzy match only to observations with missing SEMEL_YISHUV
# need_fuzzy <- rocket_alerts %>% filter(is.na(SEMEL_YISHUV)) %>% 
#   select(-SEMEL_YISHUV)
# 
# 
# # Apply fuzzy matching
# need_fuzzy_fixed <- stringdist_left_join(
#   need_fuzzy,
#   likud_percentage %>% select(loc, SEMEL_YISHUV),
#   by = "loc",
#   method = "jw",
#   max_dist = 0.15,
#   distance_col = "dist"
# ) %>%
#   group_by(loc.x) %>%
#   slice_min(dist, n = 1) %>%
#   ungroup() %>%
#   select(-loc.y, -dist) %>%
#   rename(loc = loc.x)
# 
# 
# # Bind with observations that previously had SEMEL_YISHUV 
# rocket_alerts <- rocket_alerts %>%
#   filter(!is.na(SEMEL_YISHUV)) %>%
#   bind_rows(rocket_alerts_fuzzy)
# 
#   # Updated number of red alert observations without SEMEL_YISHUV
# rocket_alerts$SEMEL_YISHUV %>% is.na %>% sum
#   # 97 out of 564 = 17.1%
# 
# 
# 
# 
# # Smallest distance to electoral locality
# rocket_alerts_sf <- st_as_sf(rocket_alerts, coords = c("long", "lat"), crs = 4326, remove = FALSE)
# likud_percentage_sf <- st_as_sf(likud_percentage, coords = c("long", "lat"), crs = 4326, remove = FALSE)
# all_rocket_alerts_sf <- st_as_sf(all_rocket_alerts, coords = c("long", "lat"), crs = 4326, remove = FALSE)
# 
# 
# nearest <- st_nearest_feature(rocket_alerts_sf, likud_percentage_sf)
# distances <- st_distance(rocket_alerts_sf, likud_percentage_sf[nearest, ], by_element = TRUE)
# 
# nearest_all <- st_nearest_feature(all_rocket_alerts_sf, likud_percentage_sf)
# distances_all <- st_distance(all_rocket_alerts_sf, likud_percentage_sf[nearest_all, ], by_element = TRUE)
# 
# # Max distance between red alert and electoral locality
# max_distance <- units::set_units(3500, "m")
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
# 
# 
# # Final check:
# rocket_alerts$SEMEL_YISHUV %>% is.na %>% sum
# # 0 out of 564
# 



# Now, all rocket_alerts observations, i.e Red Alerts, have an election locality (SEMEL_YISHUV)


# #### Assigning each observation to a city in the map ####
# israel = israel %>%
# 
# mutate(
#   # centroid = st_transform(centroid, st_crs(gaza_sf)),
#   Shape = st_transform(Shape, st_crs(gaza_sf)),
# )
# 
# st_crs(israel) = st_crs(gaza_sf)
# # israel = st_transform(israel, crs = st_crs(gaza_sf))
# 
# israel <- st_make_valid(israel)
# likud_percentage_sf <- st_make_valid(likud_percentage_sf)
# 
# # DISTANCE TO GAZA
# distance = st_distance(israel, gaza_sf) %>% 
#   set_units("km")
# 
# israel = israel %>%
#   mutate(
#     distance = as.numeric(distance)
#   )
# 
# 
# 
# # Joining by polygon
# likud_percentage_sf <- st_transform(likud_percentage_sf, st_crs(israel))
# 
# joined_data <- st_join(likud_percentage_sf,
#                        israel %>% st_as_sf(),
#                        join = st_nearest_feature,
#                        left = T)
# 
# likud_percentage$distance <- joined_data$distance.y
# likud_percentage$semel_map <- joined_data$SEMEL_YISHUV.y
#   
# 
# 
#   # adding distances to right_wing_percentage dataset
# right_wing_percentage = right_wing_percentage %>% select(-distance) %>% 
#   select(-location) %>% 
#   merge(likud_percentage %>%
#           select(SEMEL_YISHUV,
#                  distance,
#                  semel_map),
#         by = 'SEMEL_YISHUV')
# 
# # adding distances to turnout_percentage dataset
# turnout_percentage = turnout_percentage %>% select(-distance) %>% 
#   select(-location) %>% 
#   merge(likud_percentage %>%
#           select(SEMEL_YISHUV,
#                  distance,
#                  semel_map),
#         by = 'SEMEL_YISHUV')
# 
# 
# 
# # Assigning polygon distances to each red alert observation
# # rocket_alerts_sf <- st_transform(rocket_alerts_sf, st_crs(israel))
# 
# joined_data_rockets <- st_join(rocket_alerts_sf,
#                               israel,
#                               join = st_nearest_feature,
#                               left = T)
# 
# # rocket_alerts$distance <- joined_data_rockets$distance
# rocket_alerts$semel_map <- joined_data_rockets$SEMEL_YISHUV.y



likud_percentage = likud_percentage %>% select(-Shape)
right_wing_percentage = right_wing_percentage %>% select(-Shape)
turnout_percentage = turnout_percentage %>% select(-Shape)

##### EXPORTING #####
write.csv(rocket_alerts, 'treating/Red Alerts/Output/1_red_alerts_with_coordinates_and_electoral_localities.csv',
          row.names = F)
write.csv(all_rocket_alerts, 'treating/Red Alerts/Output/1_ALL_red_alerts_with_coordinates_and_electoral_localities.csv',
          row.names = F)
write.csv(likud_percentage, 'treating/Red Alerts/Output/1_likud_percentage_with_coordinates.csv',
          row.names = F)
write.csv(right_wing_percentage, 'treating/Red Alerts/Output/1_right_wing_percentage_with_coordinates.csv',
          row.names = F)
write.csv(turnout_percentage, 'treating/Red Alerts/Output/1_turnout_percentage_with_coordinates.csv',
          row.names = F)
write.csv(israel_demographics, 'treating/Red Alerts/Output/1_israel_demographics.csv')

# This file adds a district variable for each locality,
# combines all localities into districts in order to compare red alert reactions 
# and merges alerts with trends dataset
# Luiz Bines
# January 2025
# luizbines@gmail.com

# Library
library(dplyr)
library(sf)
library(tidyr)
library(lubridate)



# Directory
# Resolve the project root from the environment or by walking upward from the current directory
resolve_project_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)
  repeat {
    if (dir.exists(file.path(current_dir, "Voting")) && dir.exists(file.path(current_dir, "Trends"))) {
      return(current_dir)
    }
    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      break
    }
    current_dir <- parent_dir
  }
  stop("ERROR: Could not determine project root from start_dir=", start_dir)
}

base_path <- Sys.getenv("R_PROJECT_ROOT")
if (nzchar(base_path) && dir.exists(base_path)) {
  base_path <- normalizePath(base_path, winslash = "/", mustWork = FALSE)
  if (!dir.exists(file.path(base_path, "Voting")) || !dir.exists(file.path(base_path, "Trends"))) {
    base_path <- resolve_project_root()
  }
} else {
  base_path <- resolve_project_root()
}
setwd(base_path);


# Importing
red_alerts <- read.csv('Voting/treating/Red Alerts/Output/Datasets/1_ALL_red_alerts_with_coordinates_and_electoral_localities.csv')[,-1]

il_districts <- read_sf('Trends/raw/Israel/il_shp/il.shp') %>% 
  select(name,geometry) %>% 
  rename(district = name)

gaza_sf = read_sf('Voting/raw/Israel/Gaza/gaza.shp')[1,1]

trends <- read.csv('Trends/raw/Google Trends/Output/trends_israel.csv')

israel_demographics <- read.csv('Voting/treating/Red Alerts/Output/Datasets/1_israel_demographics.csv') %>%
  select(SEMEL_YISHUV,Pop_Total, year, lat, long)


israel_geometry = st_read("Voting/raw/Israel/Demographics/statisticalareas_demography2013.gdb")


# Cleaning Israel Shapefile
israel_geometry <- israel_geometry %>%
  group_by(SEMEL_YISHUV) %>%
  summarise(
    Shape = st_union(Shape),
    Shape_Area = sum(Shape_Area, na.rm = TRUE),
    Pop_Total = sum(Pop_Total, na.rm = TRUE),
    SHEM_YISHUV = first(SHEM_YISHUV),
    SHEM_YISHUV_ENGLISH = first(SHEM_YISHUV_ENGLISH),
    .groups = "drop"
  )

# Pivoting trends
trends <- trends %>%
  rename(Value = value,
         Keyword = keyword,
         District = district) %>% 
  pivot_wider(names_from = Keyword, values_from = Value) %>% 
  # creating year and month variables
  mutate(
    year = as.integer(substr(date, 1, 4)),
    month = as.integer(substr(date, 6, 7))
  ) %>% 
  select(-date)

##### Assigning districts to red alerts #####

# Creating "alert" variable"
red_alerts <- red_alerts %>% 
  mutate(alert = 1,
         date = as.Date(date),
         # creating year and month variables
         year = year(date),
         month = month(date)
  ) %>% 
  # maximum one alert per locality-month-year
  distinct(SEMEL_YISHUV, year, month, .keep_all = TRUE)


# Merging red alerts and israel demographics
red_alerts <- red_alerts %>% 
  merge(israel_demographics %>% select(-c(lat,long)),
        by = c('SEMEL_YISHUV','year'),
        all.x = T,
        all.y = F) 


# Transforming into sf
red_alerts_sf <- st_as_sf(red_alerts, coords = c("long", "lat"), crs = 4326)
il_districts <- st_transform(il_districts, crs = st_crs(red_alerts_sf))
israel_geometry_sf <- st_transform(israel_geometry, st_crs(red_alerts_sf))
israel_demographics_sf <- st_as_sf(israel_demographics %>% filter(!is.na(lat)), coords = c("long", "lat"), crs = st_crs(red_alerts_sf))

# Associating districts to each locality

# getting missing coordinates 
israel_demographics <- israel_demographics %>% 
  left_join(israel_demographics_sf %>% select(SEMEL_YISHUV,geometry, year),
            by = c('SEMEL_YISHUV', 'year')) %>% 
  left_join(israel_geometry_sf,
            by = 'SEMEL_YISHUV') %>%
  mutate(
    geometry = if_else(st_is_empty(geometry) & !st_is_empty(Shape), Shape, geometry),
    Shape = if_else(st_is_empty(Shape) & !st_is_empty(geometry), geometry, Shape)
  ) %>% 
  select(-Shape) %>% 
  st_as_sf()


sf::sf_use_s2(FALSE)


red_alerts <- red_alerts_sf %>% st_join(il_districts, left = FALSE)

israel_demographics <- st_join(israel_demographics, il_districts, left = FALSE)
# 3000 out of more than 20000 observations lost




# Creating district's total population variable
pop_district_year <- israel_demographics %>% 
  as.data.frame() %>% 
  distinct(district, year, SEMEL_YISHUV, .keep_all = TRUE) %>%  
  group_by(district, year) %>%
  summarise(Pop_Total_District = sum(Pop_Total.x, na.rm = TRUE), .groups = "drop")  



# Creating alerts by districts dataset
districts_alerts <- red_alerts %>% 
  # adding population per district for each year
  merge(
    pop_district_year,
    by = c('district','year'),
    all.x = T,
    all.y = F
  ) %>% 
  as.data.frame() %>% 
  mutate(
    alert = ifelse(is.na(alert),0,alert),
    Pop_Total = ifelse(is.na(Pop_Total),0,Pop_Total)
  ) %>% 
  group_by(district, year, month) %>%
  summarise(
    # alerts are calculated as the ratio of the district population that was affected
    alert = sum(alert * Pop_Total) / first(Pop_Total_District)
  ) %>% 
  # one observation per district-day
  distinct(district, month, year, alert)


# alternative: binary alert
# districts_alerts <- red_alerts %>% 
#   group_by(district, date) %>% 
#   summarise(
#     alert = first(alert)
#   ) 


# Adding district geometry
districts_alerts <- districts_alerts %>% 
  merge(il_districts, by = 'district')


##### Combining alerts with Google Trends data #####

# Changing district names to facilitate matching
districts_alerts <- districts_alerts %>%
  mutate(district = recode(district,
                           "HaDarom" = "South District",
                           "Haifa" = "Haifa District",
                           "HaMerkaz" = "Center District",
                           "HaZafon" = "North District",
                           "Jerusalem" = "Jerusalem District",
                           "Tel Aviv" = "Tel Aviv District")) %>% 
  rename(District = district)



merged_data <- trends %>% as.data.frame() %>% 
  merge(
    districts_alerts %>% select(-geometry),
    by = c('District','year','month'),
    all.x = T
  ) %>% 
  mutate(
    alert = ifelse(is.na(alert),
                   0,
                   alert)
  )




# Saving
write.csv(red_alerts, 'Trends/cleaning/Red Alerts/output/red_alerts_with_districts.csv', row.names = F)
write.csv(districts_alerts, 'Trends/cleaning/Red Alerts/output/districts_alerts.csv', row.names = F)
write.csv(merged_data, 'Trends/cleaning/Red Alerts/output/trends_alerts_data_v2.csv', row.names = F, fileEncoding = "UTF-8")


# FOR WEEKLY DATA
# # Creating trends date variables
# weekly fix
# trends <- trends %>% 
#   mutate(date = as.Date(date),
#          # lower limit for matching with red alert date
#          date_min = date - 6,
#          # upper limit for matching with red alert date
#          date_max = date  
#          )
# merged_data <- trends %>% as.data.frame() %>% 
#   left_join(districts_alerts %>% select(-geometry), by = "District") %>%
#   # Ensure alerts fall within the valid range
#   mutate(valid_match = ifelse(date.y >= date_min & date.y <= date_max, TRUE, FALSE),
#          alert = ifelse(valid_match, alert, 0)) %>%
#   # Aggregate by date and district
#   group_by(date.x, District) %>%
#   summarise(
#     alert = max(alert, na.rm = TRUE),  # Keep the maximum alert value
#     valid_match = any(valid_match),
#     across(everything(), ~ first(na.omit(.))),
#     .groups = "drop"  # Avoid unnecessary grouping
#   ) %>%
#   select(-valid_match) %>% 
#   rename(date = date.x) 
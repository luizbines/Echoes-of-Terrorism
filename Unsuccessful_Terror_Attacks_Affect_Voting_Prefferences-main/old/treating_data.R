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
library(stargazer)
library(rgdal)
library(purrr)
library(units)
library(plm)
library(lmtest)
library(multiwayvcov)
library(stringdist)
library(xtable)
library(jsonlite)
library(kableExtra)
library(modelsummary)



# wd
setwd('C:/Users/luizb/Desktop/Dissertation')

# importing
# coordinates = read.csv('Attacks/cities_coordinates.csv') %>%
#   select(-X) %>%
#   mutate(loc = gsub("[^א-ת]", "", location, perl = TRUE))

coordinates = fromJSON("Israel/israel_cities_names_and__geometric_data.json", simplifyDataFrame = TRUE) %>%
  rename(longitude = long, latitude = latt) %>% 
  mutate(
    loc = gsub(r"{\s*\([^\)]+\)}","", name),
    loc = gsub("[^א-ת]", "", loc, perl = TRUE)
  )


rocket_data = read.csv('Treated Data/rocket_data.csv') %>%
  select(-X)

likud_percentage = read.csv('Treated Data/likud_percentage.csv') %>% select(-1) 
total_votes = read.csv('Treated Data/total_votes.csv') %>% select(-1)


israel_panel = read.csv('Treated Data/israel_panel.csv')[-1,-1] %>%
  mutate(
    # SHAPE_Area is in m2
    # Converting to km2
    density = Pop_Total/(SHAPE_Area/1000000)
  )

#### election localities ####
elections_2013 = read_xlsx('Elections/expc_19.xlsx')[1:2] %>%
  rename(locality = 1,
         SEMEL_YISHUV = 2) 

elections_2015 = read_xlsx('Elections/expc_20.xlsx')[1:2] %>%
  rename(locality = 1,
         SEMEL_YISHUV = 2) 

electoral_localities = merge(elections_2013,
                             elections_2015,
                             by = 'SEMEL_YISHUV') %>%
  mutate(
    loc = gsub(r"{\s*\([^\)]+\)}","", locality.x),
    loc = gsub("[^א-ת]", "", loc, perl = TRUE)) %>%
  select(-locality.y) %>%
  rename(locality = locality.x)

rm(elections_2013,elections_2015)
# maps

gaza = read_sf('Israel/gaza.shp') %>%
  mutate(i = 1) %>%
  group_by(i) %>%
  summarise(geometry = st_union(geometry)) %>%
  select(-i)

st_crs(gaza) = 4326

israel = readOGR(dsn = 'Israel/statisticalareas_demography2017_30May_1336.mdb')

israel = israel %>%
  st_as_sf() %>%
  select(-OBJECTID, -STAT11, -YISHUV_STAT11,
         -Stat11_Unite, -Stat11_Ref, -Main_Function_Code,
         -Main_Function_Txt, -SHAPE_Length) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  group_by(SEMEL_YISHUV) %>%
  summarise(
    SEMEL_YISHUV = unique(SEMEL_YISHUV),
    SHEM_YISHUV = unique(SHEM_YISHUV),
    SHEM_YISHUV_ENGLISH = unique(SHEM_YISHUV_ENGLISH),
    Religion_yishuv_Code = unique(Religion_yishuv_Code),
    Religion_yishuv_Txt = unique(Religion_yishuv_Txt),
    Pop_Total = sum(Pop_Total, na.rm = T),
    Male_Total = sum(Male_Total, na.rm = T),
    Female_Total = sum(Female_Total, na.rm = T),
    age_0_14 = sum(age_0_14, na.rm = T),
    age_15_19 = sum(age_15_19, na.rm = T),
    age_20_29 = sum(age_20_29, na.rm = T),
    age_30_64 = sum(age_30_64, na.rm = T),
    age_65_up = sum(age_65_up, na.rm = T),
    centroid = first(centroid),
    geometry = st_union(geometry),
    do_union = F,
    SHAPE_Area = sum(SHAPE_Area)
  ) %>%
  mutate(
    centroid = st_transform(centroid, st_crs(gaza)),
    geometry = st_transform(geometry, st_crs(gaza)),
    loc = gsub(r"{\s*\([^\)]+\)}","", SHEM_YISHUV),
    loc = gsub("[^א-ת]", "", loc, perl = TRUE),
  )

centroid = israel$centroid %>%
  st_coordinates() %>%
  as.data.frame()
israel$long = centroid$X
israel$lat = centroid$Y

distance = st_distance(israel, gaza) %>% 
  set_units("km")

israel = israel %>%
  mutate(
    distance = as.numeric(distance)
  )






rm(centroid, distance)

# fixing cases where two cities have the same name but one is not populated
filtered_israel = israel %>%
  group_by(loc) %>%
  filter(Pop_Total > 0)




#### merging ####

# adding coordinates to each electoral locality
electoral_localities = merge(electoral_localities, filtered_israel, by ='SEMEL_YISHUV', all.x = T) %>%
  merge(coordinates, by.x = 'loc.x', by.y = 'loc', all.x = T) %>%
  mutate(
    long = ifelse(is.na(long), longitude,long),
    lat = ifelse(is.na(lat), latitude, lat)
  ) %>%
  rename(
    loc = loc.x
  ) %>%
  select(
    c(1:4, 6, 20:22)
  )



# Approximate matches for rocket_data
find_approx_match = function(search, observations) {
  distances = stringdistmatrix(search, observations, method = "jaccard")
  smallest_distance = apply(distances, 1, which.min)
  min_distance = apply(distances, 1, min)
  approximate_match = ifelse(min_distance < 0.1, observations[smallest_distance], NA)
  return(approximate_match)
}

approximate_match = find_approx_match(rocket_data$loc,
                                      filtered_israel$loc)
rocket_data$approximate_match = approximate_match


# adding latitude and longitude to rocket_data observations
rocket_data = rocket_data %>%
  mutate(
    # worst case scenario: approximate_match with israel data
    election_loc = ifelse(approximate_match %in% electoral_localities$loc, approximate_match, NA),
    # match with israel data: including possibility of place being a non-electoral locality
    election_loc = ifelse(loc %in% filtered_israel$loc, loc, election_loc),
    # best case scenario: locality in electoral_localities
    election_loc = ifelse(loc %in% electoral_localities$loc, loc, election_loc),
    date = date %>% ymd()) %>%
  # with Israeli official data
  merge(electoral_localities[, c('loc', 'lat', 'long')], by = 'loc', all.x = TRUE) %>%
  merge(israel[,c('loc','lat','long')], by = 'loc', all.x = T) %>%
  mutate(
    lat = coalesce(lat.x, lat.y),
    long = coalesce(long.x, long.y)
  ) %>%
  select(-lat.x, -long.x, -lat.y, -long.y)



# # if not matched, with Google Maps collected data
# left_join(coordinates[c('loc', 'lat', 'long')], by = "loc") %>%
# mutate(
#   lat = coalesce(lat.x, lat.y),
#   long = coalesce(long.x, long.y)
# ) %>%
# select(-lat.x, -lat.y, -long.x, -long.y) %>%




# rocket_data = rocket_data[!is.na(rocket_data$longitude),] %>% select(-centroid)




# merging with downloaded coords
rocket_data = 
  rocket_data %>%
  merge(coordinates[,c('loc','longitude','latitude')], by= 'loc', all.x = TRUE) %>%
  mutate(
    # if not matched with israel dataset coordinates, try with coordinates 
    long = ifelse(is.na(long), longitude,long),
    lat = ifelse(is.na(lat), latitude, lat)
  ) %>%
  select(-latitude,-longitude)





# as.shapefile
rocket_data = rocket_data[!is.na(rocket_data$long),] %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = F) 


distance = st_distance(rocket_data, gaza) %>% 
  set_units("km")

rocket_data = rocket_data %>%
  mutate(
    distance = distance
  )




# # election_location for NA observations
# # VIA NEAREST CITY (POLYGON INTERCEPTING)
# rocket_data$nearest_city = filtered_israel$SHEM_YISHUV[st_nearest_feature(rocket_data, filtered_israel)]
# 
# 
# rocket_data = rocket_data %>%
#   mutate(election_locality = ifelse(is.na(election_locality), nearest_city, election_locality),
#          election_loc = gsub("[^א-ת]", "", election_locality, perl = TRUE)) %>%
#   # distance to gaza
#   merge(israel[c("loc", "distance")] %>% as.data.frame(),
#         by.x = "election_loc",
#         by.y = "loc", 
#         all.x = T)




## ########### rocket_data[is.na(rocket_data$distance),] %>% nrow() ################# ##
## ########### NEEDS TO FIX THOSE VARIABLES (DOUBLE YUD, ETC) ##################### ##
## ############## gsub("יי", "י", dataframe$variavel) ######################### ##











############# DIFF-IN-DIFF ####################

# treatment groups
rocket_data_in_range = rocket_data %>%
  filter(distance >= set_units(75, "km") &
           distance <= set_units(150, "km")) %>%
  mutate(
    temporal_distance = abs(ymd('2015-03-17') - date) %>% as.numeric()
  )


# time distance of last attack until 2015 election per election locality
temporal_distance_until_2015 = rocket_data_in_range %>%
  filter(date < ymd('2015-03-17')) %>%
  group_by(election_loc) %>%
  summarise(
    temporal_distance = min(temporal_distance)
  )

# number of attacks per election locality
attack_numbers_until_2015 = rocket_data_in_range %>% 
  filter(date < ymd('2015-03-17')) %>%
  group_by(election_loc) %>%
  summarise(
    number_of_attacks_until_2015 = sum(hit)
  )



treated_locations_until_2015 = rocket_data_in_range %>%
  filter(date < ymd('2015-03-17')) %>%
  as.data.frame() %>%
  select(election_loc) %>%
  unique()


treated_locations_2015_to_2019 = rocket_data_in_range %>%
  filter(date > ymd('2015-03-17') &
           date < ymd('2019-04-19')) %>% as.data.frame() %>%
  select(election_loc) %>%
  unique()

treated_locations_2019_to_2019_2 = rocket_data_in_range %>%
  filter(date > ymd('2019-04-19') &
           date < ymd('2019-09-17')) %>% as.data.frame() %>%
  select(election_loc) %>%
  unique()

treated_locations_2019_2_to_2020 = rocket_data_in_range %>%
  filter(date > ymd('2019-09-17') &
           date < ymd('2020-03-02')) %>% as.data.frame() %>%
  select(election_loc) %>%
  unique()

treated_locations_2020_to_2021 = rocket_data_in_range %>%
  filter(date > ymd('2020-03-02') &
           date < ymd('2021-03-23')) %>% as.data.frame() %>%
  select(election_loc) %>%
  unique()

treated_locations_2021_to_2022 = rocket_data_in_range %>%
  filter(date > ymd('2021-03-23') &
           date < ymd('2022-11-01')) %>% as.data.frame() %>%
  select(election_loc) %>%
  unique()


# list of treatment groups
treated_locations_list <- list(treated_locations_until_2015,
                               treated_locations_2015_to_2019,
                               treated_locations_2019_to_2019_2,
                               treated_locations_2019_2_to_2020,
                               treated_locations_2020_to_2021,
                               treated_locations_2021_to_2022)

# treatment names
names_list <- c("treated_until_2015",
                "treated_2015_to_2019",
                "treated_2019_to_2019_2",
                "treated_2019_2_to_2020",
                "treated_2020_to_2021",
                "treated_2021_to_2022")


# Group of relevant cities
observations = electoral_localities %>%
  filter(distance >= 75 &
           distance <= 150)



# Adding treatment vars
add_treated_columns = function(df, treated_locations, name) {
  df %>% mutate(!!name := ifelse(loc %in% treated_locations$election_loc, TRUE, FALSE))
}

for(i in seq_along(treated_locations_list)) {
  observations = add_treated_columns(observations, treated_locations_list[[i]], names_list[i])
}


# Never treated
observations = observations %>%
  mutate(never_treated = ifelse(treated_until_2015 == F &
                                  treated_2015_to_2019 == F &
                                  treated_2019_to_2019_2 == F &
                                  treated_2019_2_to_2020 == F &
                                  treated_2020_to_2021 == F &
                                  treated_2021_to_2022 == F,
                                TRUE,
                                FALSE))


# Once treated, never retreated
check_previous_treatment = function(df, names_list) {
  for(i in seq_along(names_list)) {
    if(i > 1) {
      for(j in 1:(i-1)) {
        df[, names_list[i]] = ifelse(df[, names_list[j]] == TRUE, FALSE, df[, names_list[i]])
      }
    }
  }
  return(df)
}

observations = check_previous_treatment(observations, names_list)


# merging with elections dataset
observations = merge(observations,
                     likud_percentage[c(-12)],
                     by = 'SEMEL_YISHUV')


# restructuring
observations = observations %>% filter(!is.na(X2006)) %>%
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "right_wing_percentage") %>%
  mutate(year = as.numeric(str_replace_all(year, c("X" = "", "_2" = ".4"))),
         post = ifelse(year > 2014, T, F))



observations = merge(observations,
                     attack_numbers_until_2015,
                     by.x = 'loc',
                     by.y = 'election_loc',
                     all.x = T) %>%
  mutate(
    number_of_attacks_until_2015 = ifelse(is.na(number_of_attacks_until_2015), 0, number_of_attacks_until_2015),
  ) %>%
  merge(temporal_distance_until_2015,
        by.x = 'loc',
        by.y = 'election_loc',
        all.x = T
  )


observations = observations %>%
  mutate(
    temporal_group = cut(temporal_distance, 
                         breaks = c(-Inf, 6, Inf), 
                         labels = c("temporal_distance == 6", "temporal_distance > 200"),
                         include.lowest = TRUE) %>% as.character(),
    temporal_group = ifelse(is.na(temporal_group),'no_red_alert',temporal_group),
    
    
    number_of_attacks_group = cut(number_of_attacks_until_2015,
                                  breaks = c(-Inf,0,1,5,Inf),
                                  labels = c('No red alerts','1 red alert', '2-5 red alerts','6+ red alerts'),
                                  include.lowest = T) %>% as.character() %>% as.factor()
  )



diff_in_diff = observations %>%
  filter(year == 2013 | year == 2015) %>%
  mutate(
    post = ifelse(year == 2013, F, T)) %>%
  # merging with demographic panel
  merge(israel_panel %>% filter(year == 2013 | year == 2015) %>% select(SEMEL_YISHUV,Pop_Total,age_65_up, year, density),
        all.x = T,
        by = c('SEMEL_YISHUV','year')) %>%
  mutate(
    # number_of_attacks = ifelse(year != 2015, 0, number_of_attacks),
    age_65_up_percentage = age_65_up/Pop_Total)



# REGRESSIONS


# CONTROL GROUP = NOT TREATED UNTIL 2015
reg = feols(right_wing_percentage ~  treated*post,
            data = 
              diff_in_diff %>% 
              mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
            cluster = 'SEMEL_YISHUV')

# CONTROL GROUP = NOT TREATED UNTIL 2015; CITY FIXED EFFECTS; EXCLUDING ARAB CITIES
reg2 = feols(right_wing_percentage ~ treated*post + as.factor(SEMEL_YISHUV),
             data = 
               diff_in_diff %>% 
               filter(Religion_yishuv_Code != 2) %>%
               mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
             cluster = 'SEMEL_YISHUV')

# CONTROL GROUP = NOT TREATED UNTIL 2015; MANY CONTROL VARIABLES
reg3 = feols(right_wing_percentage ~  treated*post +
               age_65_up_percentage + distance + density + Pop_Total + as.factor(SEMEL_YISHUV),
             data = 
               diff_in_diff %>% 
               mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
             cluster = 'SEMEL_YISHUV')


# CONTROL GROUP = NEVER TREATED
reg4 = feols(right_wing_percentage ~ treated*post,
             data = 
               diff_in_diff %>% 
               filter(never_treated == T | treated_until_2015 == T) %>%
               mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
             cluster = 'SEMEL_YISHUV')

# CONTROL GROUP = NEVER TREATED; CITY FIXED EFFECTS; EXCLUDING ARAB CITIES
reg5 = feols(right_wing_percentage ~ treated:post + treated + post + as.factor(SEMEL_YISHUV),
             data = 
               diff_in_diff %>% 
               filter(Religion_yishuv_Code != 2) %>%
               filter(never_treated == T | treated_until_2015 == T) %>%
               mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
             cluster = 'SEMEL_YISHUV')


# CONTROL GROUP = NEVER TREATED; MANY CONTROL VARIABLES
reg6 = feols(right_wing_percentage ~  treated*post +
               age_65_up_percentage + distance + density + Pop_Total + as.factor(SEMEL_YISHUV),
             data = 
               diff_in_diff %>% 
               filter(never_treated == T | treated_until_2015 == T) %>%
               mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
             cluster = 'SEMEL_YISHUV')




modelsummary(list(reg, reg2, reg3, reg4, reg5, reg6
),
output = 'Output/main_results.tex',
coef_map = c('treatedTRUE:postTRUE' = 'Treated * Post',
             'treatedTRUE' = 'Treated',
             'postTRUE' = 'Post',
             '(Intercept)' = 'Intercept'),

stars = T, 
notes = 'NTU 2015 stands for Not Treated until the 2015 Election.',
gof_omit= '',
add_rows = data.frame('VARIABLES' = c('Observations',
                                      'Control Variables',
                                      'Control Group',
                                      'Locality Fixed Effects',
                                      'Include Arab Cities',
                                      'Clustered Errors'),
                      
                      '1' = c(summary(reg)$nobs/2,
                              'FALSE',
                              'NTU 2015',
                              'FALSE',
                              'TRUE',
                              'TRUE'),
                      '2' = c(summary(reg2)$nobs/2,
                              'FALSE',
                              'NTU 2015',
                              'TRUE',
                              'FALSE',
                              'TRUE'),
                      '3' = c(round(summary(reg3)$nobs/2,0),
                              'TRUE',
                              'NTU 2015',
                              'TRUE',
                              'TRUE',
                              'TRUE'),
                      '4' = c(summary(reg4)$nobs/2,
                              'FALSE',
                              'Never treated',
                              'FALSE',
                              'TRUE',
                              'TRUE'),
                      '5' = c(summary(reg5)$nobs/2,
                              'FALSE',
                              'Never treated',
                              'TRUE',
                              'FALSE',
                              'TRUE'),
                      '6' = c(summary(reg6)$nobs/2,
                              'TRUE',
                              'Never treated',
                              'TRUE',
                              'TRUE',
                              'TRUE')))





# Descriptive statistics

# first pair of groups
descriptive_statistics <- diff_in_diff %>%
  mutate(group = case_when(
    treated_until_2015 ~ "Treated",
    # never_treated ~ "Never Treated",
    !treated_until_2015 ~ "NTU 2015",
    TRUE ~ NA_character_
  )) %>%
  mutate(period = ifelse(post, "2015", "2013")) %>%
  group_by(group, period) %>%
  summarise(
    'Likud % (Average)' = mean(right_wing_percentage * 100, na.rm = TRUE),
    'Likud % (SD)' = sd(right_wing_percentage * 100, na.rm = TRUE),
    'Population Size (Average)' = mean(Pop_Total, na.rm = TRUE),
    'Population Size (SD)' = sd(Pop_Total, na.rm = TRUE),
    'Distance to Gaza (km) (Average)' = mean(distance, na.rm = TRUE),
    'Distance to Gaza (km) (SD)' = sd(distance, na.rm = TRUE),
    'Population Density (km2) (Average)' = mean(density, na.rm = TRUE),
    'Population Density (km2) (SD)' = sd(density, na.rm = TRUE)
  ) %>%
  arrange(group, desc(period))

# second pair
descriptive_statistics2 <- diff_in_diff %>%
  mutate(group = case_when(
    treated_until_2015 ~ "Treated",
    never_treated ~ "Never Treated",
    # !treated_until_2015 ~ "NTU 2015",
    TRUE ~ NA_character_
  )) %>%
  mutate(period = ifelse(post, "2015", "2013")) %>%
  group_by(group, period) %>%
  summarise(
    'Likud % (Average)' = mean(right_wing_percentage * 100, na.rm = TRUE),
    'Likud % (SD)' = sd(right_wing_percentage * 100, na.rm = TRUE),
    'Population Size (Average)' = mean(Pop_Total, na.rm = TRUE),
    'Population Size (SD)' = sd(Pop_Total, na.rm = TRUE),
    'Distance to Gaza (km) (Average)' = mean(distance, na.rm = TRUE),
    'Distance to Gaza (km) (SD)' = sd(distance, na.rm = TRUE),
    'Population Density (km2) (Average)' = mean(density, na.rm = TRUE),
    'Population Density (km2) (SD)' = sd(density, na.rm = TRUE)
  ) %>% na.omit() %>%
  arrange(group, desc(period))

# binding
descriptive_statistics = unique(rbind(descriptive_statistics, descriptive_statistics2))
remove(descriptive_statistics2)


# saving
sink('Output/descriptives.tex')
print(xtable(descriptive_statistics))
sink()





# PARALLEL TRENDS

# 1. control = NEVER_TREATED
df_mean = observations %>%
  filter(never_treated == T | treated_until_2015 == T) %>%
  group_by(year,
           treated_until_2015
  ) %>%
  summarise(mean_votes = mean(right_wing_percentage))

ggplot(df_mean, aes(x = year, y = mean_votes,
                    color = as.factor(treated_until_2015))) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black", size = 0.5) +
  labs(x = "Year", y = "Average Share of Votes: Likud", color = "Red Alerts:") +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Never", "Between the 2013 and 2015 Elections")) +
  ylim(c(0,0.3)) +
  theme_minimal() +
  theme(legend.position = "bottom") 

ggsave('Output/parallel_trends_never_treated.pdf', width = 7, height = 4)


# 2. control = not_treated_until_2015
df_mean = observations %>%
  group_by(year,
           treated_until_2015
  ) %>%
  summarise(mean_votes = mean(right_wing_percentage))

ggplot(df_mean, aes(x = year, y = mean_votes,
                    color = as.factor(treated_until_2015))) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black", size = 0.5) +
  labs(x = "Year", y = "Average Share of Votes: Likud", color = "Red Alerts:") +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Not until the 2015 Election", "Between the 2013 and 2015 Elections")) +
  ylim(c(0,0.3)) +
  theme_minimal() +
  theme(legend.position = "bottom")


ggsave('Output/parallel_trends_not_until_2015.pdf', width = 7, height = 4)



# 3. control = never_treated; discriminating by timing before 2015 election
df_mean = observations %>%
  filter(never_treated == T | treated_until_2015 == T) %>%
  group_by(year,
           treated_until_2015,
           temporal_group
  ) %>%
  summarise(mean_votes = mean(right_wing_percentage),
            SHEM_YISHUV) %>% select(-5) %>% distinct()

ggplot(df_mean, aes(x = year, y = mean_votes,
                    color = as.factor(temporal_group))) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black", size = 0.5) +
  labs(x = "Year", y = "Average Share of Votes: Likud", color = "Days between last Red Alert and 2015 election:") +
  scale_color_manual(values = c("blue", "red", "orange", "black"), 
                     labels = c("No Red Alert", "6 days", "204-232 days")) + 
  ylim(c(0,0.3)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave('Output/parallel_trends_temporal_groups_never_treated.pdf', width = 8, height = 4)



# 4. control = not treated until 2015; discriminating by timing before 2015 election
df_mean = observations %>%
  group_by(year,
           treated_until_2015,
           temporal_group
  ) %>%
  summarise(mean_votes = mean(right_wing_percentage),
            SHEM_YISHUV) %>% select(-5) %>% distinct()

ggplot(df_mean[!is.na(df_mean$temporal_group),], aes(x = year, y = mean_votes,
                                                     color = as.factor(temporal_group))) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black", size = 0.5) +
  labs(x = "Year", y = "Average Share of Votes: Likud", color = "Days between last Red Alert and 2015 election:") +
  scale_color_manual(values = c("blue", "red", "orange", "black"), 
                     labels = c("No Red Alert until 2015 Election", "6 days", "204-232 days")) + 
  ylim(c(0,0.3)) +
  theme_minimal() +
  theme(legend.position = "bottom") 

ggsave('Output/parallel_trends_temporal_groups_not_treated_until_2015.pdf', width = 8, height = 4)


# VER SOBRE LOCALIDADES HISTORICAMENTE DE ESQUERDA/DIREITA?


####### HETEROGENEITY IN TREATMENT #######



#### days before election ####
# CONTROL GROUP = NOT TREATED UNTIL 2015
reg_temporal = feols(right_wing_percentage ~  temporal_group*post,
                     data = 
                       diff_in_diff %>% 
                       mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
                     cluster = 'SEMEL_YISHUV')

# CONTROL GROUP = NOT TREATED UNTIL 2015; CITY FIXED EFFECTS; EXCLUDING ARAB CITIES
reg2_temporal = feols(right_wing_percentage ~ temporal_group*post + as.factor(SEMEL_YISHUV),
                      data = 
                        diff_in_diff %>% 
                        filter(Religion_yishuv_Code != 2) %>%
                        mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
                      cluster = 'SEMEL_YISHUV')

# CONTROL GROUP = NOT TREATED UNTIL 2015; MANY CONTROL VARIABLES
reg3_temporal = feols(right_wing_percentage ~  temporal_group*post +
                        age_65_up_percentage + distance + density + Pop_Total + as.factor(SEMEL_YISHUV),
                      data = 
                        diff_in_diff %>% 
                        mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
                      cluster = 'SEMEL_YISHUV')


# CONTROL GROUP = NEVER TREATED
reg4_temporal = feols(right_wing_percentage ~ temporal_group*post + treated + post,
                      data = 
                        diff_in_diff %>% 
                        filter(never_treated == T | treated_until_2015 == T) %>%
                        mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
                      cluster = 'SEMEL_YISHUV')

# CONTROL GROUP = NEVER TREATED; CITY FIXED EFFECTS; EXCLUDING ARAB CITIES
reg5_temporal = feols(right_wing_percentage ~ temporal_group*post + as.factor(SEMEL_YISHUV),
                      data = 
                        diff_in_diff %>% 
                        filter(Religion_yishuv_Code != 2) %>%
                        filter(never_treated == T | treated_until_2015 == T) %>%
                        mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
                      cluster = 'SEMEL_YISHUV')


# CONTROL GROUP = NEVER TREATED; MANY CONTROL VARIABLES
reg6_temporal = feols(right_wing_percentage ~  temporal_group*post +
                        age_65_up_percentage + distance + density + Pop_Total + as.factor(SEMEL_YISHUV),
                      data = 
                        diff_in_diff %>% 
                        filter(never_treated == T | treated_until_2015 == T) %>%
                        mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
                      cluster = 'SEMEL_YISHUV')




modelsummary(list(reg_temporal, reg2_temporal, reg3_temporal, reg4_temporal, reg5_temporal, reg6_temporal
),
output = 'Output/temporal_results.tex',
coef_map = c('temporal_grouptemporal_distance == 6:postTRUE' = 'Red Alert 6 Days Before * Post',
             'temporal_grouptemporal_distance > 200:postTRUE'= 'Red Alert 204-3 Days Before * Post',
             'temporal_grouptemporal_distance == 6' = 'Red Alert 6 Days Before Election',
             'temporal_grouptemporal_distance > 200' = 'Red Alert 204+ Days Before Election',
             'postTRUE' = 'Post',
             '(Intercept)' = 'Intercept'),

stars = T, 
notes = 'NTU 2015 stands for Not Treated until the 2015 Election.',
gof_omit= '',
add_rows = data.frame('VARIABLES' = c('Observations',
                                      'Control Variables',
                                      'Control Group',
                                      'Locality Fixed Effects',
                                      'Include Arab Cities',
                                      'Clustered Errors'),
                      
                      '1' = c(summary(reg_temporal)$nobs/2,
                              'FALSE',
                              'NTU 2015',
                              'FALSE',
                              'TRUE',
                              'TRUE'),
                      '2' = c(summary(reg2_temporal)$nobs/2,
                              'FALSE',
                              'NTU 2015',
                              'TRUE',
                              'FALSE',
                              'TRUE'),
                      '3' = c(round(summary(reg3_temporal)$nobs/2,0),
                              'TRUE',
                              'NTU 2015',
                              'TRUE',
                              'TRUE',
                              'TRUE'),
                      '4' = c(summary(reg4_temporal)$nobs/2,
                              'FALSE',
                              'Never treated',
                              'FALSE',
                              'TRUE',
                              'TRUE'),
                      '5' = c(summary(reg5_temporal)$nobs/2,
                              'FALSE',
                              'Never treated',
                              'TRUE',
                              'FALSE',
                              'TRUE'),
                      '6' = c(summary(reg6_temporal)$nobs/2,
                              'TRUE',
                              'Never treated',
                              'TRUE',
                              'TRUE',
                              'TRUE')))






#### number of attacks ####

# CONTROL GROUP = NOT TREATED UNTIL 2015
reg_attacks = feols(right_wing_percentage ~  number_of_attacks_group*post,
                    data = 
                      diff_in_diff %>% 
                      mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
                    cluster = 'SEMEL_YISHUV')

# CONTROL GROUP = NOT TREATED UNTIL 2015; CITY FIXED EFFECTS; EXCLUDING ARAB CITIES
reg2_attacks = feols(right_wing_percentage ~ number_of_attacks_group*post + as.factor(SEMEL_YISHUV),
                     data = 
                       diff_in_diff %>% 
                       filter(Religion_yishuv_Code != 2) %>%
                       mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
                     cluster = 'SEMEL_YISHUV')

# CONTROL GROUP = NOT TREATED UNTIL 2015; MANY CONTROL VARIABLES
reg3_attacks = feols(right_wing_percentage ~  number_of_attacks_group*post +
                       age_65_up_percentage + distance + density + Pop_Total + as.factor(SEMEL_YISHUV),
                     data = 
                       diff_in_diff %>% 
                       mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
                     cluster = 'SEMEL_YISHUV')


# CONTROL GROUP = NEVER TREATED
reg4_attacks = feols(right_wing_percentage ~ number_of_attacks_group*post + post,
                     data = 
                       diff_in_diff %>% 
                       filter(never_treated == T | treated_until_2015 == T) %>%
                       mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
                     cluster = 'SEMEL_YISHUV')

# CONTROL GROUP = NEVER TREATED; CITY FIXED EFFECTS; EXCLUDING ARAB CITIES
reg5_attacks = feols(right_wing_percentage ~ number_of_attacks_group*post + as.factor(SEMEL_YISHUV),
                     data = 
                       diff_in_diff %>% 
                       filter(Religion_yishuv_Code != 2) %>%
                       filter(never_treated == T | treated_until_2015 == T) %>%
                       mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
                     cluster = 'SEMEL_YISHUV')


# CONTROL GROUP = NEVER TREATED; MANY CONTROL VARIABLES
reg6_attacks = feols(right_wing_percentage ~  number_of_attacks_group*post +
                       age_65_up_percentage + distance + density + Pop_Total + as.factor(SEMEL_YISHUV),
                     data = 
                       diff_in_diff %>% 
                       filter(never_treated == T | treated_until_2015 == T) %>%
                       mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
                     cluster = 'SEMEL_YISHUV')




modelsummary(list(reg_attacks, reg2_attacks, reg3_attacks, reg4_attacks, reg5_attacks, reg6_attacks
),
# output = 'Output/temporal_results.tex',
# coef_map = c('number_of_attacks_grouptemporal_distance == 6:postTRUE' = 'Red Alert 6 Days Before * Post',
#              'number_of_attacks_grouptemporal_distance > 200:postTRUE'= 'Red Alert 204-3 Days Before * Post',
#              'number_of_attacks_grouptemporal_distance == 6' = 'Red Alert 6 Days Before Election',
#              'number_of_attacks_grouptemporal_distance > 200' = 'Red Alert 204+ Days Before Election',
#              'postTRUE' = 'Post',
#              '(Intercept)' = 'Intercept'),

stars = T, 
notes = 'NTU 2015 stands for Not Treated until the 2015 Election.',
gof_omit= '',
add_rows = data.frame('VARIABLES' = c('Observations',
                                      'Control Variables',
                                      'Control Group',
                                      'Locality Fixed Effects',
                                      'Include Arab Cities',
                                      'Clustered Errors'),
                      
                      '1' = c(summary(reg_attacks)$nobs/2,
                              'FALSE',
                              'NTU 2015',
                              'FALSE',
                              'TRUE',
                              'TRUE'),
                      '2' = c(summary(reg2_attacks)$nobs/2,
                              'FALSE',
                              'NTU 2015',
                              'TRUE',
                              'FALSE',
                              'TRUE'),
                      '3' = c(round(summary(reg3_attacks)$nobs/2,0),
                              'TRUE',
                              'NTU 2015',
                              'TRUE',
                              'TRUE',
                              'TRUE'),
                      '4' = c(summary(reg4_attacks)$nobs/2,
                              'FALSE',
                              'Never treated',
                              'FALSE',
                              'TRUE',
                              'TRUE'),
                      '5' = c(summary(reg5_attacks)$nobs/2,
                              'FALSE',
                              'Never treated',
                              'TRUE',
                              'FALSE',
                              'TRUE'),
                      '6' = c(summary(reg6_attacks)$nobs/2,
                              'TRUE',
                              'Never treated',
                              'TRUE',
                              'TRUE',
                              'TRUE')))







# 
# 
# 
# 
# 
# 
# 
# # CONTROL GROUP = NOT TREATED UNTIL 2015
# reg_number = feols(right_wing_percentage ~  number_of_attacks_until_2015*post,
#                      data = 
#                        diff_in_diff %>% 
#                        mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#                      cluster = 'SEMEL_YISHUV')
# 
# # CONTROL GROUP = NOT TREATED UNTIL 2015; CITY FIXED EFFECTS; EXCLUDING ARAB CITIES
# reg2_number = feols(right_wing_percentage ~ number_of_attacks_until_2015*post + as.factor(SEMEL_YISHUV),
#                       data = 
#                         diff_in_diff %>% 
#                         filter(Religion_yishuv_Code != 2) %>%
#                         mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#                       cluster = 'SEMEL_YISHUV')
# 
# # CONTROL GROUP = NOT TREATED UNTIL 2015; MANY CONTROL VARIABLES
# reg3_number = feols(right_wing_percentage ~  number_of_attacks_until_2015*post +
#                         age_65_up_percentage + distance + density + Pop_Total + as.factor(SEMEL_YISHUV),
#                       data = 
#                         diff_in_diff %>% 
#                         mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#                       cluster = 'SEMEL_YISHUV')
# 
# 
# # CONTROL GROUP = NEVER TREATED
# reg4_number = feols(right_wing_percentage ~ number_of_attacks_until_2015*post + treated + post,
#                       data = 
#                         diff_in_diff %>% 
#                         filter(never_treated == T | treated_until_2015 == T) %>%
#                         mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#                       cluster = 'SEMEL_YISHUV')
# 
# # CONTROL GROUP = NEVER TREATED; CITY FIXED EFFECTS; EXCLUDING ARAB CITIES
# reg5_number = feols(right_wing_percentage ~ number_of_attacks_until_2015*post + as.factor(SEMEL_YISHUV),
#                       data = 
#                         diff_in_diff %>% 
#                         filter(Religion_yishuv_Code != 2) %>%
#                         filter(never_treated == T | treated_until_2015 == T) %>%
#                         mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#                       cluster = 'SEMEL_YISHUV')
# 
# 
# # CONTROL GROUP = NEVER TREATED; MANY CONTROL VARIABLES
# reg6_number = feols(right_wing_percentage ~  number_of_attacks_until_2015*post +
#                         age_65_up_percentage + distance + density + Pop_Total + as.factor(SEMEL_YISHUV),
#                       data = 
#                         diff_in_diff %>% 
#                         filter(never_treated == T | treated_until_2015 == T) %>%
#                         mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#                       cluster = 'SEMEL_YISHUV')
# 
# 
# 
# 
# modelsummary(list(reg_number, reg2_number, reg3_number, reg4_number, reg5_number, reg6_number
# ),
# output = 'Output/number_of_attacks_results.tex',
# coef_map = c('number_of_attacks_until_2015:postTRUE' = 'Number of Red Alerts * Post',
#              'number_of_attacks_until_2015' = 'Number of Red Alerts',
#              'postTRUE' = 'Post',
#              '(Intercept)' = 'Intercept'),
# 
# stars = T, 
# notes = 'NTU 2015 stands for Not Treated until the 2015 Election.',
# gof_omit= '',
# add_rows = data.frame('VARIABLES' = c('Observations',
#                                       'Control Variables',
#                                       'Control Group',
#                                       'Locality Fixed Effects',
#                                       'Include Arab Cities',
#                                       'Clustered Errors'),
#                       
#                       '1' = c(summary(reg_temporal)$nobs/2,
#                               'FALSE',
#                               'NTU 2015',
#                               'FALSE',
#                               'TRUE',
#                               'TRUE'),
#                       '2' = c(summary(reg2_temporal)$nobs/2,
#                               'FALSE',
#                               'NTU 2015',
#                               'TRUE',
#                               'FALSE',
#                               'TRUE'),
#                       '3' = c(round(summary(reg3_temporal)$nobs/2,0),
#                               'TRUE',
#                               'NTU 2015',
#                               'TRUE',
#                               'TRUE',
#                               'TRUE'),
#                       '4' = c(summary(reg4_temporal)$nobs/2,
#                               'FALSE',
#                               'Never treated',
#                               'FALSE',
#                               'TRUE',
#                               'TRUE'),
#                       '5' = c(summary(reg5_temporal)$nobs/2,
#                               'FALSE',
#                               'Never treated',
#                               'TRUE',
#                               'FALSE',
#                               'TRUE'),
#                       '6' = c(summary(reg6_temporal)$nobs/2,
#                               'TRUE',
#                               'Never treated',
#                               'TRUE',
#                               'TRUE',
#                               'TRUE')))


#### number of attacks ####
# 
# # CONTROL GROUP = NOT TREATED UNTIL 2015
# reg_number = feols(right_wing_percentage ~  number_of_attacks_until_2015*post,
#                      data = 
#                        diff_in_diff %>% 
#                        mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#                      cluster = 'SEMEL_YISHUV')
# 
# # CONTROL GROUP = NOT TREATED UNTIL 2015; CITY FIXED EFFECTS; EXCLUDING ARAB CITIES
# reg2_number = feols(right_wing_percentage ~ number_of_attacks_until_2015*post + as.factor(SEMEL_YISHUV),
#                       data = 
#                         diff_in_diff %>% 
#                         filter(Religion_yishuv_Code != 2) %>%
#                         mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#                       cluster = 'SEMEL_YISHUV')
# 
# # CONTROL GROUP = NOT TREATED UNTIL 2015; MANY CONTROL VARIABLES
# reg3_number = feols(right_wing_percentage ~  number_of_attacks_until_2015*post +
#                         age_65_up_percentage + distance + density + Pop_Total + as.factor(SEMEL_YISHUV),
#                       data = 
#                         diff_in_diff %>% 
#                         mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#                       cluster = 'SEMEL_YISHUV')
# 
# 
# # CONTROL GROUP = NEVER TREATED
# reg4_number = feols(right_wing_percentage ~ number_of_attacks_until_2015*post + treated + post,
#                       data = 
#                         diff_in_diff %>% 
#                         filter(never_treated == T | treated_until_2015 == T) %>%
#                         mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#                       cluster = 'SEMEL_YISHUV')
# 
# # CONTROL GROUP = NEVER TREATED; CITY FIXED EFFECTS; EXCLUDING ARAB CITIES
# reg5_number = feols(right_wing_percentage ~ number_of_attacks_until_2015*post + as.factor(SEMEL_YISHUV),
#                       data = 
#                         diff_in_diff %>% 
#                         filter(Religion_yishuv_Code != 2) %>%
#                         filter(never_treated == T | treated_until_2015 == T) %>%
#                         mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#                       cluster = 'SEMEL_YISHUV')
# 
# 
# # CONTROL GROUP = NEVER TREATED; MANY CONTROL VARIABLES
# reg6_number = feols(right_wing_percentage ~  number_of_attacks_until_2015*post +
#                         age_65_up_percentage + distance + density + Pop_Total + as.factor(SEMEL_YISHUV),
#                       data = 
#                         diff_in_diff %>% 
#                         filter(never_treated == T | treated_until_2015 == T) %>%
#                         mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#                       cluster = 'SEMEL_YISHUV')
# 
# 
# 
# 
# modelsummary(list(reg_number, reg2_number, reg3_number, reg4_number, reg5_number, reg6_number
# ),
# output = 'Output/number_of_attacks_results.tex',
# coef_map = c('number_of_attacks_until_2015:postTRUE' = 'Number of Red Alerts * Post',
#              'number_of_attacks_until_2015' = 'Number of Red Alerts',
#              'postTRUE' = 'Post',
#              '(Intercept)' = 'Intercept'),
# 
# stars = T, 
# notes = 'NTU 2015 stands for Not Treated until the 2015 Election.',
# gof_omit= '',
# add_rows = data.frame('VARIABLES' = c('Observations',
#                                       'Control Variables',
#                                       'Control Group',
#                                       'Locality Fixed Effects',
#                                       'Include Arab Cities',
#                                       'Clustered Errors'),
#                       
#                       '1' = c(summary(reg_temporal)$nobs/2,
#                               'FALSE',
#                               'NTU 2015',
#                               'FALSE',
#                               'TRUE',
#                               'TRUE'),
#                       '2' = c(summary(reg2_temporal)$nobs/2,
#                               'FALSE',
#                               'NTU 2015',
#                               'TRUE',
#                               'FALSE',
#                               'TRUE'),
#                       '3' = c(round(summary(reg3_temporal)$nobs/2,0),
#                               'TRUE',
#                               'NTU 2015',
#                               'TRUE',
#                               'TRUE',
#                               'TRUE'),
#                       '4' = c(summary(reg4_temporal)$nobs/2,
#                               'FALSE',
#                               'Never treated',
#                               'FALSE',
#                               'TRUE',
#                               'TRUE'),
#                       '5' = c(summary(reg5_temporal)$nobs/2,
#                               'FALSE',
#                               'Never treated',
#                               'TRUE',
#                               'FALSE',
#                               'TRUE'),
#                       '6' = c(summary(reg6_temporal)$nobs/2,
#                               'TRUE',
#                               'Never treated',
#                               'TRUE',
#                               'TRUE',
#                               'TRUE')))
# 
# 

# 
# #### distance ####
# 
# # diff_in_diff = diff_in_diff %>%
# #   mutate(
# #     distance_group = cut(distance, 
# #                          breaks = c(-Inf, 100, Inf), 
# #                          labels = c("distance < 100km", "distance > 100km"),
# #                          include.lowest = TRUE) %>% as.character()
# #   )
# 
# # CONTROL GROUP = NOT TREATED UNTIL 2015
# reg_distance = feols(right_wing_percentage ~  treated*post*distance,
#             data = 
#               diff_in_diff %>% 
#               mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#             cluster = 'SEMEL_YISHUV')
# 
# # CONTROL GROUP = NOT TREATED UNTIL 2015; CITY FIXED EFFECTS; EXCLUDING ARAB CITIES
# reg2_distance = feols(right_wing_percentage ~ treated*post*distance + as.factor(SEMEL_YISHUV),
#              data = 
#                diff_in_diff %>% 
#                filter(Religion_yishuv_Code != 2) %>%
#                mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#              cluster = 'SEMEL_YISHUV')
# 
# # CONTROL GROUP = NOT TREATED UNTIL 2015; MANY CONTROL VARIABLES
# reg3_distance = feols(right_wing_percentage ~  treated*post*distance +
#                age_65_up_percentage + distance + density + Pop_Total + as.factor(SEMEL_YISHUV),
#              data = 
#                diff_in_diff %>% 
#                mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#              cluster = 'SEMEL_YISHUV')
# 
# 
# # CONTROL GROUP = NEVER TREATED
# reg4_distance = feols(right_wing_percentage ~ treated*post*distance,
#              data = 
#                diff_in_diff %>% 
#                filter(never_treated == T | treated_until_2015 == T) %>%
#                mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#              cluster = 'SEMEL_YISHUV')
# 
# # CONTROL GROUP = NEVER TREATED; CITY FIXED EFFECTS; EXCLUDING ARAB CITIES
# reg5_distance = feols(right_wing_percentage ~ treated*post*distance + treated + post + as.factor(SEMEL_YISHUV),
#              data = 
#                diff_in_diff %>% 
#                filter(Religion_yishuv_Code != 2) %>%
#                filter(never_treated == T | treated_until_2015 == T) %>%
#                mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#              cluster = 'SEMEL_YISHUV')
# 
# 
# # CONTROL GROUP = NEVER TREATED; MANY CONTROL VARIABLES
# reg6_distance = feols(right_wing_percentage ~  treated*post*distance +
#                age_65_up_percentage + distance + density + Pop_Total + as.factor(SEMEL_YISHUV),
#              data = 
#                diff_in_diff %>% 
#                filter(never_treated == T | treated_until_2015 == T) %>%
#                mutate(treated = ifelse(treated_until_2015 == T, T ,F)),
#              cluster = 'SEMEL_YISHUV')
# 
# 
# 
# 
# modelsummary(list(reg_distance, reg2_distance, reg3_distance, reg4_distance, reg5_distance, reg6_distance
# ),
# output = 'Output/distance_results.tex',
# coef_map = c('treatedTRUE:postTRUE:distance' = 'Red Alerts * Post * Distance',
#              'treatedTRUE:postTRUE' = 'Red Alerts * Post',
#              'treatedTRUE:distance' = 'Red Alerts * Distance',
#              'postTRUE:distance'= 'Post * Distance',
#              'treatedTRUE' = 'Red Alerts',
#              'postTRUE' = 'Post',
#              'distance' = 'Distance',
#              '(Intercept)' = 'Intercept'),
# 
# stars = T, 
# notes = 'NTU 2015 stands for Not Treated until the 2015 Election.',
# gof_omit= '',
# add_rows = data.frame('VARIABLES' = c('Observations',
#                                       'Control Variables',
#                                       'Control Group',
#                                       'Locality Fixed Effects',
#                                       'Include Arab Cities',
#                                       'Clustered Errors'),
#                       
#                       '1' = c(summary(reg)$nobs/2,
#                               'FALSE',
#                               'NTU 2015',
#                               'FALSE',
#                               'TRUE',
#                               'TRUE'),
#                       '2' = c(summary(reg2)$nobs/2,
#                               'FALSE',
#                               'NTU 2015',
#                               'TRUE',
#                               'FALSE',
#                               'TRUE'),
#                       '3' = c(round(summary(reg3)$nobs/2,0),
#                               'TRUE',
#                               'NTU 2015',
#                               'TRUE',
#                               'TRUE',
#                               'TRUE'),
#                       '4' = c(summary(reg4)$nobs/2,
#                               'FALSE',
#                               'Never treated',
#                               'FALSE',
#                               'TRUE',
#                               'TRUE'),
#                       '5' = c(summary(reg5)$nobs/2,
#                               'FALSE',
#                               'Never treated',
#                               'TRUE',
#                               'FALSE',
#                               'TRUE'),
#                       '6' = c(summary(reg6)$nobs/2,
#                               'TRUE',
#                               'Never treated',
#                               'TRUE',
#                               'TRUE',
#                               'TRUE')))
# 
# 
# 



pdf('Output/temporal_distance_heterogeneity.pdf', width = 6, height = 6)
diff_in_diff[diff_in_diff$treated_until_2015==T & diff_in_diff$post == T,]$temporal_distance %>% 
  sort %>% 
  plot(xlab = 'Locality', ylab = 'Days Between Last Red Alert and 2015 Election')
dev.off()




pdf('Output/number_of_attacks_heterogeneity.pdf', width = 6, height = 6)
diff_in_diff[diff_in_diff$treated_until_2015==T & diff_in_diff$post == T,]$number_of_attacks_until_2015 %>%
  sort %>%
  plot(xlab = 'Locality', ylab = 'Number of Red Alerts Until the 2015 Election')
dev.off()



pdf('Output/distance_to_gaza_heterogeneity.pdf', width = 6, height = 6)
diff_in_diff[diff_in_diff$treated_until_2015==T & diff_in_diff$post == T,]$distance %>%
  sort %>% 
  plot(xlab = 'Locality', ylab = 'Distance to the Gaza Strip (km)')
dev.off()







#### MAPS ####
cities = merge(filtered_israel %>% as.data.frame(),
               observations[,c('SEMEL_YISHUV','treated_until_2015','number_of_attacks_until_2015', 'temporal_group')], by = 'SEMEL_YISHUV', all.x = T) %>%
  mutate(treated = ifelse(treated_until_2015 == T, T, F),
         distance = as.numeric(distance))


buffer_45 = st_buffer(gaza, dist = 45000)
buffer_75 = st_buffer(gaza, dist = 75000) 
buffer_150 = st_buffer(gaza, dist = 150000) 


min_long = min(st_coordinates(gaza)[, "X"])
max_long = max(st_coordinates(israel)[,'X'])

# ranges map
ggplot() +
  geom_sf(data = israel$geometry, aes(fill = "Israel"), lwd = .1) +
  geom_sf(data = gaza, aes(fill = "Gaza")) +
  geom_sf(data = st_difference(buffer_150, buffer_75),
          fill = "red", alpha = 0.15) +
  geom_sf(data = st_difference(buffer_75, buffer_45),
          fill = "orange", alpha = 0.5) +
  geom_sf(data = st_difference(buffer_45, gaza),
          fill = "yellow", alpha = 0.5) +
  xlim(min_long, max_long + 0.3) +
  geom_sf(data = buffer_45, aes(color = "2008-2011: 45 km"), fill = NA, lwd = 1) +
  geom_sf(data = buffer_75, aes(color = "2012-2013: 75 km"), fill = NA, lwd = 1) +
  geom_sf(data = buffer_150, aes(color = "2014-present: 150 km"), fill = NA, lwd = 1) +
  
  
  scale_fill_manual(values = c("Israel" = 'lightgreen',
                               "Gaza" = 'orange'),
                    guide = guide_legend(title = "Territory:")) +
  
  scale_color_manual(values = c("2008-2011: 45 km" = "yellow",
                                "2012-2013: 75 km" = "orange",
                                "2014-present: 150 km" = "red")) +
  guides(color = guide_legend(override.aes = list(fill = c("yellow", "orange", "red"),
                                                  alpha = c(0.15, 0.15, 0.15)),
                              title = 'Rocket Range:'),
         fill = guide_legend(title = 'Territory:')) +
  
  theme_minimal()



ggsave('Output/israel_ranges.pdf', width = 8.27 , height = 11.69, units = "in", dpi = 300)


# treatment map
ggplot() + 
  geom_sf(data = israel$geometry, color = 'lightgrey')+
  # geom_sf(data = israel[israel$distance > 75 & israel$distance < 150,]$geometry, fill = 'lightgreen', lwd = .1) +
  # geom_sf(data = israel[israel$distance < 75,]$geometry, fill = 'lightgrey', lwd = .1) +
  # geom_sf(data = israel[israel$distance > 150,]$geometry, fill = 'lightgrey', lwd = .1) +
  # geom_sf(data = israel[st_intersects(israel,buffer_75, sparse = F) %>% unlist,]$geometry,
  #         color = 'lightgrey') + 
  geom_sf(data = cities[cities$treated == T,]$geometry, aes(fill = 'Treatment Group', color = 'Treatment Group'), lwd = 0) +
  geom_sf(data = cities[cities$treated == F,]$geometry, aes(fill = 'Control Group', color = 'Control Group'), lwd = 0) +
  geom_sf(data = buffer_75, fill = NA, color = 'orange', lwd = 6) +
  geom_sf(data = buffer_150, fill = NA, color = "orange", lwd = 6) +
  geom_sf(data = gaza, fill = "orange", lwd = .1) +
  xlim(min_long, max_long + 0.3) +
  scale_fill_manual(values = c("Treatment Group" = "red", "Control Group" = "blue"),
                    guide = guide_legend(title = "Localities:")) +
  scale_color_manual(values = c('Treatment Group' = 'red', 'Control Group' = 'blue')) +
  guides(color = 'none') +
  theme_minimal()

ggsave('Output/israel_attacks.pdf',width = 8.27 , height = 11.69, units = "in", dpi = 300)



# temporal distance map
ggplot() + 
  geom_sf(data = israel$geometry, color = 'lightgrey')+
  # geom_sf(data = israel[israel$distance > 75 & israel$distance < 150,]$geometry, fill = 'lightgreen', lwd = .1) +
  # geom_sf(data = israel[israel$distance < 75,]$geometry, fill = 'lightgrey', lwd = .1) +
  # geom_sf(data = israel[israel$distance > 150,]$geometry, fill = 'lightgrey', lwd = .1) +
  # geom_sf(data = israel[st_intersects(israel,buffer_75, sparse = F) %>% unlist,]$geometry,
  #         color = 'lightgrey') + 
  geom_sf(data = cities[!is.na(cities$temporal_group),]$geometry, aes(fill = cities[!is.na(cities$temporal_group),]$temporal_group,
                                                                      color = cities[!is.na(cities$temporal_group),]$temporal_group), lwd = 0) +
  geom_sf(data = buffer_75, fill = NA, color = 'orange', lwd = 6) +
  geom_sf(data = buffer_150, fill = NA, color = "orange", lwd = 6) +
  geom_sf(data = gaza, fill = "orange", lwd = .1) +
  xlim(min_long, max_long + 0.3) +
  guides(color = 'none') +
  guides(fill = guide_legend(title = "Days between last Red Alert and 2015 election:")) + 
  scale_fill_manual(values = c("blue", "red",'yellow'),
                    labels = c("No Red Alerts","6 days", "204-232 days")) +
  scale_color_manual(values = c('blue', 'red','yellow')) +
  theme_minimal()


ggsave('Output/israel_temporal_groups.pdf',width = 8.27 , height = 11.69, units = "in", dpi = 300)







# Plotando o gráfico de Israel com o polígono colorido de acordo com a quantidade de ataques
all_attack_numbers_until_2015 = rocket_data %>% 
  filter(date < ymd('2015-03-17')) %>%
  group_by(election_loc) %>%
  summarise(
    number_of_attacks_until_2015 = sum(hit)
  )


observations_2 = merge(all_attack_numbers_until_2015 , electoral_localities, by.x = 'election_loc', by.y = 'loc', all.y = T)

cities_2 = merge(filtered_israel %>% as.data.frame(),
                 observations_2[,c('SEMEL_YISHUV','number_of_attacks_until_2015')], by = 'SEMEL_YISHUV', all.x = T) %>%
  mutate(
    distance = as.numeric(distance),
    number_of_attacks_until_2015 = ifelse(is.na(number_of_attacks_until_2015),0,number_of_attacks_until_2015),
    geometry = geometry.x,
    attacks = ifelse(number_of_attacks_until_2015 > 100, 100, number_of_attacks_until_2015)
  ) %>% 
  filter(number_of_attacks_until_2015 > 0) %>%
  select('SEMEL_YISHUV','attacks','geometry') 

ggplot() +
  
  geom_sf(data = gaza$geometry, fill = 'orange', lwd = .1) +
  geom_sf(data = israel$geometry, fill = 'lightgreen', lwd = .1) +
  geom_sf(data = cities_2$geometry, aes(fill = cities_2$attacks, color = cities_2$attacks)) +
  geom_sf(data = buffer_75, fill = NA, color = 'orange', lwd = 1) +
  geom_sf(data = buffer_150, fill = NA, color = "orange", lwd = 1) +
  
  scale_fill_continuous(name = 'Number of Red Alerts:', low = "purple4", high = "red",
                        breaks = c(1,25,50,75,100),
                        labels = c("1",'25',"50",'75',"100+")) +
  scale_color_continuous(low = 'purple4', high = 'red') +
  guides(color = 'none') +
  xlim(min_long, max_long + 0.3) +
  theme_minimal()

ggsave('Output/sirens_until_2015.pdf', width = 8.27, height = 11.69, units = "in", dpi = 300)


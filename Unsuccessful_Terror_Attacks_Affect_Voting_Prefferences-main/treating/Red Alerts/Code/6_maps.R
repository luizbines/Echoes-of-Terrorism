# Luiz Bines
# September 2024
# luizbines@gmail.com
# This script plots the relevant maps

# Library
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(units)
library(patchwork)
library(cowplot)



# Directory
wd = 'C:/Users/luizb/Desktop/Echoes-of-Terrorism/Unsuccessful_Terror_Attacks_Affect_Voting_Prefferences-main/'
setwd(wd);


#### Importing ####

likud_percentage = read.csv('treating/Red Alerts/Output/2_likud_percentage_panel.csv')

gaza_sf = read_sf('raw/Israel/gaza/gaza.shp') %>% 
  summarise(geometry = st_union(geometry))

st_crs(gaza_sf) = 4326

israel = read_sf(dsn = 'raw/Israel/Demographics/statisticalareas_demography2013.gdb/')




#### PREPARING DATA ####
israel = israel %>%
  mutate(
    Religion_Yishuv = ifelse(is.na(Religion_Yishuv),
                             1,
                             Religion_Yishuv)
  ) %>% 
  group_by(SEMEL_YISHUV) %>%
  summarise(
    Pop_Total = sum(Pop_Total, na.rm = T),
    Shape = st_union(Shape),
    Shape_Area = sum(Shape_Area),
    Religion_Yishuv = first(Religion_Yishuv)
  ) 


israel = st_transform(israel, st_crs(gaza_sf))
st_crs(israel) = st_crs(gaza_sf)


israel <- st_make_valid(israel)


distance = st_distance(israel, gaza_sf) %>% 
  set_units("km")

israel = israel %>%
  mutate(
    distance = as.numeric(distance)
  )

rm(distance)

# fixing cases where two cities have the same name but one is not populated
filtered_israel = israel %>%
  group_by(SEMEL_YISHUV) %>%
  filter(Pop_Total > 0)

cities <- merge(likud_percentage %>% filter(year == 2013) %>% 
                       select('semel_map', 'treated', 'loc', 'temporal_group'),
                     filtered_israel %>% as.data.frame(),
                     by.x = 'semel_map',
                     by.y = 'SEMEL_YISHUV',
                     all.x = T,
                     all.y = T
                     )


gaza_sf$temporal_group <- "Gaza Strip"


#### PLOTTING ####

buffer_45 = st_buffer(gaza_sf, dist = 45000)
buffer_75 = st_buffer(gaza_sf, dist = 75000) 
buffer_150 = st_buffer(gaza_sf, dist = 150000) 


min_long = min(st_coordinates(gaza_sf)[, "X"])
max_long = max(st_coordinates(israel)[,'X'])

# ranges map
ggplot() +
  geom_sf(data = israel$Shape, aes(fill = "Israel"), lwd = .1) +
  geom_sf(data = gaza_sf, aes(fill = "Gaza")) +
  geom_sf(data = st_difference(buffer_150, buffer_75),
          fill = "red", alpha = 0.15) +
  geom_sf(data = st_difference(buffer_75, buffer_45),
          fill = "orange", alpha = 0.5) +
  geom_sf(data = st_difference(buffer_45, gaza_sf),
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
  
  theme_minimal() + 
  theme(
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 17)   
  )



ggsave('treating/Red Alerts/Output/Figures/6_israel_ranges.pdf', width = 8.27 , height = 11.69, units = "in", dpi = 300)


# # treatment map
# ggplot() + 
#   geom_sf(data = israel$Shape, color = 'lightgrey')+
#   # geom_sf(data = israel[israel$distance > 75 & israel$distance < 150,]$Shape, fill = 'lightgreen', lwd = .1) +
#   # geom_sf(data = israel[israel$distance < 75,]$Shape, fill = 'lightgrey', lwd = .1) +
#   # geom_sf(data = israel[israel$distance > 150,]$Shape, fill = 'lightgrey', lwd = .1) +
#   # geom_sf(data = israel[st_intersects(israel,buffer_75, sparse = F) %>% unlist,]$Shape,
#   #         color = 'lightgrey') + 
#   geom_sf(data = cities[cities$treated == T,]$Shape, aes(fill = 'Treatment Group', color = 'Treatment Group'), lwd = 0) +
#   geom_sf(data = cities[cities$treated == F,]$Shape, aes(fill = 'Control Group', color = 'Control Group'), lwd = 0) +
#   geom_sf(data = buffer_75, fill = NA, color = 'orange', lwd = 1, linetype = "dashed") +
#   geom_sf(data = buffer_150, fill = NA, color = "orange", lwd = 1, linetype = "dashed") +
#   geom_sf(data = gaza_sf, fill = "orange", lwd = .1) +
#   xlim(min_long, max_long + 0.3) +
#   scale_fill_manual(values = c("Treatment Group" = "red", "Control Group" = "blue"),
#                     guide = guide_legend(title = "Localities:")) +
#   scale_color_manual(values = c('Treatment Group' = 'red', 'Control Group' = 'blue')) +
#   guides(color = 'none') +
#   theme_minimal()
# 
# ggsave('treating/Red Alerts/Output/Figures/israel_attacks.pdf', width = 8.27 , height = 11.69, units = "in", dpi = 300)



# # temporal distance map
# ggplot() + 
#   geom_sf(data = israel$Shape, color = 'white')+
#   # geom_sf(data = israel[israel$distance > 75 & israel$distance < 150,]$Shape, fill = 'lightgreen', lwd = .1) +
#   # geom_sf(data = israel[israel$distance < 75 | israel$distance > 150,]$Shape, fill = 'lightgrey', lwd = .1) +
#   # geom_sf(data = israel[st_intersects(israel,buffer_150, sparse = F) %>% unlist,]$Shape,
#           # color = 'white') +
#   geom_sf(data = cities[!is.na(cities$temporal_group),]$Shape, aes(fill = cities[!is.na(cities$temporal_group),]$temporal_group,
#                                                                       color = cities[!is.na(cities$temporal_group),]$temporal_group), lwd = 0) +
#   geom_sf(data = buffer_75, fill = NA, color = 'orange', lwd = 1, linetype = "dashed") +
#   geom_sf(data = buffer_150, fill = NA, color = "orange", lwd = 1, linetype = "dashed") +
#   geom_sf(data = gaza_sf, fill = "orange", lwd = .1) +
#   xlim(min_long, max_long + 0.3) +
#   guides(color = 'none') +
#   guides(fill = guide_legend(title = "Days between last Red Alert and 2015 election:")) + 
#   scale_fill_manual(values = c("blue", "red",'yellow'),
#                     labels = c("No Red Alerts","6 days", "204-232 days")) +
#   scale_color_manual(values = c('blue', 'red','yellow')) +
#   theme_minimal()
# 
# 
# ggsave('treating/Red Alerts/Output/Figures/israel_temporal_groups.pdf',width = 8.27 , height = 11.69, units = "in", dpi = 300)
# 

# filtering out arab cities
cities = cities %>% 
  filter(Religion_Yishuv!=2)


# temporal distance + ranges map
ggplot() + 
  geom_sf(data = israel$Shape, color = 'white')+
  geom_sf(data = st_difference(buffer_150, buffer_75),
          fill = "purple4", alpha = 0.15,
          color = 'orange', lwd = 1,
          linetype = "dashed") +
  geom_sf(data = cities[!is.na(cities$temporal_group) ,]$Shape,
          aes(fill = cities[!is.na(cities$temporal_group) ,]$temporal_group,
              color = cities[!is.na(cities$temporal_group) ,]$temporal_group), lwd = 0) +
  
  geom_sf(data = gaza_sf, fill = 'orange', color = 'orange', lwd = 0) +
  
  geom_label(aes(x = 35.9, y = 32.25, label = "150 km"), color = "orange", fill = "white", size = 3) +
  geom_label(aes(x = 35.3, y = 31.5, label = "75 km"), color = "orange", fill = "white", size = 3) +
  
  
  xlim(min_long, max_long + 0.3) +
  guides(color = 'none') +
  guides(fill = guide_legend(title = "Days between last Red Alert and 2015 election:")) + 
  scale_fill_manual(values = c("blue",'red','yellow'),
                    labels = c("No Red Alerts","6 days", "149-232 days")) +
  scale_color_manual(values = c('blue','red','yellow')) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() 
  # theme(
  #   legend.title = element_text(size = 17),
  #   legend.text = element_text(size = 17)   
  # )


ggsave('treating/Red Alerts/Output/Figures/6_israel_ranges_temporal_groups.pdf',
       width = 8.27 ,
       height = 11.69,
       units = "in",
       dpi = 300)



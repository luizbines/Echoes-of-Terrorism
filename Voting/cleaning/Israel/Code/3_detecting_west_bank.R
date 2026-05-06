# This code identifies which localities in Israel are within or intersecting the West Bank.

# Library
library(sf)
library(dplyr)

# Get the base path from environment or parent script
if (!exists("base_path")) {
  base_path <- Sys.getenv("R_PROJECT_DIR")
  if (base_path == "") {
    base_path <- getwd()
  }
}
setwd(base_path)


israel = read_sf('raw/Israel/Demographics/statisticalareas_demography2013.gdb/')
israel_panel = read.csv('cleaning/Israel/Output/2_israel_panel_lights.csv')
west_bank = read_sf('raw/Israel/West Bank/gadm41_PSE_1.shp')


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

# Cleaning West Bank shapefile
west_bank <- west_bank %>%
  filter(NAME_1 == 'West Bank') %>%
  st_transform(st_crs(israel))

# Adding negative 200 meter buffer just to avoid false positives due to edge effects in the shapefiles
west_bank = st_buffer(west_bank, dist = -200)
  
# Check for intersection
intersection <- st_intersection(israel, west_bank)
# Check for within
within_west_bank <- st_within(israel, west_bank, sparse = FALSE)
# Check if centroid is within the west bank
centroid_within_west_bank <- st_within(st_centroid(israel), west_bank, sparse = FALSE)

# Check difference between intersection and within
intersection_ids <- israel$SEMEL_YISHUV[st_intersects(
  israel,
  west_bank,
  sparse = FALSE
)]
within_ids <- israel$SEMEL_YISHUV[st_within(israel, west_bank, sparse = FALSE)]
# Localities in Intersection but not Within:
print(israel$SHEM_YISHUV_ENGLISH[
  israel$SEMEL_YISHUV %in% setdiff(intersection_ids, within_ids)
])
   # 40 different localities

# Localities in Within but not Intersection:
print(israel$SHEM_YISHUV_ENGLISH[
  israel$SEMEL_YISHUV %in% setdiff(within_ids, intersection_ids)
])
   # None

# Creating variables for localities within the West Bank or intersecting with the West Bank
israel_panel = israel_panel %>%
  mutate(
    within_west_bank = ifelse(SEMEL_YISHUV %in% within_ids, 1, 0),
    intersecting_west_bank = ifelse(SEMEL_YISHUV %in% intersection_ids, 1, 0),
    centroid_within_west_bank = ifelse(SEMEL_YISHUV %in% israel$SEMEL_YISHUV[centroid_within_west_bank], 1, 0)
  )


# Saving
write.csv(israel_panel, 'cleaning/Israel/Output/3_israel_panel_west_bank.csv', row.names = FALSE)

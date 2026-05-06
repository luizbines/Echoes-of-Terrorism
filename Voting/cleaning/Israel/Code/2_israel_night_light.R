# This script assigns the night light illumination level to each israeli locality
# Luiz Bines
# 2024

# Library
library(terra)
library(dplyr)



# Resolve the Voting root from the environment or by walking upward from the current directory
resolve_voting_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)
  repeat {
    if (dir.exists(file.path(current_dir, "raw")) &&
        dir.exists(file.path(current_dir, "cleaning")) &&
        dir.exists(file.path(current_dir, "treating"))) {
      return(current_dir)
    }
    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      break
    }
    current_dir <- parent_dir
  }
  stop("ERROR: Could not determine Voting directory path from start_dir=", start_dir)
}

base_path <- Sys.getenv("R_MODULE_ROOT")
if (nzchar(base_path) && dir.exists(base_path)) {
  base_path <- normalizePath(base_path, winslash = "/", mustWork = FALSE)
  if (!dir.exists(file.path(base_path, "raw")) ||
      !dir.exists(file.path(base_path, "cleaning")) ||
      !dir.exists(file.path(base_path, "treating"))) {
    base_path <- resolve_voting_root()
  }
} else {
  base_path <- resolve_voting_root()
}
setwd(base_path)


##### IMPORTING #####

light_2006 = rast('raw/Israel/night_lights/Harmonized_DN_NTL_2006_calDMSP.tif')
light_2009 = rast('raw/Israel/night_lights/Harmonized_DN_NTL_2009_calDMSP.tif')
light_2013 = rast('raw/Israel/night_lights/Harmonized_DN_NTL_2013_calDMSP.tif')
light_2015 = rast('raw/Israel/night_lights/Harmonized_DN_NTL_2015_simVIIRS.tif')

israel = vect('raw/Israel/Demographics/statisticalareas_demography2013.gdb/')
israel_panel = read.csv('cleaning/Israel/Output/1_israel_panel.csv')

# cleaning
israel = aggregate(israel, by = "SEMEL_YISHUV")


# Aligning the map's CRS with the satelite CRS
israel <- terra::project(israel, crs(light_2006))

israel <- project(israel, crs(light_2006))

light_crop_2006 <- terra::crop(light_2006, terra::ext(israel))
light_crop_2009 <- terra::crop(light_2009, terra::ext(israel))
light_crop_2013 <- terra::crop(light_2013, terra::ext(israel))
light_crop_2015 <- terra::crop(light_2015, terra::ext(israel))

ntl_city_values_2006 <- terra::extract(light_crop_2006, israel, fun = mean, na.rm = TRUE)
ntl_city_values_2009 <- terra::extract(light_crop_2009, israel, fun = mean, na.rm = TRUE)
ntl_city_values_2013 <- terra::extract(light_crop_2013, israel, fun = mean, na.rm = TRUE)
ntl_city_values_2015 <- terra::extract(light_crop_2015, israel, fun = mean, na.rm = TRUE)

# # Assigning night light values to each observation
# ntl_city_values_2006 <-terra::extract(light_2006, israel, fun = mean, na.rm = TRUE)
# ntl_city_values_2009 <-terra::extract(light_2009, israel, fun = mean, na.rm = TRUE)
# ntl_city_values_2013 <- terra::extract(light_2013, israel, fun = mean, na.rm = TRUE)
# ntl_city_values_2015 <- terra::extract(light_2015, israel, fun = mean, na.rm = TRUE)


israel$ntl_2006 <- ntl_city_values_2006[, 2]
israel$ntl_2009 <- ntl_city_values_2009[, 2]
israel$ntl_2013 <- ntl_city_values_2013[, 2] 
israel$ntl_2015 <- ntl_city_values_2015[, 2]

israel = israel %>%
  as.data.frame() %>%
  select(SEMEL_YISHUV,
         ntl_2006,
         ntl_2009,
         ntl_2013,
         ntl_2015)



##### MERGING #####
israel_panel = israel_panel %>% 
  merge(israel,
        by = 'SEMEL_YISHUV',
        all.x = T,
        all.y = F)


israel_panel <- israel_panel %>%
  mutate(
    ntl = case_when(
      year == 2006 ~ ntl_2006,
      year == 2009 ~ ntl_2009,
      year == 2013 ~ ntl_2013,
      year == 2015 ~ ntl_2015,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-ntl_2006, -ntl_2009, -ntl_2013, -ntl_2015)

write.csv(israel_panel,'cleaning/Israel/Output/2_israel_panel_lights.csv')
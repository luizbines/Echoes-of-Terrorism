# This script assigns the night light illumination level to each israeli locality
# Luiz Bines
# 2024

# Library
library(terra)
library(dplyr)



# Directory
wd = '/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Unsuccessful_Terror_Attacks_Affect_Voting_Prefferences-main/'
setwd(wd);


##### IMPORTING #####

light_2006 = rast('raw/Israel/Harmonized_DN_NTL_2006_calDMSP.tif')
light_2009 = rast('raw/Israel/Harmonized_DN_NTL_2009_calDMSP.tif')
light_2013 = rast('raw/Israel/Harmonized_DN_NTL_2013_calDMSP.tif')
light_2015 = rast('raw/Israel/Harmonized_DN_NTL_2015_simVIIRS.tif')

israel = vect('raw/Israel/Demographics/statisticalareas_demography2013.gdb/')
israel_panel = read.csv('cleaning/Israel/Output/1_israel_panel.csv')

# cleaning
israel = aggregate(israel, by = "SEMEL_YISHUV")




# Aligning the map's CRS with the satelite CRS
israel <- terra::project(israel, crs(light_2006))


# Assigning night light values to each observation
ntl_city_values_2006 <-terra::extract(light_2006, israel, fun = mean, na.rm = TRUE)
ntl_city_values_2009 <-terra::extract(light_2009, israel, fun = mean, na.rm = TRUE)
ntl_city_values_2013 <- terra::extract(light_2013, israel, fun = mean, na.rm = TRUE)
ntl_city_values_2015 <- terra::extract(light_2015, israel, fun = mean, na.rm = TRUE)


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

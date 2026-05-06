# LUIZ BINES
# luizbines@gmail.com
# March, 2024
# This code creates a panel of israel demographics for the years 2006, 2008, 2009 and
# 2011 to 2022
# Additionally, it adds the socio-economic index to the panel, which is used as a control variable in the analysis.


# Library
library(sf)
library(dplyr)
library(readxl)
library(readxl)

# Get the base path from environment or parent script
if (!exists("base_path")) {
  base_path <- Sys.getenv("R_PROJECT_DIR")
  if (base_path == "") {
    base_path <- getwd()
  }
}
setwd(base_path)


rm(list = ls())


##### IMPORTING #####

# Demographic variables

israel_2008 = st_read("raw/Israel/Demographics/statisticalareas_demography2008.gdb") %>%
  as.data.frame() %>%
  group_by(SEMEL_YISHUV) %>%
  summarise(
    SEMEL_YISHUV = unique(SEMEL_YISHUV),
    SHEM_YISHUV = unique(Shem_Yishuv),
    SHEM_YISHUV_ENGLISH = unique(Shem_Yishuv_English),
    Religion_yishuv_Code = unique(Religion_Yishuv_Code),
    Religion_yishuv_Txt = unique(Religion_Yishuv_Txt),
    Pop_Total = sum(Pop_Total, na.rm = T),
    Male_Total = sum(Male_Total, na.rm = T),
    Female_Total = sum(Female_Total, na.rm = T),
    age_0_14 = sum(age_0_14, na.rm = T),
    age_15_19 = sum(age_15_19, na.rm = T),
    age_20_29 = sum(age_20_29, na.rm = T),
    age_30_64 = sum(age_30_64, na.rm = T),
    age_65_up = sum(age_65_up, na.rm = T),
  ) %>% 
  mutate(
    year = 2008
  )



  
  

israel_2011 = st_read("raw/Israel/Demographics/statisticalareas_demography2011.gdb") %>%
  as.data.frame() %>%
  group_by(SEMEL_YISHUV) %>%
  summarise(
    SEMEL_YISHUV = unique(SEMEL_YISHUV),
    SHEM_YISHUV = unique(Shem_Yishuv),
    SHEM_YISHUV_ENGLISH = unique(Shem_Yishuv_English),
    Religion_yishuv_Code = unique(Religion_yishuv_code),
    Religion_yishuv_Txt = unique(Religion_Yishuv_Txt),
    Pop_Total = sum(Pop_Total, na.rm = T),
    Male_Total = sum(Male_Total, na.rm = T),
    Female_Total = sum(Female_Total, na.rm = T),
    age_0_14 = sum(age_0_14, na.rm = T),
    age_15_19 = sum(age_15_19, na.rm = T),
    age_20_29 = sum(age_20_29, na.rm = T),
    age_30_64 = sum(age_30_64, na.rm = T),
    age_65_up = sum(age_65_up, na.rm = T),
  ) %>% 
  mutate(
    year = 2011
  )


israel_2012 = st_read("raw/Israel/Demographics/statisticalareas_demography2012.gdb") %>%
  as.data.frame() %>%
  group_by(SEMEL_YISHUV) %>%
  summarise(
    SEMEL_YISHUV = unique(SEMEL_YISHUV),
    SHEM_YISHUV = unique(SHEM_YISHUV),
    SHEM_YISHUV_ENGLISH = unique(SHEM_YISHUV_ENGLISH),
    Religion_yishuv_Code = unique(Religion_Yishuv),
    Religion_yishuv_Txt = unique(Religion_Txt),
    Pop_Total = sum(Pop_Total, na.rm = T),
    Male_Total = sum(Male_Total, na.rm = T),
    Female_Total = sum(Female_Total, na.rm = T),
    age_0_14 = sum(age_0_14, na.rm = T),
    age_15_19 = sum(age_15_19, na.rm = T),
    age_20_29 = sum(age_20_29, na.rm = T),
    age_30_64 = sum(age_30_64, na.rm = T),
    age_65_up = sum(age_65_up, na.rm = T),
  ) %>% 
  mutate(
    year = 2012
  )

israel_2013 = st_read("raw/Israel/Demographics/statisticalareas_demography2013.gdb") %>%
  as.data.frame() %>%
  group_by(SEMEL_YISHUV) %>%
  summarise(
    SEMEL_YISHUV = unique(SEMEL_YISHUV),
    SHEM_YISHUV = unique(SHEM_YISHUV),
    SHEM_YISHUV_ENGLISH = unique(SHEM_YISHUV_ENGLISH),
    Religion_yishuv_Code = unique(Religion_Yishuv),
    Religion_yishuv_Txt = unique(Religion_Txt),
    Pop_Total = sum(Pop_Total, na.rm = T),
    Male_Total = sum(Male_Total, na.rm = T),
    Female_Total = sum(Female_Total, na.rm = T),
    age_0_14 = sum(age_0_14, na.rm = T),
    age_15_19 = sum(age_15_19, na.rm = T),
    age_20_29 = sum(age_20_29, na.rm = T),
    age_30_64 = sum(age_30_64, na.rm = T),
    age_65_up = sum(age_65_up, na.rm = T),
    Shape_Area = sum(Shape_Area)
  ) %>% 
  mutate(
    year = 2013
  )


israel_2014 = st_read("raw/Israel/Demographics/statisticalareas_demography2014.gdb") %>%
  as.data.frame() %>%
  group_by(SEMEL_YISHUV) %>%
  summarise(
    SEMEL_YISHUV = unique(SEMEL_YISHUV),
    SHEM_YISHUV = unique(SHEM_YISHUV),
    SHEM_YISHUV_ENGLISH = unique(SHEM_YISHUV_ENGLISH),
    Religion_yishuv_Code = unique(Religion_Yishuv),
    Religion_yishuv_Txt = unique(Religion_Txt),
    Pop_Total = sum(Pop_Total, na.rm = T),
    Male_Total = sum(Male_Total, na.rm = T),
    Female_Total = sum(Female_Total, na.rm = T),
    age_0_14 = sum(age_0_14, na.rm = T),
    age_15_19 = sum(age_15_19, na.rm = T),
    age_20_29 = sum(age_20_29, na.rm = T),
    age_30_64 = sum(age_30_64, na.rm = T),
    age_65_up = sum(age_65_up, na.rm = T),
  ) %>% 
  mutate(
    year = 2014
  )

israel_2015 = st_read("raw/Israel/Demographics/statisticalareas_demography2015.gdb") %>%
  as.data.frame() %>%
  group_by(SEMEL_YISHUV) %>%
  summarise(
    SEMEL_YISHUV = unique(SEMEL_YISHUV),
    SHEM_YISHUV = unique(Shem_Yishuv),
    SHEM_YISHUV_ENGLISH = unique(SHEM_Yishuv_English),
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
  ) %>% 
  mutate(
    year = 2015
  )


israel_2016 = st_read("raw/Israel/Demographics/statisticalareas_demography2016.gdb") %>%
  as.data.frame() %>%
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
  ) %>% 
  mutate(
    year = 2016
  )


israel_2017 = st_read("raw/Israel/Demographics/statisticalareas_demography2017.gdb") %>%
  as.data.frame() %>%
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
  ) %>% 
  mutate(
    year = 2017
  )


israel_2018 = st_read("raw/Israel/Demographics/statisticalareas_demography2018.gdb") %>%
  as.data.frame() %>%
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
    age_0_14 = sum(age_0_4 + age_5_9 + age_10_14, na.rm = T),
    age_15_19 = sum(age_15_19, na.rm = T),
    age_20_29 = sum(age_20_24 + age_25_29, na.rm = T),
    age_30_64 = sum(age_30_34 + age_35_39 + age_40_44 + age_45_49 + age_50_54 +
                      age_55_59 + age_60_64, na.rm = T),
    age_65_up = sum(age_65_69 + age_70_74 + age_75_up, na.rm = T),
  ) %>% 
  mutate(
    year = 2018
  )


israel_2019 = st_read("raw/Israel/Demographics/statisticalareas_demography2019.gdb") %>%
  as.data.frame() %>%
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
    age_0_14 = sum(age_0_4 + age_5_9 + age_10_14, na.rm = T),
    age_15_19 = sum(age_15_19, na.rm = T),
    age_20_29 = sum(age_20_24 + age_25_29, na.rm = T),
    age_30_64 = sum(age_30_34 + age_35_39 + age_40_44 + age_45_49 + age_50_54 +
                      age_55_59 + age_60_64, na.rm = T),
    age_65_up = sum(age_65_69 + age_70_74 + age_75_79 + age_80_84 + age_85_up, na.rm = T),
  ) %>% 
  mutate(
    year = 2019
  )

israel_2020 = st_read("raw/Israel/Demographics/statisticalareas_demography2020.gdb") %>%
  as.data.frame() %>%
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
    age_0_14 = sum(age_0_4 + age_5_9 + age_10_14, na.rm = T),
    age_15_19 = sum(age_15_19, na.rm = T),
    age_20_29 = sum(age_20_24 + age_25_29, na.rm = T),
    age_30_64 = sum(age_30_34 + age_35_39 + age_40_44 + age_45_49 + age_50_54 +
                      age_55_59 + age_60_64, na.rm = T),
    age_65_up = sum(age_65_69 + age_70_74 + age_75_79 + age_80_84 + age_85_up, na.rm = T),
  ) %>% 
  mutate(
    year = 2020
  )

israel_2021 = st_read("raw/Israel/Demographics/statisticalareas_demography2021.gdb") %>%
  as.data.frame() %>%
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
    age_0_14 = sum(age_0_4 + age_5_9 + age_10_14, na.rm = T),
    age_15_19 = sum(age_15_19, na.rm = T),
    age_20_29 = sum(age_20_24 + age_25_29, na.rm = T),
    age_30_64 = sum(age_30_34 + age_35_39 + age_40_44 + age_45_49 + age_50_54 +
                      age_55_59 + age_60_64, na.rm = T),
    age_65_up = sum(age_65_69 + age_70_74 + age_75_79 + age_80_84 + age_85_up, na.rm = T),
  ) %>% 
  mutate(
    year = 2021
  )

israel_2022 = st_read("raw/Israel/Demographics/statisticalareas_demography2022.gdb") %>%
  as.data.frame() %>%
  group_by(SEMEL_YESHUV) %>%
  summarise(
    SEMEL_YISHUV = unique(SEMEL_YESHUV),
    SHEM_YISHUV = unique(Shem_Yishuv),
    SHEM_YISHUV_ENGLISH = unique(Shem_Yishuv_English),
    Religion_yishuv_Code = unique(Religion_yishuv_code),
    Religion_yishuv_Txt = unique(Religion_Yishuv_Txt),
    Pop_Total = sum(Pop_Total, na.rm = T),
    Male_Total = sum(Male_Total, na.rm = T),
    Female_Total = sum(Female_Total, na.rm = T),
    age_0_14 = sum(age_0_4 + age_5_9 + age_10_14, na.rm = T),
    age_15_19 = sum(age_15_19, na.rm = T),
    age_20_29 = sum(age_20_24 + age_25_29, na.rm = T),
    age_30_64 = sum(age_30_34 + age_35_39 + age_40_44 + age_45_49 + age_50_54 +
                      age_55_59 + age_60_64, na.rm = T),
    age_65_up = sum(age_65_69 + age_70_74 + age_75_79 + age_80_84 + age_85_up, na.rm = T),
  ) %>% 
  mutate(
    year = 2022
  ) %>%
  select(-1)



israel_list_names = ls() 
israel_list = mget(israel_list_names) 



###### MERGING ######

# Loop for each df
for(i in seq_along(israel_list)) {
  # Merge
  israel_list[[i]] = merge(israel_list[[i]],
                           israel_2013[, c("SEMEL_YISHUV", "Shape_Area")], 
                           by = "SEMEL_YISHUV", 
                           all.x = TRUE)
}

# New dfs
list2env(israel_list, envir = .GlobalEnv)

israel_2013 = israel_2013[,-16] %>%
  rename(Shape_Area = Shape_Area.x)


israel = data.frame()

for (df_name in israel_list_names) {
  df = get(df_name)
  israel = rbind(israel, df)
  rm(df)
}

israel = israel %>%
  mutate(
    Pop_Total = ifelse(year == 2008 | year == 2011, 
                       Pop_Total * 1000, Pop_Total),
    Shape_Area = Shape_Area/1000000,
    density = Pop_Total/Shape_Area,
    age_65_up_percentage = age_65_up/Pop_Total
  )


# Adding missing files (different format)
  
  # 2006
israel_2006 = read_xls('raw/Israel/Demographics/2006_data.xls') %>%
  rename(
    SEMEL_YISHUV = 'סמל יישוב',
    SHEM_YISHUV = 'שם יישוב',
    SHEM_YISHUV_ENGLISH = 'שם יישוב אנגלית',
    Religion_yishuv_Code = 'דת היישוב',
    Pop_Total = 'סה\"כ  אוכלוסייה'
  ) %>% 
  mutate(
    year = 2006,
    Pop_Total = Pop_Total %>% as.numeric()
  ) %>% 
  select(
    SEMEL_YISHUV,
    SHEM_YISHUV,
    SHEM_YISHUV_ENGLISH,
    Religion_yishuv_Code,
    Pop_Total,
    year
  )

  # 2009
israel_2009 = read_xls('raw/Israel/Demographics/2009_data.xls') %>%
  rename(
    SEMEL_YISHUV = 'סמל יישוב',
    SHEM_YISHUV = 'שם יישוב',
    SHEM_YISHUV_ENGLISH = 'שם יישוב אנגלית',
    Religion_yishuv_Code = 'דת היישוב',
    Pop_Total = 'סך הכל אוכלוסייה 2009'
  ) %>% 
  mutate(
    year = 2009
  ) %>% 
  select(
    SEMEL_YISHUV,
    SHEM_YISHUV,
    SHEM_YISHUV_ENGLISH,
    Religion_yishuv_Code,
    Pop_Total,
    year
  )


# Merging

israel = israel %>% 
  bind_rows(israel_2006) %>% 
  bind_rows(israel_2009)

# Adding missing variables
israel = israel %>% 
  group_by(SEMEL_YISHUV) %>%
  # Adding Shape Area to 2006 and 2009
  mutate(
    Shape_Area = if_else(is.na(Shape_Area), first(na.omit(Shape_Area)), Shape_Area)
    ) %>%
  ungroup() %>% 
  mutate(
    density = Pop_Total/Shape_Area
  )



### Adding socioeconomic index
# Importing
sci <- read_xlsx(
  "raw/Israel/socioeconomic_index.xlsx", 
  col_names = c("SEMEL_YISHUV", "locality_name", "locality_type", 
                "population", "sci_index_value", "sci_index_cluster")
) %>% 
  na.omit() %>% 
  # eliminating 1st row (variable names)
  slice(-1) %>%
  # locality number as integer
  mutate(SEMEL_YISHUV = as.integer(SEMEL_YISHUV),
          sci_index_value = as.numeric(sci_index_value),
          sci_index_cluster = as.integer(sci_index_cluster)
) %>%
  select(SEMEL_YISHUV,sci_index_value,sci_index_cluster)

# Merging wit israel dataframe
israel = merge(israel, sci, by = "SEMEL_YISHUV", all.x = TRUE)

write.csv(israel, file = 'cleaning/Israel/Output/1_israel_panel.csv', row.names = F)


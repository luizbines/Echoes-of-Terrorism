# THIS SCRIPT CALCULATES LIKUD'S PERCENTAGE IN EACH ELECTION
# LUIZ BINES - luizbines@gmail.com
# 2023

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

# Directory
wd = 'C:/Users/luizb/Desktop/Dissertation/Red-Alerts-and-Votes/'
setwd(wd);

##### IMPORTING #####

# Importing Elections datasets

elections_2006 = read_xls('raw/Elections/Result17.xls') %>%  select(c(-1,-3)) 
elections_2009 = read_xls('raw/Elections/results_18.xls')
elections_2013 = read_xlsx('raw/Elections/expc_19.xlsx') 
elections_2015 = read_xlsx('raw/Elections/expc_20.xlsx')
elections_2019 = read_xlsx('raw/Elections/expc_21.xlsx')
elections_2019_2 = read_xlsx('raw/Elections/expc_22.xlsx')
elections_2020 = read_xlsx('raw/Elections/expc_23.xlsx')
elections_2021 = read_xlsx('raw/Elections/expc_24.xlsx')
elections_2022 = read_xlsx('raw/Elections/expc_25.xlsx')




##### CLEANING #####
  
elections_2006 = elections_2006 %>%
  # summing votes per city
  group_by(`שם ישוב`) %>%
  summarise(
    across(
      setdiff(names(elections_2006),
              "שם ישוב"),
      sum),
    "שם ישוב" = first(`שם ישוב`)
  ) %>%
  # creating likud's 2006 percentage in each city
  mutate('2006' = מחל / כשרים) 



elections_2009 = elections_2009 %>%
  group_by(`סמל ישוב`) %>%
  summarise(
    across(
      setdiff(names(elections_2009),
              c("סמל ישוב", "שם ישוב", "ת. עדכון")),
      sum),
    "סמל ישוב" = first(`סמל ישוב` ),
    "שם ישוב" = first(`שם ישוב`),
    "ת. עדכון" = first(`ת. עדכון`)
  ) %>%
  mutate('2009' = מחל / כשרים)

elections_2013 = elections_2013 %>%
  mutate('2013' = מחל / כשרים)

elections_2015 = elections_2015 %>%
  mutate('2015' = מחל / כשרים)

elections_2019 = elections_2019 %>%
  mutate('2019' = מחל / כשרים)

elections_2019_2 = elections_2019_2 %>%
  mutate('2019_2' = מחל / כשרים)

elections_2020 = elections_2020 %>%
  mutate('2020' = מחל / כשרים)

elections_2021 = elections_2021 %>%
  mutate('2021' = מחל / כשרים)

elections_2022 = elections_2022 %>%
  mutate('2022' = מחל / כשרים)


elections_list = list(elections_2006,
                      elections_2009,
                      elections_2013,
                      elections_2015,
                      elections_2019,
                      elections_2019_2,
                      elections_2020,
                      elections_2021,
                      elections_2022)




##### MERGING #####

likud_percentage = 
  # 2013 and 2015 are the elections of interest
  # so we want to keep all cities that appear in both datasets
    # 2013
  elections_list[[3]] %>%
    # 2015
  merge(elections_list[[4]],
        by = 'סמל ישוב',
        all.x = F,
        all.y = F)
  
  
  
# merging elections besides 2013 and 2015

likud_other_elections = 
  # 2006 dataset has no city codes, so we merge by names
  elections_list[[1]] %>%
  merge(elections_list[[2]],
        by = 'שם ישוב',
        # keeping only 2006 cities that have a match 
        # (12 out of 1150 are dropped)
        all.x = F,
        all.y = T)

  # merging with the rest of the elections (2019 to 2022) 
for(df in elections_list[c(-1,-2,-3,-4)]) {
  likud_other_elections = merge(likud_other_elections,
                                df,
                                by = 'סמל ישוב',
                                all.x = T,
                                all.y = T)
}



# Merging all elections, keeping only cities that appear in the 2013 and 2015 elections
likud_percentage = likud_percentage %>%
  merge(likud_other_elections,
        by = 'סמל ישוב',
        all.x = T,
        all.y = F) %>% 
  select('סמל ישוב'
         ,'שם ישוב.x.x',
         '2006',
         '2009',
         '2013',
         '2015',
         '2019',
         '2019_2',
         '2020',
         '2021',
         '2022') %>%
  rename(
    SEMEL_YISHUV = 1,
    locality = 2
  ) %>%
  mutate(
    loc = gsub("[^א-ת]", "",
               locality,
               perl = TRUE)
  )


##### EXPORTING #####
write.csv(likud_percentage, file = 'cleaning/Elections/Output/likud_percentage.csv', row.names = F) 

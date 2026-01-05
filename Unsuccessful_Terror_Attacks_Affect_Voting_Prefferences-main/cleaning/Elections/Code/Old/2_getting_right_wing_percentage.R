# THIS SCRIPT CALCULATES THE RIGHT WING PERCENTAGE IN EACH ELECTION
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
  # adding up votes per city
  group_by(`שם ישוב`) %>%
  summarise(
    across(
      setdiff(names(elections_2006),
              "שם ישוב"),
      sum),
    "שם ישוב" = first(`שם ישוב`)
  ) %>%
  rename(
    # likud = מחל,
    shas = שס,
    israel_beiteinu = ל,
    ichud_leumi_mafdal = טב,
    iachadut_hatora = ג,
    total_votes = כשרים
  ) %>% 
  mutate(
    right_wing_percentage = 
      # likud +
      shas + israel_beiteinu +
      ichud_leumi_mafdal + iachadut_hatora,
    # creating right_wing's 2006 percentage in each city
    '2006' = right_wing_percentage/total_votes
  ) 
  


elections_2009 = elections_2009 %>%
  # adding up votes per city
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
  rename(
    # likud = מחל,
    shas = שס,
    israel_beiteinu = ל,
    iachadut_hatora = ג,
    habait_hayehudi = ב,
    haichud_haleumi = ט,
    total_votes = כשרים
  ) %>%  
  mutate(
    right_wing_percentage = 
      # likud +
      shas + israel_beiteinu +
      iachadut_hatora + habait_hayehudi + haichud_haleumi,
    # creating right_wing's 2009 percentage in each city
    '2009' = right_wing_percentage/total_votes
  ) 
  
  
  
elections_2013 = elections_2013 %>%
  rename(
    # likud_israel_beiteinu = מחל,
    shas = שס,
    habait_hayehudi = טב,
    iachadut_hatora = ג,
    total_votes = כשרים
  ) %>%  
  mutate(
    right_wing_percentage = 
      # likud_israel_beiteinu +
      shas +
      iachadut_hatora + habait_hayehudi,
    # creating right_wing's 2013 percentage in each city
    '2013' = right_wing_percentage/total_votes
  ) 


elections_2015 = elections_2015 %>%
  rename(
    # likud = מחל,
    shas = שס,
    habait_hayehudi = טב,
    iachadut_hatora = ג,
    israel_beiteinu = ל,
    total_votes = כשרים
  ) %>%  
  mutate(
    right_wing_percentage = 
      # likud +
      shas +
      habait_hayehudi + iachadut_hatora + israel_beiteinu,
    # creating right_wing's 2015 percentage in each city
    '2015' = right_wing_percentage/total_votes
  ) 



elections_2019 = elections_2019 %>%
  rename(
    # likud = מחל,
    shas = שס,
    iachadut_hatora = ג,
    israel_beiteinu = ל,
    union_right_wing = טב,
    new_right = נ,
    total_votes = כשרים
  ) %>%  
  mutate(
    right_wing_percentage = 
      # likud +
      shas +
      iachadut_hatora + israel_beiteinu +
      union_right_wing + new_right,
    # creating right_wing's 2019 percentage in each city
    '2019' = right_wing_percentage/total_votes
  ) 




elections_2019_2 = elections_2019_2 %>%
  rename(
    # likud = מחל,
    shas = שס,
    iachadut_hatora = ג,
    israel_beiteinu = ל,
    iamina = טב,
    total_votes = כשרים
  ) %>%  
  mutate(
    right_wing_percentage = 
      # likud + 
      shas +
      iachadut_hatora + israel_beiteinu +
      iamina,
    # creating right_wing's 2019_2 percentage in each city
    '2019_2' = right_wing_percentage/total_votes
  ) 



elections_2020 = elections_2020 %>%
  rename(
    # likud = מחל,
    shas = שס,
    iachadut_hatora = ג,
    israel_beiteinu = ל,
    iamina = טב,
    total_votes = כשרים
  ) %>%  
  mutate(
    right_wing_percentage = 
      # likud +
      shas +
      iachadut_hatora + israel_beiteinu +
      iamina,
    # creating right_wing's 2020 percentage in each city
    '2020' = right_wing_percentage/total_votes
  ) 



elections_2021 = elections_2021 %>%
  rename(
    # likud = מחל,
    shas = שס,
    iachadut_hatora = ג,
    israel_beiteinu = ל,
    iamina = ב,
    tzionut_hadatit = ט,
    total_votes = כשרים
  ) %>%  
  mutate(
    right_wing_percentage =
      # likud +
      shas +
      iachadut_hatora + israel_beiteinu +
      iamina + tzionut_hadatit,
    # creating right_wing's 2021 percentage in each city
    '2021' = right_wing_percentage/total_votes
  ) 



elections_2022 = elections_2022 %>%
  rename(
    # likud = מחל,
    shas = שס,
    iachadut_hatora = ג,
    israel_beiteinu = ל,
    # habait_hayehudit = ב,
    tzionut_hadatit_otzma_yehudit = ט,
    total_votes = כשרים
  ) %>%  
  mutate(
    right_wing_percentage = 
      # likud +
      shas +
      iachadut_hatora + israel_beiteinu + 
      tzionut_hadatit_otzma_yehudit,
    # creating right_wing's 2022 percentage in each city
    '2022' = right_wing_percentage/total_votes
  ) 


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

right_wing_percentage = 
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

right_wing_other_elections = 
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
  right_wing_other_elections = merge(right_wing_other_elections,
                                df,
                                by = 'סמל ישוב',
                                all.x = T,
                                all.y = T)
}



# Merging all elections, keeping only cities that appear in the 2013 and 2015 elections
right_wing_percentage = right_wing_percentage %>%
  merge(right_wing_other_elections,
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
write.csv(right_wing_percentage, file = 'cleaning/Elections/Output/right_wing_percentage.csv', row.names = F) 

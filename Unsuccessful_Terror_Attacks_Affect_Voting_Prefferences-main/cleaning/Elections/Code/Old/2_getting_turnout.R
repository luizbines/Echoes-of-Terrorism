# THIS SCRIPT CALCULATES THE RIGHT WING PERCENTAGE IN EACH ELECTION
# LUIZ BINES - luizbines@gmail.com
# 2023

# Library
library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
library(lubridate)

# Directory
wd = '/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Unsuccessful_Terror_Attacks_Affect_Voting_Prefferences-main/'
setwd(wd);

##### IMPORTING #####

# Importing Elections datasets

elections_2006 = read_xls('raw/Elections/Result17.xls')  
elections_2009 = read_xls('raw/Elections/results_18.xls')
elections_2013 = read_xlsx('raw/Elections/expc_19.xlsx') 
elections_2015 = read_xlsx('raw/Elections/expc_20.xlsx')
elections_2019 = read_xlsx('raw/Elections/expc_21.xlsx')
elections_2019_2 = read_xlsx('raw/Elections/expc_22.xlsx')
elections_2020 = read_xlsx('raw/Elections/expc_23.xlsx')
elections_2021 = read_xlsx('raw/Elections/expc_24.xlsx')
elections_2022 = read_xlsx('raw/Elections/expc_25.xlsx')


# Importing israel demographic panel
israel_panel = read.csv('cleaning/Israel/Output/2_israel_panel_lights.csv')


##### CLEANING #####


elections_2009 = elections_2009 %>%
  group_by(`סמל ישוב`) %>%
  summarise(
    across(
      setdiff(names(elections_2009),
              c("סמל ישוב", "שם ישוב", "ת. עדכון")),
      sum),
    "סמל ישוב" = first(`סמל ישוב` ),
    "שם ישוב" = first(`שם ישוב`),
    "ת. עדכון" = first(`ת. עדכון`),
    number_of_voters = sum(מצביעים),
    registered_voters = sum(`בז''ב`),
    SEMEL_YISHUV = first(`סמל ישוב`),
    SHEM_YISHUV = first(`שם ישוב`)
  ) %>%
  mutate('2009' = number_of_voters/registered_voters)



elections_2013 = elections_2013 %>%
  rename(
    number_of_voters = מצביעים,
    registered_voters = בזב
  ) %>%  
  mutate('2013' = number_of_voters/registered_voters,
         SEMEL_YISHUV = `סמל ישוב`)



elections_2015 = elections_2015 %>%
  rename(
    number_of_voters = מצביעים,
    registered_voters = בזב
  ) %>%  
  mutate('2015' = number_of_voters/registered_voters,
         SEMEL_YISHUV = `סמל ישוב`)



elections_2019 = elections_2019 %>%
  rename(
    number_of_voters = מצביעים,
    registered_voters = בזב
  ) %>%  
  mutate('2019' = number_of_voters/registered_voters)




elections_2019_2 = elections_2019_2 %>%
  rename(
    number_of_voters = מצביעים,
    registered_voters = בזב
  ) %>%  
  mutate('2019_2' = number_of_voters/registered_voters)



elections_2020 = elections_2020 %>%
  rename(
    number_of_voters = מצביעים,
    registered_voters = בזב
  ) %>%  
  mutate('2020' = number_of_voters/registered_voters)



elections_2021 = elections_2021 %>%
  rename(
    number_of_voters = מצביעים,
    registered_voters = בזב
  ) %>%  
  mutate('2021' = number_of_voters/registered_voters)



elections_2022 = elections_2022 %>%
  rename(
    number_of_voters = מצביעים,
    registered_voters = בזב
  ) %>%  
  mutate('2022' = number_of_voters/registered_voters)




# Creating elections dataframe list (excluding 2006)
elections_list = list(
  elections_2009,
  elections_2013,
  elections_2015,
  elections_2019,
  elections_2019_2,
  elections_2020,
  elections_2021,
  elections_2022)




##### MERGING #####

# Merging elections dataframes

# 2013 and 2015
turnout_percentage = 
  # 2013 and 2015 are the elections of interest
  # so we want to keep all cities that appear in both datasets
  # 2013
  elections_list[[2]] %>%
  # 2015
  merge(elections_list[[3]],
        by = 'סמל ישוב',
        all.x = F,
        all.y = F)



# Remaining elections
turnout_other_elections =
  # 2009
  elections_list[[1]]
  # 2019, 2020, 2021,2022
for(df in elections_list[c(-1,-2,-3)]) {
  turnout_other_elections = merge(turnout_other_elections,
                                  df,
                                  by = 'סמל ישוב',
                                  all.x = T,
                                  all.y = T)
}



# Merging all elections, keeping only cities that appear in the 2013 and 2015 elections
turnout_percentage = turnout_percentage %>%
  merge(turnout_other_elections,
        by = 'סמל ישוב',
        all.x = T,
        all.y = F) %>% 
  select('סמל ישוב'
         ,'שם ישוב.x.x',
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
  ) 



# Adding 2006 election:
# 2006 dataset is a special case where they don't inform number of potential voters
# Therefore, we estimate turnout using the median proportion of registered_voters/population

# cleaning 2006
elections_2006 <- elections_2006 %>%
  group_by(`שם ישוב`) %>%
  summarise(
    across(
      where(is.numeric),  
      sum, na.rm = TRUE
    ),
    SHEM_YISHUV = first(`שם ישוב`)
  ) %>% 
  rename(
    number_of_voters = מצביעים
  )

# Merging with 2009 dataset to obtain locality code (SEMEL_YISHUV)
elections_2006 = elections_2006 %>% 
  merge(
    elections_2009 %>% select(SEMEL_YISHUV, SHEM_YISHUV),
    all.x = T,
    all.y = F,
    by = 'SHEM_YISHUV'
  ) %>% 
  mutate(
    'סמל ישוב' = SEMEL_YISHUV
  )

# Merging with israel_panel to obtain total population for each locality
elections_2006 = elections_2006 %>% 
  merge(
    israel_panel %>% filter(year == 2006) %>% select(SEMEL_YISHUV,Pop_Total),
    all.x = T,
    all.y = F,
    by = 'SEMEL_YISHUV'
  )


# Calculate the percentage of registered voters to Pop_Total for each year
# Using only 2009, 2013 and 2015 (years used in the paper analysis)
for (i in 1:3) {
  elections_list[[i]] <- elections_list[[i]] %>%
    # Merging with israel_panel to obtain Pop_Total
    merge(
      israel_panel %>%
        select(SEMEL_YISHUV, year, Pop_Total) %>%
        filter(
          year == ifelse(i == 1, 2009, ifelse(i == 2, 2013, 2015)) # set year based on i
        ),
      by = 'SEMEL_YISHUV',
      all.x = TRUE,
      all.y = FALSE
    ) %>%
    mutate(
      registered_percent = registered_voters / Pop_Total # registered voters relative to total population
    ) %>%
    select(-year, -Pop_Total) # remove extra columns after calculation
}


# Calculate the median registered percentage for each 'SEMEL_YISHUV' to estimate 2006 turnout
median_registered = elections_list[1:3] %>%
  bind_rows() %>% # combine all election datasets into a single dataframe
  group_by(SEMEL_YISHUV) %>%
  summarise(
    median_registered_percent = median(registered_percent, na.rm = TRUE) # median registered percentage
  )


# Apply the calculated median for 2006
elections_2006 <- elections_2006 %>%
  left_join(median_registered, by = "SEMEL_YISHUV") %>%
  mutate(
    # creating turnout percentage based on estimated statistics
      # estimated registration percentage = median of registered percentage in 2009, 2013 and 2015 at the same locality
    estimated_registration_percent = median_registered_percent,
      # estimated registered voters at each locality in 2006
    estimated_registered_voters = estimated_registration_percent*Pop_Total,
      # estimated turnout at each locality in 2006
    # turnout cant be higher than 1
    '2006' = ifelse(number_of_voters / estimated_registered_voters <= 1,
                    number_of_voters / estimated_registered_voters,
                    1),
  ) %>% 
  select('2006', SEMEL_YISHUV)




# Merging 2006 with turnout_percentage dataset
turnout_percentage <- turnout_percentage %>%
  merge(
    elections_2006,
    by = "SEMEL_YISHUV",
    all.x = T,
    all.y = F
  ) %>%
  # creating simplified location name
  mutate(
    loc = gsub("[^א-ת]", "",
               locality,
               perl = TRUE)
  ) %>% 
  # Reordering columns in the desired chronological order
  select(SEMEL_YISHUV, locality, loc, `2006`, `2009`, `2013`, `2015`, `2019`, `2019_2`, `2020`, `2021`, `2022`)




##### EXPORTING #####
write.csv(turnout_percentage, file = 'cleaning/Elections/Output/turnout_percentage.csv', row.names = F) 

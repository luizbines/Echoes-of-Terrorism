# This file runs the main regressions and plots relevant graphs
# Luiz Bines
# January 2025
# luizbines@gmail.com

# Library
library(dplyr)
library(sf)
library(tidyr)
library(data.table)
library(fixest)
library(ggplot2)
library(purrr)
library(modelsummary)

# Directory
wd <- 'C:/Users/luizb/Desktop/Echoes-of-Terrorism/'
setwd(wd);


# Importing
trends <-  read.csv('Trends/cleaning/Red Alerts/output/trends_alerts_data.csv', 
                    fileEncoding = "UTF-8") %>% 
  mutate(across(-c(District), as.numeric))

# Adding lags 
trends <- trends %>%
  group_by(District) %>%  
  arrange(year, month) %>%  
  mutate(
    lag_alert_1 = lag(alert, 1),
    lag_alert_2 = lag(alert, 2),
    
    # to avoid losing observations, we assume (correctly) that there were no alerts
    # on the 2 previous days before collection
    lag_alert_1 = ifelse(is.na(lag_alert_1), 0, lag_alert_1),
    lag_alert_2 = ifelse(is.na(lag_alert_2), 0, lag_alert_2)
  ) %>% 
  ungroup() %>% 
  arrange(District) %>% 
  mutate(
    # setting date as first day of each month
    date = as.Date(paste(year, month, "01", sep = "-"))
  ) %>% 
  # Translating vars
  rename(
    peace = שלום,
    war = מלחמה,
    siren = אזעקה,
    terrorism = טרור,
    ceasefire = הפסקת.אש,
    hamas = חמאס,
    elections = בחירות,
    likud = ליכוד,
    netanyahu = נתניהו,
    government = ממשלה   
  )


# Regressions

# War related
  # Peace
peace_reg = feols(peace ~ alert + lag_alert_1 + lag_alert_2 | District + as.factor(year), 
      data = trends, se = 'standard')

  # War
war_reg = feols(war ~ alert + lag_alert_1 + lag_alert_2 | District + as.factor(year), 
      data = trends, se = 'standard')

  # Siren
siren_reg = feols (siren ~ alert + lag_alert_1 + lag_alert_2 | District + as.factor(year), 
       data = trends, se = 'standard')

  # Terrorism
terrorism_reg = feols(terrorism ~ alert + lag_alert_1 + lag_alert_2 | District + as.factor(year), 
      data = trends, se = 'standard')

  # Ceasefire
ceasefire_reg = feols(ceasefire ~ alert + lag_alert_1 + lag_alert_2 | District + as.factor(year), 
      data = trends, se = 'standard')

  # Hamas
hamas_reg = feols(hamas ~ alert + lag_alert_1 + lag_alert_2 | District + as.factor(year), 
      data = trends, se = 'standard')


# Elections related

  # Elections
elections_reg = feols(elections ~ alert + lag_alert_1 + lag_alert_2 | District + as.factor(year), 
      data = trends, se = 'standard')

# Likud related
  # likud
likud_reg = feols(likud ~ alert + lag_alert_1 + lag_alert_2 | District + as.factor(year), 
                  data = trends, se = 'standard')

  # netanyahu
netanyahu_reg = feols(netanyahu ~ alert + lag_alert_1 + lag_alert_2 | District + as.factor(year), 
                      data = trends, se = 'standard')

  # government
government_reg = feols(government ~ alert + lag_alert_1 + lag_alert_2 | District + as.factor(year), 
                       data = trends, se = 'standard')


# Reg table
modelsummary(
  list(
    'Peace' = peace_reg, 'War' = war_reg, 'Siren' = siren_reg, 
    'Terrorism' = terrorism_reg, 'Ceasefire' = ceasefire_reg, 
    'Hamas' = hamas_reg,
    'Elections' = elections_reg, 'Likud' = likud_reg, 'Netanyahu' = netanyahu_reg,
    'Government' = government_reg
  ),
  output = 'Trends/treating/Israel/output/trends_table.tex',
  # output = 'latex_tabular',
  coef_map = c('alert' = 'Red Alert',
               'lag_alert_1' = 'Red Alert - lag 1',
               'lag_alert_2' = 'Red Alert - lag 2'),
  stars = T, 
  names = c("Peace", "War", "Siren", "Terrorism", "Ceasefire", "Hamas", 
            "Elections", "Likud", "Netanyahu", "Government"),
  gof_map = c("nobs", "r.squared")
  )




# Plotting
vars_to_plot <- c("peace", "war", "elections", "siren",
                  "terrorism", "ceasefire", "netanyahu",
                  "hamas","likud","government")

# Plotting 
walk(vars_to_plot, ~ {
  plot <- ggplot(trends, aes(x = date, y = .data[[.x]])) +
    geom_line(color = "gray30", alpha = 1, size = 1) + 
    # geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +  
    geom_point(data = trends[trends$alert > 0,], 
               aes(x = date, y = .data[[.x]], color = alert), size = 3) +  
    
    labs(x = "Date", y = paste("Searches for", toupper(.x), "(0-100)")
         # , 
         # title = paste("Trend evolution -", toupper(.x))
         ) +
    
    theme_minimal() +
    facet_wrap(~ District, scales = "free_y") +
    
    scale_color_viridis_c(option = "plasma", direction = -1, 
                          name = "Proportion of population exposed to Red Alerts") +  
    theme(legend.position = "bottom") +  
    guides(size = "none")
  
  file_name <- paste0("Trends/treating/Israel/output/trend_", .x, ".pdf")
  
  ggsave(filename = file_name, plot = plot, width = 10, height = 6)
})

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
wd <- '/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/'
setwd(wd);


# Importing
trends <-  read.csv('Trends/cleaning/Red Alerts/output/trends_alerts_data_v2.csv', 
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
    # Security & Practical
    shelter = מקלט,
    emergency = חירום,
    hospitals = בתי.חולים,
    siren = אזעקה,
    protected_space = מרחב.מוגן,
    home_front_command = פיקוד.העורף,
    code_red = צבע.אדום,
    panic_attack = התקף.חרדה,
    mda = מד.א,
    
    # General & Political
    peace = שלום,
    war = מלחמה,
    elections = בחירות,
    terrorism = טרור,
    ceasefire = הפסקת.אש,
    netanyahu = נתניהו,
    hamas = חמאס,
    likud = ליכוד,
    government = ממשלה,
    
    # Ideological Indicators
    annexation = סיפוח,
    sovereignty = ריבונות,
    settlements = התנחלויות,
    palestinian_state = מדינה.פלסטינית
  )



# --- Define Groups for Tables ---
groups <- list(
  security = c("shelter", "emergency", "hospitals", "siren", "protected_space", 
               "home_front_command", "code_red", "panic_attack", "mda"),
  conflict = c("peace", "war", "terrorism", "ceasefire", "hamas"),
  politics = c("elections", "likud", "netanyahu", "government"),
  ideology = c("annexation", "sovereignty", "settlements", "palestinian_state")
)

# --- Function to Run Regressions ---
run_feols <- function(var) {
  fml <- as.formula(paste(var, "~ alert + lag_alert_1 + lag_alert_2 | District + as.factor(year)"))
  feols(fml, data = trends, cluster = 'District')
}

# Run all regressions
all_regs <- map(set_names(unlist(groups)), run_feols)

# --- Generate Tables ---
walk2(groups, names(groups), ~ {
  modelsummary(
    all_regs[.x],
    output = paste0('Trends/treating/Israel/output/tables/reg_table_', .y, '.tex'),
    coef_map = c('alert' = 'Red Alert',
                 'lag_alert_1' = 'Red Alert - lag 1',
                 'lag_alert_2' = 'Red Alert - lag 2'),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )
})


all_vars <- unlist(groups)

walk(all_vars, ~ {
  plot <- ggplot(trends, aes(x = date, y = .data[[.x]])) +
    geom_line(color = "gray30", alpha = 0.5, size = 0.7) + 
    geom_point(data = trends[trends$alert > 0,], 
               aes(x = date, y = .data[[.x]], color = alert), size = 2) +  
    labs(x = "Date", y = paste("Searches for", toupper(.x), "(0-100)")) +
    theme_minimal() +
    facet_wrap(~ District, scales = "free_y") +
    scale_color_viridis_c(option = "plasma", direction = -1, 
                          name = "Alert Exposure") +  
    theme(legend.position = "bottom",
          strip.text = element_text(size = 8))
  
  file_name <- paste0("Trends/treating/Israel/output/plots/trend_", .x, ".pdf")
  ggsave(filename = file_name, plot = plot, width = 10, height = 6)
})

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


# Function to save tables in correct latex format:
save_tabular <- function(table, filename) {
  tex <- as.character(table)
  
  # Clean table wrappers
  tex <- gsub("\\\\begin\\{table\\}.*?\n", "", tex)
  tex <- gsub("\\\\end\\{table\\}.*?", "", tex)
  tex <- gsub("\\\\centering\n?", "", tex)
  
  # Store the corrected file
  cat(tex, file = filename)
}


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
    siren = אזעקה,
    protected_space = מרחב.מוגן,
    home_front_command = פיקוד.העורף,
    code_red = צבע.אדום,

    # Health & Medical
    mda = מד.א,
    hospitals = בתי.חולים,
    emergency = חירום,
    panic_attack = התקף.חרדה,

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
  security = c("shelter", "siren", "protected_space", 
               "home_front_command", "code_red"),
  health = c("emergency", "hospitals", "mda", "panic_attack"),
  conflict = c("peace", "war", "terrorism", "ceasefire", "hamas"),
  politics = c("elections", "likud", "netanyahu", "government"),
  ideology = c("annexation", "sovereignty", "settlements", "palestinian_state")
)

all_vars <- unlist(groups)

# --- Datasets and Paths ---
specs <- list(
  "full" = trends,
  "restricted" = trends %>% filter(year == 2014 | (year == 2015 & month <= 3))
)

# --- Run Regressions, Tables and Plots ---
iwalk(specs, function(df, sample_name) {
  
  # Define subdirectory and suffix
  subfolder <- ifelse(sample_name == "full", "", paste0(sample_name, "/"))
  suffix <- ifelse(sample_name == "full", "", paste0("_", sample_name))
  
  # --- Function to Run Regressions ---
  run_feols <- function(var) {
    fml <- as.formula(paste(var, "~ alert + lag_alert_1 + lag_alert_2 | District + as.factor(year)"))
    feols(fml, data = df, cluster = 'District')
  }
  
  # Run all regressions
  all_regs <- map(set_names(all_vars), run_feols)
  
  # --- Generate Tables ---
  walk2(groups, names(groups), ~ {
    
    # Extract the relevant models for the current group
    model_list <- all_regs[.x]
    
    # Set names for the models in the list (for table output)
    names(model_list) <- toupper(gsub("_", " ", names(model_list)))
    
    tab_tex <- modelsummary(
      model_list,
      output = 'latex',
      coef_map = c('alert' = 'Red Alert',
                   'lag_alert_1' = 'Red Alert - lag 1',
                   'lag_alert_2' = 'Red Alert - lag 2'),
      stars = TRUE,
      gof_map = c("nobs", "r.squared")
    )
    
    save_tabular(tab_tex, 
                 filename =  paste0('Trends/treating/Israel/output/tables/', subfolder, 'reg_table_', .y, suffix, '.tex'))
  })
  
  # --- Generate Plots ---
  walk(all_vars, ~ {
    plot <- ggplot(df, aes(x = date, y = .data[[.x]])) +
      geom_line(color = "gray30", alpha = 0.5, size = 0.7) + 
      geom_point(data = df[df$alert > 0,], 
                 aes(x = date, y = .data[[.x]], color = alert), size = 2) +  
      labs(x = "Date", y = paste("Searches for", toupper(.x), "(0-100)")) +
      theme_minimal() +
      facet_wrap(~ District, scales = "free_y") +
      scale_color_viridis_c(option = "plasma", direction = -1, 
                            name = "Alert Exposure") +  
      theme(legend.position = "bottom",
            strip.text = element_text(size = 8))
    
    file_name <- paste0("Trends/treating/Israel/output/plots/", subfolder, "trend_", .x, suffix, ".pdf")
    ggsave(filename = file_name, plot = plot, width = 10, height = 6)
  })
})

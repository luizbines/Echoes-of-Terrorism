#### CREATING DESCRIPTIVE STATISTICS TABLE #####

# Library
library(tidyr)
library(dplyr)
library(ggplot2)
library(purrr)
library(webshot2)
library(kableExtra)

# Directory
wd = '/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Unsuccessful_Terror_Attacks_Affect_Voting_Prefferences-main/'
setwd(wd);


## IMPORTING ##
parties_percentages_panel =  read.csv('treating/Red Alerts/Output/2_parties_percentages_panel.csv')

descriptive_statistics <- parties_percentages_panel %>%
  
  # filtering 2013 and 2015
  filter(year == 2013 | year == 2015) %>% 
  # filtering out arab cities
  filter(Religion_yishuv_Code != 2) %>% 
  
  # # merging with right wing percentage dataset
  # merge(right_wing_percentage_panel %>% 
  #         filter(Religion_yishuv_Code !=2) %>% 
  #         select(SEMEL_YISHUV,year,right_wing_percentage),
  #       by = c('SEMEL_YISHUV','year')) %>% 
  # # merging turnout dataset
  # merge(turnout_percentage_panel %>% 
  #         filter(Religion_yishuv_Code != 2) %>% 
  #         select(SEMEL_YISHUV,year,turnout_percentage),
  #       by = c('SEMEL_YISHUV','year')) %>% 
  # defining groups
  mutate(group = case_when(
    temporal_group == 'no_red_alert' ~ 'No Red Alert',
    temporal_group == 'temporal_distance > 149' ~ 'Last Red Alert 149+ Days Before 2015 Election',
    temporal_group == 'temporal_distance == 6' ~ 'Last Red Alert 6 Days Before 2015 Election',
    TRUE ~ NA_character_
  )) %>%
  mutate(Election = year) %>%
  group_by(group, Election) %>%
  # Statistics:
  summarise(
    'Likud % (Average)' = mean(likud_percentage * 100, na.rm = TRUE),
    'Likud % (SD)' = sd(likud_percentage * 100, na.rm = TRUE),
    'Right Wing % (Average)' = mean(right_wing_percentage * 100, na.rm = TRUE),
    'Right Wing % (SD)' = sd(right_wing_percentage * 100, na.rm = TRUE),
    'Turnout % (Average)' = mean(turnout_percentage * 100, na.rm = TRUE),
    'Turnout % (SD)' = sd(turnout_percentage * 100, na.rm = TRUE),    
    'Night Lights (0-63) (Average)' = mean(ntl, na.rm = TRUE),
    'Night Lights (0-63) (SD)' = sd(ntl, na.rm = TRUE),
    'Population Size (Average)' = mean(Pop_Total, na.rm = TRUE),
    'Population Size (SD)' = sd(Pop_Total, na.rm = TRUE),
    'Population Density (per km2) (Average)' = mean(density, na.rm = TRUE),
    'Population Density (per km2) (SD)' = sd(density, na.rm = TRUE),
    'Area (km2) (Average)' = mean(Shape_Area, na.rm = TRUE),
    'Area (km2) (SD)' = sd(Shape_Area, na.rm = TRUE),
    'Distance to Gaza (km) (Average)' = mean(distance, na.rm = TRUE),
    'Distance to Gaza (km) (SD)' = sd(distance, na.rm = TRUE),
    'N' = n()
    
  ) %>%
  arrange(group, Election)


# Filtering control group "No Red Alert"
control_group <- descriptive_statistics %>%
  filter(group == "No Red Alert")

# Function to calculate difference and SE of the difference
calculate_diff_se <- function(avg1, sd1, n1, avg2, sd2, n2) {
  diff <- avg2 - avg1
  se_diff <- sqrt((sd1^2 / n1) + (sd2^2 / n2))
  return(c(diff, se_diff))
}

# Creating results list
results <- list()

# Interest variables
variables <- c("Likud % (Average)", "Right Wing % (Average)", "Turnout % (Average)",
               "Night Lights (0-63) (Average)", "Population Size (Average)",
               "Population Density (per km2) (Average)", "Area (km2) (Average)",
               "Distance to Gaza (km) (Average)")

# Iterating for treatment groups
for (group_name in c("Last Red Alert 149+ Days Before 2015 Election", 
                     "Last Red Alert 6 Days Before 2015 Election")) {
  
  # Filtering treatment group data
  group_data <- descriptive_statistics %>%
    filter(group == group_name)
  
  
  # Sample size for control and treatment group
  n1 <- control_group$N[1]
  n2 <- group_data$N[1]
  
  
  for (var in variables) {
    # Correcting variable' SD
    var_sd <- gsub(" \\(Average\\)", " (SD)", var)
    
    # Calculating differences in mean and SD
    diff_se <- calculate_diff_se(
      as.numeric(control_group[[var]][1]), as.numeric(control_group[[var_sd]][1]), n1,
      as.numeric(group_data[[var]][1]), as.numeric(group_data[[var_sd]][1]), n2
    )
    
    # Storing results
    results[[length(results) + 1]] <- c(group_name, var, diff_se[1], diff_se[2])
  }
}

# Creating results dataframe
results_df <- do.call(rbind, results)
results_df <- as.data.frame(results_df, stringsAsFactors = F)
colnames(results_df) <- c("Group", "Variable", "Difference", "SE")

results_df$Difference = as.numeric(as.character(results_df$Difference))
results_df$SE = as.numeric(as.character(results_df$SE))


# Function to determine significance level
get_significance_level <- function(diff, se) {
  z_value <- abs(diff / se)
  
  # Significance levels
  if (z_value > 3.291) {
    return("0.1%")  # p < 0.001
  } else if (z_value > 2.576) {
    return("1%")    # p < 0.01
  } else if (z_value > 1.96) {
    return("5%")    # p < 0.05
  } else if (z_value > 1.645) {
    return("10%")   # p < 0.1
  } else {
    return("")      # Not significant
  }
}

# Adding significance column
results_df$Significance <- mapply(get_significance_level, as.numeric(results_df$Difference), as.numeric(results_df$SE))


# Adjusting dataframe
results_df = results_df %>% pivot_wider(
  names_from = Group,
  values_from = c(Difference, SE, Significance),
  names_sep = "_"
)

no_red_alerts_descriptives = descriptive_statistics %>% 
  filter(Election == 2013 & group == 'No Red Alert')



#### LATEX DESCRIPTIVE STATISTICS TABLE ####
# HTML descriptive statistics table
statistics_table <- descriptive_statistics %>% filter(Election==2013) %>%
  pivot_longer(cols = -c(group, Election), names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = group, values_from = Value) %>%
  mutate(
    `Diff (vs No Red Alerts)` = `Last Red Alert 149+ Days Before 2015 Election` - `No Red Alert`,
    `Diff (vs No Red Alerts)_6days` = `Last Red Alert 6 Days Before 2015 Election` - `No Red Alert`
  ) %>%
  select(
    Statistic,
    `No Red Alert`,
    `Last Red Alert 149+ Days Before 2015 Election`,
    `Diff (vs No Red Alerts)`,
    `Last Red Alert 6 Days Before 2015 Election`,
    `Diff (vs No Red Alerts)_6days`
  )




# Adjusting names
colnames(statistics_table) <- c(
  "Statistic",
  "No Red Alerts (2013)",
  "Last Red Alert 149+ Days Before (2013)",
  "Diff (vs No Red Alerts)",
  "Last Red Alert 6 Days Before (2013)",
  "Diff (vs No Red Alerts)"
)


# Correcting Diff SE using actual SE calculated in the results_df dataset
for (i in 1:8) {
  row_st <- 2 * i
  
  # Attributing values
  statistics_table[row_st, 4] <- results_df$`SE_Last Red Alert 149+ Days Before 2015 Election`[i]
  statistics_table[row_st, 6] <- results_df$`SE_Last Red Alert 6 Days Before 2015 Election`[i]
}


# Exhibiting 
final_table = statistics_table %>%
  kable("html", digits = 2, align = 'c', caption = "Table 1: Descriptive Statistics by Groups of Interest for 2013") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

print(final_table)

save_kable(final_table, file = "treating/Red Alerts/Output/Figures/2_descriptives_table.pdf")

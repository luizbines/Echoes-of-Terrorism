#### CREATING DESCRIPTIVE STATISTICS TABLE (ROBUST VERSION) #####

# Libraries
library(dplyr)
library(tidyr)
library(fixest)       # For fast regressions with clusters
library(modelsummary)  # For automatic and elegant tables
library(kableExtra)

# Directory
wd = '/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Unsuccessful_Terror_Attacks_Affect_Voting_Prefferences-main/'
setwd(wd)

## 1. DATA IMPORT AND CLEANING ##
parties_percentages_panel = read.csv('treating/Red Alerts/Output/2_parties_percentages_panel.csv')

df_clean <- parties_percentages_panel %>%
  filter(year == 2013) %>% # Focus on 2013 baseline
  filter(Religion_yishuv_Code != 2) %>%
  mutate(group = case_when(
    temporal_group == 'no_red_alert' ~ 'Control',
    temporal_group == 'temporal_distance > 149' ~ 'Treat_149',
    temporal_group == 'temporal_distance == 6' ~ 'Treat_6',
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(group)) %>%
  # Preparing variables (multiplying by 100 where percentage)
  mutate(
    likud = likud_percentage * 100,
    right_wing = right_wing_percentage * 100,
    turnout = turnout_percentage * 100,
    ntl = ntl,
    pop = Pop_Total,
    density = density,
    area = Shape_Area,
    dist = distance
  )

# 2. VARIABLE DEFINITION
var_list <- c("likud", "right_wing", "turnout", "ntl", "pop", "density", "area", "dist")
var_labels <- c("Likud %", "Right Wing %", "Turnout %", "Night Lights", 
                "Population", "Density", "Area (km2)", "Dist. Gaza (km)")

## 3. STATISTICS CALCULATION WITH CLUSTER (SEMEL_YISHUV) ##
# This function calculates the control mean and treatment differences using OLS
# OLS: Var ~ Group, with clustered standard errors equivalent to robust Wald t-test.

get_stats <- function(var) {
  # Regression with cluster by city
  formula <- as.formula(paste(var, "~ group"))
  mod <- feols(formula, data = df_clean, cluster = ~SEMEL_YISHUV)
  
  # Extract coefficients and p-values
  sum_mod <- summary(mod)
  
  # Means by group
  means <- df_clean %>%
    group_by(group) %>%
    summarise(m = mean(!!sym(var), na.rm = TRUE), sd = sd(!!sym(var), na.rm = TRUE))
  
  # Organizing table row with p-values
  data.frame(
    Variable = var,
    Mean_Control = means$m[means$group == "Control"],
    SD_Control = means$sd[means$group == "Control"],
    Diff_149 = sum_mod$coeftable["groupTreat_149", "Estimate"],
    SE_149 = sum_mod$coeftable["groupTreat_149", "Std. Error"],
    P_149 = sum_mod$coeftable["groupTreat_149", "Pr(>|t|)"],
    Diff_6 = sum_mod$coeftable["groupTreat_6", "Estimate"],
    SE_6 = sum_mod$coeftable["groupTreat_6", "Std. Error"],
    P_6 = sum_mod$coeftable["groupTreat_6", "Pr(>|t|)"]
  )
}

# Function to add significance stars
add_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.01) return("***")
  if (p_value < 0.05) return("**")
  if (p_value < 0.10) return("*")
  return("")
}

# Apply to all variables
final_stats <- lapply(var_list, get_stats) %>% bind_rows()

## 4. FORMATTING FOR LATEX / OVERLEAF ##
# Create alternating rows: one for statistic, another for SE/SD

table_rows <- final_stats %>%
  mutate(across(c(Mean_Control, SD_Control, Diff_149, SE_149, Diff_6, SE_6), ~ round(., 2))) %>%
  mutate(
    stars_149 = sapply(P_149, add_stars),
    stars_6 = sapply(P_6, add_stars)
  )

# Create dataframe with duplicated rows (statistic + SE)
table_latex <- data.frame()
for(i in 1:nrow(table_rows)) {
  # Row with statistics
  row_stat <- data.frame(
    Variable = var_labels[i],
    Control = as.character(table_rows$Mean_Control[i]),
    `Diff. Treat 149d` = paste0(table_rows$Diff_149[i], table_rows$stars_149[i]),
    `Diff. Treat 6d` = paste0(table_rows$Diff_6[i], table_rows$stars_6[i]),
    check.names = FALSE
  )
  
  # Row with SE/SD
  row_se <- data.frame(
    Variable = "",
    Control = paste0("(", table_rows$SD_Control[i], ")"),
    `Diff. Treat 149d` = paste0("(", table_rows$SE_149[i], ")"),
    `Diff. Treat 6d` = paste0("(", table_rows$SE_6[i], ")"),
    check.names = FALSE
  )
  
  table_latex <- rbind(table_latex, row_stat, row_se)
}

rownames(table_latex) <- NULL

# Generate final table with KableExtra
final_kable <- kable(table_latex, 
                     format = "latex",
                     booktabs = TRUE, 
                     caption = "Descriptive Statistics by Groups of Interest for 2013",
                     align = "lccc",
                     escape = FALSE) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_footnote("Standard deviations (Control) and standard errors (Treatment differences), clustered at the locality level, are reported in parentheses below the estimates. Significance levels: *** p<0.01, ** p<0.05, * p<0.10. Diff. Treat 149d: Difference in means between localities whose last red alert was more than 149 days before the 2015 election and the Control group. Diff. Treat 6d: Difference in means between localities whose last red alert was exactly 6 days before the 2015 election and the Control group.", 
               notation = "none")

# Display in console to copy
print(final_kable)

# Save .tex file for Overleaf
writeLines(as.character(final_kable), "treating/Red Alerts/Output/Figures/3_descriptives_table.tex")

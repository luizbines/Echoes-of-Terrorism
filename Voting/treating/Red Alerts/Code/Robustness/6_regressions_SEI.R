# Robustness regressions using SEI instead of night lights as the control variable.
# This script replicates the main regressions from 4_main_regressions.R

# Libraries
library(dplyr)
library(readr)
library(fixest)
library(modelsummary)

# Get the base path from environment or parent script
resolve_voting_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)
  repeat {
    if (dir.exists(file.path(current_dir, "raw")) &&
        dir.exists(file.path(current_dir, "cleaning")) &&
        dir.exists(file.path(current_dir, "treating"))) {
      return(current_dir)
    }
    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      break
    }
    current_dir <- parent_dir
  }
  stop("ERROR: Could not determine Voting directory path from start_dir=", start_dir)
}

base_path <- Sys.getenv("R_MODULE_ROOT")
if (nzchar(base_path) && dir.exists(base_path)) {
  base_path <- normalizePath(base_path, winslash = "/", mustWork = FALSE)
  if (!dir.exists(file.path(base_path, "raw")) ||
      !dir.exists(file.path(base_path, "cleaning")) ||
      !dir.exists(file.path(base_path, "treating"))) {
    base_path <- resolve_voting_root()
  }
} else {
  base_path <- resolve_voting_root()
}
setwd(base_path)

##### IMPORTING #####

parties_percentages_panel = read_csv('treating/Red Alerts/Output/Datasets/2_parties_percentages_panel.csv')

##### FILTERING #####

# Keep elections up to 2015 and rename the election year variable
parties_percentages_panel = parties_percentages_panel %>%
  filter(year <= 2015) %>%
  mutate(year = as.integer(year)) %>%
  rename(year_election = year)

# Drop the treatment group used in the main specifications
parties_percentages_panel_control = parties_percentages_panel %>%
  filter(Religion_yishuv_Code != 2)


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


##### REGRESSIONS #####

#### LIKUD ####

# Fixed effects only
reg_1_likud =
  feols(likud_percentage ~ i(year_election, temporal_group,
                             ref = '2013',
                             ref2 = 'no_red_alert') |
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_control,
        cluster = 'SEMEL_YISHUV')

summary(reg_1_likud)

# Average effect calculation for the 2015 election, 6 days before treatment
coef_likud <- reg_1_likud$coefficients[5] %>% as.numeric()
mean_likud_2013 <- mean(parties_percentages_panel$likud_percentage[
  parties_percentages_panel$time_to_treatment == -1 &
    parties_percentages_panel$temporal_group == 'temporal_distance == 6'
])
effect_percent_likud <- (coef_likud / mean_likud_2013) * 100

effect_percent_likud

# Control variables + fixed effects
reg_2_likud =
  feols(likud_percentage ~ i(year_election, temporal_group,
                             ref = '2013',
                             ref2 = 'no_red_alert') +
        Pop_Total + SEI |
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_control,
        cluster = 'SEMEL_YISHUV')


summary(reg_2_likud)

#### RIGHT WING ####

# Fixed effects only
reg_1_right_wing =
  feols(right_wing_percentage ~ i(year_election, temporal_group,
                                  ref = '2013',
                                  ref2 = 'no_red_alert') |
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_control,
        cluster = 'SEMEL_YISHUV')

# Control variables + fixed effects
reg_2_right_wing =
  feols(right_wing_percentage ~ i(year_election, temporal_group,
                                  ref = '2013',
                                  ref2 = 'no_red_alert') +
        Pop_Total + SEI |
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_control,
        cluster = 'SEMEL_YISHUV')

#### TURNOUT ####

# Fixed effects only
reg_1_turnout =
  feols(turnout_percentage ~ i(year_election, temporal_group,
                               ref = '2013',
                               ref2 = 'no_red_alert') |
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_control,
        cluster = 'SEMEL_YISHUV')

# Control variables + fixed effects
reg_2_turnout =
  feols(turnout_percentage ~ i(year_election, temporal_group,
                               ref = '2013',
                               ref2 = 'no_red_alert') +
          Pop_Total + SEI |
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_control,
        cluster = 'SEMEL_YISHUV')

#### COALITION 2015 ####

# Fixed effects only
reg_1_coalition =
  feols(coalition_2015_percentage ~ i(year_election, temporal_group,
                                      ref = '2013',
                                      ref2 = 'no_red_alert') |
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_control,
        cluster = 'SEMEL_YISHUV')

# Control variables + fixed effects
reg_2_coalition =
  feols(coalition_2015_percentage ~ i(year_election, temporal_group,
                                      ref = '2013',
                                      ref2 = 'no_red_alert') +
          Pop_Total + SEI |
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = parties_percentages_panel_control,
        cluster = 'SEMEL_YISHUV')

# Main regression table
SEI_model = modelsummary(
  list(
    reg_1_likud,
    reg_2_likud,
    reg_1_coalition,
    reg_2_coalition,
    reg_1_right_wing,
    reg_2_right_wing,
    reg_1_turnout,
    reg_2_turnout
  ),
  output = 'latex',
  coef_map = c(
    'year_election::2015:temporal_group::temporal_distance == 6' = 'Red Alert 6 Days Before * 2015 Election',
    'year_election::2015:temporal_group::temporal_distance > 149' = 'Red Alert 149+ Days Before * 2015 Election',
    'year_election::2009:temporal_group::temporal_distance == 6' = 'Red Alert 6 Days Before * 2009 Election',
    'year_election::2009:temporal_group::temporal_distance > 149' = 'Red Alert 149+ Days Before * 2009 Election',
    'year_election::2006:temporal_group::temporal_distance == 6' = 'Red Alert 6 Days Before * 2006 Election',
    'year_election::2006:temporal_group::temporal_distance > 149' = 'Red Alert 149+ Days Before * 2006 Election'
  ),
  stars = TRUE,
  gof_omit = '',
  add_rows = data.frame(
    'VARIABLES' = c('Dependent Variable', 'Control Variables', 'Locality Fixed Effects', 'Control Group', 'Observations'),
    '1' = c('Likud', 'No', 'Yes', 'NRA 2015', summary(reg_1_likud)$nobs),
    '2' = c('Likud', 'Yes', 'Yes', 'NRA 2015', summary(reg_2_likud)$nobs),
    '3' = c('2015 Coalition (excluding Likud)', 'No', 'Yes', 'NRA 2015', summary(reg_1_coalition)$nobs),
    '4' = c('2015 Coalition (excluding Likud)', 'Yes', 'Yes', 'NRA 2015', summary(reg_2_coalition)$nobs),
    '5' = c('Right Wing (excluding Likud)', 'No', 'Yes', 'NRA 2015', summary(reg_1_right_wing)$nobs),
    '6' = c('Right Wing (excluding Likud)', 'Yes', 'Yes', 'NRA 2015', summary(reg_2_right_wing)$nobs),
    '7' = c('Turnout', 'No', 'Yes', 'NRA 2015', summary(reg_1_turnout)$nobs),
    '8' = c('Turnout', 'Yes', 'Yes', 'NRA 2015', summary(reg_2_turnout)$nobs)
  )
)


# Saving
save_tabular(SEI_model, 'treating/Red Alerts/Output/Tables/Robustness/Robustness_SEI_regressions.tex')

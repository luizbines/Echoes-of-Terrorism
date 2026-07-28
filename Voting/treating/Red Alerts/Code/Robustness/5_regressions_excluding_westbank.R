# This script runs the main regressions
# Luiz Bines
# 2024

# Library
library(dplyr)
library(readr)
library(fixest)
library(modelsummary)
library(janitor)

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


##### IMPORTING #####

parties_percentages_panel = read_csv(
  'treating/Red Alerts/Output/Datasets/2_parties_percentages_panel.csv'
)


### FILTERING ###

# filtering years
parties_percentages_panel = parties_percentages_panel %>%
  filter(year <= 2015) %>%
  mutate(
    year = as.integer(year),
  ) %>%
  rename(year_election = year)

# Removing observations with missing voting data (only 28 observations, from 2006 and 2009 elections)
parties_percentages_panel %>%
  filter(is.na(likud_percentage)) %>% 
  select(year_election) %>% table

parties_percentages_panel = parties_percentages_panel %>% filter(!is.na(likud_percentage))

# Excluding Arab localities
parties_percentages_panel = parties_percentages_panel %>%
  filter(Religion_yishuv_Code != 2)


# Checking number of localities per group that are within the West Bank
parties_percentages_panel %>%
  distinct(SEMEL_YISHUV, .keep_all = TRUE) %>%
  janitor::tabyl(temporal_group, intersecting_west_bank) %>%
  janitor::adorn_totals(c("row", "col"))

# Excluding West Bank localities
parties_percentages_panel = parties_percentages_panel %>%
  filter(intersecting_west_bank != 1)
  # Results are virtually identical when excluding localities with centroid within the West Bank, any intersection or the whole area within the West Bank.


##### REGRESSIONS #####

#### LIKUD ####

# FIXED EFFECTS

reg_1_likud =
  feols(
    likud_percentage ~ i(
      year_election,
      temporal_group,
      ref = '2013',
      ref2 = 'no_red_alert'
    ) |
      as.factor(SEMEL_YISHUV) + as.factor(year_election),
    data = parties_percentages_panel,
    cluster = 'SEMEL_YISHUV'
  )


# CONTROL VARIABLES + FIXED EFFECTS
reg_2_likud =
  feols(
    likud_percentage ~ i(
      year_election,
      temporal_group,
      ref = '2013',
      ref2 = 'no_red_alert'
    ) +
      Pop_Total +
      ntl |
      as.factor(SEMEL_YISHUV) + as.factor(year_election),
    data = parties_percentages_panel,
    cluster = 'SEMEL_YISHUV'
  )


#### RIGHT WING ####

# FIXED EFFECTS

reg_1_right_wing =
  feols(
    right_wing_percentage ~ i(
      year_election,
      temporal_group,
      ref = '2013',
      ref2 = 'no_red_alert'
    ) |
      as.factor(SEMEL_YISHUV) + as.factor(year_election),
    data = parties_percentages_panel,
    cluster = 'SEMEL_YISHUV'
  )


# CONTROL VARIABLES + FIXED EFFECTS
reg_2_right_wing =
  feols(
    right_wing_percentage ~ i(
      year_election,
      temporal_group,
      ref = '2013',
      ref2 = 'no_red_alert'
    ) +    
      Pop_Total +
      ntl |
      as.factor(SEMEL_YISHUV) + as.factor(year_election),
    data = parties_percentages_panel,
    cluster = 'SEMEL_YISHUV'
  )


#### TURNOUT ####

# FIXED EFFECTS

reg_1_turnout =
  feols(
    turnout_percentage ~ i(
      year_election,
      temporal_group,
      ref = '2013',
      ref2 = 'no_red_alert'
    ) |
      as.factor(SEMEL_YISHUV) + as.factor(year_election),
    data = parties_percentages_panel,
    cluster = 'SEMEL_YISHUV'
  )


# CONTROL VARIABLES + FIXED EFFECTS
reg_2_turnout =
  feols(
    turnout_percentage ~ i(
      year_election,
      temporal_group,
      ref = '2013',
      ref2 = 'no_red_alert'
    ) +     
      Pop_Total +
      ntl |
      as.factor(SEMEL_YISHUV) + as.factor(year_election),
    data = parties_percentages_panel,
    cluster = 'SEMEL_YISHUV'
  )


#### COALITION 2015 ####

# FIXED EFFECTS
reg_1_coalition =
  feols(
    coalition_2015_percentage ~ i(
      year_election,
      temporal_group,
      ref = '2013',
      ref2 = 'no_red_alert'
    ) |
      as.factor(SEMEL_YISHUV) + as.factor(year_election),
    data = parties_percentages_panel,
    cluster = 'SEMEL_YISHUV'
  )

# CONTROL VARIABLES + FIXED EFFECTS
reg_2_coalition =
  feols(
    coalition_2015_percentage ~ i(
      year_election,
      temporal_group,
      ref = '2013',
      ref2 = 'no_red_alert'
    ) +      
      Pop_Total +
      ntl |
      as.factor(SEMEL_YISHUV) + as.factor(year_election),
    data = parties_percentages_panel,
    cluster = 'SEMEL_YISHUV'
  )

# Likud, Right Wing, turnout, coalition
no_west_bank_model = modelsummary(
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
  stars = T,
  notes = 'NRA 2015 stands for No Red Alerts until the 2015 Election.',
  gof_omit = '',
  add_rows = data.frame(
    'VARIABLES' = c(
      'Dependent Variable',
      'Control Variables',
      'Locality Fixed Effects',
      'Control Group',
      'Observations'
    ),
    '1' = c(
      'Likud',
      'No',
      'Yes',
      'NRA 2015',
      summary(reg_1_likud)$nobs
    ),

    '2' = c(
      'Likud',
      'Yes',
      'Yes',
      'NRA 2015',
      summary(reg_2_likud)$nobs
    ),

    '3' = c(
      '2015 Coalition',
      'No',
      'Yes',
      'NRA 2015',
      summary(reg_1_coalition)$nobs
    ),

    '4' = c(
      '2015 Coalition',
      'Yes',
      'Yes',
      'NRA 2015',
      summary(reg_2_coalition)$nobs
    ),

    '5' = c(
      'Right Wing',
      'No',
      'Yes',
      'NRA 2015',
      summary(reg_1_right_wing)$nobs
    ),

    '6' = c(
      'Right Wing',
      'Yes',
      'Yes',
      'NRA 2015',
      summary(reg_2_right_wing)$nobs
    ),

    '7' = c(
      'Turnout',
      'No',
      'Yes',
      'NRA 2015',
      summary(reg_1_turnout)$nobs
    ),

    '8' = c(
      'Turnout',
      'Yes',
      'Yes',
      'NRA 2015',
      summary(reg_2_turnout)$nobs
    )
  )
)


# Saving
save_tabular(no_west_bank_model, 'treating/Red Alerts/Output/Tables/Robustness/Robustness_excluding_west_bank.tex')

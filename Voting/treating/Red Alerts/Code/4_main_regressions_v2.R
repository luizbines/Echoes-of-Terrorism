# This script runs the main regressions
# Luiz Bines
# 2024

# Library
library(tidyr)
library(dplyr)
library(readxl)
library(kableExtra)
library(fixest)
library(modelsummary)
library(knitr)
library(broom)
library(panelsummary)
library(knitr)
library(tidyverse)

# Get the base path from environment or parent script
if (!exists("base_path")) {
  base_path <- Sys.getenv("R_PROJECT_DIR")
  if (base_path == "") {
    base_path <- getwd()
  }
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

# Alert distribution tables

# 6 days before election
parties_percentages_panel %>%
  filter(year_election == 2015 & temporal_distance == 6) %>%
  select(number_of_red_alerts) %>%
  table


# 149-250 days before election
parties_percentages_panel %>%
  filter(year_election == 2015 & temporal_distance > 6) %>%
  select(number_of_red_alerts) %>%
  table


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
    data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
    cluster = 'SEMEL_YISHUV'
  )

summary(reg_1_likud)


# calculating average effect
coef_likud <- reg_1_likud$coefficients[5] %>% as.numeric()

mean_likud_2013 <- mean(parties_percentages_panel$likud_percentage[
  parties_percentages_panel$time_to_treatment == -1 &
    parties_percentages_panel$temporal_group == "temporal_distance == 6"
])
effect_percent_likud <- (coef_likud / mean_likud_2013) * 100

effect_percent_likud

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
    data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
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
    data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
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
    data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
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
    data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
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
    data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
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
    data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
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
    data = parties_percentages_panel %>% filter(Religion_yishuv_Code != 2),
    cluster = 'SEMEL_YISHUV'
  )

# Likud, Right Wing, turnout, coalition
main_model = modelsummary(
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
  # output = 'treating/Red Alerts/Output/Tables/4_main_results.tex',
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
save_tabular(main_model, 'treating/Red Alerts/Output/Tables/4_main_results.tex')

#### ROBUSTNESS: CONLEY STANDARD ERRORS ####

# Altering standard errors to Conley spatial standard errors for the main models with control variables and fixed effects
# Note that, in general, the VCOV matrix is not positive semi-definite for larger cutoffs, due to the small number of clusters they generate
# 1. LIKUD
mod_likud_2  = summary(reg_2_likud, vcov = conley(2) ~ lat + long)
mod_likud_3 = summary(reg_2_likud, vcov = conley(3) ~ lat + long)
mod_likud_10 = summary(reg_2_likud, vcov = conley(10) ~ lat + long)
mod_likud_20 = summary(reg_2_likud, vcov = conley(20) ~ lat + long)

# 2. COALITION
mod_coal_2  = summary(reg_2_coalition, vcov = conley(2) ~ lat + long)
mod_coal_3 = summary(reg_2_coalition, vcov = conley(3) ~ lat + long)
mod_coal_10 = summary(reg_2_coalition, vcov = conley(10) ~ lat + long)
mod_coal_20 = summary(reg_2_coalition, vcov = conley(20) ~ lat + long)

# 3. RIGHT WING
mod_rw_2  = summary(reg_2_right_wing, vcov = conley(2) ~ lat + long)
mod_rw_3 = summary(reg_2_right_wing, vcov = conley(3) ~ lat + long)
mod_rw_10 = summary(reg_2_right_wing, vcov = conley(10) ~ lat + long)
mod_rw_20 = summary(reg_2_right_wing, vcov = conley(20) ~ lat + long)

# 4. TURNOUT
mod_turn_2  = summary(reg_2_turnout, vcov = conley(2) ~ lat + long)
mod_turn_3 = summary(reg_2_turnout, vcov = conley(3) ~ lat + long)
mod_turn_10 = summary(reg_2_turnout, vcov = conley(10) ~ lat + long)
mod_turn_20 = summary(reg_2_turnout, vcov = conley(20) ~ lat + long)

# Generating panel table
conley_panels = panelsummary(
  list(reg_2_likud, mod_likud_2, mod_likud_3, mod_likud_10, mod_likud_20),
  list(reg_2_coalition, mod_coal_2, mod_coal_3, mod_coal_10, mod_coal_20),
  list(reg_2_right_wing, mod_rw_2, mod_rw_3, mod_rw_10, mod_rw_20),
  list(reg_2_turnout, mod_turn_2, mod_turn_3, mod_turn_10, mod_turn_20),
  panel_labels = c("Panel A: Likud", "Panel B: 2015 Coalition", "Panel C: Right-Wing", "Panel D: Turnout"),
  coef_map = c(
    'year_election::2015:temporal_group::temporal_distance == 6' = '6 days before elections',
    'year_election::2015:temporal_group::temporal_distance > 149' = '149+ days before elections'
  ),
  gof_map = c('nobs', 'r.squared'),
  colnames = c('', 'Baseline', 'Conley (2km)', 'Conley (3km)', 'Conley (10km)', 'Conley (20km)'),
  format = 'latex',
  stars = TRUE
)

# Exportando
writeLines(conley_panels, 'treating/Red Alerts/Output/Tables/Robustness/Robustness_conley_SEs.tex')

#### HETEROGENEITIES ####

#### by likud vote share in 2013 ####

# Create quintile variable based on Likud share in 2013
quintiles <- quantile(
  parties_percentages_panel$likud_percentage[
    parties_percentages_panel$year_election == 2013 &
      parties_percentages_panel$Religion_yishuv_Code != 2
  ],
  probs = seq(0, 1, 0.2),
  na.rm = TRUE
)


# Assigning specific quintile to each locality
parties_percentages_panel <- parties_percentages_panel %>%
  group_by(SEMEL_YISHUV) %>%
  mutate(
    quintile2013 = cut(
      likud_percentage * (year_election == 2013),
      breaks = quintiles,
      labels = paste0("Q", 1:5)
    )
  ) %>%
  fill(quintile2013, .direction = "downup") %>%
  ungroup()


# Models for likud_percentage
mod_likud_q1 <- feols(
  likud_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q1') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_likud_q2 <- feols(
  likud_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q2') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_likud_q3 <- feols(
  likud_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q3') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_likud_q4 <- feols(
  likud_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q4') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_likud_q5 <- feols(
  likud_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q5') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)


# Models for right_wing_percentage
mod_right_wing_q1 <- feols(
  right_wing_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q1') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_right_wing_q2 <- feols(
  right_wing_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q2') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_right_wing_q3 <- feols(
  right_wing_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q3') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_right_wing_q4 <- feols(
  right_wing_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q4') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_right_wing_q5 <- feols(
  right_wing_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q5') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)


# Models for turnout_percentage
mod_turnout_q1 <- feols(
  turnout_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q1') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_turnout_q2 <- feols(
  turnout_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q2') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_turnout_q3 <- feols(
  turnout_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q3') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_turnout_q4 <- feols(
  turnout_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q4') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_turnout_q5 <- feols(
  turnout_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q5') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)


# Models for coalition_percentage
mod_coalition_q1 <- feols(
  coalition_2015_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q1') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_coalition_q2 <- feols(
  coalition_2015_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q2') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_coalition_q3 <- feols(
  coalition_2015_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q3') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_coalition_q4 <- feols(
  coalition_2015_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q4') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)
mod_coalition_q5 <- feols(
  coalition_2015_percentage ~ i(
    year_election,
    temporal_group,
    ref = "2013",
    ref2 = "no_red_alert"
  ) |
    as.factor(SEMEL_YISHUV) + as.factor(year_election),
  data = parties_percentages_panel %>%
    filter(quintile2013 == 'Q5') %>%
    filter(Religion_yishuv_Code != 2),
  cluster = 'SEMEL_YISHUV'
)


# Creating table
polarization_quintiles = panelsummary(
  list(mod_likud_q1, mod_likud_q2, mod_likud_q3, mod_likud_q4, mod_likud_q5),
  list(
    mod_coalition_q1,
    mod_coalition_q2,
    mod_coalition_q3,
    mod_coalition_q4,
    mod_coalition_q5
  ),
  list(
    mod_right_wing_q1,
    mod_right_wing_q2,
    mod_right_wing_q3,
    mod_right_wing_q4,
    mod_right_wing_q5
  ),
  list(
    mod_turnout_q1,
    mod_turnout_q2,
    mod_turnout_q3,
    mod_turnout_q4,
    mod_turnout_q5
  ),
  panel_labels = c(
    "Panel A: Likud",
    "Panel B: Coalition",
    "Panel C: Right-Wing",
    "Panel D: Turnout"
  ),
  coef_map = c(
    'year_election::2015:temporal_group::temporal_distance == 6' = '6 days before elections',
    'year_election::2015:temporal_group::temporal_distance > 149' = '149+ days before elections'
  ),
  gof_map = c('nobs', 'r.squared'),
  colnames = c('', 'Q1', 'Q2', 'Q3', 'Q4', 'Q5'),
  format = 'latex',
  stars = TRUE
)


# Saving
writeLines(
  polarization_quintiles,
  'treating/Red Alerts/Output/Tables/4_quintiles_results.tex'
)

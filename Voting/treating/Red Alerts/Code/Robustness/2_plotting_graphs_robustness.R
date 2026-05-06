# This script plots alternative graphs (including all elections after 2015)
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
library(tidyverse)
library(broom)

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
election_percentages = read.csv('treating/Red Alerts/Output/Datasets/2_parties_percentages_panel.csv') %>% 
  mutate(
    year_election = year
  )


##### REGRESSIONS #####

# creating time to treatment variable
election_percentages = election_percentages %>% 
  mutate(
    
    time_to_treatment = case_when(
      year_election == 2006 ~ -3,
      year_election == 2009 ~ -2,
      year_election == 2013 ~ -1,
      year_election == 2015 ~ 0,
      year_election == 2019 ~ 1,
      year_election == 2019.2 ~ 2,
      year_election == 2020 ~ 3,
      year_election == 2021 ~ 4,
      year_election == 2022 ~ 5,
      TRUE ~ NA_real_ 
    )
    
  )



# excluding localities that did not exist in 2006 or 2009
election_percentages <- election_percentages %>%
  group_by(loc) %>%                
  filter(!any(is.na(likud_percentage))) %>% 
  ungroup() %>% 
  mutate(
    year_election = as.factor(year_election)
  )



####### REGRESSIONS #########


####################################
#### GRAPHS INCLUDING ALL YEARS ####
####################################

#### LIKUD ####


reg_1_likud =
  
  feols(likud_percentage ~ i(year_election, temporal_group,
                             ref = '2013', 
                             ref2 = 'no_red_alert')| 
          as.factor(SEMEL_YISHUV) + as.factor(year_election),
        data = election_percentages %>% filter(Religion_yishuv_Code != 2),
        cluster = ~SEMEL_YISHUV)


# Creating a data frame from the regression results
coef_data_likud <- as.data.frame(coeftable(reg_1_likud)) %>%
  rownames_to_column(var = "term") %>%
  filter(grepl("year_election::", term)) %>%
  mutate(term = gsub("year_election::", "", term),
         year_election = sub(":temporal_group::.*", "", term) %>% as.numeric(),
         temporal_group = gsub(".*:temporal_group::", "", term)) %>% 
rename(estimate = Estimate, std_error = `Std. Error`) %>%
  mutate(
    ci_lower = estimate - 1.96 * std_error,
    ci_upper = estimate + 1.96 * std_error
  )


# Adding the refference period (2013) to the dataframe
coef_data_likud <- coef_data_likud %>%
  bind_rows(data.frame(
    term = "Reference (2013)",
    year_election = 2013,
    estimate = 0,
    std_error = 0,
    ci_lower = 0,
    ci_upper = 0,
    temporal_group = c("temporal_distance == 6", "temporal_distance > 149")
  ))

# Setting uniform spacing in the x axis
coef_data_likud <- coef_data_likud %>%
  arrange(year_election) %>%
  mutate(year_position = as.numeric(factor(year_election, levels = unique(year_election)))) 



# Plotting
ggplot(coef_data_likud, aes(x = year_position, y = estimate, color = temporal_group)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 3, linetype = 'dashed') +  # Mark the reference year position
  geom_point(size = 4) +
  # Including Confidence Intervals (except for the reference year position)
  geom_errorbar(data = subset(coef_data_likud, year_position != 3), 
                aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_x_continuous(
    breaks = coef_data_likud$year_position,
    labels = coef_data_likud$year_election  # Use the actual years as labels
  ) +  # Sets uniform positions with actual year labels
  labs(
    x = "Election Year",
    y = "Difference in Likud's Average Vote Share",
    color = "Temporal Group"
  ) +
  theme_bw() +
  theme(
    legend.position = "right", 
    strip.background = element_blank(), 
    strip.text = element_text(size = 12),
    panel.spacing = unit(1, "lines"),
    panel.grid.minor.x = element_blank()   # Remove minor vertical grid lines
  ) +
  facet_wrap(~ temporal_group, 
             scales = "free_x",
             labeller = labeller(temporal_group = c(
               "temporal_distance == 6" = "Panel 1: Red Alert 6 Days Before Election",
               "temporal_distance > 149" = "Panel 2: Red Alert 149+ Days Before Election"
             )), 
             ncol = 1) + # Stack panels vertically
  scale_color_manual(values = c("temporal_distance == 6" = "blue", "temporal_distance > 149" = "red"), 
                     labels = c("temporal_distance == 6" = "6 Days", "temporal_distance > 149" = "More than 149 Days")) +  
  guides(color = guide_none()) + 
  scale_y_continuous(labels = scales::percent)  



ggsave('treating/Red Alerts/Output/Figures/Robustness/Robustness_ALL_YEARS_event_study_likud.pdf', 
       width = 8.5,
       height = 5.5)



##### 2015 Coalition #####

reg_1_coalition_2015 = feols(coalition_2015_percentage ~ i(year_election, temporal_group,
                                                           ref = '2013', ref2 = 'no_red_alert')| 
                               as.factor(SEMEL_YISHUV) + as.factor(year_election),
                             data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                             cluster = ~SEMEL_YISHUV)



# Creating a data frame from the regression results
coef_data_coalition_2015 <- as.data.frame(coeftable(reg_1_coalition_2015)) %>%
  rownames_to_column(var = "term") %>%
  filter(grepl("year_election::", term)) %>%
  mutate(term = gsub("year_election::", "", term),
         year_election = sub(":temporal_group::.*", "", term) %>% as.numeric(),
         temporal_group = gsub(".*:temporal_group::", "", term)) %>% 
  rename(estimate = Estimate, std_error = `Std. Error`) %>%
  mutate(
    ci_lower = estimate - 1.96 * std_error,
    ci_upper = estimate + 1.96 * std_error
  )


# Adding the refference period (2013) to the dataframe
coef_data_coalition_2015 <- coef_data_coalition_2015 %>%
  bind_rows(data.frame(
    term = "Reference (2013)",
    year_election = 2013,
    estimate = 0,
    std_error = 0,
    ci_lower = 0,
    ci_upper = 0,
    temporal_group = c("temporal_distance == 6", "temporal_distance > 149")
  ))




# Setting uniform spacing in the x axis
coef_data_coalition_2015 <- coef_data_coalition_2015 %>%
  arrange(year_election) %>%
  mutate(year_position = as.numeric(factor(year_election, levels = unique(year_election)))) 


# Plotting
ggplot(coef_data_coalition_2015, aes(x = year_position, y = estimate, color = temporal_group)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 3, linetype = 'dashed') +  # Mark the reference year position
  geom_point(size = 4) +
  # Including Confidence Intervals (except for the reference year position)
  geom_errorbar(data = subset(coef_data_coalition_2015, year_position != 3), 
                aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_x_continuous(
    breaks = coef_data_coalition_2015$year_position,
    labels = coef_data_coalition_2015$year_election  # Use the actual years as labels
  ) +  # Sets uniform positions with actual year labels
  labs(
    x = "Election Year",
    y = "Difference in 2015 Coalition's Average Vote Share",
    color = "Temporal Group"
  ) +
  theme_bw() +
  theme(
    legend.position = "right", 
    strip.background = element_blank(), 
    strip.text = element_text(size = 12),
    panel.spacing = unit(1, "lines"),
    panel.grid.minor.x = element_blank()   # Remove minor vertical grid lines
  ) +
  facet_wrap(~ temporal_group, 
             scales = "free_x",
             labeller = labeller(temporal_group = c(
               "temporal_distance == 6" = "Panel 1: Red Alert 6 Days Before Election",
               "temporal_distance > 149" = "Panel 2: Red Alert 149+ Days Before Election"
             )), 
             ncol = 1) + # Stack panels vertically
  scale_color_manual(values = c("temporal_distance == 6" = "blue", "temporal_distance > 149" = "red"), 
                     labels = c("temporal_distance == 6" = "6 Days", "temporal_distance > 149" = "More than 149 Days")) +  
  guides(color = guide_none()) + 
  scale_y_continuous(labels = scales::percent)



ggsave('treating/Red Alerts/Output/Figures/Robustness/Robustness_ALL_YEARS_event_study_coalition.pdf', 
       width = 8.5,
       height = 5.5)




##### RIGHT WING #####

reg_1_right_wing = feols(right_wing_percentage ~ i(year_election, temporal_group,
                                                   ref = '2013', ref2 = 'no_red_alert')| 
                           as.factor(SEMEL_YISHUV) + as.factor(year_election),
                    data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                    cluster = ~SEMEL_YISHUV)



# Creating a data frame from the regression results
coef_data_right_wing <- as.data.frame(coeftable(reg_1_right_wing)) %>%
  rownames_to_column(var = "term") %>%
  filter(grepl("year_election::", term)) %>%
  mutate(term = gsub("year_election::", "", term),
         year_election = sub(":temporal_group::.*", "", term) %>% as.numeric(),
         temporal_group = gsub(".*:temporal_group::", "", term)) %>% 
  rename(estimate = Estimate, std_error = `Std. Error`) %>%
  mutate(
    ci_lower = estimate - 1.96 * std_error,
    ci_upper = estimate + 1.96 * std_error
  )


# Adding the refference period (2013) to the dataframe
coef_data_right_wing <- coef_data_right_wing %>%
  bind_rows(data.frame(
    term = "Reference (2013)",
    year_election = 2013,
    estimate = 0,
    std_error = 0,
    ci_lower = 0,
    ci_upper = 0,
    temporal_group = c("temporal_distance == 6", "temporal_distance > 149")
  ))




# Setting uniform spacing in the x axis
coef_data_right_wing <- coef_data_right_wing %>%
  arrange(year_election) %>%
  mutate(year_position = as.numeric(factor(year_election, levels = unique(year_election)))) 


# Plotting
ggplot(coef_data_right_wing, aes(x = year_position, y = estimate, color = temporal_group)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 3, linetype = 'dashed') +  # Mark the reference year position
  geom_point(size = 4) +
  # Including Confidence Intervals (except for the reference year position)
  geom_errorbar(data = subset(coef_data_right_wing, year_position != 3), 
                aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_x_continuous(
    breaks = coef_data_right_wing$year_position,
    labels = coef_data_right_wing$year_election  # Use the actual years as labels
  ) +  # Sets uniform positions with actual year labels
  labs(
    x = "Election Year",
    y = "Difference in Right Wing's Average Vote Share",
    color = "Temporal Group"
  ) +
  theme_bw() +
  theme(
    legend.position = "right", 
    strip.background = element_blank(), 
    strip.text = element_text(size = 12),
    panel.spacing = unit(1, "lines"),
    panel.grid.minor.x = element_blank()   # Remove minor vertical grid lines
  ) +
  facet_wrap(~ temporal_group, 
             scales = "free_x",
             labeller = labeller(temporal_group = c(
               "temporal_distance == 6" = "Panel 1: Red Alert 6 Days Before Election",
               "temporal_distance > 149" = "Panel 2: Red Alert 149+ Days Before Election"
             )), 
             ncol = 1) + # Stack panels vertically
  scale_color_manual(values = c("temporal_distance == 6" = "blue", "temporal_distance > 149" = "red"), 
                     labels = c("temporal_distance == 6" = "6 Days", "temporal_distance > 149" = "More than 149 Days")) +  
  guides(color = guide_none()) + 
  scale_y_continuous(labels = scales::percent)



ggsave('treating/Red Alerts/Output/Figures/Robustness/Robustness_ALL_YEARS_event_study_right_wing.pdf', 
       width = 8.5,
       height = 5.5)



##### TURNOUT #####

reg_1_turnout = feols(turnout_percentage ~ i(year_election, temporal_group, ref = '2013',
                                             ref2 = 'no_red_alert')|
                        as.factor(SEMEL_YISHUV) + as.factor(year_election),
                         data = election_percentages %>% filter(Religion_yishuv_Code != 2),
                      cluster = ~SEMEL_YISHUV)



# Creating a data frame from the regression results
coef_data_turnout <- as.data.frame(coeftable(reg_1_turnout)) %>%
  rownames_to_column(var = "term") %>%
  filter(grepl("year_election::", term)) %>%
  mutate(term = gsub("year_election::", "", term),
         year_election = sub(":temporal_group::.*", "", term) %>% as.numeric(),
         temporal_group = gsub(".*:temporal_group::", "", term)) %>% 
  rename(estimate = Estimate, std_error = `Std. Error`) %>%
  mutate(
    ci_lower = estimate - 1.96 * std_error,
    ci_upper = estimate + 1.96 * std_error
  )


# Adding the refference period (2013) to the dataframe
coef_data_turnout <- coef_data_turnout %>%
  bind_rows(data.frame(
    term = "Reference (2013)",
    year_election = 2013,
    estimate = 0,
    std_error = 0,
    ci_lower = 0,
    ci_upper = 0,
    temporal_group = c("temporal_distance == 6", "temporal_distance > 149")
  ))



# Setting uniform spacing in the x axis
coef_data_turnout <- coef_data_turnout %>%
  arrange(year_election) %>%
  mutate(year_position = as.numeric(factor(year_election, levels = unique(year_election))))


# Plotting
ggplot(coef_data_turnout, aes(x = year_position, y = estimate, color = temporal_group)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 3, linetype = 'dashed') +  # Assuming 2013 is the reference year position
  geom_point(size = 4) +
  # Including Confidence Intervals (except for the reference year 2013)
  geom_errorbar(data = subset(coef_data_turnout, year_position != 3), 
                aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_x_continuous(
    breaks = coef_data_turnout$year_position,
    labels = coef_data_turnout$year_election  # Use the actual election years as labels
  ) +  # Sets uniform positions with actual year labels
  labs(
    x = "Election Year",
    y = "Difference in Average Turnout",
    color = "Temporal Group"
  ) +
  theme_bw() +
  theme(
    legend.position = "right", 
    strip.background = element_blank(), 
    strip.text = element_text(size = 12),
    panel.spacing = unit(1, "lines"),
    panel.grid.minor.x = element_blank()   # Remove minor vertical grid lines
  ) +
  facet_wrap(~ temporal_group, 
             scales = "free_x",
             labeller = labeller(temporal_group = c(
               "temporal_distance == 6" = "Panel 1: Red Alert 6 Days Before Election",
               "temporal_distance > 149" = "Panel 2: Red Alert 149+ Days Before Election"
             )), 
             ncol = 1) +  # Stack panels vertically
  scale_color_manual(values = c("temporal_distance == 6" = "blue", "temporal_distance > 149" = "red"), 
                     labels = c("temporal_distance == 6" = "6 Days", "temporal_distance > 149" = "More than 149 Days")) +  
  guides(color = guide_none()) + 
  scale_y_continuous(labels = scales::percent)  



ggsave('treating/Red Alerts/Output/Figures/Robustness/Robustness_ALL_YEARS_event_study_turnout.pdf', 
       width = 8.5,
       height = 5.5)


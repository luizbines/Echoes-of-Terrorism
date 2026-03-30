# This script plots the graphs
# Luiz Bines
# 2024

# Library
library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
library(ggplot2)
library(sf)
library(fixest)
library(modelsummary)
library(geosphere)
library(lubridate)
library(scales)
library(stargazer)
library(units)
library(stringi)
library(fixest)
library(stringdist)
library(gridExtra)
library(lmtest)
library(sandwich)
library(clubSandwich)
library(broom)
library(purrr)
library(tibble)


# Directory
wd = 'C:/Users/luizb/Desktop/Dissertation/Red-Alerts-and-Votes/'
setwd(wd);

# Importing
likud_percentage = read.csv('treating/Red Alerts/Output/2_likud_percentage_panel.csv')
right_wing_percentage = read.csv('treating/Red Alerts/Output/2_right_wing_percentage_panel.csv')
turnout_percentage = read.csv('treating/Red Alerts/Output/2_turnout_percentage_panel.csv')

election_percentages = read.csv('treating/Red Alerts/Output/3_election_percentages.csv')



# Fixing years (including both 2019 elections)

election_percentages = election_percentages %>% 
  select(-year) %>% 
  rename(year = year_election)



# Fixing NA Religion Code Yishuv
election_percentages = election_percentages %>% 
  group_by(SEMEL_YISHUV) %>%
    mutate(Religion_yishuv_Code = ifelse(is.na(Religion_yishuv_Code),
                                         first(Religion_yishuv_Code[!is.na(Religion_yishuv_Code)]),
                                         Religion_yishuv_Code)) %>%
    ungroup()



### LIKUD ###

# Model estimation
model = feols(likud_percentage ~ temporal_group * post,
              data = election_percentages %>% filter(Religion_yishuv_Code != 2 & 
                                                       year >= 2013 & year <= 2015),
              cluster = 'SEMEL_YISHUV')

# Confidence intervals
confint_results <- confint(model)

# Transforming CIs into a dataframe
confint_df <- as.data.frame(confint_results) %>%
  rownames_to_column(var = "term") %>%
  rename(conf_low = `2.5 %`, conf_high = `97.5 %`) %>%
  mutate(estimate = model$coefficients)  # Adding estimates

# Filtering the terms of interest
plot_data_cis <- confint_df %>%
  filter(term %in% c("temporal_grouptemporal_distance == 6:postTRUE", 
                     "temporal_grouptemporal_distance == 6",
                     "temporal_grouptemporal_distance > 200",
                     "temporal_grouptemporal_distance > 200:postTRUE")) %>%
  mutate(term = factor(term, levels = c("temporal_grouptemporal_distance == 6", 
                                        "temporal_grouptemporal_distance == 6:postTRUE",
                                        "temporal_grouptemporal_distance > 200",
                                        "temporal_grouptemporal_distance > 200:postTRUE")))  # Ensuring the order


plot_data_cis <- plot_data_cis %>%
  mutate(
    year = ifelse(grepl("postTRUE", term), 2015, 2013) %>% as.numeric(),  # Assign year based on term
    temporal_group = ifelse(grepl("temporal_distance == 6", term), 
                            "temporal_distance == 6", 
                            "temporal_distance > 200")  # Assign temporal_group based on term
  ) %>% 
  select(-term)


# Grouping the data by temporal_group and modifying estimates for the year 2015
plot_data_cis <- plot_data_cis %>% 
  group_by(temporal_group) %>% 
  mutate(
    estimate = ifelse(year == 2015, estimate + first(estimate), estimate)  # Adjusting estimates for 2015 by adding the 2013 estimate
  ) %>% filter(year != 2015)  # Filtering out the year 2015 for further calculations

# Extracting the variance-covariance matrix
vcov_matrix <- vcov(model)

# Calculations for temporal_distance == 6
# Coefficients for the terms of interest
coef_2013_6 <- coef(model)['temporal_grouptemporal_distance == 6']  # Coefficient for 2013
coef_2015_6 <- coef(model)['temporal_grouptemporal_distance == 6:postTRUE']  # Coefficient for 2015

# Standard errors for the coefficients
se_2013_6 <- sqrt(vcov_matrix['temporal_grouptemporal_distance == 6', 'temporal_grouptemporal_distance == 6'])  # SE for 2013
se_2015_6 <- sqrt(vcov_matrix['temporal_grouptemporal_distance == 6:postTRUE', 'temporal_grouptemporal_distance == 6:postTRUE'])  # SE for 2015

# Covariance between the coefficients
cov_2013_2015_6 <- vcov_matrix['temporal_grouptemporal_distance == 6', 'temporal_grouptemporal_distance == 6:postTRUE']  # Covariance between 2013 and 2015

# Combined standard error
se_combined_6 <- sqrt(se_2013_6^2 + se_2015_6^2 + 2 * cov_2013_2015_6)  # Calculating combined SE

# Combined coefficient for 2015
coef_combined_6 <- coef_2013_6 + coef_2015_6  # Adding coefficients for 2013 and 2015

# Confidence interval for 2015
ci_low_6 <- coef_combined_6 - 1.96 * se_combined_6  # Lower bound of the CI
ci_high_6 <- coef_combined_6 + 1.96 * se_combined_6  # Upper bound of the CI

# Creating a dataframe for temporal_distance == 6 in 2015
combined_2015_df_6 <- data.frame(
  estimate = coef_combined_6,
  conf_low = ci_low_6,
  conf_high = ci_high_6,
  year = 2015,
  temporal_group = "temporal_distance == 6"
)

# Calculations for temporal_distance > 200
# Coefficients for the terms of interest
coef_2013_200 <- coef(model)['temporal_grouptemporal_distance > 200']  # Coefficient for 2013
coef_2015_200 <- coef(model)['temporal_grouptemporal_distance > 200:postTRUE']  # Coefficient for 2015

# Standard errors for the coefficients
se_2013_200 <- sqrt(vcov_matrix['temporal_grouptemporal_distance > 200', 'temporal_grouptemporal_distance > 200'])  # SE for 2013
se_2015_200 <- sqrt(vcov_matrix['temporal_grouptemporal_distance > 200:postTRUE', 'temporal_grouptemporal_distance > 200:postTRUE'])  # SE for 2015

# Covariance between the coefficients
cov_2013_2015_200 <- vcov_matrix['temporal_grouptemporal_distance > 200', 'temporal_grouptemporal_distance > 200:postTRUE']  # Covariance between 2013 and 2015

# Combined standard error
se_combined_200 <- sqrt(se_2013_200^2 + se_2015_200^2 + 2 * cov_2013_2015_200)  # Calculating combined SE

# Combined coefficient for 2015
coef_combined_200 <- coef_2013_200 + coef_2015_200  # Adding coefficients for 2013 and 2015

# Confidence interval for 2015
ci_low_200 <- coef_combined_200 - 1.96 * se_combined_200  # Lower bound of the CI
ci_high_200 <- coef_combined_200 + 1.96 * se_combined_200  # Upper bound of the CI

# Creating a dataframe for temporal_distance > 200 in 2015
combined_2015_df_200 <- data.frame(
  estimate = coef_combined_200,
  conf_low = ci_low_200,
  conf_high = ci_high_200,
  year = 2015,
  temporal_group = "temporal_distance > 200"
)

# Combining the 2015 results into the original dataframe
plot_data_cis <- plot_data_cis %>%
  bind_rows(combined_2015_df_6, combined_2015_df_200)  # Merging the new 2015 data into the main dataframe


# Prepare data for other years (without CIs)
# Create a dataframe for the estimates of all years
years_data <- election_percentages %>%
  filter(Religion_yishuv_Code != 2 & year != 2013 & year != 2015) %>%
  group_by(year, temporal_group) %>%
  summarize(estimate = mean(likud_percentage, na.rm = TRUE), .groups = 'drop') %>%
  mutate(year = as.numeric(year))  # Ensure year is numeric

# Get the estimate for "no_red_alert" for each year
no_red_alerts_data <- years_data %>%
  filter(temporal_group == "no_red_alert") %>%
  select(year, no_red_alerts_estimate = estimate)  # Select year and estimate for "no_red_alert"

# Join the no_red_alerts data back to the years_data
years_data_with_differences <- years_data %>%
  left_join(no_red_alerts_data, by = "year") %>%
  mutate(estimate = estimate - no_red_alerts_estimate) %>%  # Calculate the difference
  select(year, temporal_group, estimate) %>% 
  mutate(
    conf_low = NA,
    conf_high = NA
  )
  
  
plot_data_combined <- bind_rows(plot_data_cis, years_data_with_differences) %>% 
  arrange(year) %>%
  filter(temporal_group != "no_red_alert") %>% 
  mutate(
    event_time = case_when(
      year == 2006 ~ "-3",
      year == 2009 ~ "-2",
      year == 2013 ~ "-1",
      year == 2015 ~ "0",
      year == 2019 ~ "1",
      year == 2019.4 ~ "2",
      year == 2020 ~ "3",
      year == 2021 ~ "4",
      year == 2022 ~ "5"
    ),
    
    # uniformly spacing
    event_time = factor(year, levels = c(2006, 2009, 2013, 2015, 2019, 2019.4, 2020, 2021, 2022))  
  )



# Create the base plot
base_plot <- ggplot(plot_data_combined, aes(x = event_time, y = estimate * 100, group = temporal_group, color = temporal_group)) +  
  geom_point(size = 3) +  
  geom_hline(yintercept = 0, linetype = "dashed") +  
  geom_rect(aes(xmin = 3, xmax = 4, ymin = -Inf, ymax = Inf),
            fill = "blue", color = NA, alpha = 0.01) +
  geom_line(size = 1.2) +  
  geom_errorbar(aes(ymin = conf_low * 100, ymax = conf_high * 100), width = 0.2, size = 0.8) +  
  labs(
       x = "Number of Elections Relative to 2015 Election",
       y = "Difference in Average Share of Votes (%)",
       caption = "95% Confidence Intervals") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(hjust = 0, size = 14)) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1))  


# Add facets to separate plots by temporal_group and set them to be one above the other
final_plot <- base_plot + 
  facet_wrap(~ temporal_group, scales = "fixed", nrow = 2, labeller = labeller(temporal_group = c(
    "temporal_distance == 6" = "Panel A: Localities with Red Alerts 6 Days before the 2015 Election",
    "temporal_distance > 200" = "Panel B: Localities with Red Alerts More Than 200 Days before the 2015 Election"
  )))  # Criar painéis um acima do outro com subtítulos

# Save the final plot
pdf("treating/Red Alerts/Output/Figures/likud_CIs.pdf", width = 8.5, height = 5.5)

final_plot
dev.off()


### RIGHT WING ###

# Model estimation
model = feols(right_wing_percentage ~ temporal_group * post,
              data = election_percentages %>% filter(Religion_yishuv_Code != 2 & 
                                                       year >= 2013 & year <= 2015),
              cluster = 'SEMEL_YISHUV')

# Confidence intervals
confint_results <- confint(model)

# Transforming CIs into a dataframe
confint_df <- as.data.frame(confint_results) %>%
  rownames_to_column(var = "term") %>%
  rename(conf_low = `2.5 %`, conf_high = `97.5 %`) %>%
  mutate(estimate = model$coefficients)  # Adding estimates

# Filtering the terms of interest
plot_data_cis <- confint_df %>%
  filter(term %in% c("temporal_grouptemporal_distance == 6:postTRUE", 
                     "temporal_grouptemporal_distance == 6",
                     "temporal_grouptemporal_distance > 200",
                     "temporal_grouptemporal_distance > 200:postTRUE")) %>%
  mutate(term = factor(term, levels = c("temporal_grouptemporal_distance == 6", 
                                        "temporal_grouptemporal_distance == 6:postTRUE",
                                        "temporal_grouptemporal_distance > 200",
                                        "temporal_grouptemporal_distance > 200:postTRUE")))  # Ensuring the order


plot_data_cis <- plot_data_cis %>%
  mutate(
    year = ifelse(grepl("postTRUE", term), 2015, 2013) %>% as.numeric(),  # Assign year based on term
    temporal_group = ifelse(grepl("temporal_distance == 6", term), 
                            "temporal_distance == 6", 
                            "temporal_distance > 200")  # Assign temporal_group based on term
  ) %>% 
  select(-term)

# Grouping the data by temporal_group and modifying estimates for the year 2015
plot_data_cis <- plot_data_cis %>% 
  group_by(temporal_group) %>% 
  mutate(
    estimate = ifelse(year == 2015, estimate + first(estimate), estimate)  # Adjusting estimates for 2015 by adding the 2013 estimate
  ) %>% filter(year != 2015)  # Filtering out the year 2015 for further calculations

# Extracting the variance-covariance matrix
vcov_matrix <- vcov(model)

# Calculations for temporal_distance == 6
# Coefficients for the terms of interest
coef_2013_6 <- coef(model)['temporal_grouptemporal_distance == 6']  # Coefficient for 2013
coef_2015_6 <- coef(model)['temporal_grouptemporal_distance == 6:postTRUE']  # Coefficient for 2015

# Standard errors for the coefficients
se_2013_6 <- sqrt(vcov_matrix['temporal_grouptemporal_distance == 6', 'temporal_grouptemporal_distance == 6'])  # SE for 2013
se_2015_6 <- sqrt(vcov_matrix['temporal_grouptemporal_distance == 6:postTRUE', 'temporal_grouptemporal_distance == 6:postTRUE'])  # SE for 2015

# Covariance between the coefficients
cov_2013_2015_6 <- vcov_matrix['temporal_grouptemporal_distance == 6', 'temporal_grouptemporal_distance == 6:postTRUE']  # Covariance between 2013 and 2015

# Combined standard error
se_combined_6 <- sqrt(se_2013_6^2 + se_2015_6^2 + 2 * cov_2013_2015_6)  # Calculating combined SE

# Combined coefficient for 2015
coef_combined_6 <- coef_2013_6 + coef_2015_6  # Adding coefficients for 2013 and 2015

# Confidence interval for 2015
ci_low_6 <- coef_combined_6 - 1.96 * se_combined_6  # Lower bound of the CI
ci_high_6 <- coef_combined_6 + 1.96 * se_combined_6  # Upper bound of the CI

# Creating a dataframe for temporal_distance == 6 in 2015
combined_2015_df_6 <- data.frame(
  estimate = coef_combined_6,
  conf_low = ci_low_6,
  conf_high = ci_high_6,
  year = 2015,
  temporal_group = "temporal_distance == 6"
)

# Calculations for temporal_distance > 200
# Coefficients for the terms of interest
coef_2013_200 <- coef(model)['temporal_grouptemporal_distance > 200']  # Coefficient for 2013
coef_2015_200 <- coef(model)['temporal_grouptemporal_distance > 200:postTRUE']  # Coefficient for 2015

# Standard errors for the coefficients
se_2013_200 <- sqrt(vcov_matrix['temporal_grouptemporal_distance > 200', 'temporal_grouptemporal_distance > 200'])  # SE for 2013
se_2015_200 <- sqrt(vcov_matrix['temporal_grouptemporal_distance > 200:postTRUE', 'temporal_grouptemporal_distance > 200:postTRUE'])  # SE for 2015

# Covariance between the coefficients
cov_2013_2015_200 <- vcov_matrix['temporal_grouptemporal_distance > 200', 'temporal_grouptemporal_distance > 200:postTRUE']  # Covariance between 2013 and 2015

# Combined standard error
se_combined_200 <- sqrt(se_2013_200^2 + se_2015_200^2 + 2 * cov_2013_2015_200)  # Calculating combined SE

# Combined coefficient for 2015
coef_combined_200 <- coef_2013_200 + coef_2015_200  # Adding coefficients for 2013 and 2015

# Confidence interval for 2015
ci_low_200 <- coef_combined_200 - 1.96 * se_combined_200  # Lower bound of the CI
ci_high_200 <- coef_combined_200 + 1.96 * se_combined_200  # Upper bound of the CI

# Creating a dataframe for temporal_distance > 200 in 2015
combined_2015_df_200 <- data.frame(
  estimate = coef_combined_200,
  conf_low = ci_low_200,
  conf_high = ci_high_200,
  year = 2015,
  temporal_group = "temporal_distance > 200"
)

# Combining the 2015 results into the original dataframe
plot_data_cis <- plot_data_cis %>%
  bind_rows(combined_2015_df_6, combined_2015_df_200)  # Merging the new 2015 data into the main dataframe


# Prepare data for other years (without CIs)
# Create a dataframe for the estimates of all years
years_data <- election_percentages %>%
  filter(Religion_yishuv_Code != 2 & year != 2013 & year != 2015) %>%
  group_by(year, temporal_group) %>%
  summarize(estimate = mean(right_wing_percentage, na.rm = TRUE), .groups = 'drop') %>%
  mutate(year = as.numeric(year))  # Ensure year is numeric

# Get the estimate for "no_red_alert" for each year
no_red_alerts_data <- years_data %>%
  filter(temporal_group == "no_red_alert") %>%
  select(year, no_red_alerts_estimate = estimate)  # Select year and estimate for "no_red_alert"

# Join the no_red_alerts data back to the years_data
years_data_with_differences <- years_data %>%
  left_join(no_red_alerts_data, by = "year") %>%
  mutate(estimate = estimate - no_red_alerts_estimate) %>%  # Calculate the difference
  select(year, temporal_group, estimate) %>% 
  mutate(
    conf_low = NA,
    conf_high = NA
  )


plot_data_combined <- bind_rows(plot_data_cis, years_data_with_differences) %>% 
  arrange(year) %>%
  filter(temporal_group != "no_red_alert") %>% 
  mutate(
    event_time = case_when(
      year == 2006 ~ "-3",
      year == 2009 ~ "-2",
      year == 2013 ~ "-1",
      year == 2015 ~ "0",
      year == 2019 ~ "1",
      year == 2019.4 ~ "2",
      year == 2020 ~ "3",
      year == 2021 ~ "4",
      year == 2022 ~ "5"
    ),
    
    event_time = factor(year, levels = c(2006, 2009, 2013, 2015, 2019, 2019.4, 2020, 2021, 2022))  
  )



# Create the base plot
base_plot <- ggplot(plot_data_combined, aes(x = event_time, y = estimate * 100, group = temporal_group, color = temporal_group)) +  
  geom_point(size = 3) +  
  geom_hline(yintercept = 0, linetype = "dashed") +  
  geom_rect(aes(xmin = 3, xmax = 4, ymin = -Inf, ymax = Inf),
            fill = "blue", color = NA, alpha = 0.01) +
  geom_line(size = 1.2) +  
  geom_errorbar(aes(ymin = conf_low * 100, ymax = conf_high * 100), width = 0.2, size = 0.8) +  
  labs(
    x = "Number of Elections Relative to 2015 Election",
    y = "Difference in Average Share of Votes (%)",
    caption = "95% Confidence Intervals") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(hjust = 0, size = 14)) +  
  scale_y_continuous(labels = scales::percent_format(scale = 1))  


# Add facets to separate plots by temporal_group and set them to be one above the other
final_plot <- base_plot + 
  facet_wrap(~ temporal_group, scales = "fixed", nrow = 2, labeller = labeller(temporal_group = c(
    "temporal_distance == 6" = "Panel A: Localities with Red Alerts 6 Days before the 2015 Election",
    "temporal_distance > 200" = "Panel B: Localities with Red Alerts More Than 200 Days before the 2015 Election"
  )))  # Criar painéis um acima do outro com subtítulos

# Save the final plot
pdf("treating/Red Alerts/Output/Figures/right_wing_CIs.pdf", width = 8.5, height = 5.5)

final_plot
dev.off()








### TURNOUT ###

# Model estimation
model = feols(turnout_percentage ~ temporal_group * post,
              data = election_percentages %>% filter(Religion_yishuv_Code != 2 & 
                                                       year >= 2013 & year <= 2015),
              cluster = 'SEMEL_YISHUV')

# Confidence intervals
confint_results <- confint(model)

# Transforming CIs into a dataframe
confint_df <- as.data.frame(confint_results) %>%
  rownames_to_column(var = "term") %>%
  rename(conf_low = `2.5 %`, conf_high = `97.5 %`) %>%
  mutate(estimate = model$coefficients)  # Adding estimates

# Filtering the terms of interest
plot_data_cis <- confint_df %>%
  filter(term %in% c("temporal_grouptemporal_distance == 6:postTRUE", 
                     "temporal_grouptemporal_distance == 6",
                     "temporal_grouptemporal_distance > 200",
                     "temporal_grouptemporal_distance > 200:postTRUE")) %>%
  mutate(term = factor(term, levels = c("temporal_grouptemporal_distance == 6", 
                                        "temporal_grouptemporal_distance == 6:postTRUE",
                                        "temporal_grouptemporal_distance > 200",
                                        "temporal_grouptemporal_distance > 200:postTRUE")))  # Ensuring the order


plot_data_cis <- plot_data_cis %>%
  mutate(
    year = ifelse(grepl("postTRUE", term), 2015, 2013) %>% as.numeric(),  # Assign year based on term
    temporal_group = ifelse(grepl("temporal_distance == 6", term), 
                            "temporal_distance == 6", 
                            "temporal_distance > 200")  # Assign temporal_group based on term
  ) %>% 
  select(-term)


# Grouping the data by temporal_group and modifying estimates for the year 2015
plot_data_cis <- plot_data_cis %>% 
  group_by(temporal_group) %>% 
  mutate(
    estimate = ifelse(year == 2015, estimate + first(estimate), estimate)  # Adjusting estimates for 2015 by adding the 2013 estimate
  ) %>% filter(year != 2015)  # Filtering out the year 2015 for further calculations

# Extracting the variance-covariance matrix
vcov_matrix <- vcov(model)

# Calculations for temporal_distance == 6
# Coefficients for the terms of interest
coef_2013_6 <- coef(model)['temporal_grouptemporal_distance == 6']  # Coefficient for 2013
coef_2015_6 <- coef(model)['temporal_grouptemporal_distance == 6:postTRUE']  # Coefficient for 2015

# Standard errors for the coefficients
se_2013_6 <- sqrt(vcov_matrix['temporal_grouptemporal_distance == 6', 'temporal_grouptemporal_distance == 6'])  # SE for 2013
se_2015_6 <- sqrt(vcov_matrix['temporal_grouptemporal_distance == 6:postTRUE', 'temporal_grouptemporal_distance == 6:postTRUE'])  # SE for 2015

# Covariance between the coefficients
cov_2013_2015_6 <- vcov_matrix['temporal_grouptemporal_distance == 6', 'temporal_grouptemporal_distance == 6:postTRUE']  # Covariance between 2013 and 2015

# Combined standard error
se_combined_6 <- sqrt(se_2013_6^2 + se_2015_6^2 + 2 * cov_2013_2015_6)  # Calculating combined SE

# Combined coefficient for 2015
coef_combined_6 <- coef_2013_6 + coef_2015_6  # Adding coefficients for 2013 and 2015

# Confidence interval for 2015
ci_low_6 <- coef_combined_6 - 1.96 * se_combined_6  # Lower bound of the CI
ci_high_6 <- coef_combined_6 + 1.96 * se_combined_6  # Upper bound of the CI

# Creating a dataframe for temporal_distance == 6 in 2015
combined_2015_df_6 <- data.frame(
  estimate = coef_combined_6,
  conf_low = ci_low_6,
  conf_high = ci_high_6,
  year = 2015,
  temporal_group = "temporal_distance == 6"
)

# Calculations for temporal_distance > 200
# Coefficients for the terms of interest
coef_2013_200 <- coef(model)['temporal_grouptemporal_distance > 200']  # Coefficient for 2013
coef_2015_200 <- coef(model)['temporal_grouptemporal_distance > 200:postTRUE']  # Coefficient for 2015

# Standard errors for the coefficients
se_2013_200 <- sqrt(vcov_matrix['temporal_grouptemporal_distance > 200', 'temporal_grouptemporal_distance > 200'])  # SE for 2013
se_2015_200 <- sqrt(vcov_matrix['temporal_grouptemporal_distance > 200:postTRUE', 'temporal_grouptemporal_distance > 200:postTRUE'])  # SE for 2015

# Covariance between the coefficients
cov_2013_2015_200 <- vcov_matrix['temporal_grouptemporal_distance > 200', 'temporal_grouptemporal_distance > 200:postTRUE']  # Covariance between 2013 and 2015

# Combined standard error
se_combined_200 <- sqrt(se_2013_200^2 + se_2015_200^2 + 2 * cov_2013_2015_200)  # Calculating combined SE

# Combined coefficient for 2015
coef_combined_200 <- coef_2013_200 + coef_2015_200  # Adding coefficients for 2013 and 2015

# Confidence interval for 2015
ci_low_200 <- coef_combined_200 - 1.96 * se_combined_200  # Lower bound of the CI
ci_high_200 <- coef_combined_200 + 1.96 * se_combined_200  # Upper bound of the CI

# Creating a dataframe for temporal_distance > 200 in 2015
combined_2015_df_200 <- data.frame(
  estimate = coef_combined_200,
  conf_low = ci_low_200,
  conf_high = ci_high_200,
  year = 2015,
  temporal_group = "temporal_distance > 200"
)

# Combining the 2015 results into the original dataframe
plot_data_cis <- plot_data_cis %>%
  bind_rows(combined_2015_df_6, combined_2015_df_200)  # Merging the new 2015 data into the main dataframe

# Prepare data for other years (without CIs)
# Create a dataframe for the estimates of all years
years_data <- election_percentages %>%
  filter(Religion_yishuv_Code != 2 & year != 2013 & year != 2015 ) %>%
  group_by(year, temporal_group) %>%
  summarize(estimate = mean(turnout_percentage, na.rm = TRUE), .groups = 'drop') %>%
  mutate(year = as.numeric(year))  # Ensure year is numeric

# Get the estimate for "no_red_alert" for each year
no_red_alerts_data <- years_data %>%
  filter(temporal_group == "no_red_alert") %>%
  select(year, no_red_alerts_estimate = estimate)  # Select year and estimate for "no_red_alert"

# Join the no_red_alerts data back to the years_data
years_data_with_differences <- years_data %>%
  left_join(no_red_alerts_data, by = "year") %>%
  mutate(estimate = estimate - no_red_alerts_estimate) %>%  # Calculate the difference
  select(year, temporal_group, estimate) %>% 
  mutate(
    conf_low = NA,
    conf_high = NA
  )


plot_data_combined <- bind_rows(plot_data_cis, years_data_with_differences) %>% 
  arrange(year) %>%
  filter(temporal_group != "no_red_alert") %>% 
  mutate(
    event_time = case_when(
      year == 2006 ~ "-3",
      year == 2009 ~ "-2",
      year == 2013 ~ "-1",
      year == 2015 ~ "0",
      year == 2019 ~ "1",
      year == 2019.4 ~ "2",
      year == 2020 ~ "3",
      year == 2021 ~ "4",
      year == 2022 ~ "5"
    ),
    
    event_time = factor(year, levels = c(2006, 2009, 2013, 2015, 2019, 2019.4, 2020, 2021, 2022))  # Anos com espaçamento uniforme
  )



# Create the base plot
base_plot <- ggplot(plot_data_combined, aes(x = event_time, y = estimate * 100, group = temporal_group, color = temporal_group)) +  
  geom_point(size = 3) +  
  geom_hline(yintercept = 0, linetype = "dashed") +  
  geom_rect(aes(xmin = 3, xmax = 4, ymin = -Inf, ymax = Inf),
            fill = "blue", color = NA, alpha = 0.01) +
  geom_line(size = 1.2) +  
  geom_errorbar(aes(ymin = conf_low * 100, ymax = conf_high * 100), width = 0.2, size = 0.8) +  
  labs(
    x = "Number of Elections Relative to 2015 Election",
    y = "Difference in Average Share of Votes (%)",
    caption = "95% Confidence Intervals") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(hjust = 0, size = 14)) +  # Aumentar a fonte dos subtítulos dos painéis
  scale_y_continuous(labels = scales::percent_format(scale = 1))  


# Add facets to separate plots by temporal_group and set them to be one above the other
final_plot <- base_plot + 
  facet_wrap(~ temporal_group, scales = "fixed", nrow = 2, labeller = labeller(temporal_group = c(
    "temporal_distance == 6" = "Panel A: Localities with Red Alerts 6 Days before the 2015 Election",
    "temporal_distance > 200" = "Panel B: Localities with Red Alerts More Than 200 Days before the 2015 Election"
  )))  # Criar painéis um acima do outro com subtítulos

# Save the final plot
pdf("treating/Red Alerts/Output/Figures/turnout_CIs.pdf", width = 8.5, height = 5.5)

final_plot
dev.off()
















# # Calculating means
# likud_mean = election_percentages %>%
#   # mutate(
#   #   Religion_yishuv_Code = ifelse(is.na(Religion_yishuv_Code),
#   #                                 1,
#   #                                 Religion_yishuv_Code)
#   # ) %>%
#   filter(Religion_yishuv_Code != 2) %>%
#   group_by(year,
#            temporal_group
#   ) %>%
#   summarise(
#     mean_votes = mean(likud_percentage, na.rm = TRUE),
#     sd_votes = sd(likud_percentage, na.rm = TRUE),
#     n_votes = n()
#   )
# 
# 
# 
# # likud_mean_weighted = likud_percentage %>%
# #   mutate(
# #     Religion_yishuv_Code = ifelse(is.na(Religion_yishuv_Code),
# #                                   1,
# #                                   Religion_yishuv_Code)
# #   ) %>%
# #   filter(Religion_yishuv_Code != 2) %>%
# #   group_by(year,
# #            temporal_group
# #   ) %>%
# #   summarise(mean_votes = weighted.mean(x = likud_percentage, w = Pop_Total, na.rm = TRUE))
# 
# 
# 
# right_wing_mean = right_wing_percentage %>%
#   # mutate(
#   #   Religion_yishuv_Code = ifelse(is.na(Religion_yishuv_Code),
#   #                                 1,
#   #                                 Religion_yishuv_Code)
#   # ) %>%
#   filter(Religion_yishuv_Code != 2) %>%
#   group_by(year,
#            temporal_group
#   ) %>%
#   summarise(mean_votes = mean(right_wing_percentage, na.rm = T))
# 
# turnout_mean = turnout_percentage %>%
#   # mutate(
#   #   Religion_yishuv_Code = ifelse(is.na(Religion_yishuv_Code),
#   #                                 1,
#   #                                 Religion_yishuv_Code)
#   # ) %>%
#   filter(Religion_yishuv_Code != 2) %>%
#   group_by(year,
#            temporal_group
#   ) %>%
#   summarise(mean_votes = mean(turnout_percentage, na.rm = T))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ### EVENT STUDY ###
# 
# # Plotting
#   # diff_6_vs_no_alert
# plot_6 <- ggplot(likud_diff, aes(x = event_time)) +
#   geom_rect(aes(xmin = 3, xmax = 4, ymin = -Inf, ymax = Inf),
#             fill = "blue", alpha = 0.01) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#   geom_line(aes(y = diff_6_vs_no_alert), color = "blue", size = 1) +
#   geom_point(aes(y = diff_6_vs_no_alert), color = "blue") +
#   geom_errorbar(aes(ymin = ci_lower_6, ymax = ci_upper_6), width = 0.2, color = "blue") +
#   labs(x = "Number of Elections Relative to 2015 Election",
#        y = "Difference in Average Share of Votes",
#        subtitle = "Panel A: Localities with Red Alerts 6 Days before the 2015 Election") +
#   scale_y_continuous(labels = percent_format(), limits = c(-0.03, 0.11)) +
#   scale_x_discrete(labels = c("-3", "-2", "-1", "0", "1", "2", "3", "4", "5")) +
#   theme_minimal()
# 
#   # diff_200_vs_no_alert
# plot_200 <- ggplot(likud_diff, aes(x = event_time)) +
#   geom_rect(aes(xmin = 3, xmax = 4, ymin = -Inf, ymax = Inf),
#             fill = "blue", alpha = 0.01) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#   geom_line(aes(y = diff_200_vs_no_alert), color = "red", size = 1) +
#   geom_point(aes(y = diff_200_vs_no_alert), color = "red") +
#   geom_errorbar(aes(ymin = ci_lower_200, ymax = ci_upper_200), width = 0.2, color = "red") +
#   labs(x = "Number of Elections Relative to 2015 Election",
#        y = "Difference in Average Share of Votes",
#        subtitle = "Panel B: Localities with Red Alerts 200+ Days before the 2015 Election") +
#   scale_x_discrete(labels = c("-3", "-2", "-1", "0", "1", "2", "3", "4", "5")) +
#   scale_y_continuous(labels = percent_format(), limits = c(-0.06, 0.11)) +
#   theme_minimal()
# 
# # Plotting both panels
# pdf("treating/Red Alerts/Output/Figures/ICs_event_study_likud_percentage.pdf", width = 8.5, height = 5.5)
# 
# combined_plot = grid.arrange(plot_6, plot_200, ncol = 1)
# 
# # Close the device
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# # RIGHT WING
# # Calculate differences in mean votes relative to 'no_red_alert'
# right_wing_diff <- right_wing_mean %>%
#   pivot_wider(names_from = temporal_group, values_from = mean_votes) %>%
#   mutate(
#     diff_6_vs_no_alert = `temporal_distance == 6` - no_red_alert,
#     diff_200_vs_no_alert = `temporal_distance > 200` - no_red_alert,
#     # Create the event_time variable based on the year column
#     event_time = case_when(
#       year == 2006 ~ "-3",
#       year == 2009 ~ "-2",
#       year == 2013 ~ "-1",
#       year == 2015 ~ "0",
#       year == 2019 ~ "1",
#       year == 2019.4 ~ "2",  # assuming the second election in 2019 is marked as 2019.4
#       year == 2020 ~ "3",
#       year == 2021 ~ "4",
#       year == 2022 ~ "5"
#     )
#   )
# 
# # Convert event_time to a factor to ensure uniform spacing
# right_wing_diff$event_time <- factor(right_wing_diff$event_time, levels = c("-3", "-2", "-1", "0", "1", "2", "3", "4", "5"))
# 
# # Create the event-study plot with differences
# ggplot(right_wing_diff, aes(x = event_time)) +
#   geom_rect(aes(xmin = 3, xmax = 4., ymin = -Inf, ymax = Inf),
#             fill = "blue", alpha = 0.01) +
#   geom_line(aes(y = diff_6_vs_no_alert, group = 1, color = "6 days vs No Red Alert", linetype = "6 days vs No Red Alert"), size = 0.5) +
#   geom_line(aes(y = diff_200_vs_no_alert, group = 1, color = "200+ days vs No Red Alert", linetype = "200+ days vs No Red Alert"), size = 0.5) +
#   geom_vline(xintercept = "0", linetype = "dashed", color = "black", size = 0.5) +
#   labs(
#     x = "Number of Elections Relative to 2015 Election",
#     y = "Difference in Right Wing's Average Share of Votes",
#     color = "Comparison to Control Group:",
#     linetype = "Comparison to Control Group:"
#   ) +
#   scale_color_manual(values = c("grey55", "grey25")) +
#   scale_linetype_manual(values = c(2, 1)) + # Differentiate line types
#   scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.1, 0.1)) +
#   scale_x_discrete(labels = c("-3", "-2", "-1", "0", "1", "2", "3", "4", "5")) +  # Display event time on x-axis
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# ggsave('treating/Red Alerts/Output/Figures/event_study_right_wing_percentage.pdf',
#        width = 8.5,
#        height = 5.5)
# 
# 
# 
# # TURNOUT
# 
# # Calculate differences in mean votes relative to 'no_red_alert'
# turnout_diff <- turnout_mean %>%
#   pivot_wider(names_from = temporal_group, values_from = mean_votes) %>%
#   mutate(
#     diff_6_vs_no_alert = `temporal_distance == 6` - no_red_alert,
#     diff_200_vs_no_alert = `temporal_distance > 200` - no_red_alert,
#     # Create the event_time variable based on the year column
#     event_time = case_when(
#       year == 2006 ~ "-3",
#       year == 2009 ~ "-2",
#       year == 2013 ~ "-1",
#       year == 2015 ~ "0",
#       year == 2019 ~ "1",
#       year == 2019.4 ~ "2",  # assuming the second election in 2019 is marked as 2019.4
#       year == 2020 ~ "3",
#       year == 2021 ~ "4",
#       year == 2022 ~ "5"
#     )
#   )
# 
# # Convert event_time to a factor to ensure uniform spacing
# turnout_diff$event_time <- factor(turnout_diff$event_time, levels = c("-3", "-2", "-1", "0", "1", "2", "3", "4", "5"))
# 
# # Create the event-study plot with differences
# ggplot(turnout_diff, aes(x = event_time)) +
#   geom_rect(aes(xmin = 3, xmax = 4., ymin = -Inf, ymax = Inf),
#             fill = "blue", alpha = 0.01) +
#   geom_line(aes(y = diff_6_vs_no_alert, group = 1, color = "6 days vs No Red Alert", linetype = "6 days vs No Red Alert"), size = 0.5) +
#   geom_line(aes(y = diff_200_vs_no_alert, group = 1, color = "200+ days vs No Red Alert", linetype = "200+ days vs No Red Alert"), size = 0.5) +
#   geom_vline(xintercept = "0", linetype = "dashed", color = "black", size = 0.5) +
#   labs(
#     x = "Number of Elections Relative to 2015 Election",
#     y = "Difference in Turnout",
#     color = "Comparison to Control Group:",
#     linetype = "Comparison to Control Group:"
#   ) +
#   scale_color_manual(values = c("grey55", "grey25")) +
#   scale_linetype_manual(values = c(2, 1)) + # Differentiate line types
#   scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.05, 0.05)) +
#   scale_x_discrete(labels = c("-3", "-2", "-1", "0", "1", "2", "3", "4", "5")) +  # Display event time on x-axis
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# ggsave('treating/Red Alerts/Output/Figures/event_study_turnout_percentage.pdf',
#        width = 8.5,
#        height = 5.5)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 

# #### GRAPHS ####
#
# ggplot(likud_mean, aes(x = year, y = mean_votes, color = as.factor(temporal_group))) +
#   geom_rect(aes(xmin = 2012.8, xmax = 2015.2, ymin = -Inf, ymax = Inf),
#             fill = "grey85", alpha = 0.2, color = 'white') +
#   geom_line() +
#   geom_point() +
#   geom_vline(xintercept = 2014, linetype = "dashed", color = "black", size = 0.5) +
#   labs(x = "Year",
#        y = "Average Share of Votes: Likud",
#        color = "Days between last Red Alert and 2015 election:",
#        title = "Likud’s Average Share of Votes: 2006 to 2022 - Days Between Last Red Alert and 2015 Election") +
#   scale_color_manual(values = c("blue", "red", "orange", "black"),
#                      labels = c("No Red Alerts until the 2015 Election", "6 days", "204-232 days")) +
#   scale_y_continuous(labels = percent_format(accuracy = 1)) +
#   theme_minimal() +
#   theme(legend.position = "bottom")
#
# ggsave('treating/Red Alerts/Output/Figures/likud_parallel_trends.pdf',
#        width = 8.5,
#        height = 5.5)
#
#
#
# ggplot(right_wing_mean, aes(x = year, y = mean_votes, color = as.factor(temporal_group))) +
#   geom_rect(aes(xmin = 2012.8, xmax = 2015.2, ymin = -Inf, ymax = Inf),
#             fill = "grey85", alpha = 0.2, color = 'white') +
#   geom_line() +
#   geom_point() +
#   geom_vline(xintercept = 2014, linetype = "dashed", color = "black", size = 0.5) +
#   labs(x = "Year",
#        y = "Average Share of Votes: Right Wing Parties",
#        color = "Days between last Red Alert and 2015 election:",
#        title = "Right-Wing Parties Average Share of Votes: 2006 to 2022 - Days Between Last Red Alert and 2015 Election") +
#   scale_color_manual(values = c("blue", "red", "orange", "black"),
#                      labels = c("No Red Alerts until the 2015 Election", "6 days", "204-232 days")) +
#   scale_y_continuous(labels = percent_format(accuracy = 1)) +
#   theme_minimal() +
#   theme(legend.position = "bottom")
#
# ggsave('treating/Red Alerts/Output/Figures/right_wing_parallel_trends.pdf',
#        width = 8.5,
#        height = 5.5)
#
#
# ggplot(turnout_mean, aes(x = year, y = mean_votes, color = as.factor(temporal_group))) +
#   geom_rect(aes(xmin = 2012.8, xmax = 2015.2, ymin = -Inf, ymax = Inf),
#             fill = "grey85", alpha = 0.2, color = 'white') +
#   geom_line() +
#   geom_point() +
#   geom_vline(xintercept = 2014, linetype = "dashed", color = "black", size = 0.5) +
#   labs(x = "Year",
#        y = "Average Turnout",
#        color = "Days between last Red Alert and 2015 election:",
#        title = "Average Turnout: 2009 to 2022 - Days Between Last Red Alert and 2015 Election") +
#   scale_color_manual(values = c("blue", "red", "orange", "black"),
#                      labels = c("No Red Alerts until the 2015 Election", "6 days", "204-232 days")) +
#   scale_y_continuous(labels = percent_format(accuracy = 1)) +
#   theme_minimal() +
#   theme(legend.position = "bottom")
#
# ggsave('treating/Red Alerts/Output/Figures/turnout_parallel_trends.pdf',
#        width = 8.5,
#        height = 5.5)
#
#
#
# # Weighted means graph
# ggplot(likud_mean_weighted, aes(x = year, y = mean_votes, color = as.factor(temporal_group))) +
#   geom_rect(aes(xmin = 2012.8, xmax = 2015.2, ymin = -Inf, ymax = Inf),
#             fill = "grey85", alpha = 0.2, color = 'white') +
#   geom_line() +
#   geom_point() +
#   geom_vline(xintercept = 2014, linetype = "dashed", color = "black", size = 0.5) +
#   labs(x = "Year",
#        y = "Average Share of Votes: Likud (Weighted by Population)",
#        color = "Days between last Red Alert and 2015 election:",
#        title = "Likud’s Average Share of Votes (Weighted): 2006 to 2022 - Days Between Last Red Alert and 2015 Election") +
#   scale_color_manual(values = c("blue", "red", "orange", "black"),
#                      labels = c("No Red Alerts until the 2015 Election", "6 days", "204-232 days")) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   theme_minimal() +
#   theme(legend.position = "bottom")


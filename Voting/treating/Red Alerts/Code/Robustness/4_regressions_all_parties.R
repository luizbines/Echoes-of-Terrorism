################################################################################
# AUTOMATED REGRESSION FOR ALL PARTY PERCENTAGES
################################################################################

library(tidyverse)
library(fixest)
library(modelsummary)
library(Hmisc)

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


# Importing
parties_percentages_panel = read_csv('treating/Red Alerts/Output/Datasets/2_parties_percentages_panel.csv')


# filtering years
parties_percentages_panel = parties_percentages_panel %>% 
  filter(year <= 2015) %>% 
  mutate(year = as.integer(year)) %>% 
  rename(year_election = year)


# Removing observations with missing voting data (only 28 observations, from 2006 and 2009 elections)
parties_percentages_panel = parties_percentages_panel %>% filter(!is.na(likud_percentage))


# 1. Prepare the Data ---------------------------------------------------------

# list of 2013 parties that also ran in 2015 (or joined/merged into a party that ran in 2015)
parties_2013 = c(
  "likud_percentage",
  "yesh_atid_percentage",
  "zionist_union_percentage",
  "shas_percentage",
  "joint_list_percentage",
  "meretz_percentage",
  "habait_hayehudi_percentage",
  "aleh_yarok_percentage",
  "democratura_percentage",
  "economy_party_percentage",
  "hope_for_change_percentage",
  # "israel_beiteinu_percentage", # We exclude this one because it was merged with likud (only in 2013)
  "na_nach_percentage",
  "or_party_percentage",
  "pirate_party_percentage",
  "the_greens_percentage",
  "yahad_percentage",
  "yahadut_hatora_percentage"
)

data_reg <- parties_percentages_panel %>% 
  filter(Religion_yishuv_Code != 2) %>% 
# Keeping only the parties that also ran in 2013 (time 0 before treatment)
# This includes both parties that remained unchanged and parties that merged into
# other ones
  select(
    !contains("_percentage"),
    any_of(parties_2013)
  )
  
  
# 2. Identify Variables -------------------------------------------------------
# Selection: all columns containing "_percentage"
all_party_vars <- grep("_percentage", names(data_reg), value = TRUE)

# 3. Run Regressions ----------------------------------------------------------
# Multi-LHS estimation (runs all parties in one command)
# regs_base <- feols(.[all_party_vars] ~ i(year_election, temporal_group, 
#                                          ref = '2013', 
#                                          ref2 = 'no_red_alert') | 
#                      as.factor(SEMEL_YISHUV) + as.factor(year_election), 
#                    data = data_reg, 
#                    se = 'standard')

regs_ctrl <- feols(.[all_party_vars] ~ i(year_election, temporal_group, 
                                         ref = '2013', 
                                         ref2 = 'no_red_alert') + 
                     Pop_Total + ntl | 
                     as.factor(SEMEL_YISHUV) + as.factor(year_election), 
                   data = data_reg, 
                   cluster = ~SEMEL_YISHUV)

# 4. Organize for modelsummary ------------------------------------------------
# We use numeric indexing to avoid the "integer scalar" error
combined_models <- list()

for(i in seq_along(all_party_vars)){
  # Get current party name
  p_name <- all_party_vars[i]
  clean_name <- str_remove(p_name, "_percentage") %>% str_replace_all("_", " ") %>% str_to_title()
  
  # Extract by integer index
  # combined_models[[paste(clean_name, "(Base)")]] <- regs_base[[i]]
  # combined_models[[paste(clean_name, "(Ctrl)")]] <- regs_ctrl[[i]]
  combined_models[[clean_name]] <- regs_ctrl[[i]]
}

# 5. Export Vertical Table ----------------------------------------------------
all_parties_model = modelsummary(
  combined_models,
  # output = 'treating/Red Alerts/Output/Tables/Robustness/Robustness_all_parties_results.tex',
  output = 'latex',
  shape = model ~ term, 
  stars = TRUE,
  coef_map = c(
    'year_election::2015:temporal_group::temporal_distance == 6' = '2015: 6 Days Before',
    'year_election::2015:temporal_group::temporal_distance > 149' = '2015: 149+ Days Before',
    'year_election::2009:temporal_group::temporal_distance == 6' = '2009: 6 Days Before',
    'year_election::2009:temporal_group::temporal_distance > 149' = '2009: 149+ Days Before',
    'year_election::2006:temporal_group::temporal_distance == 6' = '2006: 6 Days Before',
    'year_election::2006:temporal_group::temporal_distance > 149' = '2006: 149+ Days Before'
  ),
  gof_omit = 'IC|Log|Adj|Within|Pseudo|FE'
)

# Saving
save_tabular(all_parties_model, 'treating/Red Alerts/Output/Tables/Robustness/Robustness_all_parties_results.tex')

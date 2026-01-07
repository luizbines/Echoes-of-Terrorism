################################################################################
# AUTOMATED REGRESSION FOR ALL PARTY PERCENTAGES
################################################################################

library(tidyverse)
library(fixest)
library(modelsummary)

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
regs_base <- feols(.[all_party_vars] ~ i(year_election, temporal_group, 
                                         ref = '2013', 
                                         ref2 = 'no_red_alert') | 
                     as.factor(SEMEL_YISHUV) + as.factor(year_election), 
                   data = data_reg, 
                   se = 'standard')

regs_ctrl <- feols(.[all_party_vars] ~ i(year_election, temporal_group, 
                                         ref = '2013', 
                                         ref2 = 'no_red_alert') + 
                     density + Pop_Total + ntl | 
                     as.factor(SEMEL_YISHUV) + as.factor(year_election), 
                   data = data_reg, 
                   se = 'standard')

# 4. Organize for modelsummary ------------------------------------------------
# We use numeric indexing to avoid the "integer scalar" error
combined_models <- list()

for(i in seq_along(all_party_vars)){
  # Get current party name
  p_name <- all_party_vars[i]
  clean_name <- str_remove(p_name, "_percentage")
  
  # Extract by integer index
  combined_models[[paste(clean_name, "(Base)")]] <- regs_base[[i]]
  combined_models[[paste(clean_name, "(Ctrl)")]] <- regs_ctrl[[i]]
}

# 5. Export Vertical Table ----------------------------------------------------
modelsummary(
  combined_models,
  output = 'treating/Red Alerts/Output/Figures/all_parties_results.tex',
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
  gof_omit = 'IC|Log|Adj|Within|Pseudo|FE',
  notes = "Standard errors in parentheses. Models include Locality and Year Fixed Effects."
)

# This script consolidates individual party vote shares into a unified longitudinal master panel.
# LUIZ BINES - 2026

library(tidyr)
library(dplyr)
library(stringr)
library(purrr)

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

# --- 1. SETTINGS ---
folder_path <- "cleaning/Elections/Output/Parties/"
files <- list.files(path = folder_path, pattern = "^Party_.*\\.csv$", full.names = TRUE)

# --- 2. DYNAMIC READING AND PIVOTING FUNCTION ---
read_pivot_and_prefix <- function(file_path) {
  # Extract the entity name (e.g., likud, turnout, shas)
  party_name <- basename(file_path) %>% 
    str_remove(".csv") %>% 
    str_remove("Party_")
  
  df <- read.csv(file_path)
  
  # Dynamically identify all year columns present in the file
  # Captures formats like 2015, 2019_2, etc.
  year_cols <- names(df)[str_detect(names(df), "^X?[0-9]{4}(_[0-9])?$")]
  
  # Columns serving as geographic keys
  static_keys <- c("SEMEL_YISHUV", "locality", "loc")
  
  df_long <- df %>%
    # Pivot to long format using only the years available in THIS specific file
    pivot_longer(
      cols = all_of(year_cols),
      names_to = "year",
      values_to = "percentage" # "party_" removed from here
    ) %>%
    mutate(
      # Clean the 'X' prefix that R sometimes adds to numeric headers
      year = str_remove(year, "^X"),
      SEMEL_YISHUV = as.integer(SEMEL_YISHUV)
    )
  
  # Add prefix to differentiate variables (e.g., likud_percentage)
  df_long <- df_long %>%
    rename_with(~ paste0(party_name, "_", .), -c(any_of(static_keys), year))
  
  return(df_long)
}

# --- 3. EXECUTION WITH FULL JOIN ---

# Map the function to all files
list_of_dfs <- map(files, read_pivot_and_prefix)

# Consolidate using FULL_JOIN to preserve extra years (2019-2022) 
# for entities that possess long series
master_evolution_panel <- list_of_dfs %>% 
  reduce(full_join, by = c("SEMEL_YISHUV", "locality", "loc", "year")) %>%
  arrange(SEMEL_YISHUV, year)

# Export the master panel
dir.create("cleaning/Elections/Output/Final", recursive = TRUE, showWarnings = FALSE)
write.csv(master_evolution_panel, 
          "cleaning/Elections/Output/parties_percentages.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")

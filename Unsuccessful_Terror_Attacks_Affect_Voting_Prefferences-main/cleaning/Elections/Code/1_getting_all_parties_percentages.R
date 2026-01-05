# LUIZ BINES - 2023 (Updated 2026)
# THIS SCRIPT CALCULATES THE FULL EVOLUTION (2006-2022) FOR ALL 2015 PARTIES
# GENERATES INDIVIDUAL CSV FILES PER PARTY + THE 2015 COALITION BLOCK

# Load necessary libraries for data manipulation, Excel reading, and string handling
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)

# Set the working directory to the project folder
wd = '/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Unsuccessful_Terror_Attacks_Affect_Voting_Prefferences-main/'
setwd(wd)

# --- 1. THE COMPLETE 2015 PARTY LIST (All 26 candidate lists) ---
# This list serves as the anchor for the analysis, using 2015 ballot letters (siglas)
parties_2015_list <- c(
  "zionist_union" = "אמת", "yahadut_hatora" = "ג", "joint_list" = "ודעם",
  "economy_party" = "ז", "democratura" = "זך", "peoples_team" = "זץ",
  "habait_hayehudi" = "טב", "rent_with_dignity" = "י", "social_leadership" = "יז",
  "protect_children" = "יך", "hope_for_change" = "יץ", "kulanu" = "כ",
  "israel_beiteinu" = "ל", "likud" = "מחל", "meretz" = "מרצ",
  "ubizchutan" = "נז", "or_party" = "ני", "perach_party" = "נץ",
  "arab_list" = "ע", "yesh_atid" = "פה", "pirate_party" = "ף",
  "na_nach" = "ףץ", "aleh_yarok" = "קנ", "yahad" = "קץ",
  "the_greens" = "רק", "shas" = "שס"
)

# --- 2. HISTORICAL MAPPING FUNCTION ---
# This function defines which ballot letters correspond to the 2015 entities in other years.
# It handles party mergers, splits, and the creation of ideological aggregates.
get_mapping <- function(year) {
  if (year == "2006") {
    return(list(
      "zionist_union" = "אמת", # Only Labor (Hatnuah didn't exist yet)
      "joint_list" = c("ד", "ו", "עם"), # Balad, Hadash, and Ra'am-Ta'al ran as separate lists
      "likud" = "מחל", "habait_hayehudi" = "טב", "shas" = "שס", 
      "israel_beiteinu" = "ל", "yahadut_hatora" = "ג",
      "right_wing_aggregate" = c("שס", "ל", "טב", "ג"), # Extra-Likud right-wing/religious bloc
      "coalition_2015_aggregate" = c("מחל", "שס", "טב", "ג") # The future 2015 coalition base
    ))
  }
  if (year == "2009") {
    return(list(
      "zionist_union" = "אמת", "joint_list" = c("ד", "ו", "עם"),
      "likud" = "מחל", "shas" = "שס", "israel_beiteinu" = "ל", 
      "yahadut_hatora" = "ג", 
      "habait_hayehudi" = c("ב", "ט"), # Split between Habait Hayehudi and National Union
      "right_wing_aggregate" = c("שס", "ל", "ג", "ב", "ט"),
      "coalition_2015_aggregate" = c("מחל", "שס", "ב", "ט", "ג")
    ))
  }
  if (year == "2013") {
    return(list(
      "zionist_union" = c("אמת", "צפ"), # Merger of Labor (אמת) and Tzipi Livni's Hatnuah (צפ)
      "joint_list" = c("ד", "ו", "עם"),
      "likud" = "מחל", 
      "israel_beiteinu" = "מחל", # Likud and Beiteinu ran as a joint list (Likud-Beiteinu)
      "habait_hayehudi" = "טב", "shas" = "שס", "yahadut_hatora" = "ג",
      "yesh_atid" = "פה", "meretz" = "מרץ",
      "right_wing_aggregate" = c("שס", "טב", "ג"),
      "coalition_2015_aggregate" = c("מחל", "שס", "טב", "ג")
    ))
  }
  if (year == "2015") {
    return(list(
      # Definition of aggregates based on the 2015 election results/coalition formation
      "right_wing_aggregate" = c("שס", "טב", "ג", "ל"),
      "coalition_2015_aggregate" = c("מחל", "כ", "טב", "שס", "ג")
    ))
  }
  if (year %in% c("2019", "2019_2", "2020", "2021", "2022")) {
    # Logic for modern elections handling the splintering and re-merging of the Right and Center
    rw_siglas <- if(year == "2019") c("שס", "ג", "ל", "טב", "נ") else if(year == "2021") c("שס", "ג", "ל", "ב", "ט") else if(year == "2022") c("שס", "ג", "ל", "ט") else c("שס", "ג", "ל", "טב")
    coa_siglas <- if(year %in% c("2021", "2022")) c("מחל", "שס", "ג", "ב", "ט") else c("מחל", "שס", "ג", "טב", "נ", "ב")
    
    return(list(
      "likud" = "מחל", # Now includes Kulanu voters
      "right_wing_aggregate" = rw_siglas, 
      "coalition_2015_aggregate" = coa_siglas,
      "yesh_atid" = "פה", # Yesh Atid within Blue and White (פה)
      "habait_hayehudi" = c("טב", "נ", "ב", "ט"), # Evolution into Yamina/Religious Zionism
      "zionist_union" = c("אמת", "נר", "מרצ"), # Labor-Meretz-Gesher mergers
      "meretz" = c("מרצ", "אמת"),
      "joint_list" = c("ודעם", "ו", "ד", "עם"), # Handling periods where the Joint List split and reunited
      "israel_beiteinu" = "ל"
    ))
  }
  return(list()) 
}

# --- 3. METADATA BRIDGE ---
# Creates a reference table from 2015 data to link municipality names to their numeric codes
bridge_metadata <- read_xlsx('raw/Elections/expc_20.xlsx') %>%
  select(locality = `שם ישוב`, locality_id = `סמל ישוב`) %>%
  mutate(locality_id = as.character(locality_id)) %>%
  distinct()

# --- 4. PROCESSING FUNCTION ---
# Cleans raw election files, calculates vote shares (percentages), and standardizes IDs
process_year <- function(file_path, year_label, is_xls = FALSE) {
  df <- if(is_xls) read_xls(file_path) else read_xlsx(file_path)
  id_col <- if("סמל ישוב" %in% names(df)) "סמל ישוב" else "שם ישוב"
  
  if (year_label == "2006") {
    # For 2006 (names only), join with the bridge to get numeric IDs
    df <- df %>% rename(locality_name = `שם ישוב`) %>%
      left_join(bridge_metadata, by = c("locality_name" = "locality")) %>%
      rename(locality_id = locality_id, total_votes = `כשרים`)
  } else {
    df <- df %>% rename(locality_id = !!id_col, total_votes = `כשרים`)
  }
  
  # Group data by locality and sum votes (handling cases where a locality appears twice)
  df <- df %>% mutate(locality_id = as.character(locality_id)) %>%
    filter(!is.na(locality_id)) %>% group_by(locality_id) %>% 
    summarise(across(where(is.numeric), sum, na.rm=T), .groups = 'drop')
  
  special_map <- get_mapping(year_label)
  df_res <- df %>% select(locality_id, total_votes)
  
  # List of all entities to be tracked
  all_possible_parties <- unique(c(names(parties_2015_list), "right_wing_aggregate", "coalition_2015_aggregate"))
  
  for (p_name in all_possible_parties) {
    col_name <- paste0(p_name, "_", year_label)
    if (p_name %in% names(special_map)) {
      # Use custom mapping for mergers/splits
      siglas <- special_map[[p_name]]
      existing <- siglas[siglas %in% colnames(df)]
      df_res[[col_name]] <- if(length(existing) > 0) rowSums(df[, existing, drop=F], na.rm=T) / df$total_votes else 0
    } else if (year_label == "2015" && p_name %in% names(parties_2015_list)) {
      # Standard mapping for the baseline year
      sigla_2015 <- parties_2015_list[[p_name]]
      df_res[[col_name]] <- if(sigla_2015 %in% colnames(df)) df[[sigla_2015]] / df$total_votes else 0
    } else {
      # Assign 0 if the party didn't exist or run under a tracked name in that year
      df_res[[col_name]] <- 0
    }
  }
  return(df_res %>% select(-total_votes))
}

# --- 5. EXECUTION ---
# Process all election years and merge them into a single master dataframe
years <- c("2006", "2009", "2013", "2015", "2019", "2019_2", "2020", "2021", "2022")
files <- c('raw/Elections/Result17.xls', 'raw/Elections/results_18.xls', 'raw/Elections/expc_19.xlsx', 
           'raw/Elections/expc_20.xlsx', 'raw/Elections/expc_21.xlsx', 'raw/Elections/expc_22.xlsx', 
           'raw/Elections/expc_23.xlsx', 'raw/Elections/expc_24.xlsx', 'raw/Elections/expc_25.xlsx')

all_data_list <- list()
for(i in 1:length(years)) {
  all_data_list[[years[i]]] <- process_year(files[i], years[i], is_xls = (years[i] %in% c("2006", "2009")))
}

# Start merging with metadata and the 2013/2015 core
master_df <- bridge_metadata %>%
  inner_join(all_data_list[["2013"]], by = "locality_id") %>%
  inner_join(all_data_list[["2015"]], by = "locality_id")

# Append all other processed years via left joins
for(y in years[!years %in% c("2013", "2015")]) { 
  master_df <- master_df %>% left_join(all_data_list[[y]], by = "locality_id") 
}

# --- 6. ASYMMETRIC EXPORT WITH ALL 2015 PARTIES ---
# Generate CSV files with numeric IDs, locality names, and time-series data
dir.create("cleaning/Elections/Output/Evolution", recursive = TRUE, showWarnings = FALSE)

long_series_entities <- c("likud", "right_wing_aggregate", "coalition_2015_aggregate")
all_entities_to_export <- unique(c(names(parties_2015_list), long_series_entities))

for (p in all_entities_to_export) {
  # Asymmetric logic: only Likud and Aggregates go up to 2022; others stop at 2015
  target_years <- if(p %in% long_series_entities) years else c("2006", "2009", "2013", "2015")
  
  cols_to_select <- c("locality_id", "locality", paste0(p, "_", target_years))
  cols_to_select <- cols_to_select[cols_to_select %in% names(master_df)]
  
  df_export <- master_df %>%
    select(all_of(cols_to_select)) %>%
    # Ensure ID is numeric for proper mathematical sorting (1, 2, 10 instead of 1, 10, 2)
    mutate(SEMEL_YISHUV = as.numeric(locality_id)) %>%
    # Create 'loc' column containing only Hebrew characters for cleaner merge keys
    mutate(loc = gsub("[^א-ת]", "", locality, perl = TRUE)) %>%
    # Organize columns: ID first, followed by Name, Year data, and the Regex clean Name last
    select(SEMEL_YISHUV, locality, everything(), -locality_id) %>%
    arrange(SEMEL_YISHUV) %>%
    # Remove party prefix from year column names
    rename_with(~str_remove(., paste0(p, "_")), contains(p))
  
  # Save the individual party evolution file
  write.csv(df_export, paste0("cleaning/Elections/Output/Evolution/Party_", p, ".csv"), row.names = F)
}
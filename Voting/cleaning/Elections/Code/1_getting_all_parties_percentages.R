# LUIZ BINES - 2023 (Updated 2026)
# THIS SCRIPT CALCULATES THE FULL EVOLUTION (2006-2022) FOR ALL 2015 PARTIES
# GENERATES INDIVIDUAL CSV FILES PER PARTY + THE 2015 COALITION BLOCK + ESTIMATED TURNOUT

# Load necessary libraries
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)

# Resolve the Voting root from the environment or by walking upward from the current directory
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

# --- 1. THE COMPLETE 2015 PARTY LIST ---
parties_2015_list <- c(
  "likud" = "מחל",
  "zionist_union" = "אמת",
  "joint_list" = "ודעם",
  "yesh_atid" = "פה",
  "kulanu" = "כ",
  "habait_hayehudi" = "טב",
  "shas" = "שס",
  "israel_beiteinu" = "ל",
  "yahadut_hatora" = "ג",
  "meretz" = "מרצ",
  "yahad" = "קץ", # Although yahad is a right-wing party, it did not run in 2013
  "aleh_yarok" = "קנ",
  "arab_list" = "ע",
  "the_greens" = "רק",
  "na_nach" = "ףץ",
  "ubizchutan" = "נז",
  "hope_for_change" = "יץ",
  "pirate_party" = "ף",
  "perach_party" = "נץ",
  "peoples_team" = "זץ", 
  "or_party" = "ני",
  "rent_with_dignity" = "י", 
  "economy_party" = "ז", 
  "democratura" = "זך",
  "social_leadership" = "יז",
  "protect_children" = "יך"

)

# Here, we only care about parties that either ran in 2015 
# or merged into another party that ran in 2015
get_mapping <- function(year) {
  if (year == "2006") {
    return(list(
      "zionist_union"   = "אמת",            # HaAvoda-Meimad in 2006
      "joint_list"      = c("ד", "עם", "ו"), # Balad (ד), Ra'am-Ta'al (עם) and Hadash (ו)
      "habait_hayehudi" = "טב",             # Ichud Leumi-Mafdal
      "yahad"           = "כ",              # Marzel became Otzma, which joined Yahad em 2015
      "likud"           = "מחל",            
      "israel_beiteinu" = "ל",               
      "shas"            = "שס",              
      "yahadut_hatora"  = "ג",               
      "meretz"          = "מרצ",             
      "aleh_yarok"      = "קנ",              
      "the_greens"      = "רק",
      "right_wing" = c("שס", "ל", "טב", "ג"), # Note that Likud is excluded from 
      "coalition_2015" = c("שס", "טב", "ג")   # both right_wing and coalision (for all years)
    ))
  }
  if (year == "2009") {
    return(list(
      "zionist_union"   = "אמת",           # HaAvoda in 2009
      "joint_list"      = c("ד", "ו", "עם"), # Balad (ד), Hadash (ו) and Ra'am-Ta'al (עם)
      "habait_hayehudi" = c("ב", "ט"),      # Mafdal (ב) and Ichud Leumi (ט) in 2009
      "likud"           = "מחל",           
      "israel_beiteinu" = "ל",              
      "shas"            = "שס",             
      "yahadut_hatora"  = "ג",              
      "meretz"          = "מרצ",            
      "aleh_yarok"      = "קנ",             
      "the_greens"      = "רק",             
      "or_party"        = "אר",              # Or changed its symbol to 'ני' in 2015
      "right_wing" = c("שס", "ל", "ג", "ב", "ט"),
      "coalition_2015" = c("שס", "ב", "ט", "ג")
    ))
  }
  if (year == "2013") {
    return(list(
      "zionist_union"   = c("אמת", "צפ"),
      "joint_list"      = c("ד", "ו", "עם"),
      "likud"           = "מחל", 
      "israel_beiteinu" = "מחל",
      "habait_hayehudi" = "טב", 
      "shas"            = "שס", 
      "yahadut_hatora"  = "ג",
      "yesh_atid"       = "פה", 
      "meretz"          = "מרץ",
      "yahad"           = "נץ", # In 2015 another unrelated party (perach_party) used this symbol
      "aleh_yarok"      = "קנ",
      "the_greens"      = "רק",
      "pirate_party"    = "פ",
      "na_nach"         = "פץ",
      "economy_party"   = "פי",
      "hope_for_change" = "הק",
      "or_party"        = "ני",
      "democratura"     = "זך",
      "right_wing" = c("שס", "טב", "ג"),
      "coalition_2015" = c("שס", "טב", "ג") 
    ))
  }
  if (year == "2015") {
    return(list(
      "right_wing" = c("שס", "טב", "ג", "ל"),
      "coalition_2015" = c("כ", "טב", "שס", "ג") 
    ))
  }
  if (year %in% c("2019", "2019_2", "2020", "2021", "2022")) {
    rw_siglas <- if(year == "2019") c("שס", "ג", "ל", "טב", "נ") else if(year == "2021") c("שס", "ג", "ל", "ב", "ט") else if(year == "2022") c("שס", "ג", "ל", "ט") else c("שס", "ג", "ל", "טב")
    # coa_siglas defined without "מחל"
    coa_siglas <- if(year %in% c("2021", "2022")) c("שס", "ג", "ב", "ט") else c("שס", "ג", "טב", "נ", "ב")
    
    return(list(
      "likud" = "מחל", "right_wing" = rw_siglas, 
      "coalition_2015" = coa_siglas, "yesh_atid" = "פה",
      "habait_hayehudi" = c("טב", "נ", "ב", "ט"), "zionist_union" = c("אמת", "נר", "מרצ"),
      "meretz" = c("מרצ", "אמת"), "joint_list" = c("ודעם", "ו", "ד", "עם"), "israel_beiteinu" = "ל"
    ))
  }
  return(list()) 
}

# --- 3. METADATA & EXTERNAL DATA ---
bridge_metadata <- read_xlsx('raw/Elections/expc_20.xlsx') %>%
  select(locality = `שם ישוב`, locality_id = `סמל ישוב`) %>%
  mutate(locality_id = as.character(locality_id)) %>%
  distinct()

# Israel Demographic Panel for 2006 estimation
israel_panel <- read.csv('cleaning/Israel/Output/2_israel_panel_lights.csv') %>%
  mutate(locality_id = as.character(SEMEL_YISHUV))

# --- 4. PROCESSING FUNCTION (With Votes/Registered Capture) ---
process_year <- function(file_path, year_label, is_xls = FALSE) {
  df <- if(is_xls) read_xls(file_path) else read_xlsx(file_path)
  id_col <- if("סמל ישוב" %in% names(df)) "סמל ישוב" else "שם ישוב"
  
  # Identify vote and registration columns
  vote_col <- "מצביעים"
  reg_col <- if("בזב" %in% names(df)) "בזב" else if("בז''ב" %in% names(df)) "בז''ב" else NA
  
  if (year_label == "2006") {
    df <- df %>% rename(locality_name = `שם ישוב`) %>%
      left_join(bridge_metadata, by = c("locality_name" = "locality")) %>%
      rename(locality_id = locality_id, total_votes = `כשרים`)
  } else {
    df <- df %>% rename(locality_id = !!id_col, total_votes = `כשרים`)
  }
  
  df <- df %>% mutate(locality_id = as.character(locality_id)) %>%
    filter(!is.na(locality_id)) %>% group_by(locality_id) %>% 
    summarise(across(where(is.numeric), sum, na.rm=T), .groups = 'drop')
  
  special_map <- get_mapping(year_label)
  df_res <- df %>% select(locality_id, total_votes)
  
  # Temporary columns for turnout calculation logic
  if (!is.na(reg_col)) {
    df_res$registered_voters <- df[[reg_col]]
    df_res$number_of_voters <- df[[vote_col]]
    df_res[[paste0("turnout_", year_label)]] <- df_res$number_of_voters / df_res$registered_voters
  } else {
    df_res$number_of_voters <- df[[vote_col]]
  }
  
  all_possible_parties <- unique(c(names(parties_2015_list), "right_wing", "coalition_2015"))
  for (p_name in all_possible_parties) {
    col_name <- paste0(p_name, "_", year_label)
    if (p_name %in% names(special_map)) {
      siglas <- special_map[[p_name]]
      existing <- siglas[siglas %in% colnames(df)]
      df_res[[col_name]] <- if(length(existing) > 0) rowSums(df[, existing, drop=F], na.rm=T) / df$total_votes else 0
    } else if (year_label == "2015" && p_name %in% names(parties_2015_list)) {
      sigla_2015 <- parties_2015_list[[p_name]]
      df_res[[col_name]] <- if(sigla_2015 %in% colnames(df)) df[[sigla_2015]] / df$total_votes else 0
    } else { df_res[[col_name]] <- 0 }
  }
  return(df_res)
}

# --- 5. EXECUTION & MERGING ---
years <- c("2006", "2009", "2013", "2015", "2019", "2019_2", "2020", "2021", "2022")
files <- c('raw/Elections/Result17.xls', 'raw/Elections/results_18.xls', 'raw/Elections/expc_19.xlsx', 
           'raw/Elections/expc_20.xlsx', 'raw/Elections/expc_21.xlsx', 'raw/Elections/expc_22.xlsx', 
           'raw/Elections/expc_23.xlsx', 'raw/Elections/expc_24.xlsx', 'raw/Elections/expc_25.xlsx')

all_data_list <- list()
for(i in 1:length(years)) {
  all_data_list[[years[i]]] <- process_year(files[i], years[i], is_xls = (years[i] %in% c("2006", "2009")))
}

# --- 6. 2006 TURNOUT ESTIMATION LOGIC ---
# Calculate median registration percentage from 2009, 2013, 2015
median_reg_calc <- bind_rows(
  all_data_list[["2009"]] %>% mutate(year = 2009),
  all_data_list[["2013"]] %>% mutate(year = 2013),
  all_data_list[["2015"]] %>% mutate(year = 2015)
) %>%
  left_join(israel_panel %>% select(locality_id, year, Pop_Total), by = c("locality_id", "year")) %>%
  mutate(reg_percent = registered_voters / Pop_Total) %>%
  group_by(locality_id) %>%
  summarise(median_reg_percent = median(reg_percent, na.rm = TRUE))

# Apply estimation to 2006
all_data_list[["2006"]] <- all_data_list[["2006"]] %>%
  left_join(median_reg_calc, by = "locality_id") %>%
  left_join(israel_panel %>% filter(year == 2006) %>% select(locality_id, Pop_Total), by = "locality_id") %>%
  mutate(
    est_reg_voters = median_reg_percent * Pop_Total,
    turnout_2006 = ifelse(number_of_voters / est_reg_voters <= 1, number_of_voters / est_reg_voters, 1)
  )

# --- 7. MASTER MERGE ---
master_df <- bridge_metadata %>%
  inner_join(all_data_list[["2013"]], by = "locality_id") %>%
  inner_join(all_data_list[["2015"]], by = "locality_id")

for(y in years[!years %in% c("2013", "2015")]) { 
  master_df <- master_df %>% left_join(all_data_list[[y]], by = "locality_id") 
}

# --- 8. ASYMMETRIC EXPORT ---
dir.create("cleaning/Elections/Output/Evolution", recursive = TRUE, showWarnings = FALSE)
long_series_entities <- c("likud", "right_wing", "coalition_2015", "turnout")
all_entities_to_export <- unique(c(names(parties_2015_list), long_series_entities))

for (p in all_entities_to_export) {
  target_years <- if(p %in% long_series_entities) years else c("2006", "2009", "2013", "2015")
  cols_to_select <- c("locality_id", "locality", paste0(p, "_", target_years))
  cols_to_select <- cols_to_select[cols_to_select %in% names(master_df)]
  
  df_export <- master_df %>%
    select(all_of(cols_to_select)) %>%
    mutate(SEMEL_YISHUV = as.numeric(locality_id)) %>%
    mutate(loc = gsub("[^א-ת]", "", locality, perl = TRUE)) %>%
    select(SEMEL_YISHUV, locality, everything(), -locality_id) %>%
    arrange(SEMEL_YISHUV) %>%
    rename_with(~str_remove(., paste0(p, "_")), contains(p))
  
  write.csv(df_export, paste0("cleaning/Elections/Output/Parties/Party_", p, ".csv"), row.names = F)
}
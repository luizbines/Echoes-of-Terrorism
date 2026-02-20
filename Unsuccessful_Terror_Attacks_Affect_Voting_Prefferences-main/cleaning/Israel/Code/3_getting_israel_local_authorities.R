library(pdftools)
library(tidyverse)

# wd
wd = '/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Unsuccessful_Terror_Attacks_Affect_Voting_Prefferences-main/'
setwd(wd);

# 1. Load the PDF with positional metadata (x, y coordinates)
# Ensure the file path matches your project structure
pdf_dat <- pdf_data("raw/Israel/reshimalefishem.pdf")

# 2. Function to process each page individually based on spatial layout
process_page <- function(page_data) {
  page_data %>%
    # Group words that share the same vertical alignment (line)
    # Using round() helps consolidate words that are slightly offset
    mutate(y_group = round(y)) %>% 
    group_by(y_group) %>%
    # Filter only for lines containing a 4-digit locality code 
    # and an English name (capital letters) to skip headers and noise
    filter(any(str_detect(text, "^\\d{4}$") & x > 380)) %>%
    summarise(
      # Extract variables based on specific X-axis coordinate ranges (columns)
      
      # SEMEL_YISHUV: Locality Code with 4 digits located in the right-side numeric block
      SEMEL_YISHUV = text[x > 380 & x < 415 & str_detect(text, "^\\d{4}$")][1],
      
      # Sub-district (Nafa): 2 digits located just to the left of the code
      sub_district  = text[x > 365 & x < 380 & str_detect(text, "^\\d{2}$")][1],
      
      # Type of Locality: 3 digits representing settlement type (e.g., 160, 310)
      type_of_locality = text[x > 295 & x < 325 & str_detect(text, "^\\d{3}$")][1],
      
      # Local Authority Cluster: 3 digits in the designated cluster column range
      # This prevents "shifting" when the Metropolitan Area column is present
      local_authority_cluster = text[x > 230 & x < 265 & str_detect(text, "^\\d{3}$")][1],
      
      # Locality Name: Concatenate all words in the English name area
      locality_name = paste(text[x > 415 & str_detect(text, "^[A-Z'./ -]+$")], collapse = " "),
      .groups = "drop"
    ) %>%
    # Remove the grouping variable from the final page dataframe
    select(-y_group,-locality_name)
}

# 3. Apply the extraction to all pages and consolidate into a single dataframe
df_final <- map_dfr(pdf_dat, process_page) %>%
  # Remove rows where extraction failed (e.g., lines that aren't data entries)
  filter(!is.na(SEMEL_YISHUV)) %>%
  # Ensure unique entries for each locality code
  distinct(SEMEL_YISHUV, .keep_all = TRUE)

# 4. Saving
write_csv(df_final, "cleaning/Israel/Output/3.israel_localities_mapping.csv")
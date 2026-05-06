# Add SEI to the Israel panel and backfill 2013 using each locality's SEI trend.

# Load packages
library(sf)
library(dplyr)
library(readxl)

# Get the base path from environment or parent script
if (!exists("base_path")) {
  base_path <- Sys.getenv("R_PROJECT_DIR")
  if (base_path == "") {
    base_path <- getwd()
  }
}
setwd(base_path)

# Read the panel used in the analysis
israel_panel = read.csv('cleaning/Israel/Output/3_israel_panel_west_bank.csv')

# Read all available SEI spreadsheets and stack them into one data frame
sei_files = list.files('raw/Israel/SEI', pattern = '[.]xlsx$', full.names = TRUE)

sei = lapply(sei_files, function(file_path) {
	# Extract the SEI year from the file name and read the first sheet
	sei_year = as.integer(gsub('.*SEI_(\\d{4})[.]xlsx$', '\\1', basename(file_path)))

	read_excel(file_path, sheet = 1) %>%
		# Keep the locality code and the SEI index value
		select(
			SEMEL_YISHUV = `Locality Code`,
			SEI = starts_with('INDEX VALUE')
		) %>%
		# Standardize column types for the merge
		mutate(
			year = sei_year,
			SEMEL_YISHUV = as.integer(SEMEL_YISHUV),
			SEI = as.numeric(SEI)
		)
}) %>%
	bind_rows()

# Estimate a locality-level linear trend and use it to backfill 2013
sei_2013 = sei %>%
	group_by(SEMEL_YISHUV) %>%
	group_modify(function(data, key) {
		# Keep only valid observations for the locality
		data_valid = data %>%
			filter(!is.na(SEI), !is.na(year)) %>%
			arrange(year)

		if (nrow(data_valid) == 0) {
			return(tibble())
		}

		# If the locality has at least two observations, estimate its own trend.
		# Otherwise, fall back to carrying the latest observed level backward.
		sei_2013 = if (nrow(data_valid) >= 2 && n_distinct(data_valid$year) >= 2) {
			model = lm(SEI ~ year, data = data_valid)
			as.numeric(predict(model, newdata = data.frame(year = 2013)))
		} else {
			base_row = data_valid %>%
				slice_tail(n = 1)
			as.numeric(base_row$SEI)
		}

		tibble(
			year = 2013,
			SEI = sei_2013
		)
	}) %>%
	ungroup() %>%
	select(SEMEL_YISHUV, year, SEI)

# Append the estimated 2013 rows to the original SEI data
sei = bind_rows(sei, sei_2013)

# Merge SEI into the panel by locality code and year
israel_panel = israel_panel %>%
	mutate(
		year = as.integer(year),
		SEMEL_YISHUV = as.integer(SEMEL_YISHUV)
	) %>%
	left_join(sei, by = c('year', 'SEMEL_YISHUV'))


# Save the updated panel with SEI included
write.csv(israel_panel, 'cleaning/Israel/Output/4_israel_panel_SEI.csv', row.names = FALSE)
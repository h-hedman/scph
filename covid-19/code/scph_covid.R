# Title: SCPH Automated COVID-19 Output Data for Dashboard
# Author: Hayden Hedman
# Date: "2025-12-03"
# ============================================================================
# CONFIGURATION - Update these paths if your directory structure changes
# ============================================================================
## Important: Modify global paths and dfs below:
GDRIVE_OUTPUT_DIR <- "output folder path" # needs modified
DOWNLOADS_DIR <- "downloads folder path" # needs modified
INPUT_FILENAME <- "EpiTrax_Hospitalization_Data.xlsx"
OUTPUT_FILENAME <- "hospitalizations_6mo_prior.csv"
# ============================================================================

# Load packages - check if installed, install if needed, then load
required_packages <- c("dplyr", "data.table", "bit64", "curl", "tidyr", 
                       "tidyverse", "zoo", "gsheet", "googlesheets4", "readxl")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

cat("✓ All required packages loaded successfully\n\n")

# Define date ranges
cutoff_six_month_ago <- Sys.Date() - 180
start_date <- as.Date("2020-03-10")
end_date <- Sys.Date() - 1

# Load hospitalization data
input_file <- file.path(DOWNLOADS_DIR, INPUT_FILENAME)
co_hosp <- read_excel(input_file, sheet = 1)
cat("✓ Loaded", nrow(co_hosp), "hospitalization records\n\n")

# Ensure column names are trimmed
colnames(co_hosp) <- trimws(colnames(co_hosp))

# Process hospitalization data
co_hosp <- co_hosp %>%
  mutate(
    # Extract and clean the Admission Date column
    admission_date_clean = trimws(`Admission Date`),
    # Parse the date part only (before the time) and convert to Date type
    admission_date = as.Date(sub(" .*", "", admission_date_clean), format = "%m/%d/%Y"),
    Date = format(admission_date, "%Y-%m-%d"),
    freq = 1
  ) %>%
  group_by(Date) %>%
  summarize(hosp_count = sum(freq)) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  complete(Date = seq(cutoff_six_month_ago, end_date, by = "day")) %>%
  replace_na(list(hosp_count = 0)) %>%
  arrange(Date)

# Subset 6 months of hospitalization data for bar plot
hosp_six_month <- filter(co_hosp, Date >= cutoff_six_month_ago)

# Write csv
output_file <- file.path(GDRIVE_OUTPUT_DIR, OUTPUT_FILENAME)
write.csv(hosp_six_month, output_file, row.names = FALSE)
cat("✓ Successfully saved", OUTPUT_FILENAME, "\n")
cat("  Location:", output_file, "\n")
cat("  Total rows written:", nrow(hosp_six_month), "\n\n")

# Calculate 28-day and 7-day period hospitalizations
hosp_28_day_avg <- co_hosp %>%
  tail(28) %>%
  summarize(avg = round(mean(hosp_count), 1)) %>%
  pull(avg)

hosp_7_day_avg <- co_hosp %>%
  tail(7) %>%
  summarize(avg = round(mean(hosp_count), 1)) %>%
  pull(avg)

# Print averages for verification
cat("28-day average hospitalizations:", hosp_28_day_avg, "\n")
cat("7-day average hospitalizations:", hosp_7_day_avg, "\n\n")

# Clean up: remove downloaded file
file.remove(input_file)
cat("✓ Cleaned up input file from Downloads\n")

# End of script

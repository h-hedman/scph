# =============================================================================
# UTIL: Inventory all tabs across input_email XLSXs
# Output: utils/qc_dd/tab_inventory.csv
# Run from project root (.Rproj must be open)
# =============================================================================

library(tidyverse)
library(readxl)
library(here)

# --- PATHS -------------------------------------------------------------------
input_email_dir <- here("data", "input_email")
report_out      <- here("utils", "qc_dd", "tab_inventory.csv")

# --- SCAN XLSX FILES ---------------------------------------------------------
xlsx_files <- list.files(
  path       = input_email_dir,
  pattern    = "\\.xlsx$",
  full.names = TRUE
)

if (length(xlsx_files) == 0) stop("No XLSX files found in data/input_email/")

# --- EXTRACT TAB NAMES -------------------------------------------------------
inventory <- map_dfr(xlsx_files, function(fp) {
  source_xlsx <- basename(fp)
  tabs        <- excel_sheets(fp)
  
  tibble(
    source_xlsx = source_xlsx,
    source_tab  = tabs
  )
})

# Add blank column for manual fill-in
inventory <- inventory %>%
  mutate(output_csv_filename = NA_character_)

# --- CONSOLE PREVIEW ---------------------------------------------------------
cat("\n========== TAB INVENTORY ===================\n")
cat("XLSXs found:  ", length(xlsx_files), "\n")
cat("Total tabs:   ", nrow(inventory), "\n")
cat("=============================================\n\n")
print(inventory, n = Inf)

# --- WRITE OUTPUT ------------------------------------------------------------
write_csv(inventory, report_out)
cat("\nSaved to:", report_out, "\n")
cat("Fill in output_csv_filename column and copy into DD.\n")
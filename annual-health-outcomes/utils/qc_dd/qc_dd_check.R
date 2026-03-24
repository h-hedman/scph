# =============================================================================
# QC: Validate current_processed CSVs against Data Dictionary
# Output: console report + utils/qc_dd/qc_dd_report.csv
# Run from project root (.Rproj must be open)
# =============================================================================

library(tidyverse)
library(here)

# --- PATHS -------------------------------------------------------------------
dd_path          <- here("utils", "qc_dd", "dd.csv")
current_proc_dir <- here("data", "current_processed")
report_out       <- here("utils", "qc_dd", "qc_dd_report.csv")

# --- LOAD DD -----------------------------------------------------------------
dd <- read_csv(dd_path, show_col_types = FALSE)

# Identify rolling_mean output files — their current_processed structure
# intentionally differs from DD input cols, skip column checks for these
rolling_mean_files <- dd %>%
  filter(!is.na(transform_type) & transform_type == "rolling_mean") %>%
  pull(output_csv_filename) %>%
  unique()

# Build expected schema per csv: ordered vector of column names
dd_schema <- dd %>%
  arrange(output_csv_filename, input_col_num) %>%
  group_by(output_csv_filename) %>%
  summarise(expected_cols = list(csv_field_name), .groups = "drop")

dd_filenames <- dd_schema$output_csv_filename

# --- SCAN current_processed --------------------------------------------------
all_csvs <- list.files(
  path       = current_proc_dir,
  pattern    = "\\.csv$",
  recursive  = TRUE,
  full.names = TRUE
)

if (length(all_csvs) == 0) stop("No CSVs found under data/current_processed/")

# --- RUN CHECKS --------------------------------------------------------------
results <- map_dfr(all_csvs, function(fp) {

  fname     <- basename(fp)
  dashboard <- basename(dirname(fp))

  # Check 1: filename registered in DD?
  in_dd <- fname %in% dd_filenames

  if (!in_dd) {
    return(tibble(
      dashboard       = dashboard,
      filename        = fname,
      check_in_dd     = FALSE,
      check_col_names = NA,
      check_col_order = NA,
      check_col_count = NA,
      expected_cols   = NA_character_,
      actual_cols     = NA_character_,
      missing_cols    = NA_character_,
      extra_cols      = NA_character_,
      order_mismatch  = NA_character_,
      notes           = "FILENAME NOT IN DD"
    ))
  }

  # Skip column checks for rolling_mean outputs — output structure is
  # intentionally different from DD input cols (collapsed/derived)
  if (fname %in% rolling_mean_files) {
    return(tibble(
      dashboard       = dashboard,
      filename        = fname,
      check_in_dd     = TRUE,
      check_col_names = NA,
      check_col_order = NA,
      check_col_count = NA,
      expected_cols   = NA_character_,
      actual_cols     = NA_character_,
      missing_cols    = NA_character_,
      extra_cols      = NA_character_,
      order_mismatch  = NA_character_,
      notes           = "SKIPPED — rolling_mean transform, output structure differs from DD input"
    ))
  }

  # Read header only
  actual_cols   <- names(read_csv(fp, n_max = 0, show_col_types = FALSE))
  expected_cols <- dd_schema %>%
    filter(output_csv_filename == fname) %>%
    pull(expected_cols) %>%
    .[[1]]

  missing_cols <- setdiff(expected_cols, actual_cols)
  extra_cols   <- setdiff(actual_cols, expected_cols)

  # Check 2: column names match (ignoring order)
  check_col_names <- length(missing_cols) == 0 && length(extra_cols) == 0

  # Check 3: column count
  check_col_count <- length(actual_cols) == length(expected_cols)

  # Check 4: column order
  order_mismatch <- if (check_col_names) {
    if (identical(actual_cols, expected_cols)) NA_character_
    else paste("Expected:", paste(expected_cols, collapse = ", "),
               "| Got:",    paste(actual_cols,   collapse = ", "))
  } else NA_character_

  check_col_order <- check_col_names && identical(actual_cols, expected_cols)

  tibble(
    dashboard       = dashboard,
    filename        = fname,
    check_in_dd     = TRUE,
    check_col_names = check_col_names,
    check_col_order = check_col_order,
    check_col_count = check_col_count,
    expected_cols   = paste(expected_cols, collapse = ", "),
    actual_cols     = paste(actual_cols,   collapse = ", "),
    missing_cols    = if (length(missing_cols) > 0) paste(missing_cols, collapse = ", ") else NA_character_,
    extra_cols      = if (length(extra_cols)   > 0) paste(extra_cols,   collapse = ", ") else NA_character_,
    order_mismatch  = order_mismatch,
    notes           = case_when(
      !check_col_names ~ "COLUMN NAME MISMATCH",
      !check_col_order ~ "COLUMN ORDER WRONG",
      !check_col_count ~ "COLUMN COUNT MISMATCH",
      TRUE             ~ "OK"
    )
  )
})

# --- CHECK: DD files missing from current_processed -------------------------
found_files       <- basename(all_csvs)
missing_from_disk <- dd_filenames[!dd_filenames %in% found_files]

if (length(missing_from_disk) > 0) {
  missing_rows <- tibble(
    dashboard       = NA_character_,
    filename        = missing_from_disk,
    check_in_dd     = TRUE,
    check_col_names = NA,
    check_col_order = NA,
    check_col_count = NA,
    expected_cols   = NA_character_,
    actual_cols     = NA_character_,
    missing_cols    = NA_character_,
    extra_cols      = NA_character_,
    order_mismatch  = NA_character_,
    notes           = "FILE IN DD BUT NOT FOUND ON DISK"
  )
  results <- bind_rows(results, missing_rows)
}

# --- CONSOLE SUMMARY ---------------------------------------------------------
cat("\n========== QC SUMMARY ======================\n")
cat("Total CSVs scanned:       ", sum(!is.na(results$dashboard)),                           "\n")
cat("Not in DD:                ", sum(results$notes == "FILENAME NOT IN DD",               na.rm = TRUE), "\n")
cat("Missing from disk:        ", sum(results$notes == "FILE IN DD BUT NOT FOUND ON DISK", na.rm = TRUE), "\n")
cat("Column name issues:       ", sum(results$notes == "COLUMN NAME MISMATCH",             na.rm = TRUE), "\n")
cat("Column order issues:      ", sum(results$notes == "COLUMN ORDER WRONG",               na.rm = TRUE), "\n")
cat("Column count issues:      ", sum(results$notes == "COLUMN COUNT MISMATCH",            na.rm = TRUE), "\n")
cat("Clean (OK):               ", sum(results$notes == "OK",                               na.rm = TRUE), "\n")
cat("Skipped (transform):      ", sum(grepl("SKIPPED", results$notes),                       na.rm = TRUE), "\n")
cat("=============================================\n\n")

issues <- results %>% filter(notes != "OK" & !grepl("SKIPPED", notes))

if (nrow(issues) == 0) {
  cat("All CSVs pass. No issues found.\n")
} else {
  cat("Issues found:\n\n")
  issues %>%
    select(dashboard, filename, notes, missing_cols, extra_cols, order_mismatch) %>%
    print(n = Inf)
}

# --- WRITE REPORT ------------------------------------------------------------
write_csv(results, report_out)
cat("\nFull report saved to:", report_out, "\n")

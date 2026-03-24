# =============================================================================
# validate_test_output.R
# Validates test_output/ against data/ to confirm pipeline behavior is correct
# before flipping test_mode = FALSE for a live run.
#
# Checks:
#   1. Every real master CSV exists in test_output/master/
#   2. test_output/master/ has exactly N+1 rows vs real master (dummy row added)
#   3. Real data/master/ CSVs are unchanged (no 2050 contamination)
#   4. All test_output/current_processed/ CSVs have <= 5 unique years
#   5. All test_output/current_processed/ CSVs match DD column structure
#   6. No 2050 rows exist in any real master CSV
#   7. test_output/shiny/data/ status CSVs exist and have correct row counts
#   8. test_output/output_email/ XLSXs exist for all 10 dashboards
#
# Run from project root (.Rproj must be open)
# Run AFTER inject_dummy_data.R + pipeline_run.R in test_mode = TRUE
# =============================================================================

library(tidyverse)
library(readxl)
library(here)

# =============================================================================
# CONFIG
# =============================================================================

DUMMY_YEAR    <- 2050L
MAX_CP_YEARS  <- 5
EXPECTED_DASH <- c("DEATH.xlsx", "DIS.xlsx", "EH.xlsx", "FAM.xlsx",
                   "HEARVIS.xlsx", "NFP.xlsx", "OPI.xlsx", "TOB.xlsx",
                   "WIC.xlsx", "YOUTH.xlsx")

# =============================================================================
# PATHS
# =============================================================================

dd_path        <- here("utils", "qc_dd", "dd.csv")
real_master    <- here("data", "master")
test_master    <- here("test_output", "master")
real_cp        <- here("data", "current_processed")
test_cp        <- here("test_output", "current_processed")
test_shiny     <- here("test_output", "shiny", "data")
test_email     <- here("test_output", "output_email")
input_email    <- here("data", "input_email")

# =============================================================================
# HELPERS
# =============================================================================

pass <- function(msg) cat(paste0("  [PASS] ", msg, "\n"))
fail <- function(msg) {
  cat(paste0("  [FAIL] ", msg, "\n"))
  validation_errors <<- c(validation_errors, msg)
}
warn <- function(msg) cat(paste0("  [WARN] ", msg, "\n"))
header <- function(msg) cat(paste0("\n--- ", msg, " ---\n"))

validation_errors <- character(0)

# =============================================================================
# LOAD DD
# =============================================================================

dd <- read_csv(dd_path, show_col_types = FALSE)

# Get all master CSVs from DD
master_lookup <- dd %>%
  distinct(output_csv_filename, pbi) %>%
  mutate(
    real_path = file.path(real_master, pbi, output_csv_filename),
    test_path = file.path(test_master, pbi, output_csv_filename),
    cp_real   = file.path(real_cp,    pbi, output_csv_filename),
    cp_test   = file.path(test_cp,    pbi, output_csv_filename)
  )

# rolling_mean files — current_processed structure differs from DD
rolling_mean_files <- dd %>%
  filter(!is.na(transform_type) & transform_type == "rolling_mean") %>%
  pull(output_csv_filename) %>%
  unique()

cat("=============================================================\n")
cat("  PIPELINE TEST OUTPUT VALIDATION\n")
cat("  Project:", here(), "\n")
cat("  Dummy year:", DUMMY_YEAR, "\n")
cat("  Run date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=============================================================\n")

# =============================================================================
# CHECK 1: Real master CSVs unchanged — no dummy year contamination
# =============================================================================

header("CHECK 1: Real master CSVs have no dummy year contamination")

for (i in seq_len(nrow(master_lookup))) {
  row  <- master_lookup[i, ]
  fp   <- row$real_path
  
  if (!file.exists(fp)) {
    warn(paste("Real master not found (expected):", basename(fp)))
    next
  }
  
  df <- tryCatch(read_csv(fp, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(df)) {
    fail(paste("Could not read real master:", basename(fp)))
    next
  }
  
  if ("year" %in% names(df)) {
    if (any(df$year == DUMMY_YEAR, na.rm = TRUE)) {
      fail(paste("DUMMY YEAR", DUMMY_YEAR, "FOUND IN REAL MASTER:", basename(fp)))
    } else {
      pass(paste("Clean:", basename(fp), "| max year:", max(df$year, na.rm = TRUE)))
    }
  } else {
    pass(paste("No year col (expected):", basename(fp)))
  }
}

# =============================================================================
# CHECK 2: test_output/master/ has dummy row appended
# =============================================================================

header("CHECK 2: test_output/master/ has dummy year row appended vs real master")

for (i in seq_len(nrow(master_lookup))) {
  row      <- master_lookup[i, ]
  real_fp  <- row$real_path
  test_fp  <- row$test_path
  
  if (!file.exists(real_fp)) next
  if (!file.exists(test_fp)) {
    fail(paste("Test master missing:", basename(test_fp)))
    next
  }
  
  real_df <- tryCatch(read_csv(real_fp, show_col_types = FALSE), error = function(e) NULL)
  test_df <- tryCatch(read_csv(test_fp, show_col_types = FALSE), error = function(e) NULL)
  
  if (is.null(real_df) || is.null(test_df)) next
  
  if (!"year" %in% names(test_df)) {
    pass(paste("No year col, skipping row check:", basename(test_fp)))
    next
  }
  
  # single_year tabs replace rather than append — just check dummy year exists
  trans <- dd %>%
    filter(output_csv_filename == row$output_csv_filename) %>%
    pull(transform_type) %>%
    .[!is.na(.)] %>%
    .[1]
  trans <- if (length(trans) == 0) NA_character_ else trans
  
  has_dummy <- any(test_df$year == DUMMY_YEAR, na.rm = TRUE)
  
  if (!has_dummy) {
    fail(paste("Dummy year", DUMMY_YEAR, "NOT found in test master:", basename(test_fp)))
    next
  }
  
  if (!is.na(trans) && trans == "single_year") {
    pass(paste("single_year replace OK:", basename(test_fp),
               "| rows:", nrow(test_df)))
  } else {
    expected_rows <- nrow(real_df) + 1
    if (nrow(test_df) == expected_rows) {
      pass(paste("Row count OK:", basename(test_fp),
                 "| real:", nrow(real_df), "+ 1 =", nrow(test_df)))
    } else {
      fail(paste("Row count mismatch:", basename(test_fp),
                 "| expected:", expected_rows, "| got:", nrow(test_df)))
    }
  }
}

# =============================================================================
# CHECK 3: test_output/current_processed/ has <= 5 unique years
# =============================================================================

header("CHECK 3: test_output/current_processed/ has <= 5 unique years per CSV")

test_cp_files <- list.files(test_cp, pattern = "\\.csv$",
                            recursive = TRUE, full.names = TRUE)

if (length(test_cp_files) == 0) {
  fail("No CSVs found in test_output/current_processed/")
} else {
  for (fp in test_cp_files) {
    fname <- basename(fp)
    
    # Skip rolling_mean files — no year col in output
    if (fname %in% rolling_mean_files) {
      pass(paste("rolling_mean skip (no year col):", fname))
      next
    }
    
    df <- tryCatch(read_csv(fp, show_col_types = FALSE), error = function(e) NULL)
    if (is.null(df) || !"year" %in% names(df)) {
      warn(paste("No year col:", fname))
      next
    }
    
    n_years <- length(unique(df$year))
    if (n_years > MAX_CP_YEARS) {
      fail(paste("Too many years in current_processed:", fname,
                 "| found:", n_years, "| max:", MAX_CP_YEARS))
    } else {
      pass(paste("Year count OK:", fname,
                 "| years:", paste(sort(unique(df$year)), collapse = ", ")))
    }
  }
}

# =============================================================================
# CHECK 4: test_output/current_processed/ column structure matches DD
# =============================================================================

header("CHECK 4: test_output/current_processed/ column names match DD")

dd_schema <- dd %>%
  arrange(output_csv_filename, input_col_num) %>%
  group_by(output_csv_filename) %>%
  summarise(expected_cols = list(csv_field_name), .groups = "drop")

for (fp in test_cp_files) {
  fname <- basename(fp)
  
  if (fname %in% rolling_mean_files) {
    pass(paste("rolling_mean skip (transformed output):", fname))
    next
  }
  
  schema <- dd_schema %>% filter(output_csv_filename == fname)
  if (nrow(schema) == 0) {
    warn(paste("Not in DD:", fname))
    next
  }
  
  actual_cols   <- names(read_csv(fp, n_max = 0, show_col_types = FALSE))
  expected_cols <- schema %>% pull(expected_cols) %>% .[[1]]
  missing       <- setdiff(expected_cols, actual_cols)
  extra         <- setdiff(actual_cols, expected_cols)
  
  if (length(missing) > 0 || length(extra) > 0) {
    fail(paste("Column mismatch:", fname,
               if (length(missing) > 0) paste("| missing:", paste(missing, collapse = ", ")) else "",
               if (length(extra) > 0)   paste("| extra:",   paste(extra,   collapse = ", ")) else ""))
  } else if (!identical(actual_cols, expected_cols)) {
    fail(paste("Column order wrong:", fname))
  } else {
    pass(paste("Columns OK:", fname))
  }
}

# =============================================================================
# CHECK 5: Shiny status CSVs exist and have correct dimensions
# =============================================================================

header("CHECK 5: Shiny status CSVs exist with correct dimensions")

summary_path <- file.path(test_shiny, "status_summary.csv")
detail_path  <- file.path(test_shiny, "status_detail.csv")

if (!file.exists(summary_path)) {
  fail("status_summary.csv not found in test_output/shiny/data/")
} else {
  df <- read_csv(summary_path, show_col_types = FALSE)
  if (nrow(df) == 10 && "dashboard" %in% names(df) && "status" %in% names(df)) {
    pass(paste("status_summary.csv OK | rows:", nrow(df),
               "| statuses:", paste(sort(unique(df$status)), collapse = ", ")))
  } else {
    fail(paste("status_summary.csv unexpected structure | rows:", nrow(df),
               "| cols:", paste(names(df), collapse = ", ")))
  }
}

if (!file.exists(detail_path)) {
  fail("status_detail.csv not found in test_output/shiny/data/")
} else {
  df <- read_csv(detail_path, show_col_types = FALSE)
  expected_cols <- c("dashboard", "source_tab", "output_csv", "most_recent_year", "status")
  missing_cols  <- setdiff(expected_cols, names(df))
  if (nrow(df) == 32 && length(missing_cols) == 0) {
    pass(paste("status_detail.csv OK | rows:", nrow(df)))
  } else {
    fail(paste("status_detail.csv unexpected structure | rows:", nrow(df),
               if (length(missing_cols) > 0) paste("| missing cols:", paste(missing_cols, collapse = ", ")) else ""))
  }
}

# =============================================================================
# CHECK 6: output_email XLSXs exist for all 10 dashboards
# =============================================================================

header("CHECK 6: output_email XLSXs exist for all 10 dashboards")

for (xlsx in EXPECTED_DASH) {
  fp <- file.path(test_email, xlsx)
  if (!file.exists(fp)) {
    fail(paste("output_email missing:", xlsx))
  } else {
    tabs <- tryCatch(excel_sheets(fp), error = function(e) NULL)
    if (is.null(tabs)) {
      fail(paste("Could not read tabs from:", xlsx))
    } else {
      pass(paste("output_email OK:", xlsx, "| tabs:", paste(tabs, collapse = ", ")))
    }
  }
}

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n=============================================================\n")
cat("  VALIDATION COMPLETE\n")
cat("=============================================================\n")

if (length(validation_errors) == 0) {
  cat("\n  ALL CHECKS PASSED\n")
  cat("  Pipeline is ready for live run.\n")
  cat("  Set test_mode <- FALSE in pipeline_run.R to go live.\n\n")
} else {
  cat(paste0("\n  ", length(validation_errors), " FAILURE(S) FOUND:\n\n"))
  for (e in validation_errors) {
    cat(paste0("    [FAIL] ", e, "\n"))
  }
  cat("\n  DO NOT flip test_mode = FALSE until all failures are resolved.\n\n")
}

cat("=============================================================\n")

# =============================================================================
# SAVE REPORT TO utils/validation_reports/
# =============================================================================

report_dir  <- here("utils", "validation_reports")
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
ts          <- format(Sys.time(), "%Y%m%d_%H%M%S")
report_path <- file.path(report_dir, paste0("validation_", ts, ".txt"))

status_line <- if (length(validation_errors) == 0) "ALL CHECKS PASSED" else paste(length(validation_errors), "FAILURE(S) FOUND")

report_lines <- c(
  "=============================================================",
  "  PIPELINE TEST OUTPUT VALIDATION",
  paste0("  Run date:   ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("  Dummy year: ", DUMMY_YEAR),
  paste0("  Result:     ", status_line),
  "=============================================================",
  ""
)

if (length(validation_errors) > 0) {
  report_lines <- c(report_lines,
                    "  Failures:",
                    paste0("    [FAIL] ", validation_errors),
                    "",
                    "  DO NOT flip test_mode = FALSE until all failures are resolved."
  )
} else {
  report_lines <- c(report_lines,
                    "  Pipeline is ready for live run.",
                    "  Set test_mode <- FALSE in pipeline_run.R to go live."
  )
}

writeLines(report_lines, report_path)
cat(paste0("\nValidation report saved to: ", report_path, "\n"))
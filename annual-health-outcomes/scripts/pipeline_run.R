# =============================================================================
# pipeline_run.R
# Master controller — sources functions, sets mode, executes full pipeline
# Always open annual_pbi_dashboards.Rproj before running
# =============================================================================

library(here)
source(here("scripts", "pipeline_functions.R"))

# Null coalescing operator used in KPI function
`%||%` <- function(a, b) if (!is.null(a)) a else b

# =============================================================================
# CONFIG
# =============================================================================

# !! Keep test_mode = TRUE until pipeline is fully verified !!
test_mode <- FALSE

if (test_mode) {
  message("\n*** TEST MODE ACTIVE — all writes going to test_output/ ***\n")
} else {
  message("\n*** LIVE MODE — writing to data/master/ and data/current_processed/ ***\n")
}

# Reset log for this run
pipeline_log <<- character(0)
log_msg(paste("Pipeline started | test_mode:", test_mode))

# =============================================================================
# PATHS
# =============================================================================

input_dir <- if (test_mode) here("test_output", "input_email") else here("data", "input_email")
log_dir   <- here("logs")

# Ensure test_output subdirs exist when in test mode
if (test_mode) {
  for (subdir in c("master", "current_processed", "archive")) {
    dir.create(here("test_output", subdir), recursive = TRUE, showWarnings = FALSE)
  }
}

# =============================================================================
# LOAD DD + RECODES
# =============================================================================

dd      <- load_dd()
recodes <- load_recodes()

# =============================================================================
# DISCOVER XLSX FILES
# =============================================================================

xlsx_files <- list.files(input_dir, pattern = "\\.xlsx$", full.names = TRUE)

if (length(xlsx_files) == 0) {
  log_msg("WARN — no XLSX files found in data/input_email/", "WARN")
  write_log(log_dir)
  stop("No input files found")
}

log_msg(paste("XLSXs found:", paste(basename(xlsx_files), collapse = ", ")))

# =============================================================================
# MAIN LOOP
# =============================================================================

for (xlsx_path in xlsx_files) {
  
  src_xlsx <- basename(xlsx_path)
  log_msg(paste("======= START:", src_xlsx, "======="))
  
  all_tabs <- tryCatch(
    excel_sheets(xlsx_path),
    error = function(e) {
      log_msg(paste("ERROR reading tabs |", src_xlsx, "|", e$message), "ERROR")
      return(NULL)
    }
  )
  if (is.null(all_tabs)) next
  
  # Warn if Guide tab is missing
  if (!"Guide" %in% all_tabs) {
    log_msg(paste("WARN — Guide tab missing from:", src_xlsx), "WARN")
  }
  
  for (tab_name in all_tabs) {
    tryCatch(
      process_tab(xlsx_path, tab_name, dd, recodes, test_mode),
      error = function(e) {
        log_msg(paste("ERROR |", src_xlsx, "|", tab_name, "|", e$message), "ERROR")
      }
    )
  }
  
  log_msg(paste("======= END:", src_xlsx, "======="))
}

# =============================================================================
# OUTPUT EMAIL GENERATION
# =============================================================================

generate_output_email(dd, test_mode)

# =============================================================================
# SHINY STATUS CSVs
# =============================================================================

generate_status(dd, test_mode)

# =============================================================================
# KPI SUMMARY PDF
# =============================================================================

generate_kpi_pdf(test_mode)

# =============================================================================
# WRAP UP
# =============================================================================

log_msg("Pipeline complete")
write_log(log_dir)
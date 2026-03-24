# =============================================================================
# inject_dummy_data.R
# Injects a fake 2050 row into every tab (except Guide) across all input XLSXs
# Saves modified copies to test_output/input_email/ — never touches real files
# Run this once before running pipeline_run.R in test_mode to validate writes
# =============================================================================

library(tidyverse)
library(readxl)
library(writexl)
library(here)

# =============================================================================
# PATHS
# =============================================================================

input_dir  <- here("data", "input_email")
output_dir <- here("test_output", "input_email")
dd_path    <- here("documentation", "dd.csv")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# LOAD DD — needed to know col types per tab
# =============================================================================

dd <- read_csv(dd_path, show_col_types = FALSE)

# =============================================================================
# DUMMY VALUE HELPER
# =============================================================================

dummy_value <- function(field_type) {
  switch(field_type,
    "year"      = 2050L,
    "integer"   = 888L,
    "float"     = 888.88,
    "numeric"   = 888.88,
    "character" = "TEST",
    "TEST"       # fallback
  )
}

# =============================================================================
# INJECT DUMMY ROWS
# =============================================================================

xlsx_files <- list.files(input_dir, pattern = "\\.xlsx$", full.names = TRUE)

if (length(xlsx_files) == 0) stop("No XLSX files found in data/input_email/")

for (xlsx_path in xlsx_files) {

  src_xlsx <- basename(xlsx_path)
  cat("\nProcessing:", src_xlsx, "\n")

  all_tabs    <- excel_sheets(xlsx_path)
  output_tabs <- list()

  for (tab_name in all_tabs) {

    # Always preserve Guide tab as-is
    if (tolower(trimws(tab_name)) == "guide") {
      output_tabs[[tab_name]] <- read_excel(xlsx_path, sheet = tab_name,
                                            col_types = "text")
      cat("  Preserved Guide tab\n")
      next
    }

    # Get DD rows for this tab
    dd_tab <- dd %>%
      filter(source_xlsx == src_xlsx, source_tab == tab_name) %>%
      arrange(input_col_num)

    if (nrow(dd_tab) == 0) {
      cat("  SKIP — not in DD:", tab_name, "\n")
      # Preserve tab as-is if not in DD
      output_tabs[[tab_name]] <- tryCatch(
        read_excel(xlsx_path, sheet = tab_name, col_types = "text"),
        error = function(e) NULL
      )
      next
    }

    # Read existing data
    existing <- read_excel(xlsx_path, sheet = tab_name, col_types = "text")

    # Build dummy row from DD col types
    dummy_row <- dd_tab %>%
      arrange(input_col_num) %>%
      rowwise() %>%
      mutate(val = as.character(dummy_value(field_type))) %>%
      ungroup() %>%
      select(csv_field_name, val) %>%
      pivot_wider(names_from = csv_field_name, values_from = val)

    # Align dummy row col names to existing sheet col names (by position)
    names(dummy_row) <- names(existing)[seq_len(ncol(dummy_row))]

    # Append dummy row
    combined <- bind_rows(existing, dummy_row)
    output_tabs[[tab_name]] <- combined

    cat("  Injected 2050 dummy row into:", tab_name, "\n")
  }

  # Write modified XLSX to test_output/input_email/
  out_path <- file.path(output_dir, src_xlsx)
  write_xlsx(output_tabs, out_path)
  cat("  Saved to:", out_path, "\n")
}

cat("\n=== Dummy injection complete ===\n")
cat("Modified XLSXs saved to:", output_dir, "\n")
cat("Now run pipeline_run.R with test_mode = TRUE\n")

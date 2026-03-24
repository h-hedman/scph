# =============================================================================
# pipeline_functions.R
# All core functions for the annual PBI dashboard ingestion pipeline
# Source this file from pipeline_run.R - do not execute directly
#
# DD schema (documentation/dd.csv):
#   source_xlsx | source_tab | input_col_num | csv_field_name |
#   output_csv_filename | field_type | pbi | pbi_visual_target |
#   pct | req_transform | transform_type | notes
#
# Recode schema (documentation/recode.csv):
#   output_csv_filename | csv_field_name | raw_value | clean_value
# =============================================================================

library(tidyverse)
library(readxl)
library(here)

# =============================================================================
# 1. LOGGING
# =============================================================================

pipeline_log <- character(0)

log_msg <- function(msg, level = "INFO") {
  ts    <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  entry <- paste0("[", level, "] ", ts, " | ", msg)
  cat(entry, "\n")
  pipeline_log <<- c(pipeline_log, entry)
}

write_log <- function(log_dir) {
  ts       <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(log_dir, paste0("pipeline_", ts, ".log"))
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(pipeline_log, log_file)
  cat("\nLog saved to:", log_file, "\n")
}

# =============================================================================
# 2. LOAD DATA DICTIONARY + RECODE TABLE
# =============================================================================

load_dd <- function() {
  dd_path <- here("documentation", "dd.csv")
  if (!file.exists(dd_path)) stop("DD not found at documentation/dd.csv")
  
  dd <- read_csv(dd_path, show_col_types = FALSE)
  
  required_cols <- c(
    "source_xlsx", "source_tab", "input_col_num", "csv_field_name",
    "output_csv_filename", "field_type", "pbi", "pbi_visual_target",
    "pct", "req_transform", "transform_type", "notes"
  )
  missing <- setdiff(required_cols, names(dd))
  if (length(missing) > 0) stop(paste("DD missing columns:", paste(missing, collapse = ", ")))
  
  log_msg(paste("DD loaded -", nrow(dd), "rows,",
                length(unique(dd$output_csv_filename)), "output CSVs"))
  return(dd)
}

load_recodes <- function() {
  recode_path <- here("documentation", "recode.csv")
  
  if (!file.exists(recode_path)) {
    log_msg("No recode.csv found - skipping recodes", "INFO")
    return(tibble(
      output_csv_filename = character(),
      csv_field_name      = character(),
      raw_value           = character(),
      clean_value         = character()
    ))
  }
  
  recodes <- read_csv(recode_path, show_col_types = FALSE)
  
  required_cols <- c("output_csv_filename", "csv_field_name", "raw_value", "clean_value")
  missing <- setdiff(required_cols, names(recodes))
  if (length(missing) > 0) stop(paste("recode.csv missing columns:", paste(missing, collapse = ", ")))
  
  log_msg(paste("Recode table loaded -", nrow(recodes), "recode rules across",
                length(unique(recodes$output_csv_filename)), "file(s)"))
  return(recodes)
}

# =============================================================================
# 3. PATH RESOLUTION (test_mode aware)
# =============================================================================

resolve_path <- function(type, dashboard, filename, test_mode, timestamp = NULL) {
  # type: "master" | "current_processed" | "archive"
  base <- if (test_mode) here("test_output") else here("data")
  
  if (type == "archive") {
    if (is.null(timestamp)) stop("timestamp required for archive path")
    fname <- sub("\\.csv$", paste0("_", timestamp, ".csv"), filename)
    return(file.path(base, "archive", dashboard, fname))
  }
  
  file.path(base, type, dashboard, filename)
}

# =============================================================================
# 4. READ SHEET
# =============================================================================

read_sheet <- function(xlsx_path, tab_name, dd_tab) {
  
  src_xlsx <- basename(xlsx_path)
  
  # Always skip Guide tab
  if (tolower(trimws(tab_name)) == "guide") {
    log_msg(paste("Skipping Guide tab |", src_xlsx))
    return(NULL)
  }
  
  # Skip if tab not in DD
  if (nrow(dd_tab) == 0) {
    log_msg(paste("SKIP - tab not in DD |", src_xlsx, "|", tab_name), "WARN")
    return(NULL)
  }
  
  # Read all as text to preserve raw values before cleaning
  raw <- tryCatch(
    read_excel(xlsx_path, sheet = tab_name, col_names = TRUE, col_types = "text"),
    error = function(e) {
      log_msg(paste("ERROR reading sheet |", src_xlsx, "|", tab_name, "|", e$message), "ERROR")
      return(NULL)
    }
  )
  if (is.null(raw)) return(NULL)
  
  # Drop fully empty rows
  raw <- raw %>% filter(if_any(everything(), ~ !is.na(.x) & trimws(.x) != ""))
  
  expected_n <- nrow(dd_tab)
  actual_n   <- ncol(raw)
  
  if (actual_n != expected_n) {
    log_msg(paste(
      "HARD STOP - column count mismatch |", src_xlsx, "|", tab_name,
      "| DD expects:", expected_n, "| Sheet has:", actual_n
    ), "ERROR")
    stop(paste("Column count mismatch:", src_xlsx, tab_name))
  }
  
  # Rename columns by position from DD
  expected_names <- dd_tab %>% arrange(input_col_num) %>% pull(csv_field_name)
  names(raw)     <- expected_names
  
  log_msg(paste("Read OK |", src_xlsx, "|", tab_name,
                "| Cols:", paste(expected_names, collapse = ", ")))
  return(raw)
}

# =============================================================================
# 5. CLEAN SHEET
# =============================================================================

clean_sheet <- function(df, dd_tab, recodes, src_xlsx, tab_name) {
  
  log_msg(paste("Cleaning |", src_xlsx, "|", tab_name))
  
  filename <- unique(dd_tab$output_csv_filename)
  
  for (i in seq_len(nrow(dd_tab))) {
    
    col    <- dd_tab$csv_field_name[i]
    ftype  <- dd_tab$field_type[i]
    is_pct <- isTRUE(dd_tab$pct[i])
    
    if (!col %in% names(df)) next
    
    raw_vals <- as.character(df[[col]])
    
    # --- Apply recode map if exists for this file + col
    col_recodes <- recodes %>%
      filter(output_csv_filename == filename, csv_field_name == col)
    
    if (nrow(col_recodes) > 0) {
      recode_map        <- setNames(col_recodes$clean_value, col_recodes$raw_value)
      recoded_vals      <- recode(raw_vals, !!!recode_map)
      n_recoded         <- sum(recoded_vals != raw_vals, na.rm = TRUE)
      if (n_recoded > 0) {
        log_msg(paste("Recode applied |", col, "| n =", n_recoded, "|",
                      src_xlsx, "|", tab_name))
      }
      df[[col]] <- recoded_vals
      next  # skip further cleaning for recoded character cols
    }
    
    # Strip $, %, commas, and whitespace from all non-character cols
    cleaned <- if (ftype == "character") raw_vals else str_remove_all(raw_vals, "[$%,\\s]")
    
    # --- Percentage validation (pct == TRUE cols)
    # If ANY value < 1 after stripping → assume decimal format, auto x100 + warn
    if (is_pct) {
      numeric_vals <- suppressWarnings(as.numeric(cleaned))
      any_under_1  <- any(numeric_vals < 1, na.rm = TRUE)
      
      if (any_under_1) {
        log_msg(paste(
          "WARN - pct col has values < 1, auto x100 applied |",
          col, "|", src_xlsx, "|", tab_name
        ), "WARN")
        cleaned <- as.character(round(numeric_vals * 100, 2))
      }
    }
    
    # --- Type coercion per DD field_type
    df[[col]] <- switch(ftype,
                        "year"      = as.integer(cleaned),
                        "integer"   = as.integer(cleaned),
                        "float"     = round(as.numeric(cleaned), 2),
                        "numeric"   = round(as.numeric(cleaned), 2),
                        "character" = as.character(raw_vals),
                        as.character(raw_vals)
    )
  }
  
  log_msg(paste("Clean OK |", src_xlsx, "|", tab_name))
  return(df)
}

# =============================================================================
# 6. VALIDATE SHEET
# =============================================================================

# Allowed values per categorical column
# Radon uses full descriptive labels as entered by staff
categorical_rules <- list(
  inspection_status    = c("Pass", "Re-Inspection Required", "Closed"),
  radon_metric         = c("Median radon test (pCi/L)",
                           "Maximum radon test value (pCi/L)",
                           "Percentage of tests above 4.0 pCi/L",
                           "Total tests conducted"),
  overdose_type        = c("Total drug overdoses",
                           "Opioid-involved drug overdoses",
                           "Fentanyl-involved drug overdoses"),
  screening_result     = c("Total Passed", "Total Referred"),
  mentor_program       = c("Mountain Mentor - Youth Enrolled",
                           "Mountain Mentor - Waiting for Adult Mentor",
                           "Mountain Mentor - Matched with an Adult Mentor",
                           "Peer Mentor - Youth Enrolled"),
  contaminant_category = c("Bacteria", "High nitrates", "No contaminates"),
  produce_type         = c("Herbs & Garlic", "Bunched Greens", "Carrots",
                           "Baby Greens", "Peas & Broccoli", "Summer Produce",
                           "Potatoes & Beets", "Radishes & Turnips"),
  nfp_metric           = c("Clients", "Visits"),
  region               = c("Summit", "CO"),
  breastfeeding_metric = c("At Birth", "At 6 months"),
  location             = c("Summit Middle School", "Colorado", "Summit High School")
)

validate_sheet <- function(df, dd_tab, master_df, src_xlsx, tab_name) {
  
  log_msg(paste("Validating |", src_xlsx, "|", tab_name))
  has_year  <- "year" %in% names(df)
  trans_type <- dd_tab$transform_type[!is.na(dd_tab$transform_type)][1]
  trans_type <- if (length(trans_type) == 0 || is.na(trans_type)) NA_character_ else trans_type
  
  # --- single_year: skip year gate entirely, always pass through
  if (!is.na(trans_type) && trans_type == "single_year") {
    log_msg(paste("single_year tab - skipping year gate |", src_xlsx, "|", tab_name))
    return(df)
  }
  
  # --- Year range check
  if (has_year) {
    bad_years <- df$year[!is.na(df$year) & (df$year < 2000 | df$year > 2100)]
    if (length(bad_years) > 0) {
      log_msg(paste(
        "WARN - suspicious year values:", paste(bad_years, collapse = ", "),
        "|", src_xlsx, "|", tab_name
      ), "WARN")
    }
  }
  
  # --- Categorical value checks
  for (col in names(categorical_rules)) {
    if (col %in% names(df)) {
      bad_vals <- setdiff(unique(na.omit(df[[col]])), categorical_rules[[col]])
      if (length(bad_vals) > 0) {
        log_msg(paste(
          "WARN - unexpected values in", col, ":",
          paste(bad_vals, collapse = ", "),
          "|", src_xlsx, "|", tab_name
        ), "WARN")
      }
    }
  }
  
  # --- New year gate
  if (!has_year) {
    log_msg(paste("No year col - skipping duplicate year check |",
                  src_xlsx, "|", tab_name))
    return(df)
  }
  
  master_years   <- unique(master_df$year)
  incoming_years <- unique(df$year)
  new_years      <- setdiff(incoming_years, master_years)
  
  if (length(new_years) == 0) {
    log_msg(paste(
      "SKIP - no new years |", src_xlsx, "|", tab_name,
      "| Incoming:", paste(sort(incoming_years), collapse = ", "),
      "| Already in master:", paste(sort(master_years), collapse = ", ")
    ), "INFO")
    return(NULL)
  }
  
  log_msg(paste(
    "New years accepted:", paste(sort(new_years), collapse = ", "),
    "|", src_xlsx, "|", tab_name
  ))
  
  df %>% filter(year %in% new_years)
}

# =============================================================================
# 7. UPDATE MASTER
# =============================================================================

update_master <- function(new_df, master_path, dashboard, filename,
                          test_mode, trans_type = NA_character_) {
  
  if (!file.exists(master_path)) {
    log_msg(paste("HARD STOP - master not found:", master_path), "ERROR")
    stop(paste("Master CSV not found:", master_path))
  }
  
  master_df <- read_csv(master_path, show_col_types = FALSE)
  
  # Archive current master BEFORE any write
  ts           <- format(Sys.time(), "%Y%m%d_%H%M%S")
  archive_path <- resolve_path("archive", dashboard, filename, test_mode, ts)
  dir.create(dirname(archive_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(master_df, archive_path)
  log_msg(paste("Master archived |", archive_path))
  
  # single_year: replace master entirely rather than append
  if (!is.na(trans_type) && trans_type == "single_year") {
    write_csv(new_df, master_path)
    log_msg(paste("Master replaced (single_year) |", master_path,
                  "| Rows:", nrow(new_df)))
    return(new_df)
  }
  
  # Coerce new_df cols to match master col types before binding
  # Prevents type conflicts e.g. character vs integer on read_excel vs read_csv
  for (col in intersect(names(master_df), names(new_df))) {
    master_type <- class(master_df[[col]])[1]
    tryCatch({
      new_df[[col]] <- switch(master_type,
                              "integer"   = as.integer(new_df[[col]]),
                              "numeric"   = as.numeric(new_df[[col]]),
                              "character" = as.character(new_df[[col]]),
                              new_df[[col]]
      )
    }, error = function(e) NULL)
  }
  
  # Standard: append new rows
  updated <- bind_rows(master_df, new_df)
  write_csv(updated, master_path)
  log_msg(paste(
    "Master updated |", master_path,
    "| +", nrow(new_df), "rows | Total:", nrow(updated)
  ))
  
  return(updated)
}

# =============================================================================
# 8. WRITE CURRENT PROCESSED
# =============================================================================

write_current <- function(master_df, dd_tab, dashboard, filename, test_mode) {
  
  current_path <- resolve_path("current_processed", dashboard, filename, test_mode)
  dir.create(dirname(current_path), recursive = TRUE, showWarnings = FALSE)
  
  has_year   <- "year" %in% names(master_df)
  trans_type <- dd_tab$transform_type[!is.na(dd_tab$transform_type)][1]
  trans_type <- if (length(trans_type) == 0 || is.na(trans_type)) NA_character_ else trans_type
  
  if (!is.na(trans_type) && trans_type == "single_year") {
    # Pass through as-is - master already holds only current year
    output_df <- master_df
    
  } else if (!is.na(trans_type) && trans_type == "rolling_mean") {
    # rolling_mean transform:
    # - Filter master to <=5 most recent unique years
    # - Group by contaminant_category
    # - Mean the pct col (pct == TRUE in DD) -> renamed to avg_result in output
    # - Append year_range metadata col (e.g. "2018_2022")
    # - Output is 3 cols: contaminant_category | avg_result | year_range
    # To extend: ensure pct == TRUE on exactly one col, group_by col must
    # be contaminant_category (or extend logic below for other grouping cols)
    recent_years <- master_df %>%
      distinct(year) %>%
      arrange(desc(year)) %>%
      slice_head(n = 5) %>%
      pull(year)
    
    n_years <- length(recent_years)
    if (n_years < 3) {
      log_msg(paste("WARN - rolling_mean using only", n_years,
                    "year(s) for", filename), "WARN")
    }
    
    # Identify the pct col to average from DD
    pct_col <- dd_tab %>% filter(isTRUE(pct)) %>% pull(csv_field_name)
    if (length(pct_col) != 1) {
      stop(paste("rolling_mean requires exactly one pct == TRUE col in DD for", filename))
    }
    
    year_range <- paste(min(recent_years), max(recent_years), sep = "_")
    
    output_df <- master_df %>%
      filter(year %in% recent_years) %>%
      group_by(contaminant_category) %>%
      summarise(
        avg_result = round(mean(.data[[pct_col]], na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      mutate(year_range = year_range)
    
    log_msg(paste("rolling_mean applied | pct_col:", pct_col,
                  "| year_range:", year_range, "| n_years:", n_years,
                  "| categories:", nrow(output_df), "|", filename))
    
  } else if (has_year) {
    # Standard: ≤5 most recent unique years
    recent_years <- master_df %>%
      distinct(year) %>%
      arrange(desc(year)) %>%
      slice_head(n = 5) %>%
      pull(year)
    
    output_df <- master_df %>% filter(year %in% recent_years)
    
  } else {
    # No year col, no transform - full pass-through
    output_df <- master_df
  }
  
  # --- pct_divide: /100 on all pct == TRUE cols
  # Skipped for rolling_mean tabs - pct col already consumed into avg_result
  if (is.na(trans_type) || trans_type != "rolling_mean") {
    pct_cols <- dd_tab %>% filter(isTRUE(pct)) %>% pull(csv_field_name)
    for (col in pct_cols) {
      if (col %in% names(output_df)) {
        output_df[[col]] <- round(output_df[[col]] / 100, 4)
        log_msg(paste("pct_divide applied |", col, "|", filename))
      }
    }
  }
  
  # Write current_processed
  write_csv(output_df, current_path)
  log_msg(paste("current_processed written |", current_path,
                "| Rows:", nrow(output_df)))
  
  # Archive current_processed snapshot
  ts           <- format(Sys.time(), "%Y%m%d_%H%M%S")
  archive_path <- resolve_path("archive", dashboard, filename, test_mode, ts)
  dir.create(dirname(archive_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(output_df, archive_path)
  log_msg(paste("current_processed archived |", archive_path))
  
  return(invisible(output_df))
}

# =============================================================================
# 9. PROCESS ONE TAB - full chain
# =============================================================================

process_tab <- function(xlsx_path, tab_name, dd, recodes, test_mode) {
  
  src_xlsx <- basename(xlsx_path)
  
  # Filter DD to this exact xlsx + tab
  dd_tab <- dd %>%
    filter(source_xlsx == src_xlsx, source_tab == tab_name) %>%
    arrange(input_col_num)
  
  if (nrow(dd_tab) == 0) {
    if (tolower(trimws(tab_name)) != "guide") {
      log_msg(paste("SKIP - not in DD |", src_xlsx, "|", tab_name), "WARN")
    }
    return(invisible(NULL))
  }
  
  filename   <- unique(dd_tab$output_csv_filename)
  dashboard  <- unique(dd_tab$pbi)
  trans_type <- dd_tab$transform_type[!is.na(dd_tab$transform_type)][1]
  trans_type <- if (length(trans_type) == 0 || is.na(trans_type)) NA_character_ else trans_type
  
  if (length(filename) > 1 || length(dashboard) > 1) {
    log_msg(paste("HARD STOP - tab maps to multiple output files |",
                  src_xlsx, "|", tab_name), "ERROR")
    stop("Tab maps to multiple output files")
  }
  
  # Master path respects test_mode - in test_mode writes go to test_output/master/
  # In test_mode, seed master from real data if test master doesn't exist yet
  master_path <- resolve_path("master", dashboard, filename, test_mode)
  real_master_path <- resolve_path("master", dashboard, filename, test_mode = FALSE)
  
  if (test_mode && !file.exists(master_path)) {
    # Seed test master from real master on first test run
    if (!file.exists(real_master_path)) {
      log_msg(paste("HARD STOP - real master not found |", real_master_path), "ERROR")
      stop(paste("Master not found:", real_master_path))
    }
    dir.create(dirname(master_path), recursive = TRUE, showWarnings = FALSE)
    file.copy(real_master_path, master_path)
    log_msg(paste("Test master seeded from real master |", master_path))
  }
  
  if (!file.exists(master_path)) {
    log_msg(paste("HARD STOP - master not found |", master_path), "ERROR")
    stop(paste("Master not found:", master_path))
  }
  
  master_df <- read_csv(master_path, show_col_types = FALSE)
  
  # --- Pipeline chain
  raw_df <- read_sheet(xlsx_path, tab_name, dd_tab)
  if (is.null(raw_df)) return(invisible(NULL))
  
  clean_df <- clean_sheet(raw_df, dd_tab, recodes, src_xlsx, tab_name)
  
  validated_df <- validate_sheet(clean_df, dd_tab, master_df, src_xlsx, tab_name)
  if (is.null(validated_df)) return(invisible(NULL))
  
  updated_master <- update_master(
    new_df      = validated_df,
    master_path = master_path,
    dashboard   = dashboard,
    filename    = filename,
    test_mode   = test_mode,
    trans_type  = trans_type
  )
  
  write_current(
    master_df = updated_master,
    dd_tab    = dd_tab,
    dashboard = dashboard,
    filename  = filename,
    test_mode = test_mode
  )
  
  log_msg(paste("SUCCESS |", src_xlsx, "|", tab_name, "->", filename))
  return(invisible(NULL))
}

# =============================================================================
# 10. GENERATE OUTPUT EMAIL XLSXs
# =============================================================================
# For each dashboard XLSX:
#   - Read each tab's master CSV
#   - Filter to most recent year only (all rows for that year)
#   - Preserve Guide tab from input_email XLSX as-is
#   - Write output XLSX to output_email/ (or test_output/output_email/ in test_mode)
#   - Identical structure to input_email - raw master format, no transforms
#   - Overwrites silently on every run

generate_output_email <- function(dd, test_mode) {
  
  log_msg("--- Generating output_email XLSXs ---")
  
  input_dir  <- here("data", "input_email")
  output_dir <- if (test_mode) here("test_output", "output_email") else here("data", "output_email")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  xlsx_files <- list.files(input_dir, pattern = "\\.xlsx$", full.names = TRUE)
  
  if (length(xlsx_files) == 0) {
    log_msg("WARN - no XLSX files found in data/input_email/ for output_email generation", "WARN")
    return(invisible(NULL))
  }
  
  for (xlsx_path in xlsx_files) {
    
    src_xlsx    <- basename(xlsx_path)
    all_tabs    <- tryCatch(excel_sheets(xlsx_path), error = function(e) NULL)
    if (is.null(all_tabs)) {
      log_msg(paste("ERROR reading tabs for output_email |", src_xlsx), "ERROR")
      next
    }
    
    output_tabs <- list()
    
    for (tab_name in all_tabs) {
      
      # Preserve Guide tab as-is from input_email
      if (tolower(trimws(tab_name)) == "guide") {
        output_tabs[[tab_name]] <- tryCatch(
          read_excel(xlsx_path, sheet = tab_name, col_types = "text"),
          error = function(e) NULL
        )
        next
      }
      
      # Get DD rows for this tab
      dd_tab <- dd %>%
        filter(source_xlsx == src_xlsx, source_tab == tab_name) %>%
        arrange(input_col_num)
      
      if (nrow(dd_tab) == 0) {
        log_msg(paste("WARN - tab not in DD, skipping for output_email |",
                      src_xlsx, "|", tab_name), "WARN")
        next
      }
      
      filename  <- unique(dd_tab$output_csv_filename)
      dashboard <- unique(dd_tab$pbi)
      
      # output_email always reads from real master regardless of test_mode
      # so staff always get real data in their request emails
      master_path <- resolve_path("master", dashboard, filename, test_mode = FALSE)
      
      if (!file.exists(master_path)) {
        log_msg(paste("WARN - master not found for output_email |", master_path), "WARN")
        next
      }
      
      master_df <- read_csv(master_path, show_col_types = FALSE)
      
      # Filter to most recent year only (all rows for that year)
      if ("year" %in% names(master_df)) {
        most_recent <- max(master_df$year, na.rm = TRUE)
        tab_df      <- master_df %>% filter(year == most_recent)
        log_msg(paste("output_email | most recent year:", most_recent,
                      "| rows:", nrow(tab_df), "|", src_xlsx, "|", tab_name))
      } else {
        # No year col (e.g. future tabs) - pass full master through
        tab_df <- master_df
        log_msg(paste("output_email | no year col, full passthrough |",
                      src_xlsx, "|", tab_name))
      }
      
      # Read original human-readable col headers from input_email XLSX by position
      original_headers <- tryCatch(
        names(read_excel(xlsx_path, sheet = tab_name, n_max = 0, col_types = "text")),
        error = function(e) NULL
      )
      
      if (!is.null(original_headers) && length(original_headers) == ncol(tab_df)) {
        names(tab_df) <- original_headers
      } else {
        log_msg(paste("WARN - could not map original headers for output_email |",
                      src_xlsx, "|", tab_name), "WARN")
      }
      
      output_tabs[[tab_name]] <- tab_df
    }
    
    # Write XLSX to output_email dir
    out_path <- file.path(output_dir, src_xlsx)
    tryCatch({
      writexl::write_xlsx(output_tabs, out_path)
      log_msg(paste("output_email written |", out_path))
    }, error = function(e) {
      log_msg(paste("ERROR writing output_email |", src_xlsx, "|", e$message), "ERROR")
    })
  }
  
  log_msg("--- output_email generation complete ---")
  return(invisible(NULL))
}

# =============================================================================
# 11. GENERATE SHINY STATUS CSVs
# =============================================================================
# Reads every master CSV, extracts most recent year per tab, applies stoplight
# logic, rolls up to dashboard level (worst status wins).
#
# Status rules (based on current system year):
#   Green  = most_recent_year >= current_year - 1
#   Yellow = most_recent_year == current_year - 2
#   Red    = most_recent_year <= current_year - 3
#   Red    = no year col or master unreadable
#
# Outputs:
#   status_summary.csv  - one row per dashboard (B1)
#   status_detail.csv   - one row per master CSV (B2)

generate_status <- function(dd, test_mode) {
  
  log_msg("--- Generating Shiny status CSVs ---")
  
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  log_msg(paste("Status reference year:", current_year))
  
  out_dir <- if (test_mode) here("test_output", "shiny", "data") else here("shiny", "data")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Build lookup: one row per unique output_csv_filename
  tab_lookup <- dd %>%
    select(source_tab, output_csv_filename, pbi) %>%
    distinct(output_csv_filename, .keep_all = TRUE)
  
  # Status helper
  assign_status <- function(most_recent_year) {
    if (is.na(most_recent_year)) return("Red")
    if (most_recent_year >= current_year - 1) return("Green")
    if (most_recent_year == current_year - 2) return("Yellow")
    return("Red")
  }
  
  # Status rank for worst-wins rollup
  status_rank <- c("Green" = 1, "Yellow" = 2, "Red" = 3)
  
  # =============================================================================
  # B2 - Detail: one row per master CSV
  # =============================================================================
  
  detail_rows <- map_dfr(seq_len(nrow(tab_lookup)), function(i) {
    
    row       <- tab_lookup[i, ]
    filename  <- row$output_csv_filename
    dashboard <- row$pbi
    tab_name  <- row$source_tab
    
    # Status always reads from real master - reflects true data freshness
    master_path <- resolve_path("master", dashboard, filename, test_mode = FALSE)
    
    if (!file.exists(master_path)) {
      log_msg(paste("WARN - master not found for status |", master_path), "WARN")
      return(tibble(
        dashboard        = dashboard,
        source_tab       = tab_name,
        output_csv       = filename,
        most_recent_year = NA_integer_,
        status           = "Red"
      ))
    }
    
    master_df <- tryCatch(
      read_csv(master_path, show_col_types = FALSE),
      error = function(e) {
        log_msg(paste("ERROR reading master for status |", master_path), "ERROR")
        return(NULL)
      }
    )
    
    if (is.null(master_df) || !"year" %in% names(master_df)) {
      # No year col (e.g. eh_well_water before transform) - use NA, flag Red
      return(tibble(
        dashboard        = dashboard,
        source_tab       = tab_name,
        output_csv       = filename,
        most_recent_year = NA_integer_,
        status           = "Red"
      ))
    }
    
    most_recent <- max(master_df$year, na.rm = TRUE)
    status      <- assign_status(most_recent)
    
    tibble(
      dashboard        = dashboard,
      source_tab       = tab_name,
      output_csv       = filename,
      most_recent_year = as.integer(most_recent),
      status           = status
    )
  })
  
  # =============================================================================
  # B1 - Summary: one row per dashboard, worst status wins
  # =============================================================================
  
  summary_rows <- detail_rows %>%
    group_by(dashboard) %>%
    summarise(
      status = names(which.max(status_rank[status])),
      .groups = "drop"
    ) %>%
    arrange(dashboard)
  
  # =============================================================================
  # WRITE OUTPUTS
  # =============================================================================
  
  summary_path <- file.path(out_dir, "status_summary.csv")
  detail_path  <- file.path(out_dir, "status_detail.csv")
  
  write_csv(summary_rows, summary_path)
  write_csv(detail_rows,  detail_path)
  
  log_msg(paste("status_summary.csv written |", summary_path,
                "| Rows:", nrow(summary_rows)))
  log_msg(paste("status_detail.csv written |",  detail_path,
                "| Rows:", nrow(detail_rows)))
  
  # Console preview
  cat("\n========== DATA STATUS SUMMARY =============\n")
  cat("Reference year:", current_year, "| Green >= ", current_year - 1,
      "| Yellow =", current_year - 2, "| Red <=", current_year - 3, "\n\n")
  print(summary_rows, n = Inf)
  cat("=============================================\n\n")
  
  log_msg("--- Shiny status CSV generation complete ---")
  return(invisible(NULL))
}

# =============================================================================
# 12. GENERATE KPI SUMMARY PDF
# =============================================================================
# Reads current_processed CSVs, computes YOY change + trend arrow per metric
# Outputs a single gt-based PDF to reports/kpi_summary.pdf (overwrites each run)
# In test_mode writes to test_output/reports/kpi_summary.pdf

generate_kpi_pdf <- function(test_mode) {
  
  library(gt)
  library(gtExtras)
  
  log_msg("--- Generating KPI summary PDF ---")
  
  cp_dir   <- here("data", "current_processed")
  out_dir  <- if (test_mode) here("test_output", "reports") else here("reports")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, "kpi_summary.pdf")
  
  report_date <- format(Sys.Date(), "%B %Y")
  
  # ---------------------------------------------------------------------------
  # HELPERS
  # ---------------------------------------------------------------------------
  
  read_cp <- function(dashboard, filename) {
    fp <- file.path(cp_dir, dashboard, filename)
    if (!file.exists(fp)) return(NULL)
    read_csv(fp, show_col_types = FALSE)
  }
  
  trend_arrow <- function(pct_change) {
    if (is.na(pct_change)) return("")
    if (pct_change > 2)  return("(+)")
    if (pct_change < -2) return("(-)")
    return("(=)")
  }
  
  fmt_val <- function(x, is_pct = FALSE) {
    if (is.na(x)) return("")
    if (is_pct) return(paste0(round(x * 100, 1), "%"))
    if (x == round(x)) return(format(as.integer(x), big.mark = ","))
    return(as.character(round(x, 2)))
  }
  
  # Build one metric row from two year values
  make_row <- function(metric, val_recent, val_prior, is_pct = FALSE) {
    yoy_change <- if (!is.na(val_recent) && !is.na(val_prior)) val_recent - val_prior else NA
    yoy_pct    <- if (!is.na(val_prior) && val_prior != 0) (yoy_change / abs(val_prior)) * 100 else NA
    tibble(
      Metric           = metric,
      `Most Recent`    = fmt_val(val_recent, is_pct),
      `Prior Year`     = fmt_val(val_prior,  is_pct),
      `YOY Change`     = if (!is.na(yoy_change)) fmt_val(yoy_change, is_pct) else "",
      `YOY % Change`   = if (!is.na(yoy_pct))    paste0(round(yoy_pct, 1), "%") else "",
      Trend            = trend_arrow(yoy_pct)
    )
  }
  
  # Extract most recent and prior year rows from a df
  get_years <- function(df) {
    if (is.null(df) || !"year" %in% names(df)) return(list(recent = NULL, prior = NULL, yr = NA, pr = NA))
    yrs    <- sort(unique(df$year), decreasing = TRUE)
    yr     <- yrs[1]
    pr     <- if (length(yrs) >= 2) yrs[2] else NA
    recent <- df %>% filter(year == yr)
    prior  <- if (!is.na(pr)) df %>% filter(year == pr) else NULL
    list(recent = recent, prior = prior, yr = yr, pr = pr)
  }
  
  # Build gt table for one dashboard
  make_gt <- function(rows_df, title) {
    rows_df %>%
      gt() %>%
      tab_header(title = md(paste0("**", title, "**"))) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      ) %>%
      tab_options(
        table.font.size = 10,
        heading.title.font.size = 12,
        column_labels.font.weight = "bold",
        table.border.top.color = "black",
        table.border.bottom.color = "black",
        column_labels.border.bottom.color = "black"
      ) %>%
      cols_align(align = "center", columns = c(`Most Recent`, `Prior Year`, `YOY Change`, `YOY % Change`, Trend)) %>%
      cols_align(align = "left", columns = Metric)
  }
  
  # ---------------------------------------------------------------------------
  # METRIC EXTRACTION PER DASHBOARD
  # ---------------------------------------------------------------------------
  
  tables <- list()
  
  # --- EH ---
  tryCatch({
    rows <- tibble()
    
    # Retail inspections - one row per status
    retail <- read_cp("eh", "eh_retail.csv")
    if (!is.null(retail)) {
      y <- get_years(retail)
      for (s in c("Pass", "Re-Inspection Required", "Closed")) {
        rv <- y$recent %>% filter(inspection_status == s) %>% pull(inspection_pct) %>% first()
        pv <- if (!is.null(y$prior)) y$prior %>% filter(inspection_status == s) %>% pull(inspection_pct) %>% first() else NA
        rows <- bind_rows(rows, make_row(paste("Retail Inspection -", s, "%"), rv, pv, is_pct = TRUE))
      }
    }
    
    # Complaints
    comp <- read_cp("eh", "eh_complaints.csv")
    if (!is.null(comp)) {
      y <- get_years(comp)
      for (col in c("complaints_water_quality", "complaints_retail_food", "complaints_other")) {
        label <- str_to_title(str_replace_all(str_remove(col, "complaints_"), "_", " "))
        rv <- y$recent %>% pull(!!sym(col)) %>% sum(na.rm = TRUE)
        pv <- if (!is.null(y$prior)) y$prior %>% pull(!!sym(col)) %>% sum(na.rm = TRUE) else NA
        rows <- bind_rows(rows, make_row(paste("Complaints -", label), rv, pv))
      }
    }
    
    # Wastewater
    ww <- read_cp("eh", "eh_wastewater.csv")
    if (!is.null(ww)) {
      y <- get_years(ww)
      for (s in unique(ww$wastewater_result)) {
        rv <- y$recent %>% filter(wastewater_result == s) %>% pull(wastewater_pct) %>% first()
        pv <- if (!is.null(y$prior)) y$prior %>% filter(wastewater_result == s) %>% pull(wastewater_pct) %>% first() else NA
        rows <- bind_rows(rows, make_row(paste("Wastewater -", s, "%"), rv, pv, is_pct = TRUE))
      }
    }
    
    if (nrow(rows) > 0) {
      yr_label <- get_years(retail %||% comp %||% ww)$yr
      tables[["EH"]] <- make_gt(rows, paste0("Environmental Health (", yr_label, " vs prior year)"))
    }
  }, error = function(e) log_msg(paste("WARN - EH KPI error:", e$message), "WARN"))
  
  # --- OPI ---
  tryCatch({
    rows <- tibble()
    
    kits <- read_cp("opi", "opi_kits.csv")
    if (!is.null(kits)) {
      y  <- get_years(kits)
      rv <- y$recent %>% pull(naloxone_kits_distributed) %>% sum()
      pv <- if (!is.null(y$prior)) y$prior %>% pull(naloxone_kits_distributed) %>% sum() else NA
      rows <- bind_rows(rows, make_row("Naloxone Kits Distributed", rv, pv))
    }
    
    tb <- read_cp("opi", "opi_takeback.csv")
    if (!is.null(tb)) {
      y  <- get_years(tb)
      rv <- y$recent %>% pull(pounds_collected) %>% sum()
      pv <- if (!is.null(y$prior)) y$prior %>% pull(pounds_collected) %>% sum() else NA
      rows <- bind_rows(rows, make_row("Medication Take-Back (lbs)", rv, pv))
    }
    
    od <- read_cp("opi", "opi_overdose.csv")
    if (!is.null(od)) {
      y <- get_years(od)
      for (s in c("Total drug overdoses", "Opioid-involved drug overdoses", "Fentanyl-involved drug overdoses")) {
        rv <- y$recent %>% filter(overdose_type == s) %>% pull(overdoses) %>% first()
        pv <- if (!is.null(y$prior)) y$prior %>% filter(overdose_type == s) %>% pull(overdoses) %>% first() else NA
        rows <- bind_rows(rows, make_row(s, rv, pv))
      }
    }
    
    if (nrow(rows) > 0) tables[["OPI"]] <- make_gt(rows, paste0("Opioid Overdose Prevention (", get_years(kits)$yr, " vs prior year)"))
  }, error = function(e) log_msg(paste("WARN - OPI KPI error:", e$message), "WARN"))
  
  # --- DIS ---
  tryCatch({
    rows <- tibble()
    specs <- list(
      list(f = "dis_comm_dis.csv",  col = "cases_reported_investigated",      label = "Communicable Disease Cases"),
      list(f = "dis_animal.csv",    col = "animal_bites_reported_investigated", label = "Animal Bites"),
      list(f = "dis_imz.csv",       col = "immunizations_administered",         label = "Immunizations Administered"),
      list(f = "dis_outbreak.csv",  col = "outbreaks_identified",               label = "Outbreaks Identified")
    )
    ref_yr <- NA
    for (s in specs) {
      df <- read_cp("dis", s$f)
      if (!is.null(df)) {
        y  <- get_years(df)
        if (is.na(ref_yr)) ref_yr <- y$yr
        rv <- y$recent %>% pull(!!sym(s$col)) %>% sum(na.rm = TRUE)
        pv <- if (!is.null(y$prior)) y$prior %>% pull(!!sym(s$col)) %>% sum(na.rm = TRUE) else NA
        rows <- bind_rows(rows, make_row(s$label, rv, pv))
      }
    }
    if (nrow(rows) > 0) tables[["DIS"]] <- make_gt(rows, paste0("Communicable Diseases & Exposures (", ref_yr, " vs prior year)"))
  }, error = function(e) log_msg(paste("WARN - DIS KPI error:", e$message), "WARN"))
  
  # --- HEARVIS ---
  tryCatch({
    rows <- tibble()
    hv <- read_cp("hearvis", "hearvis_screen.csv")
    if (!is.null(hv)) {
      y <- get_years(hv)
      for (s in c("Total Passed", "Total Referred")) {
        rv <- y$recent %>% filter(screening_result == s) %>% pull(screenings) %>% first()
        pv <- if (!is.null(y$prior)) y$prior %>% filter(screening_result == s) %>% pull(screenings) %>% first() else NA
        rows <- bind_rows(rows, make_row(s, rv, pv))
      }
    }
    if (nrow(rows) > 0) tables[["HEARVIS"]] <- make_gt(rows, paste0("Hearing & Vision Screenings Ages 0-5 (", get_years(hv)$yr, " vs prior year)"))
  }, error = function(e) log_msg(paste("WARN - HEARVIS KPI error:", e$message), "WARN"))
  
  # --- FAM ---
  tryCatch({
    rows <- tibble()
    specs <- list(
      list(f = "fam_homevisit.csv", col = "families_enrolled",  label = "Families Enrolled (Home Visitation)"),
      list(f = "fam_playgroup.csv", col = "playgroup_attendees", label = "Playgroup Attendees")
    )
    ref_yr <- NA
    for (s in specs) {
      df <- read_cp("fam", s$f)
      if (!is.null(df)) {
        y  <- get_years(df)
        if (is.na(ref_yr)) ref_yr <- y$yr
        rv <- y$recent %>% pull(!!sym(s$col)) %>% sum(na.rm = TRUE)
        pv <- if (!is.null(y$prior)) y$prior %>% pull(!!sym(s$col)) %>% sum(na.rm = TRUE) else NA
        rows <- bind_rows(rows, make_row(s$label, rv, pv))
      }
    }
    if (nrow(rows) > 0) tables[["FAM"]] <- make_gt(rows, paste0("Connecting Families (", ref_yr, " vs prior year)"))
  }, error = function(e) log_msg(paste("WARN - FAM KPI error:", e$message), "WARN"))
  
  # --- YOUTH ---
  tryCatch({
    rows <- tibble()
    ref_yr <- NA
    
    hks <- read_cp("youth", "youth_hks.csv")
    if (!is.null(hks)) {
      y <- get_years(hks)
      if (is.na(ref_yr)) ref_yr <- y$yr
      for (col in c("shs_pct", "sms_pct")) {
        label <- if (col == "shs_pct") "Summit High School E-Cig %" else "Summit Middle School E-Cig %"
        rv <- y$recent %>% pull(!!sym(col)) %>% first()
        pv <- if (!is.null(y$prior)) y$prior %>% pull(!!sym(col)) %>% first() else NA
        rows <- bind_rows(rows, make_row(label, rv, pv, is_pct = TRUE))
      }
    }
    
    yess <- read_cp("youth", "youth_yess.csv")
    if (!is.null(yess)) {
      y  <- get_years(yess)
      rv <- y$recent %>% pull(yess_attendance) %>% sum()
      pv <- if (!is.null(y$prior)) y$prior %>% pull(yess_attendance) %>% sum() else NA
      rows <- bind_rows(rows, make_row("YESS Attendance", rv, pv))
    }
    
    tc <- read_cp("youth", "youth_teen_center.csv")
    if (!is.null(tc)) {
      y  <- get_years(tc)
      rv <- y$recent %>% pull(youth_visits) %>% sum()
      pv <- if (!is.null(y$prior)) y$prior %>% pull(youth_visits) %>% sum() else NA
      rows <- bind_rows(rows, make_row("Teen Center Visits", rv, pv))
    }
    
    mentors <- read_cp("youth", "youth_mentors.csv")
    if (!is.null(mentors)) {
      y <- get_years(mentors)
      for (prog in c("Mountain Mentor - Youth Enrolled", "Mountain Mentor - Waiting for Adult Mentor",
                     "Mountain Mentor - Matched with an Adult Mentor", "Peer Mentor - Youth Enrolled")) {
        rv <- y$recent %>% filter(mentor_program == prog) %>% pull(count) %>% first()
        pv <- if (!is.null(y$prior)) y$prior %>% filter(mentor_program == prog) %>% pull(count) %>% first() else NA
        rows <- bind_rows(rows, make_row(prog, rv, pv))
      }
    }
    
    if (nrow(rows) > 0) tables[["YOUTH"]] <- make_gt(rows, paste0("Connecting Youth (", ref_yr, " vs prior year)"))
  }, error = function(e) log_msg(paste("WARN - YOUTH KPI error:", e$message), "WARN"))
  
  # --- TOB ---
  tryCatch({
    rows <- tibble()
    ref_yr <- NA
    
    ret <- read_cp("tob", "tob_retail.csv")
    if (!is.null(ret)) {
      y <- get_years(ret)
      if (is.na(ref_yr)) ref_yr <- y$yr
      for (col in c("compliance_checks", "retailers_failed")) {
        label <- if (col == "compliance_checks") "Compliance Checks" else "Retailers Failed"
        rv <- y$recent %>% pull(!!sym(col)) %>% sum()
        pv <- if (!is.null(y$prior)) y$prior %>% pull(!!sym(col)) %>% sum() else NA
        rows <- bind_rows(rows, make_row(label, rv, pv))
      }
    }
    
    vape <- read_cp("tob", "tob_vape.csv")
    if (!is.null(vape)) {
      y  <- get_years(vape)
      rv <- y$recent %>% pull(vapes_collected) %>% sum()
      pv <- if (!is.null(y$prior)) y$prior %>% pull(vapes_collected) %>% sum() else NA
      rows <- bind_rows(rows, make_row("Vapes Collected", rv, pv))
    }
    
    sms <- read_cp("tob", "tob_sms_ecig.csv")
    if (!is.null(sms)) {
      y  <- get_years(sms)
      rv <- y$recent %>% filter(location == "Summit Middle School") %>% pull(percentage) %>% first()
      pv <- if (!is.null(y$prior)) y$prior %>% filter(location == "Summit Middle School") %>% pull(percentage) %>% first() else NA
      rows <- bind_rows(rows, make_row("SMS E-Cigarette Use %", rv, pv, is_pct = TRUE))
    }
    
    shs <- read_cp("tob", "tob_shs_ecig.csv")
    if (!is.null(shs)) {
      y  <- get_years(shs)
      rv <- y$recent %>% filter(location == "Summit High School") %>% pull(percentage) %>% first()
      pv <- if (!is.null(y$prior)) y$prior %>% filter(location == "Summit High School") %>% pull(percentage) %>% first() else NA
      rows <- bind_rows(rows, make_row("SHS E-Cigarette Use %", rv, pv, is_pct = TRUE))
    }
    
    if (nrow(rows) > 0) tables[["TOB"]] <- make_gt(rows, paste0("Nicotine and Tobacco Prevention (", ref_yr, " vs prior year)"))
  }, error = function(e) log_msg(paste("WARN - TOB KPI error:", e$message), "WARN"))
  
  # --- WIC ---
  tryCatch({
    rows <- tibble()
    ref_yr <- NA
    
    wic <- read_cp("wic", "wic_wic.csv")
    if (!is.null(wic)) {
      y <- get_years(wic)
      if (is.na(ref_yr)) ref_yr <- y$yr
      for (col in c("summit_county_clients", "outside_summit_clients")) {
        label <- if (col == "summit_county_clients") "Summit County Clients" else "Outside Summit Clients"
        rv <- y$recent %>% pull(!!sym(col)) %>% sum()
        pv <- if (!is.null(y$prior)) y$prior %>% pull(!!sym(col)) %>% sum() else NA
        rows <- bind_rows(rows, make_row(label, rv, pv))
      }
    }
    
    bc <- read_cp("wic", "wic_babycafe.csv")
    if (!is.null(bc)) {
      y <- get_years(bc)
      for (col in c("unique_clients", "return_visits")) {
        label <- if (col == "unique_clients") "Baby Cafe - Unique Clients" else "Baby Cafe - Return Visits"
        rv <- y$recent %>% pull(!!sym(col)) %>% sum()
        pv <- if (!is.null(y$prior)) y$prior %>% pull(!!sym(col)) %>% sum() else NA
        rows <- bind_rows(rows, make_row(label, rv, pv))
      }
    }
    
    gts <- read_cp("wic", "wic_gts_kpi.csv")
    if (!is.null(gts)) {
      y <- get_years(gts)
      for (metric in unique(gts$gts_metric)) {
        rv <- y$recent %>% filter(gts_metric == metric) %>% pull(value) %>% first()
        pv <- if (!is.null(y$prior)) y$prior %>% filter(gts_metric == metric) %>% pull(value) %>% first() else NA
        rows <- bind_rows(rows, make_row(paste("Grow to Share -", metric), rv, pv))
      }
    }
    
    if (nrow(rows) > 0) tables[["WIC"]] <- make_gt(rows, paste0("Community Nutrition & WIC (", ref_yr, " vs prior year)"))
  }, error = function(e) log_msg(paste("WARN - WIC KPI error:", e$message), "WARN"))
  
  # --- NFP ---
  tryCatch({
    rows <- tibble()
    ref_yr <- NA
    
    cv <- read_cp("nfp", "nfp_client_visit.csv")
    if (!is.null(cv)) {
      y <- get_years(cv)
      if (is.na(ref_yr)) ref_yr <- y$yr
      for (metric in c("Clients", "Visits")) {
        rv <- y$recent %>% filter(nfp_metric == metric) %>% pull(value) %>% sum(na.rm = TRUE)
        pv <- if (!is.null(y$prior)) y$prior %>% filter(nfp_metric == metric) %>% pull(value) %>% sum(na.rm = TRUE) else NA
        rows <- bind_rows(rows, make_row(paste("NFP -", metric), rv, pv))
      }
    }
    
    bf <- read_cp("nfp", "nfp_breastfeed.csv")
    if (!is.null(bf)) {
      y <- get_years(bf)
      for (metric in c("At Birth", "At 6 months")) {
        rv <- y$recent %>% filter(region == "Summit", breastfeeding_metric == metric) %>% pull(percentage) %>% first()
        pv <- if (!is.null(y$prior)) y$prior %>% filter(region == "Summit", breastfeeding_metric == metric) %>% pull(percentage) %>% first() else NA
        rows <- bind_rows(rows, make_row(paste("Breastfeeding -", metric, "%"), rv, pv, is_pct = TRUE))
      }
    }
    
    work <- read_cp("nfp", "nfp_work.csv")
    if (!is.null(work)) {
      y  <- get_years(work)
      rv <- y$recent %>% filter(region == "Summit") %>% pull(work_participation_rate) %>% first()
      pv <- if (!is.null(y$prior)) y$prior %>% filter(region == "Summit") %>% pull(work_participation_rate) %>% first() else NA
      rows <- bind_rows(rows, make_row("Work Participation Rate %", rv, pv, is_pct = TRUE))
    }
    
    imz <- read_cp("nfp", "nfp_imz.csv")
    if (!is.null(imz)) {
      y  <- get_years(imz)
      rv <- y$recent %>% filter(region == "Summit") %>% pull(immunization_rate) %>% first()
      pv <- if (!is.null(y$prior)) y$prior %>% filter(region == "Summit") %>% pull(immunization_rate) %>% first() else NA
      rows <- bind_rows(rows, make_row("Infant Immunization Rate %", rv, pv, is_pct = TRUE))
    }
    
    if (nrow(rows) > 0) tables[["NFP"]] <- make_gt(rows, paste0("Nurse-Family Partnership (", ref_yr, " vs prior year)"))
  }, error = function(e) log_msg(paste("WARN - NFP KPI error:", e$message), "WARN"))
  
  # ---------------------------------------------------------------------------
  # RENDER TO PDF
  # ---------------------------------------------------------------------------
  
  if (length(tables) == 0) {
    log_msg("WARN - no KPI tables generated, skipping PDF", "WARN")
    return(invisible(NULL))
  }
  
  # Build combined HTML then render to PDF via gt's built-in save
  # Title page as first gt object
  title_gt <- gt(tibble(` ` = character(0))) %>%
    tab_header(
      title    = md("**Summit County Public Health**"),
      subtitle = md(paste0(
        "**Annual Reported Health Outcomes Metrics**<br>",
        report_date, "<br><br>",
        "Produced by: Hayden Hedman, PhD - Data Analyst"
      ))
    ) %>%
    tab_options(
      heading.title.font.size    = 16,
      heading.subtitle.font.size = 12,
      table.border.top.style     = "hidden",
      table.border.bottom.style  = "hidden",
      column_labels.hidden       = TRUE
    )
  
  all_tables <- c(list(title = title_gt), tables)
  
  # Save each gt to temp HTML then combine and render PDF
  tmp_html <- tempfile(fileext = ".html")
  
  html_parts <- map(all_tables, function(tbl) {
    as_raw_html(tbl)
  })
  
  full_html <- paste(
    "<html><head><style>",
    "body { font-family: Arial, sans-serif; font-size: 10pt; margin: 20px; }",
    "table { border-collapse: collapse; width: 100%; margin-bottom: 30px; }",
    "th, td { border: 1px solid #ccc; padding: 6px 10px; }",
    "th { background-color: #f2f2f2; }",
    "</style></head><body>",
    paste(html_parts, collapse = "<hr style='margin: 20px 0;'>"),
    "</body></html>"
  )
  
  writeLines(full_html, tmp_html)
  
  tryCatch({
    pagedown::chrome_print(tmp_html, output = out_path, timeout = 60)
    log_msg(paste("KPI PDF written |", out_path))
  }, error = function(e) {
    log_msg(paste("ERROR rendering KPI PDF - is Chrome/Chromium installed? |", e$message), "ERROR")
    log_msg(paste("HTML fallback saved to:", tmp_html), "INFO")
  })
  
  log_msg("--- KPI PDF generation complete ---")
  return(invisible(NULL))
}
# =============================================================================
# app.R
# SCPH Annual PBI Dashboard Status Tracker
# Reads from shiny/data/status_summary.csv and shiny/data/status_detail.csv
# Deploy from shiny/ folder with project .Rproj open
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(here)
library(DT)

# =============================================================================
# CONFIG
# =============================================================================

PBI_LINK <- "https://summitcountycolorado-my.sharepoint.com/:w:/r/personal/haydenh_co_summit_co_us/_layouts/15/Doc.aspx?sourcedoc=%7B9AE21A3F-74C7-4DB9-BE39-176DB2799791%7D&file=SCPH%20Dashboards%20Updates.docx&action=default&mobileredirect=true&DefaultItemOpen=1"

# Clean display names per dashboard code
DASHBOARD_NAMES <- c(
  "eh"      = "Environmental Health",
  "opi"     = "Opioid Overdose Prevention",
  "death"   = "Vital Statistics: Top Causes of Death",
  "dis"     = "Communicable Diseases & Exposures",
  "hearvis" = "Hearing & Vision Screenings",
  "fam"     = "Connecting Families",
  "youth"   = "Connecting Youth",
  "tob"     = "Nicotine and Tobacco Prevention",
  "wic"     = "Community Nutrition (WIC)",
  "nfp"     = "Nurse-Family Partnership"
)

# Status colors
STATUS_COLORS <- c(
  "Green"  = "#2e7d32",
  "Yellow" = "#f9a825",
  "Red"    = "#c62828"
)

STATUS_LABELS <- c(
  "Green"  = "Current",
  "Yellow" = "Review Needed",
  "Red"    = "Update Required"
)

# =============================================================================
# LOAD DATA
# =============================================================================

load_data <- function() {
    app_dir      <- getwd()
    summary_path <- file.path(app_dir, "data", "status_summary.csv")
    detail_path  <- file.path(app_dir, "data", "status_detail.csv")
  
  if (!file.exists(summary_path) || !file.exists(detail_path)) {
    return(list(summary = NULL, detail = NULL))
  }
  list(
    summary = read_csv(summary_path, show_col_types = FALSE),
    detail  = read_csv(detail_path,  show_col_types = FALSE)
  )
}

# =============================================================================
# UI
# =============================================================================

ui <- page_fluid(
  theme = bs_theme(
    bootswatch = "flatly",
    base_font  = font_google("Inter"),
    primary    = "#1a3a5c"
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f7fa;
        color: #1a1a1a;
      }
      .app-header {
        background-color: #1a3a5c;
        color: white;
        padding: 24px 32px 20px 32px;
        margin-bottom: 28px;
        border-radius: 0 0 8px 8px;
      }
      .app-title {
        font-size: 22px;
        font-weight: 700;
        margin: 0 0 6px 0;
        letter-spacing: 0.3px;
      }
      .app-subtitle {
        font-size: 13px;
        opacity: 0.85;
        margin: 0;
      }
      .app-subtitle a {
        color: #90caf9;
        text-decoration: none;
      }
      .app-subtitle a:hover {
        text-decoration: underline;
      }
      .status-legend {
        display: flex;
        gap: 20px;
        padding: 10px 32px;
        margin-bottom: 24px;
        font-size: 12px;
        color: #555;
      }
      .legend-item {
        display: flex;
        align-items: center;
        gap: 6px;
      }
      .legend-dot {
        width: 12px;
        height: 12px;
        border-radius: 50%;
        display: inline-block;
      }
      .tile-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(260px, 1fr));
        gap: 14px;
        padding: 0 32px;
      }
      .dash-tile {
        border: none;
        border-radius: 8px;
        padding: 18px 20px;
        cursor: pointer;
        width: 100%;
        text-align: left;
        color: white;
        font-family: 'Inter', sans-serif;
        transition: opacity 0.15s, transform 0.15s;
        box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      }
      .dash-tile:hover {
        opacity: 0.92;
        transform: translateY(-1px);
        box-shadow: 0 4px 10px rgba(0,0,0,0.2);
      }
      .dash-tile:active {
        transform: translateY(0px);
      }
      .tile-name {
        font-size: 14px;
        font-weight: 600;
        margin: 0 0 4px 0;
        line-height: 1.3;
      }
      .tile-status {
        font-size: 11px;
        opacity: 0.9;
        margin: 0;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .detail-panel {
        margin: 24px 32px 0 32px;
        background: white;
        border-radius: 8px;
        padding: 20px 24px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }
      .detail-title {
        font-size: 15px;
        font-weight: 600;
        color: #1a3a5c;
        margin: 0 0 4px 0;
      }
      .detail-subtitle {
        font-size: 12px;
        color: #888;
        margin: 0 0 16px 0;
      }
      .status-badge {
        display: inline-block;
        padding: 2px 10px;
        border-radius: 12px;
        color: white;
        font-size: 11px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.4px;
      }
      .footer-bar {
        margin-top: 40px;
        padding: 16px 32px;
        font-size: 11px;
        color: #aaa;
        text-align: center;
      }
    "))
  ),
  
  # Header
  div(class = "app-header",
      style = "display: flex; justify-content: space-between; align-items: center;",
      div(
        p(class = "app-title", "Summit County Public Health"),
        p(class = "app-subtitle",
          "Annual Reported PBI Dashboards | Data Update Status",
          HTML("&nbsp;&nbsp;|&nbsp;&nbsp;"),
          tags$a(href = PBI_LINK, target = "_blank", "View PBI Dashboards")
        )
      ),
      tags$img(src = "scph_logo.png", height = "48px",
               style = "opacity: 0.92; margin-left: 16px;",
               onerror = "this.style.display='none'")
  ),
  
  # Legend
  div(class = "status-legend",
      div(class = "legend-item",
          span(class = "legend-dot", style = paste0("background:", STATUS_COLORS["Green"])),
          tagList(tags$strong("Current"), tags$em(" (data within last year)"))
      ),
      div(class = "legend-item",
          span(class = "legend-dot", style = paste0("background:", STATUS_COLORS["Yellow"])),
          tagList(tags$strong("Review Needed"), tags$em(" (data 2 years old)"))
      ),
      div(class = "legend-item",
          span(class = "legend-dot", style = paste0("background:", STATUS_COLORS["Red"])),
          tagList(tags$strong("Update Required"), tags$em(" (data 3+ years old)"))
      )
  ),
  
  # Tile grid
  div(class = "tile-grid",
      uiOutput("tiles")
  ),
  
  # Detail panel (cascades below on click)
  uiOutput("detail_panel"),
  
  # Footer
  div(class = "footer-bar",
      paste0("Last updated: ", format(Sys.Date(), "%B %d, %Y"),
             " | Summit County Public Health | Automatically reflects current data submissions")
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # Load data
  data <- load_data()
  
  # Track which tile is selected (NULL = none)
  selected <- reactiveVal(NULL)
  
  # Render tiles
  output$tiles <- renderUI({
    if (is.null(data$summary)) {
      return(p("Status data not found. Run pipeline_run.R first.", style = "color: red; padding: 0 32px;"))
    }
    
    summary <- data$summary %>%
      mutate(display_name = DASHBOARD_NAMES[dashboard]) %>%
      arrange(display_name)
    
    map(seq_len(nrow(summary)), function(i) {
      row    <- summary[i, ]
      dash   <- row$dashboard
      name   <- row$display_name
      status <- row$status
      color  <- STATUS_COLORS[status]
      label  <- STATUS_LABELS[status]
      
      actionButton(
        inputId = paste0("tile_", dash),
        label   = tagList(
          p(class = "tile-name", name),
          p(class = "tile-status", label)
        ),
        class = "dash-tile",
        style = paste0("background-color:", color, ";")
      )
    })
  })
  
  # Observe all tile clicks
  observe({
    if (is.null(data$summary)) return()
    lapply(data$summary$dashboard, function(dash) {
      observeEvent(input[[paste0("tile_", dash)]], {
        if (!is.null(selected()) && selected() == dash) {
          selected(NULL)  # collapse if same tile clicked again
        } else {
          selected(dash)
        }
      }, ignoreInit = TRUE)
    })
  })
  
  # Render detail panel
  output$detail_panel <- renderUI({
    dash <- selected()
    if (is.null(dash) || is.null(data$detail)) return(NULL)
    
    detail <- data$detail %>%
      filter(dashboard == dash) %>%
      mutate(
        display_name = DASHBOARD_NAMES[dashboard],
        Status = map(status, function(s) {
          color <- STATUS_COLORS[s]
          label <- STATUS_LABELS[s]
          as.character(tags$span(
            class = "status-badge",
            style = paste0("background-color:", color, ";"),
            label
          ))
        })
      ) %>%
      select(
        `Excel Tab`              = source_tab,
        `Most Recent Year on File` = most_recent_year,
        Status
      )
    
    display_name <- DASHBOARD_NAMES[dash]
    
    div(class = "detail-panel",
        p(class = "detail-title", display_name),
        p(class = "detail-subtitle", tags$em("Click tile again to collapse")),
        DT::datatable(
          detail,
          escape    = FALSE,
          rownames  = FALSE,
          selection = "none",
          options   = list(
            dom        = "t",
            pageLength = 20,
            ordering   = FALSE,
            columnDefs = list(
              list(className = "dt-left",   targets = 0),
              list(className = "dt-center", targets = c(1, 2))
            )
          ),
          class = "compact stripe"
        )
    )
  })
}

# =============================================================================
# RUN
# =============================================================================

shinyApp(ui = ui, server = server)
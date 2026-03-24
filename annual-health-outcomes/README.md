# Annual Health Outcomes
**Summit County Public Health**

End-to-end R pipeline converting annual Excel submissions into Power BI dashboards, a live Shiny tracker, and automated reports across 10 public health program areas.

**Data Update Status:** [scph-analytics.shinyapps.io/scph_annual_dashboard_status](https://scph-analytics.shinyapps.io/scph_annual_dashboard_status/)

---

## What it does

- Ingests 10 Excel workbooks (31 tabs, 97 fields) with type coercion, categorical validation, and full audit logging
- Maintains 32 master CSVs with timestamped snapshots before every write
- Produces PBI-ready CSVs, a KPI summary PDF, pre-filled data request XLSXs, and Shiny status outputs in a single run
- Full test mode sandbox with automated validation before any live write

## Stack

`R` · `tidyverse` · `readxl` · `writexl` · `gt` · `pagedown` · `shiny` · `bslib` · `DT`

---

*Hayden Hedman, PhD*

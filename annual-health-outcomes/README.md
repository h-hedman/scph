# Annual Health Outcomes
**Summit County Public Health**

Production R pipeline for ingesting, validating, and publishing annual public health data across 10 program areas. Runs end-to-end from raw Excel submissions to Power BI dashboards, a live Shiny status app, and auto-generated reports — fully automated, zero manual steps.

---

## What it does

- Ingests 10 Excel workbooks (31 tabs, 97 fields) via a data-dictionary-driven pipeline with type coercion, symbol stripping, categorical validation, and full audit logging
- Maintains 32 versioned master CSVs with timestamped archive snapshots before every write
- Outputs PBI-ready 5-year window CSVs, pre-filled next-year data request XLSXs, a KPI summary PDF, and live Shiny status CSVs — all in one pipeline run
- Includes a full test mode sandbox, dummy data injection, and automated validation before any live write

## Stack

`R` · `tidyverse` · `readxl` · `writexl` · `gt` · `pagedown` · `shiny` · `bslib` · `DT`

---

*Hayden Hedman, PhD*

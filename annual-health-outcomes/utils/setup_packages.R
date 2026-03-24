

# 0. Check version of Base R
version$version.string


# 1. Update everything already installed
update.packages(ask = FALSE, checkBuilt = TRUE)

# 2. Then install anything missing
packages <- c(
  "tidyverse", "readxl", "writexl", "here", "janitor",
  "gt", "gtExtras", "pagedown", "rmarkdown",
  "shiny", "bslib"
)

installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]

if (length(to_install) > 0) {
  install.packages(to_install)
} else {
  cat("All packages already installed\n")
}
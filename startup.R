# ==============================================================================
# startup.R
# Installs and loads all required packages for the thesis
# ==============================================================================

packages <- c(
  "tidyverse", "readxl", "plotly", "slider", 
  "kableExtra", "corrplot", "knitr", "broom", 
  "lubridate", "zoo", "ggplot2", "purrr"
)

# Install missing packages automatically
installed_packages <- packages %in% rownames(installed.packages())
if (any(!installed_packages)) {
  install.packages(packages[!installed_packages])
}

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))

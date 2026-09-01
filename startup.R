# ==============================================================================
# startup.R
# Installs and loads all required packages for the thesis
# ==============================================================================

# Define packages required for data processing, analysis, and visualisation
packages <- c("tidyverse", "readxl", "plotly", "slider", "kableExtra", "corrplot", "knitr", "broom", "lubridate", "zoo", "ggplot2", "purrr", "sandwich")

# Check which required packages are already installed

installed_packages <- packages %in% rownames(installed.packages())

# Install any missing packages

if (any(!installed_packages)) {install.packages(packages[!installed_packages])}

# Load all required packages
invisible(lapply(packages, library, character.only = TRUE))

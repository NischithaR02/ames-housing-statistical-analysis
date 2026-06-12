## ===============================
## 00_setup.R
## Project setup and data loading
## ===============================

rm(list = ls())
set.seed(123)

required_packages <- c(
  "tidyverse",
  "modeldata",
  "fitdistrplus",
  "car",
  "caret"
)

# Install missing packages (run once on your machine)
installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)
if (length(to_install) > 0) install.packages(to_install)

# Load libraries
invisible(lapply(required_packages, library, character.only = TRUE))

# Load Ames data
data(ames, package = "modeldata")
ames_data <- ames

# Quick checks
cat("Rows, Cols:", nrow(ames_data), ncol(ames_data), "\n")
print(head(ames_data, 3))
print(summary(ames_data$Sale_Price))

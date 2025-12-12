##------------------------------------------------------------------------------##
## File:        00_setup.R
## Project:     Natural Rate of Interest: Critique and Extensions
## Description: Setup script - install packages and define paths
##------------------------------------------------------------------------------##

# Clear workspace
rm(list = ls())

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# =============================================================================
# INSTALL REQUIRED PACKAGES
# =============================================================================

required_packages <- c(
  # Core packages from LW/HLW replication code
  "tis",          # Time series package
  "nloptr",       # Optimization
  "mFilter",      # HP filter
  "openxlsx",     # Read/write Excel


  # Data access
  "fredr",        # FRED API
  "httr",         # HTTP requests
  "jsonlite",     # JSON parsing

  # Data manipulation
  "dplyr",
  "tidyr",
  "lubridate",
  "zoo",

  # Visualization
  "ggplot2",
  "ggthemes",
  "gridExtra",
  "scales",

  # Tables
  "xtable",
  "stargazer",

  # Additional econometrics
  "vars",         # VAR models
  "dlm",          # State space models (alternative)
  "KFAS"          # Kalman filter
)

# Install missing packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# =============================================================================
# DEFINE PATHS
# =============================================================================

# Project root
project_dir <- "G:/My Drive/book drafts/natural_rate"

# Subdirectories
paths <- list(
  root        = project_dir,
  code        = file.path(project_dir, "code"),
  data_raw    = file.path(project_dir, "data", "raw"),
  data_proc   = file.path(project_dir, "data", "processed"),
  output_base = file.path(project_dir, "output", "baseline"),
  output_sens = file.path(project_dir, "output", "sensitivity"),
  output_alt  = file.path(project_dir, "output", "alternatives"),
  figures     = file.path(project_dir, "figures"),
  tables      = file.path(project_dir, "tables"),
  paper       = file.path(project_dir, "paper"),

  # Original replication code
  lw_code     = file.path(project_dir, "LW_model", "LW_2023_Replication_Code", "LW_2023_Replication_Code"),
  hlw_code    = file.path(project_dir, "HLW_model", "HLW_2023_Replication_Code", "HLW_2023_Replication_Code"),

  # Literature
  literature  = file.path(project_dir, "_literature")
)

# =============================================================================
# API KEYS
# =============================================================================

# API keys - set these as environment variables or in a local .Renviron file
# Do NOT commit actual API keys to version control!
api_keys <- list(
  fred  = Sys.getenv("FRED_API_KEY", ""),
  bea   = Sys.getenv("BEA_API_KEY", ""),
  bls   = Sys.getenv("BLS_API_KEY", ""),
  census = Sys.getenv("CENSUS_API_KEY", "")
)

# Check that FRED key is set (required)
if (api_keys$fred == "") {
  warning("FRED API key not set. Get a free key at https://fred.stlouisfed.org/docs/api/api_key.html")
  warning("Set it with: Sys.setenv(FRED_API_KEY='your_key_here')")
}

# Set FRED API key
fredr_set_key(api_keys$fred)

# =============================================================================
# COMMON SETTINGS
# =============================================================================

# Sample periods
sample_periods <- list(
  full       = list(start = c(1961, 1), end = c(2025, 2)),
  post_volcker = list(start = c(1985, 1), end = c(2025, 2)),
  post_gfc   = list(start = c(2010, 1), end = c(2025, 2)),
  pre_covid  = list(start = c(1961, 1), end = c(2019, 4))
)

# Baseline constraint values (from LW/HLW code)
baseline_constraints <- list(
  a_r = -0.0025,  # Upper bound on IS curve slope
  b_y = 0.025     # Lower bound on Phillips curve slope
)

# Sensitivity test values
sensitivity_values <- list(
  a_r = c(-0.001, -0.0025, -0.005, -0.01, -Inf),  # -Inf = unconstrained
  b_y = c(0.01, 0.025, 0.05, 0.1, 0)               # 0 = unconstrained
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Convert year-quarter to date
yq_to_date <- function(year, quarter) {
  as.Date(paste0(year, "-", (quarter - 1) * 3 + 1, "-01"))
}

# Create quarterly date sequence
quarterly_seq <- function(start_yq, end_yq) {
  start_date <- yq_to_date(start_yq[1], start_yq[2])
  end_date <- yq_to_date(end_yq[1], end_yq[2])
  seq(start_date, end_date, by = "quarter")
}

# Format numbers for tables
fmt_num <- function(x, digits = 3) {
  formatC(x, digits = digits, format = "f")
}

# Publication-quality theme for ggplot
theme_publication <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "grey50"),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold")
    )
}

# =============================================================================
# SAVE SETUP
# =============================================================================

# Save configuration for use by other scripts
save(paths, api_keys, sample_periods, baseline_constraints, sensitivity_values,
     yq_to_date, quarterly_seq, fmt_num, theme_publication,
     file = file.path(paths$data_proc, "setup_config.RData"))

cat("Setup complete. Configuration saved to:",
    file.path(paths$data_proc, "setup_config.RData"), "\n")

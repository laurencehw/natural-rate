##------------------------------------------------------------------------------##
## File:        01_download_data.R
## Project:     Natural Rate of Interest: Critique and Extensions
## Description: Download and prepare data for LW/HLW estimation
##------------------------------------------------------------------------------##

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Run setup first
source("G:/My Drive/book drafts/natural_rate/code/00_setup.R")

# =============================================================================
# US DATA FROM FRED
# =============================================================================

cat("Downloading US data from FRED...\n")

# Define FRED series to download
fred_series <- list(
  # GDP and output
  gdp         = "GDPC1",           # Real GDP (billions, chained 2017$, SA)
  gdp_deflator = "GDPDEF",         # GDP implicit price deflator

  # Prices
  pce_price   = "PCEPI",           # PCE price index
  pce_core    = "PCEPILFE",        # Core PCE (excluding food & energy)
  cpi         = "CPIAUCSL",        # CPI all items

  # Interest rates
  fed_funds   = "FEDFUNDS",        # Federal funds effective rate
  tbill_3m    = "TB3MS",           # 3-month Treasury bill
  treas_10y   = "GS10",            # 10-year Treasury constant maturity

  # Labor market
  unrate      = "UNRATENSA",       # Unemployment rate (not seasonally adjusted)
  unrate_sa   = "UNRATE",          # Unemployment rate (SA)

  # Inflation expectations
  mich_infl_exp = "MICH",          # University of Michigan inflation expectations
  tips_5y     = "T5YIE",           # 5-year breakeven inflation (starts 2003)

  # Financial conditions
  nfci        = "NFCI",            # Chicago Fed National Financial Conditions Index
  nfci_credit = "NFCINONFINLEVERAGE", # NFCI credit subindex (nonfinancial leverage)

  # Import prices
  import_price = "IR",             # Import price index

  # Oil prices
  wti_oil     = "WTISPLC",         # WTI crude oil spot price

  # Demographics
  pop_total   = "POPTHM",          # Total population
  pop_65plus  = "SPPOP65UPTOZSUSA" # Population 65+
)

# Download each series
us_data <- list()
for (name in names(fred_series)) {
  cat("  Downloading:", name, "...\n")
  tryCatch({
    us_data[[name]] <- fredr(
      series_id = fred_series[[name]],
      observation_start = as.Date("1950-01-01"),
      observation_end = Sys.Date()
    )
  }, error = function(e) {
    cat("    Error downloading", name, ":", e$message, "\n")
  })
}

# =============================================================================
# DOWNLOAD LW/HLW ORIGINAL DATA FROM FRBNY
# =============================================================================

cat("\nDownloading LW/HLW estimates from FRBNY...\n")

# URLs for FRBNY data
frbny_urls <- list(
  lw_current = "https://www.newyorkfed.org/medialibrary/media/research/economists/williams/data/Laubach_Williams_current_estimates.xlsx",
  lw_realtime = "https://www.newyorkfed.org/medialibrary/media/research/economists/williams/data/Laubach_Williams_real_time_estimates.xlsx",
  hlw_current = "https://www.newyorkfed.org/medialibrary/media/research/economists/williams/data/Holston_Laubach_Williams_current_estimates.xlsx",
  hlw_realtime = "https://www.newyorkfed.org/medialibrary/media/research/economists/williams/data/Holston_Laubach_Williams_real_time_estimates.xlsx"
)

# Download files
for (name in names(frbny_urls)) {
  dest_file <- file.path(paths$data_raw, paste0(name, ".xlsx"))
  if (!file.exists(dest_file)) {
    cat("  Downloading:", name, "...\n")
    tryCatch({
      download.file(frbny_urls[[name]], dest_file, mode = "wb", quiet = TRUE)
    }, error = function(e) {
      cat("    Error downloading", name, ":", e$message, "\n")
    })
  } else {
    cat("  File exists:", name, "\n")
  }
}

# =============================================================================
# READ FRBNY INPUT DATA FOR REPLICATION
# =============================================================================

cat("\nReading FRBNY input data...\n")

# Read LW input data (if available)
lw_input_file <- file.path(paths$data_raw, "lw_current.xlsx")
if (file.exists(lw_input_file)) {
  lw_input <- read.xlsx(lw_input_file, sheet = "input data", na.strings = ".")
  cat("  LW input data loaded:", nrow(lw_input), "rows\n")
} else {
  # Try the original location
  lw_input_file <- file.path(paths$root, "LW_model", "Laubach_Williams_current_estimates.xlsx")
  if (file.exists(lw_input_file)) {
    lw_input <- read.xlsx(lw_input_file, sheet = "input data", na.strings = ".")
    cat("  LW input data loaded from original location:", nrow(lw_input), "rows\n")
  }
}

# Read HLW input data
hlw_input_file <- file.path(paths$data_raw, "hlw_current.xlsx")
if (file.exists(hlw_input_file)) {
  hlw_us_input <- read.xlsx(hlw_input_file, sheet = "US input data", na.strings = ".")
  cat("  HLW US input data loaded:", nrow(hlw_us_input), "rows\n")

  # Try to read EA data
  tryCatch({
    hlw_ea_input <- read.xlsx(hlw_input_file, sheet = "EA input data", na.strings = ".")
    cat("  HLW EA input data loaded:", nrow(hlw_ea_input), "rows\n")
  }, error = function(e) {
    cat("  Note: EA data sheet not found\n")
  })
} else {
  # Try original location
  hlw_input_file <- file.path(paths$root, "HLW_model", "Holston_Laubach_Williams_current_estimates.xlsx")
  if (file.exists(hlw_input_file)) {
    hlw_us_input <- read.xlsx(hlw_input_file, sheet = "US input data", na.strings = ".")
    cat("  HLW US input data loaded from original location:", nrow(hlw_us_input), "rows\n")
  }
}

# =============================================================================
# EURO AREA DATA (ECB SDW)
# =============================================================================

cat("\nPreparing Euro Area data download instructions...\n")

# Note: ECB Statistical Data Warehouse requires web access
# Key series for EA:
# - GDP: MNA.Q.Y.I8.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.LR.N
# - HICP: ICP.M.U2.N.000000.4.ANR
# - ECB main refinancing rate: FM.B.U2.EUR.4F.KR.MRR_RT.LEV

cat("  Euro Area data can be downloaded from ECB SDW:\n")
cat("  https://sdw.ecb.europa.eu/\n")
cat("  Or use the published HLW estimates for EA\n")

# =============================================================================
# PROCESS US DATA INTO QUARTERLY FORMAT
# =============================================================================

cat("\nProcessing US data into quarterly format...\n")

# Function to aggregate to quarterly
to_quarterly <- function(df, agg_func = mean) {
  df %>%
    dplyr::mutate(
      year = year(date),
      quarter = quarter(date)
    ) %>%
    dplyr::group_by(year, quarter) %>%
    dplyr::summarise(value = agg_func(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(date = yq_to_date(year, quarter))
}

# Process each series
us_quarterly <- list()

# GDP (already quarterly)
if (!is.null(us_data$gdp)) {
  us_quarterly$gdp <- us_data$gdp %>%
    dplyr::select(date, value) %>%
    dplyr::mutate(
      year = year(date),
      quarter = quarter(date)
    )
}

# Price indices (average of months)
for (name in c("pce_price", "pce_core", "cpi", "gdp_deflator")) {
  if (!is.null(us_data[[name]])) {
    us_quarterly[[name]] <- to_quarterly(us_data[[name]] %>% dplyr::select(date, value))
  }
}

# Interest rates (average of months)
for (name in c("fed_funds", "tbill_3m", "treas_10y")) {
  if (!is.null(us_data[[name]])) {
    us_quarterly[[name]] <- to_quarterly(us_data[[name]] %>% dplyr::select(date, value))
  }
}

# Unemployment rate
if (!is.null(us_data$unrate_sa)) {
  us_quarterly$unrate <- to_quarterly(us_data$unrate_sa %>% dplyr::select(date, value))
}

# Financial conditions
if (!is.null(us_data$nfci)) {
  us_quarterly$nfci <- to_quarterly(us_data$nfci %>% dplyr::select(date, value))
}

# Inflation expectations
if (!is.null(us_data$mich_infl_exp)) {
  us_quarterly$infl_exp <- to_quarterly(us_data$mich_infl_exp %>% dplyr::select(date, value))
}

# Import prices
if (!is.null(us_data$import_price)) {
  us_quarterly$import_price <- to_quarterly(us_data$import_price %>% dplyr::select(date, value))
}

# Oil prices
if (!is.null(us_data$wti_oil)) {
  us_quarterly$oil_price <- to_quarterly(us_data$wti_oil %>% dplyr::select(date, value))
}

# =============================================================================
# CALCULATE DERIVED VARIABLES
# =============================================================================

cat("Calculating derived variables...\n")

# Create master quarterly dataset
quarters <- quarterly_seq(c(1959, 1), c(2025, 2))
master_us <- data.frame(date = quarters)
master_us$year <- year(master_us$date)
master_us$quarter <- quarter(master_us$date)

# Merge all quarterly data
for (name in names(us_quarterly)) {
  if (!is.null(us_quarterly[[name]])) {
    merge_data <- us_quarterly[[name]] %>%
      dplyr::select(year, quarter, value) %>%
      dplyr::rename(!!name := value)
    master_us <- dplyr::left_join(master_us, merge_data, by = c("year", "quarter"))
  }
}

# Calculate log GDP
if ("gdp" %in% names(master_us)) {
  master_us$log_gdp <- log(master_us$gdp)
}

# Calculate inflation rates (annualized quarter-over-quarter)
if ("pce_price" %in% names(master_us)) {
  master_us$inflation_pce <- c(NA, 400 * diff(log(master_us$pce_price)))
}

if ("cpi" %in% names(master_us)) {
  master_us$inflation_cpi <- c(NA, 400 * diff(log(master_us$cpi)))
}

if ("gdp_deflator" %in% names(master_us)) {
  master_us$inflation_gdp <- c(NA, 400 * diff(log(master_us$gdp_deflator)))
}

# Calculate real interest rate (ex ante using inflation expectations)
if ("fed_funds" %in% names(master_us) && "infl_exp" %in% names(master_us)) {
  master_us$real_rate <- master_us$fed_funds - master_us$infl_exp
}

# Calculate oil price inflation
if ("oil_price" %in% names(master_us)) {
  master_us$oil_inflation <- c(NA, 400 * diff(log(master_us$oil_price)))
}

# Calculate import price inflation
if ("import_price" %in% names(master_us)) {
  master_us$import_inflation <- c(NA, 400 * diff(log(master_us$import_price)))
}

# Relative supply shock terms
if ("oil_inflation" %in% names(master_us) && "inflation_pce" %in% names(master_us)) {
  master_us$rel_oil_inflation <- master_us$oil_inflation - master_us$inflation_pce
}

if ("import_inflation" %in% names(master_us) && "inflation_pce" %in% names(master_us)) {
  master_us$rel_import_inflation <- master_us$import_inflation - master_us$inflation_pce
}

# COVID indicator (2020 Q2 onwards)
master_us$covid_indicator <- ifelse(
  master_us$year == 2020 & master_us$quarter >= 2 |
  master_us$year == 2021 |
  master_us$year == 2022,
  1, 0
)

# =============================================================================
# DOWNLOAD DEMOGRAPHIC DATA
# =============================================================================

cat("Processing demographic data...\n")

# Old age dependency ratio from World Bank (manual entry of key values)
# Alternative: use FRED series SPPOP65UPTOZSUSA for 65+ share
demo_data <- data.frame(
  year = 1960:2024,
  old_age_ratio = c(
    # 1960-1979
    16.5, 16.6, 16.7, 16.8, 16.8, 16.9, 17.0, 17.0, 17.1, 17.1,
    17.2, 17.3, 17.4, 17.5, 17.6, 17.8, 17.9, 18.1, 18.2, 18.4,
    # 1980-1999
    18.6, 18.8, 19.0, 19.1, 19.2, 19.2, 19.2, 19.3, 19.3, 19.4,
    19.4, 19.5, 19.5, 19.6, 19.6, 19.6, 19.6, 19.6, 19.6, 19.5,
    # 2000-2019
    19.4, 19.3, 19.2, 19.1, 19.1, 19.0, 19.1, 19.2, 19.4, 19.7,
    20.1, 20.6, 21.2, 21.9, 22.5, 23.2, 23.8, 24.4, 25.0, 25.6,
    # 2020-2024
    26.1, 26.7, 27.2, 27.7, 28.2
  )
)

# Merge with master data (annual -> quarterly via interpolation)
master_us <- master_us %>%
  dplyr::left_join(demo_data, by = "year")

# =============================================================================
# SAVE PROCESSED DATA
# =============================================================================

cat("\nSaving processed data...\n")

# Save master US dataset
save(master_us, file = file.path(paths$data_proc, "us_quarterly_data.RData"))
write.csv(master_us, file.path(paths$data_proc, "us_quarterly_data.csv"), row.names = FALSE)

# Save raw data lists
save(us_data, us_quarterly, file = file.path(paths$data_proc, "us_raw_data.RData"))

# Copy input data to replication code folders
if (exists("lw_input")) {
  lw_input_dest <- file.path(paths$lw_code, "inputData", "Laubach_Williams_current_estimates.xlsx")
  if (!file.exists(dirname(lw_input_dest))) {
    dir.create(dirname(lw_input_dest), recursive = TRUE)
  }
  # Copy from original location or downloaded file
  if (file.exists(file.path(paths$root, "LW_model", "Laubach_Williams_current_estimates.xlsx"))) {
    file.copy(
      file.path(paths$root, "LW_model", "Laubach_Williams_current_estimates.xlsx"),
      lw_input_dest,
      overwrite = TRUE
    )
    cat("  Copied LW input data to replication code folder\n")
  }
}

if (exists("hlw_us_input")) {
  hlw_input_dest <- file.path(paths$hlw_code, "inputData", "Holston_Laubach_Williams_estimates.xlsx")
  if (!file.exists(dirname(hlw_input_dest))) {
    dir.create(dirname(hlw_input_dest), recursive = TRUE)
  }
  if (file.exists(file.path(paths$root, "HLW_model", "Holston_Laubach_Williams_current_estimates.xlsx"))) {
    file.copy(
      file.path(paths$root, "HLW_model", "Holston_Laubach_Williams_current_estimates.xlsx"),
      hlw_input_dest,
      overwrite = TRUE
    )
    cat("  Copied HLW input data to replication code folder\n")
  }
}

cat("\nData download and processing complete!\n")
cat("Files saved to:", paths$data_proc, "\n")

# Summary of available data
cat("\nData summary:\n")
cat("  US quarterly observations:", nrow(master_us), "\n")
cat("  Date range:", as.character(min(master_us$date)), "to", as.character(max(master_us$date)), "\n")
cat("  Variables:", paste(names(master_us), collapse = ", "), "\n")

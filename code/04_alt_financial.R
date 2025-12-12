##------------------------------------------------------------------------------##
## File:        04_alt_financial.R
## Project:     Natural Rate of Interest: Critique and Extensions
## Description: Alternative specification adding financial conditions to IS curve
##------------------------------------------------------------------------------##

# Load setup
source("G:/My Drive/book drafts/natural_rate/code/00_setup.R")

cat(paste(rep("=", 60), collapse=""), "
")
cat("ALTERNATIVE SPECIFICATION: FINANCIAL CONDITIONS IN IS CURVE
")
cat(paste(rep("=", 60), collapse=""), "
")

# =============================================================================
# MOTIVATION
# =============================================================================

# The LW model IS curve: y_t = a_1*y_{t-1} + a_2*y_{t-2} + a_r*(r_{t-1} - r*) + e
#
# This assumes monetary policy works through a single interest rate channel.
# However, financial conditions (credit availability, spreads, asset prices)
# may be important additional channels.
#
# We test:
# 1. Adding financial conditions index (NFCI) to IS curve
# 2. Adding credit spreads (BAA-AAA or BAA-10yr)
# 3. Whether financial conditions subsume the interest rate effect

# =============================================================================
# LOAD DATA
# =============================================================================

# Load processed US data
load(file.path(paths$data_proc, "us_quarterly_data.RData"))

# Load baseline results
load(file.path(paths$output_base, "lw_baseline.RData"))

# =============================================================================
# DOWNLOAD ADDITIONAL FINANCIAL DATA
# =============================================================================

cat("Downloading financial conditions data...
")

# Chicago Fed National Financial Conditions Index
nfci <- fredr(
  series_id = "NFCI",
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2025-06-30"),
  frequency = "q"
)

# BAA corporate bond yield
baa_yield <- fredr(
  series_id = "BAA",
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2025-06-30"),
  frequency = "q"
)

# AAA corporate bond yield
aaa_yield <- fredr(
  series_id = "AAA",
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2025-06-30"),
  frequency = "q"
)

# 10-year Treasury
treasury_10y <- fredr(
  series_id = "GS10",
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2025-06-30"),
  frequency = "q"
)

# =============================================================================
# MERGE FINANCIAL DATA
# =============================================================================

cat("Merging financial data...
")

# Create financial conditions dataframe
fin_data <- data.frame(
  date = nfci$date,
  nfci = nfci$value
)

# Add credit spreads
fin_data <- fin_data %>%
  left_join(baa_yield %>% dplyr::select(date, baa = value), by = "date") %>%
  left_join(aaa_yield %>% dplyr::select(date, aaa = value), by = "date") %>%
  left_join(treasury_10y %>% dplyr::select(date, gs10 = value), by = "date")

# Calculate spreads
fin_data <- fin_data %>%
  mutate(
    baa_aaa_spread = baa - aaa,
    baa_10y_spread = baa - gs10
  )

# =============================================================================
# PREPARE IS CURVE DATA
# =============================================================================

cat("Preparing IS curve data...
")

# Get output gap and real rate from baseline
output_gap <- lw_baseline$stage3$output.gap.smoothed
T_baseline <- length(output_gap)

# Create dates
dates_baseline <- quarterly_seq(c(1961, 1), c(2025, 2))[1:T_baseline]

# IS curve data
is_data <- data.frame(
  date = dates_baseline,
  output_gap = output_gap,
  real_rate = tail(lw_baseline$input_data$interest - lw_baseline$input_data$inflation, T_baseline)
)

# Get r* for rate gap
is_data$rstar <- lw_baseline$stage3$rstar.smoothed
is_data$rate_gap <- is_data$real_rate - is_data$rstar

# Merge with financial data
is_data <- is_data %>%
  left_join(fin_data, by = "date")

# Create lags
is_data <- is_data %>%
  mutate(
    gap_l1 = lag(output_gap, 1),
    gap_l2 = lag(output_gap, 2),
    rate_gap_l1 = lag(rate_gap, 1),
    nfci_l1 = lag(nfci, 1),
    spread_l1 = lag(baa_aaa_spread, 1),
    spread_10y_l1 = lag(baa_10y_spread, 1)
  )

# Filter to complete cases (NFCI starts 1971)
is_data_complete <- is_data %>%
  filter(!is.na(nfci_l1) & !is.na(gap_l2))

cat("IS curve sample with NFCI:", min(is_data_complete$date), "to", max(is_data_complete$date), "
")
cat("N =", nrow(is_data_complete), "quarters

")
# =============================================================================
# IS CURVE REGRESSIONS
# =============================================================================

cat("--- IS Curve Regression Analysis ---

")

# Model 1: Baseline (LW specification)
is_baseline <- lm(output_gap ~ gap_l1 + gap_l2 + rate_gap_l1, data = is_data_complete)
cat("Model 1: Baseline IS Curve (rate gap only)
")
print(summary(is_baseline))

# Model 2: Add NFCI
is_nfci <- lm(output_gap ~ gap_l1 + gap_l2 + rate_gap_l1 + nfci_l1, data = is_data_complete)
cat("
Model 2: IS Curve with NFCI
")
print(summary(is_nfci))

# Model 3: NFCI only (no rate gap)
is_nfci_only <- lm(output_gap ~ gap_l1 + gap_l2 + nfci_l1, data = is_data_complete)
cat("
Model 3: IS Curve with NFCI only (no rate gap)
")
print(summary(is_nfci_only))

# Model 4: Add credit spread
is_spread <- lm(output_gap ~ gap_l1 + gap_l2 + rate_gap_l1 + spread_l1, data = is_data_complete)
cat("
Model 4: IS Curve with Credit Spread
")
print(summary(is_spread))

# Model 5: NFCI + rate gap interaction test
is_interact <- lm(output_gap ~ gap_l1 + gap_l2 + rate_gap_l1 * nfci_l1, data = is_data_complete)
cat("
Model 5: IS Curve with Rate Gap x NFCI Interaction
")
print(summary(is_interact))

# =============================================================================
# MODEL COMPARISON
# =============================================================================

cat("

--- Model Comparison ---

")

models <- list(
  "Baseline" = is_baseline,
  "With NFCI" = is_nfci,
  "NFCI Only" = is_nfci_only,
  "With Spread" = is_spread,
  "Interaction" = is_interact
)

model_comparison <- data.frame(
  Model = names(models),
  R_squared = sapply(models, function(m) summary(m)$r.squared),
  Adj_R_squared = sapply(models, function(m) summary(m)$adj.r.squared),
  AIC = sapply(models, AIC),
  BIC = sapply(models, BIC),
  Rate_gap_coef = sapply(models, function(m) {
    if ("rate_gap_l1" %in% names(coef(m))) coef(m)["rate_gap_l1"] else NA
  }),
  Rate_gap_tstat = sapply(models, function(m) {
    if ("rate_gap_l1" %in% rownames(summary(m)$coefficients)) {
      summary(m)$coefficients["rate_gap_l1", "t value"]
    } else NA
  })
)

print(model_comparison)

# F-test: Does NFCI add explanatory power?
cat("
F-test: Baseline vs With NFCI
")
print(anova(is_baseline, is_nfci))

# F-test: Is rate gap still significant with NFCI?
cat("
F-test: With NFCI vs NFCI Only
")
print(anova(is_nfci_only, is_nfci))
# =============================================================================
# SUBSAMPLE ANALYSIS
# =============================================================================

cat("

--- Subsample Analysis ---

")

# Pre-2008 vs Post-2008
pre_gfc <- is_data_complete %>% filter(date < as.Date("2008-01-01"))
post_gfc <- is_data_complete %>% filter(date >= as.Date("2008-01-01"))

cat("Pre-GFC sample:", nrow(pre_gfc), "quarters
")
is_pre_baseline <- lm(output_gap ~ gap_l1 + gap_l2 + rate_gap_l1, data = pre_gfc)
is_pre_nfci <- lm(output_gap ~ gap_l1 + gap_l2 + rate_gap_l1 + nfci_l1, data = pre_gfc)

cat("Pre-GFC Baseline - Rate gap coef:", round(coef(is_pre_baseline)["rate_gap_l1"], 4),
    "(t =", round(summary(is_pre_baseline)$coefficients["rate_gap_l1", "t value"], 2), ")
")
cat("Pre-GFC + NFCI - Rate gap coef:", round(coef(is_pre_nfci)["rate_gap_l1"], 4),
    "(t =", round(summary(is_pre_nfci)$coefficients["rate_gap_l1", "t value"], 2), ")
")
cat("Pre-GFC + NFCI - NFCI coef:", round(coef(is_pre_nfci)["nfci_l1"], 4),
    "(t =", round(summary(is_pre_nfci)$coefficients["nfci_l1", "t value"], 2), ")

")

cat("Post-GFC sample:", nrow(post_gfc), "quarters
")
is_post_baseline <- lm(output_gap ~ gap_l1 + gap_l2 + rate_gap_l1, data = post_gfc)
is_post_nfci <- lm(output_gap ~ gap_l1 + gap_l2 + rate_gap_l1 + nfci_l1, data = post_gfc)

cat("Post-GFC Baseline - Rate gap coef:", round(coef(is_post_baseline)["rate_gap_l1"], 4),
    "(t =", round(summary(is_post_baseline)$coefficients["rate_gap_l1", "t value"], 2), ")
")
cat("Post-GFC + NFCI - Rate gap coef:", round(coef(is_post_nfci)["rate_gap_l1"], 4),
    "(t =", round(summary(is_post_nfci)$coefficients["rate_gap_l1", "t value"], 2), ")
")
cat("Post-GFC + NFCI - NFCI coef:", round(coef(is_post_nfci)["nfci_l1"], 4),
    "(t =", round(summary(is_post_nfci)$coefficients["nfci_l1", "t value"], 2), ")
")

# =============================================================================
# IMPLICATIONS FOR R* ESTIMATION
# =============================================================================

cat("

", paste(rep("=", 60), collapse=""), "
")
cat("IMPLICATIONS FOR R* ESTIMATION
")
cat(paste(rep("=", 60), collapse=""), "
")

cat("Key Findings:

")

# Rate gap coefficient comparison
rate_gap_baseline <- coef(is_baseline)["rate_gap_l1"]
rate_gap_with_nfci <- coef(is_nfci)["rate_gap_l1"]
pct_change <- (rate_gap_with_nfci - rate_gap_baseline) / abs(rate_gap_baseline) * 100

cat("1. Rate gap coefficient:
")
cat("   Baseline:", round(rate_gap_baseline, 4), "
")
cat("   With NFCI:", round(rate_gap_with_nfci, 4), "
")
cat("   Change:", round(pct_change, 1), "%

")

# NFCI significance
nfci_coef <- coef(is_nfci)["nfci_l1"]
nfci_tstat <- summary(is_nfci)$coefficients["nfci_l1", "t value"]

cat("2. NFCI coefficient:", round(nfci_coef, 4), "(t =", round(nfci_tstat, 2), ")
")
cat("   Interpretation: A 1-unit increase in NFCI (tighter conditions)
")
cat("   is associated with a", round(nfci_coef, 2), "ppt change in output gap

")

# Model fit improvement
r2_improvement <- summary(is_nfci)$r.squared - summary(is_baseline)$r.squared
cat("3. R-squared improvement from adding NFCI:", round(r2_improvement * 100, 2), "percentage points

")

cat("4. Policy Implications:
")
cat("   - If NFCI captures variation that rate gap does not, the LW model
")
cat("     may attribute too much output gap variation to r* movements
")
cat("   - This could bias r* estimates, particularly during financial stress periods
")
cat("   - Alternative: Include NFCI in IS curve and re-estimate state-space model

")
# =============================================================================
# ROLLING WINDOW ANALYSIS
# =============================================================================

cat("--- Rolling Window: Rate Gap vs NFCI Relative Importance ---

")

window_size <- 60  # 15 years
n_obs <- nrow(is_data_complete)
n_windows <- n_obs - window_size + 1

rolling_is <- data.frame(
  end_date = is_data_complete$date[(window_size):n_obs],
  rate_gap_coef = NA,
  rate_gap_tstat = NA,
  nfci_coef = NA,
  nfci_tstat = NA,
  r_squared = NA
)

for (i in 1:n_windows) {
  window_data <- is_data_complete[i:(i + window_size - 1), ]

  tryCatch({
    model <- lm(output_gap ~ gap_l1 + gap_l2 + rate_gap_l1 + nfci_l1, data = window_data)
    rolling_is$rate_gap_coef[i] <- coef(model)["rate_gap_l1"]
    rolling_is$rate_gap_tstat[i] <- summary(model)$coefficients["rate_gap_l1", "t value"]
    rolling_is$nfci_coef[i] <- coef(model)["nfci_l1"]
    rolling_is$nfci_tstat[i] <- summary(model)$coefficients["nfci_l1", "t value"]
    rolling_is$r_squared[i] <- summary(model)$r.squared
  }, error = function(e) NULL)
}

# Summary by decade
rolling_is$decade <- floor(year(rolling_is$end_date) / 10) * 10
decade_is_summary <- rolling_is %>%
  group_by(decade) %>%
  summarise(
    mean_rate_gap = mean(rate_gap_coef, na.rm = TRUE),
    mean_nfci = mean(nfci_coef, na.rm = TRUE),
    pct_rate_sig = mean(abs(rate_gap_tstat) > 1.96, na.rm = TRUE) * 100,
    pct_nfci_sig = mean(abs(nfci_tstat) > 1.96, na.rm = TRUE) * 100
  )

cat("Rolling window results by decade:
")
print(decade_is_summary)

# =============================================================================
# SAVE RESULTS
# =============================================================================

financial_is_results <- list(
  models = models,
  model_comparison = model_comparison,
  subsample = list(
    pre_gfc = list(baseline = is_pre_baseline, nfci = is_pre_nfci),
    post_gfc = list(baseline = is_post_baseline, nfci = is_post_nfci)
  ),
  rolling = rolling_is,
  decade_summary = decade_is_summary,
  data = is_data_complete,
  financial_data = fin_data
)

save(financial_is_results, file = file.path(paths$output_alt, "financial_is_results.RData"))
write.csv(model_comparison, file.path(paths$tables, "is_curve_model_comparison.csv"), row.names = FALSE)
write.csv(rolling_is, file.path(paths$output_alt, "is_curve_rolling_estimates.csv"), row.names = FALSE)

cat("

Financial conditions IS curve analysis complete!
")
cat("Results saved to:", paths$output_alt, "
")

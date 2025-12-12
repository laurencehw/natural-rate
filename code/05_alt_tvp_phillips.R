##------------------------------------------------------------------------------##
## File:        05_alt_tvp_phillips.R
## Project:     Natural Rate of Interest: Critique and Extensions
## Description: Alternative specification: Time-varying Phillips curve slope
##------------------------------------------------------------------------------##

# Load setup
source("G:/My Drive/book drafts/natural_rate/code/00_setup.R")

cat(paste(rep("=", 60), collapse=""), "\n")
cat("ALTERNATIVE SPECIFICATION: TIME-VARYING PHILLIPS CURVE\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

# =============================================================================
# MOTIVATION
# =============================================================================

# The LW model imposes a constant Phillips curve slope (b_y > 0.025).
# However, there is substantial evidence that the Phillips curve has
# "flattened" over time - the relationship between inflation and the
# output gap has weakened.
#
# This matters for r* estimation because:
# 1. If the Phillips curve slope varies, the constraint may bind at
#    different times
# 2. Identification of the output gap (and hence r*) depends on the
#    inflation-output gap relationship
#
# We examine:
# 1. Rolling window estimates of the Phillips curve slope
# 2. Break point analysis
# 3. Implications for r* estimates

# =============================================================================
# LOAD DATA
# =============================================================================

# Load processed US data
load(file.path(paths$data_proc, "us_quarterly_data.RData"))

# Load baseline results
load(file.path(paths$output_base, "lw_baseline.RData"))

# =============================================================================
# PHILLIPS CURVE DATA PREPARATION
# =============================================================================

cat("Preparing data for Phillips curve analysis...\n")

# Get output gap from baseline
output_gap <- lw_baseline$stage3$output.gap.smoothed
T_baseline <- length(output_gap)

# Create dates
dates_baseline <- quarterly_seq(c(1961, 1), c(2025, 2))[1:T_baseline]

# Phillips curve data
pc_data <- data.frame(
  date = dates_baseline,
  output_gap = output_gap
)

# Merge with inflation data
pc_data <- pc_data %>%
  left_join(
    master_us %>% dplyr::select(date, inflation_pce, inflation_cpi, oil_inflation, import_inflation, unrate),
    by = "date"
  )

# Use PCE inflation if available, else CPI
pc_data$inflation <- ifelse(!is.na(pc_data$inflation_pce), pc_data$inflation_pce, pc_data$inflation_cpi)

# Create lags for Phillips curve regression
# Standard specification: pi_t = b_1*pi_{t-1} + b_2*avg(pi_{t-2:t-4}) + b_y*gap_{t-1} + supply_shocks
pc_data <- pc_data %>%
  mutate(
    infl_l1 = lag(inflation, 1),
    infl_l2 = lag(inflation, 2),
    infl_l3 = lag(inflation, 3),
    infl_l4 = lag(inflation, 4),
    infl_l5 = lag(inflation, 5),
    infl_l6 = lag(inflation, 6),
    infl_l7 = lag(inflation, 7),
    infl_l8 = lag(inflation, 8),
    # Average inflation terms as in LW
    infl_234 = (infl_l2 + infl_l3 + infl_l4) / 3,
    infl_5678 = (infl_l5 + infl_l6 + infl_l7 + infl_l8) / 4,
    gap_l1 = lag(output_gap, 1),
    rel_oil = oil_inflation - inflation,
    rel_oil_l1 = lag(rel_oil, 1),
    rel_import = import_inflation - inflation,
    year = year(date)
  )

# Filter to complete cases
pc_data_complete <- pc_data %>%
  filter(!is.na(infl_5678) & !is.na(gap_l1))

cat("Phillips curve sample:", min(pc_data_complete$date), "to", max(pc_data_complete$date), "\n")
cat("N =", nrow(pc_data_complete), "quarters\n\n")

# =============================================================================
# FULL SAMPLE PHILLIPS CURVE
# =============================================================================

cat("--- Full Sample Phillips Curve ---\n\n")

# Standard specification (sum of inflation coefficients = 1)
# Dependent variable is change in inflation to impose this restriction
pc_data_complete <- pc_data_complete %>%
  mutate(
    delta_infl = inflation - infl_l1
  )

# Accelerationist Phillips curve
pc_full <- lm(inflation ~ infl_l1 + infl_234 + infl_5678 + gap_l1 + rel_oil_l1, data = pc_data_complete)
cat("Full sample estimates:\n")
print(summary(pc_full))

# Get Phillips curve slope
b_y_full <- coef(pc_full)["gap_l1"]
cat("\nPhillips curve slope (full sample):", round(b_y_full, 4), "\n")

# =============================================================================
# ROLLING WINDOW ESTIMATION
# =============================================================================

cat("\n\n--- Rolling Window Phillips Curve ---\n\n")

# Window size: 60 quarters (15 years)
window_size <- 60

# Rolling estimates
n_obs <- nrow(pc_data_complete)
n_windows <- n_obs - window_size + 1

rolling_results <- data.frame(
  end_date = pc_data_complete$date[(window_size):n_obs],
  b_y = NA,
  b_y_se = NA,
  b_y_tstat = NA,
  r_squared = NA,
  n_obs = window_size
)

for (i in 1:n_windows) {
  window_data <- pc_data_complete[i:(i + window_size - 1), ]

  tryCatch({
    pc_window <- lm(inflation ~ infl_l1 + infl_234 + infl_5678 + gap_l1 + rel_oil_l1, data = window_data)
    rolling_results$b_y[i] <- coef(pc_window)["gap_l1"]
    rolling_results$b_y_se[i] <- summary(pc_window)$coefficients["gap_l1", "Std. Error"]
    rolling_results$b_y_tstat[i] <- summary(pc_window)$coefficients["gap_l1", "t value"]
    rolling_results$r_squared[i] <- summary(pc_window)$r.squared
  }, error = function(e) {
    rolling_results$b_y[i] <- NA
  })
}

# Summary statistics by decade
rolling_results$decade <- floor(year(rolling_results$end_date) / 10) * 10
decade_summary <- rolling_results %>%
  group_by(decade) %>%
  summarise(
    mean_b_y = mean(b_y, na.rm = TRUE),
    sd_b_y = sd(b_y, na.rm = TRUE),
    min_b_y = min(b_y, na.rm = TRUE),
    max_b_y = max(b_y, na.rm = TRUE),
    pct_significant = mean(abs(b_y_tstat) > 1.96, na.rm = TRUE) * 100
  )

cat("Rolling window results by decade (15-year windows):\n")
print(decade_summary)

# =============================================================================
# STRUCTURAL BREAK ANALYSIS
# =============================================================================

cat("\n\n--- Structural Break Analysis ---\n\n")

# Test for break around key dates
break_dates <- c(1985, 1995, 2000, 2008)

for (break_year in break_dates) {
  pre_data <- pc_data_complete %>% filter(year < break_year)
  post_data <- pc_data_complete %>% filter(year >= break_year)

  if (nrow(pre_data) > 20 && nrow(post_data) > 20) {
    pc_pre <- lm(inflation ~ infl_l1 + infl_234 + infl_5678 + gap_l1 + rel_oil_l1, data = pre_data)
    pc_post <- lm(inflation ~ infl_l1 + infl_234 + infl_5678 + gap_l1 + rel_oil_l1, data = post_data)

    cat(sprintf("\nBreak at %d:\n", break_year))
    cat(sprintf("  Pre-break b_y:  %.4f (t = %.2f, N = %d)\n",
                coef(pc_pre)["gap_l1"],
                summary(pc_pre)$coefficients["gap_l1", "t value"],
                nrow(pre_data)))
    cat(sprintf("  Post-break b_y: %.4f (t = %.2f, N = %d)\n",
                coef(pc_post)["gap_l1"],
                summary(pc_post)$coefficients["gap_l1", "t value"],
                nrow(post_data)))

    # Chow test (simplified)
    pc_full_break <- lm(inflation ~ infl_l1 + infl_234 + infl_5678 + gap_l1 + rel_oil_l1,
                        data = pc_data_complete)
    ssr_full <- sum(residuals(pc_full_break)^2)
    ssr_pre <- sum(residuals(pc_pre)^2)
    ssr_post <- sum(residuals(pc_post)^2)
    k <- length(coef(pc_full_break))
    n <- nrow(pc_data_complete)

    chow_f <- ((ssr_full - ssr_pre - ssr_post) / k) / ((ssr_pre + ssr_post) / (n - 2*k))
    chow_p <- 1 - pf(chow_f, k, n - 2*k)

    cat(sprintf("  Chow F-stat: %.2f (p = %.4f)\n", chow_f, chow_p))
  }
}

# =============================================================================
# IMPLICATIONS FOR R* ESTIMATION
# =============================================================================

cat("\n\n", paste(rep("=", 60), collapse=""), "\n")
cat("IMPLICATIONS FOR R* ESTIMATION\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

# Key finding: If Phillips curve slope has declined, the constraint b_y > 0.025
# may be binding more often in recent data, potentially biasing r* estimates

# Compare constrained vs unconstrained estimates
cat("The LW model constrains b_y > 0.025\n\n")

# Calculate percentage of rolling windows where b_y < 0.025
pct_below_constraint <- mean(rolling_results$b_y < 0.025, na.rm = TRUE) * 100
cat("Percentage of 15-year windows with b_y < 0.025:", round(pct_below_constraint, 1), "%\n")

# Recent estimates
recent_rolling <- rolling_results %>%
  filter(end_date >= as.Date("2015-01-01"))

if (nrow(recent_rolling) > 0) {
  cat("\nRecent estimates (post-2015):\n")
  cat("  Mean b_y:", round(mean(recent_rolling$b_y, na.rm = TRUE), 4), "\n")
  cat("  Median b_y:", round(median(recent_rolling$b_y, na.rm = TRUE), 4), "\n")
  cat("  % below 0.025:", round(mean(recent_rolling$b_y < 0.025, na.rm = TRUE) * 100, 1), "%\n")
}

# =============================================================================
# RUN LW WITH DIFFERENT B_Y CONSTRAINTS
# =============================================================================

cat("\n--- Running LW Model with Different b_y Constraints ---\n\n")

# Load model functions
load(file.path(paths$code, "model_functions.RData"))

b_y_test_values <- c(0, 0.01, 0.025, 0.05)  # 0 = unconstrained
b_y_rstar_results <- list()

for (b_y_val in b_y_test_values) {
  lab <- ifelse(b_y_val == 0, "Unconstrained", sprintf("b_y > %.3f", b_y_val))
  cat("Running with", lab, "...\n")

  tryCatch({
    result <- run_lw_model(
      sample_start = c(1961, 1),
      sample_end = c(2025, 2),
      a_r_constraint = -0.0025,
      b_y_constraint = ifelse(b_y_val == 0, -Inf, b_y_val),
      use_kappa = TRUE,
      fix_phi = NA,
      run_se = FALSE
    )

    b_y_rstar_results[[lab]] <- list(
      constraint = b_y_val,
      rstar = result$stage3$rstar.smoothed,
      theta = result$theta,
      param_num = result$param.num,
      b_y_estimated = result$theta[result$param.num["b_3"]]
    )

    cat("  Estimated b_y:", round(result$theta[result$param.num["b_3"]], 4), "\n")
    cat("  Final r*:", round(tail(result$stage3$rstar.smoothed, 1), 2), "\n")

  }, error = function(e) {
    cat("  Error:", e$message, "\n")
  })
}

# Compare r* estimates
if (length(b_y_rstar_results) > 1) {
  cat("\n--- r* Comparison Across b_y Constraints ---\n")

  rstar_compare_df <- data.frame(
    date = quarterly_seq(c(1961, 1), c(2025, 2))[1:length(b_y_rstar_results[[1]]$rstar)]
  )

  for (lab in names(b_y_rstar_results)) {
    rstar_compare_df[[lab]] <- b_y_rstar_results[[lab]]$rstar
  }

  # Statistics for post-2000 period
  rstar_recent <- rstar_compare_df %>%
    filter(date >= as.Date("2000-01-01"))

  cat("\nPost-2000 r* statistics:\n")
  for (lab in names(b_y_rstar_results)) {
    cat(sprintf("  %s: mean = %.2f, final = %.2f\n",
                lab,
                mean(rstar_recent[[lab]], na.rm = TRUE),
                tail(rstar_recent[[lab]], 1)))
  }

  # Correlation between estimates
  cat("\nCorrelation matrix of r* estimates:\n")
  cor_vars <- names(b_y_rstar_results)
  cor_matrix <- cor(rstar_compare_df[, cor_vars], use = "complete.obs")
  print(round(cor_matrix, 3))
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

tvp_phillips_results <- list(
  pc_full_sample = pc_full,
  rolling_results = rolling_results,
  decade_summary = decade_summary,
  b_y_rstar_results = b_y_rstar_results,
  rstar_compare = if(exists("rstar_compare_df")) rstar_compare_df else NULL
)

save(tvp_phillips_results, file = file.path(paths$output_alt, "tvp_phillips_results.RData"))
write.csv(rolling_results, file.path(paths$output_alt, "phillips_rolling_estimates.csv"), row.names = FALSE)

cat("\n\nTime-varying Phillips curve analysis complete!\n")
cat("Results saved to:", paths$output_alt, "\n")

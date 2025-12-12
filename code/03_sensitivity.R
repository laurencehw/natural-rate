##------------------------------------------------------------------------------##
## File:        03_sensitivity.R
## Project:     Natural Rate of Interest: Critique and Extensions
## Description: Sensitivity analysis of LW model to key assumptions
##------------------------------------------------------------------------------##

# Load setup and model functions
source("G:/My Drive/book drafts/natural_rate/code/00_setup.R")
load(file.path(paths$code, "model_functions.RData"))

# =============================================================================
# SENSITIVITY ANALYSIS CONFIGURATION
# =============================================================================

cat(paste(rep("=", 60), collapse=""), "\n")
cat("SENSITIVITY ANALYSIS OF LW MODEL\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

# Define sensitivity tests
sensitivity_tests <- list(

  # 1. IS Curve Slope (a_r constraint)
  a_r_sensitivity = list(
    name = "IS Curve Slope Constraint",
    param = "a_r_constraint",
    values = c(-0.001, -0.0025, -0.005, -0.01, -Inf),
    labels = c("a_r < -0.001", "a_r < -0.0025 (baseline)", "a_r < -0.005", "a_r < -0.01", "Unconstrained")
  ),

  # 2. Phillips Curve Slope (b_y constraint)
  b_y_sensitivity = list(
    name = "Phillips Curve Slope Constraint",
    param = "b_y_constraint",
    values = c(0.01, 0.025, 0.05, 0.1, -Inf),
    labels = c("b_y > 0.01", "b_y > 0.025 (baseline)", "b_y > 0.05", "b_y > 0.1", "Unconstrained")
  ),

  # 3. Sample Period
  sample_period = list(
    name = "Sample Period",
    param = "sample_start",
    values = list(
      full = c(1961, 1),
      post_volcker = c(1985, 1),
      post_gfc = c(2010, 1)
    ),
    labels = c("1961-present (baseline)", "1985-present", "2010-present")
  ),

  # 4. COVID Treatment
  covid_treatment = list(
    name = "COVID Period Treatment",
    param = "covid",
    values = list(
      baseline = list(use_kappa = TRUE, fix_phi = NA, end = c(2025, 2)),
      no_kappa = list(use_kappa = FALSE, fix_phi = NA, end = c(2025, 2)),
      phi_zero = list(use_kappa = TRUE, fix_phi = 0, end = c(2025, 2)),
      pre_covid = list(use_kappa = FALSE, fix_phi = NA, end = c(2019, 4))
    ),
    labels = c("Baseline (kappa + phi)", "No variance scaling", "phi = 0", "End 2019Q4")
  )
)

# =============================================================================
# RUN SENSITIVITY TESTS - IS CURVE SLOPE
# =============================================================================

cat("\n--- IS Curve Slope Sensitivity ---\n")

a_r_results <- list()
for (i in seq_along(sensitivity_tests$a_r_sensitivity$values)) {
  val <- sensitivity_tests$a_r_sensitivity$values[i]
  lab <- sensitivity_tests$a_r_sensitivity$labels[i]
  cat("\nRunning:", lab, "\n")

  tryCatch({
    result <- run_lw_model(
      sample_start = c(1961, 1),
      sample_end = c(2025, 2),
      a_r_constraint = val,
      b_y_constraint = 0.025,
      use_kappa = TRUE,
      fix_phi = NA,
      run_se = FALSE
    )

    a_r_results[[lab]] <- list(
      constraint = val,
      lambda_g = result$lambda.g,
      lambda_z = result$lambda.z,
      rstar = result$stage3$rstar.smoothed,
      trend_growth = result$stage3$trend.smoothed,
      theta = result$theta,
      param_num = result$param.num,
      output = result$output
    )

    cat("  Final r*:", tail(result$stage3$rstar.smoothed, 1), "\n")
    cat("  IS slope (a_3):", result$theta[result$param.num["a_3"]], "\n")

  }, error = function(e) {
    cat("  Error:", e$message, "\n")
    a_r_results[[lab]] <- list(error = e$message)
  })
}

# =============================================================================
# RUN SENSITIVITY TESTS - PHILLIPS CURVE SLOPE
# =============================================================================

cat("\n--- Phillips Curve Slope Sensitivity ---\n")

b_y_results <- list()
for (i in seq_along(sensitivity_tests$b_y_sensitivity$values)) {
  val <- sensitivity_tests$b_y_sensitivity$values[i]
  lab <- sensitivity_tests$b_y_sensitivity$labels[i]
  cat("\nRunning:", lab, "\n")

  tryCatch({
    result <- run_lw_model(
      sample_start = c(1961, 1),
      sample_end = c(2025, 2),
      a_r_constraint = -0.0025,
      b_y_constraint = val,
      use_kappa = TRUE,
      fix_phi = NA,
      run_se = FALSE
    )

    b_y_results[[lab]] <- list(
      constraint = val,
      lambda_g = result$lambda.g,
      lambda_z = result$lambda.z,
      rstar = result$stage3$rstar.smoothed,
      trend_growth = result$stage3$trend.smoothed,
      theta = result$theta,
      param_num = result$param.num,
      output = result$output
    )

    cat("  Final r*:", tail(result$stage3$rstar.smoothed, 1), "\n")
    cat("  Phillips slope (b_3):", result$theta[result$param.num["b_3"]], "\n")

  }, error = function(e) {
    cat("  Error:", e$message, "\n")
    b_y_results[[lab]] <- list(error = e$message)
  })
}

# =============================================================================
# RUN SENSITIVITY TESTS - SAMPLE PERIOD
# =============================================================================

cat("\n--- Sample Period Sensitivity ---\n")

sample_results <- list()
sample_starts <- sensitivity_tests$sample_period$values
sample_labels <- sensitivity_tests$sample_period$labels

for (i in seq_along(sample_starts)) {
  start_yq <- sample_starts[[i]]
  lab <- sample_labels[i]
  cat("\nRunning:", lab, "\n")

  tryCatch({
    # For post-2010, disable kappa since sample is too short
    use_kappa_adj <- if (start_yq[1] >= 2010) FALSE else TRUE

    result <- run_lw_model(
      sample_start = start_yq,
      sample_end = c(2025, 2),
      a_r_constraint = -0.0025,
      b_y_constraint = 0.025,
      use_kappa = use_kappa_adj,
      fix_phi = NA,
      run_se = FALSE
    )

    sample_results[[lab]] <- list(
      sample_start = start_yq,
      lambda_g = result$lambda.g,
      lambda_z = result$lambda.z,
      rstar = result$stage3$rstar.smoothed,
      trend_growth = result$stage3$trend.smoothed,
      theta = result$theta,
      param_num = result$param.num,
      output = result$output
    )

    cat("  Final r*:", tail(result$stage3$rstar.smoothed, 1), "\n")
    cat("  Lambda_g:", result$lambda.g, "\n")
    cat("  Lambda_z:", result$lambda.z, "\n")

  }, error = function(e) {
    cat("  Error:", e$message, "\n")
    sample_results[[lab]] <<- list(error = e$message)
  })
}

# =============================================================================
# RUN SENSITIVITY TESTS - COVID TREATMENT
# =============================================================================

cat("\n--- COVID Treatment Sensitivity ---\n")

covid_results <- list()
covid_settings <- sensitivity_tests$covid_treatment$values
covid_labels <- sensitivity_tests$covid_treatment$labels

for (i in seq_along(covid_settings)) {
  settings <- covid_settings[[i]]
  lab <- covid_labels[i]
  cat("\nRunning:", lab, "\n")

  tryCatch({
    result <- run_lw_model(
      sample_start = c(1961, 1),
      sample_end = settings$end,
      a_r_constraint = -0.0025,
      b_y_constraint = 0.025,
      use_kappa = settings$use_kappa,
      fix_phi = settings$fix_phi,
      run_se = FALSE
    )

    covid_results[[lab]] <- list(
      settings = settings,
      lambda_g = result$lambda.g,
      lambda_z = result$lambda.z,
      rstar = result$stage3$rstar.smoothed,
      trend_growth = result$stage3$trend.smoothed,
      theta = result$theta,
      param_num = result$param.num,
      output = result$output
    )

    cat("  Final r*:", tail(result$stage3$rstar.smoothed, 1), "\n")
    if (!is.null(result$param.num["phi"])) {
      cat("  phi:", result$theta[result$param.num["phi"]], "\n")
    }

  }, error = function(e) {
    cat("  Error:", e$message, "\n")
    covid_results[[lab]] <- list(error = e$message)
  })
}

# =============================================================================
# COMPILE SENSITIVITY RESULTS
# =============================================================================

sensitivity_results <- list(
  a_r = a_r_results,
  b_y = b_y_results,
  sample = sample_results,
  covid = covid_results,
  test_config = sensitivity_tests
)

# Save results
save(sensitivity_results, file = file.path(paths$output_sens, "sensitivity_results.RData"))

# =============================================================================
# CREATE SUMMARY TABLE
# =============================================================================

create_sensitivity_summary <- function(results, param_name) {
  summary_rows <- list()
  for (lab in names(results)) {
    res <- results[[lab]]
    if (!is.null(res$error)) {
      summary_rows[[lab]] <- data.frame(
        Specification = lab,
        Final_rstar = NA,
        Lambda_g = NA,
        Lambda_z = NA,
        IS_slope = NA,
        Phillips_slope = NA,
        stringsAsFactors = FALSE
      )
    } else {
      summary_rows[[lab]] <- data.frame(
        Specification = lab,
        Final_rstar = round(tail(res$rstar, 1), 2),
        Lambda_g = round(res$lambda_g, 4),
        Lambda_z = round(res$lambda_z, 4),
        IS_slope = round(res$theta[res$param_num["a_3"]], 4),
        Phillips_slope = round(res$theta[res$param_num["b_3"]], 4),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, summary_rows)
}

# Create summary tables
summary_a_r <- create_sensitivity_summary(a_r_results, "a_r")
summary_b_y <- create_sensitivity_summary(b_y_results, "b_y")
summary_sample <- create_sensitivity_summary(sample_results, "sample")
summary_covid <- create_sensitivity_summary(covid_results, "covid")

# Print summaries
cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("SENSITIVITY ANALYSIS SUMMARY\n")
cat(paste(rep("=", 60), collapse=""), "\n")

cat("\n--- IS Curve Slope Constraint ---\n")
print(summary_a_r)

cat("\n--- Phillips Curve Slope Constraint ---\n")
print(summary_b_y)

cat("\n--- Sample Period ---\n")
print(summary_sample)

cat("\n--- COVID Treatment ---\n")
print(summary_covid)

# Save summary tables
write.csv(summary_a_r, file.path(paths$tables, "sensitivity_is_curve.csv"), row.names = FALSE)
write.csv(summary_b_y, file.path(paths$tables, "sensitivity_phillips.csv"), row.names = FALSE)
write.csv(summary_sample, file.path(paths$tables, "sensitivity_sample.csv"), row.names = FALSE)
write.csv(summary_covid, file.path(paths$tables, "sensitivity_covid.csv"), row.names = FALSE)

cat("\nSensitivity analysis complete!\n")
cat("Results saved to:", paths$output_sens, "\n")
cat("Tables saved to:", paths$tables, "\n")

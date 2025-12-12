##------------------------------------------------------------------------------##
## File:        02_run_baseline.R
## Project:     Natural Rate of Interest: Critique and Extensions
## Description: Run baseline LW and HLW models, replicate FRBNY estimates
##------------------------------------------------------------------------------##

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Load setup
source("G:/My Drive/book drafts/natural_rate/code/00_setup.R")

# =============================================================================
# SETUP FOR LW MODEL
# =============================================================================

cat(paste(rep("=", 60), collapse=""), "\n")
cat("RUNNING BASELINE LW MODEL (US)\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

# Set directories for LW code
lw_working_dir <- paths$lw_code
lw_code_dir <- paths$lw_code

# Check if input data exists
lw_input_file <- file.path(lw_working_dir, "inputData", "Laubach_Williams_current_estimates.xlsx")
if (!file.exists(lw_input_file)) {
  # Try to copy from parent folder
  source_file <- file.path(paths$root, "LW_model", "Laubach_Williams_current_estimates.xlsx")
  if (file.exists(source_file)) {
    dir.create(file.path(lw_working_dir, "inputData"), showWarnings = FALSE, recursive = TRUE)
    file.copy(source_file, lw_input_file)
    cat("Copied input data to LW code folder\n")
  } else {
    stop("LW input data not found. Please download from FRBNY website.")
  }
}

# =============================================================================
# RUN LW MODEL
# =============================================================================

# We'll create a modified version of run.lw.R that returns the results

run_lw_model <- function(sample_start = c(1961, 1),
                         sample_end = c(2025, 2),
                         a_r_constraint = -0.0025,
                         b_y_constraint = 0.025,
                         use_kappa = TRUE,
                         fix_phi = NA,
                         run_se = FALSE) {

  # Load required packages
  library(tis)
  library(nloptr)
  library(mFilter)
  library(openxlsx)

  # Set working directory
  working_dir <- paths$lw_code
  code_dir <- paths$lw_code

  # Source all LW functions
  setwd(code_dir)
  source("kalman.log.likelihood.R")
  source("kalman.states.R")
  source("kalman.standard.errors.R")
  source("median.unbiased.estimator.stage1.R")
  source("median.unbiased.estimator.stage2.R")
  source("calculate.covariance.R")
  source("log.likelihood.wrapper.R")
  source("kalman.states.wrapper.R")
  source("unpack.parameters.stage1.R")
  source("unpack.parameters.stage2.R")
  source("unpack.parameters.stage3.R")
  source("rstar.stage1.R")
  source("rstar.stage2.R")
  source("rstar.stage3.R")
  source("utilities.R")
  source("format.output.R")

  setwd(working_dir)

  # Sample dates
  sample.start <- sample_start
  sample.end <- sample_end
  est.data.start <- shiftQuarter(sample.start, -8)

  # Initialize state vectors as NA (default)
  xi.00.stage1 <- NA
  xi.00.stage2 <- NA
  xi.00.stage3 <- NA
  P.00.stage1 <- NA
  P.00.stage2 <- NA
  P.00.stage3 <- NA

  # Constraints
  a.r.constraint <- a_r_constraint
  b.y.constraint <- b_y_constraint

  # Monte Carlo SE settings
  run.se <- run_se
  niter <- 5000

  # COVID settings
  use.kappa <- use_kappa
  fix.phi <- fix_phi

  # Kappa inputs for COVID period
  kappa.inputs <- data.frame(
    'name' = c('kappa2020Q2-Q4', 'kappa2021', 'kappa2022'),
    'year' = c(2020, 2021, 2022),
    'T.start' = c(NA, NA, NA),
    'T.end' = c(NA, NA, NA),
    'init' = c(1, 1, 1),
    'lower.bound' = c(1, 1, 1),
    'upper.bound' = c(Inf, Inf, Inf),
    'theta.index' = c(NA, NA, NA),
    't.stat.null' = c(1, 1, 1)
  )

  # Set kappa timing
  if (use.kappa) {
    n.kappa <- dim(kappa.inputs)[1]
    for (k in 1:n.kappa) {
      covid.variance.start.yq <- c(kappa.inputs$year[k], 1) - sample.start
      kappa.inputs$T.start[k] <- max(covid.variance.start.yq[1] * 4 + covid.variance.start.yq[2] + 1, 0)
      covid.variance.end.yq <- c(kappa.inputs$year[k], 4) - sample.start
      kappa.inputs$T.end[k] <- max(covid.variance.end.yq[1] * 4 + covid.variance.end.yq[2] + 1, 0)

      if (kappa.inputs$year[k] == 2020) {
        kappa.inputs$T.start[k] <- kappa.inputs$T.start[k] + 1
      }
    }
  }

  # Read input data
  data <- read.xlsx("inputData/Laubach_Williams_current_estimates.xlsx",
                    sheet = "input data", na.strings = ".", colNames = TRUE, rowNames = FALSE, detectDates = TRUE)

  log.output <- data$gdp
  inflation <- data$inflation
  relative.oil.price.inflation <- data$oil.price.inflation - inflation
  relative.import.price.inflation <- data$import.price.inflation - inflation
  nominal.interest.rate <- data$interest
  inflation.expectations <- data$inflation.expectations
  covid.dummy <- data$covid.ind
  real.interest.rate <- nominal.interest.rate - inflation.expectations

  cat("Running Stage 1...\n")
  out.stage1 <- rstar.stage1(
    log.output = log.output,
    inflation = inflation,
    relative.oil.price.inflation = relative.oil.price.inflation,
    relative.import.price.inflation = relative.import.price.inflation,
    covid.dummy = covid.dummy,
    sample.end = sample.end,
    b.y.constraint = b.y.constraint,
    xi.00 = xi.00.stage1,
    P.00 = P.00.stage1,
    use.kappa = use.kappa,
    kappa.inputs = kappa.inputs,
    fix.phi = fix.phi
  )

  lambda.g <- median.unbiased.estimator.stage1(out.stage1$potential.smoothed)
  cat("Lambda_g:", lambda.g, "\n")

  cat("Running Stage 2...\n")
  out.stage2 <- rstar.stage2(
    log.output = log.output,
    inflation = inflation,
    relative.oil.price.inflation = relative.oil.price.inflation,
    relative.import.price.inflation = relative.import.price.inflation,
    real.interest.rate = real.interest.rate,
    covid.dummy = covid.dummy,
    lambda.g = lambda.g,
    sample.end = sample.end,
    a.r.constraint = a.r.constraint,
    b.y.constraint = b.y.constraint,
    xi.00 = xi.00.stage2,
    P.00 = P.00.stage2,
    use.kappa = use.kappa,
    kappa.inputs = kappa.inputs,
    fix.phi = fix.phi
  )

  lambda.z <- median.unbiased.estimator.stage2(out.stage2$y, out.stage2$x, out.stage2$kappa.vec)
  cat("Lambda_z:", lambda.z, "\n")

  cat("Running Stage 3...\n")
  out.stage3 <- rstar.stage3(
    log.output = log.output,
    inflation = inflation,
    relative.oil.price.inflation = relative.oil.price.inflation,
    relative.import.price.inflation = relative.import.price.inflation,
    real.interest.rate = real.interest.rate,
    covid.dummy = covid.dummy,
    lambda.g = lambda.g,
    lambda.z = lambda.z,
    sample.end = sample.end,
    a.r.constraint = a.r.constraint,
    b.y.constraint = b.y.constraint,
    run.se = run.se,
    xi.00 = xi.00.stage3,
    P.00 = P.00.stage3,
    use.kappa = use.kappa,
    kappa.inputs = kappa.inputs,
    fix.phi = fix.phi
  )

  # Format output
  output <- format.output(
    estimation = out.stage3,
    real.rate = real.interest.rate,
    covid.dummy = covid.dummy,
    start = sample.start,
    end = sample.end,
    run.se = run.se
  )

  # Return results
  results <- list(
    output = output,
    stage1 = out.stage1,
    stage2 = out.stage2,
    stage3 = out.stage3,
    lambda.g = lambda.g,
    lambda.z = lambda.z,
    theta = out.stage3$theta,
    param.num = out.stage3$param.num,
    input_data = data,
    settings = list(
      sample_start = sample_start,
      sample_end = sample_end,
      a_r_constraint = a_r_constraint,
      b_y_constraint = b_y_constraint,
      use_kappa = use_kappa,
      fix_phi = fix_phi
    )
  )

  return(results)
}

# Run baseline LW model
cat("\nRunning baseline LW estimation...\n")
tryCatch({
  lw_baseline <- run_lw_model(
    sample_start = c(1961, 1),
    sample_end = c(2025, 2),
    a_r_constraint = -0.0025,
    b_y_constraint = 0.025,
    use_kappa = TRUE,
    fix_phi = NA,
    run_se = FALSE  # Set TRUE for full run with standard errors
  )

  cat("\nLW Baseline Results:\n")
  cat("  Lambda_g:", lw_baseline$lambda.g, "\n")
  cat("  Lambda_z:", lw_baseline$lambda.z, "\n")
  cat("  Final r* (smoothed):", tail(lw_baseline$stage3$rstar.smoothed, 1), "\n")

  # Extract key parameters
  theta <- lw_baseline$theta
  param.num <- lw_baseline$param.num
  cat("\n  Parameter estimates:\n")
  cat("    a_1 (output gap AR1):", theta[param.num["a_1"]], "\n")
  cat("    a_2 (output gap AR2):", theta[param.num["a_2"]], "\n")
  cat("    a_3 (IS curve slope):", theta[param.num["a_3"]], "\n")
  cat("    b_3 (Phillips curve slope):", theta[param.num["b_3"]], "\n")
  cat("    c (r* loading on trend growth):", theta[param.num["c"]], "\n")
  cat("    phi (COVID shift):", theta[param.num["phi"]], "\n")

  # Save results
  save(lw_baseline, file = file.path(paths$output_base, "lw_baseline.RData"))
  write.csv(lw_baseline$output, file.path(paths$output_base, "lw_baseline_output.csv"), row.names = FALSE)

  cat("\nLW baseline saved to:", paths$output_base, "\n")

}, error = function(e) {
  cat("Error running LW model:", e$message, "\n")
  cat("Please ensure input data is available in the inputData folder.\n")
})

# =============================================================================
# SETUP FOR HLW MODEL (US)
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("RUNNING BASELINE HLW MODEL (US)\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

# Similar function for HLW
run_hlw_model <- function(country = "US",
                          sample_start = c(1961, 1),
                          sample_end = c(2025, 2),
                          a_r_constraint = -0.0025,
                          b_y_constraint = 0.025,
                          use_kappa = TRUE,
                          fix_phi = NA,
                          run_se = FALSE) {

  library(tis)
  library(nloptr)
  library(mFilter)
  library(openxlsx)

  working_dir <- paths$hlw_code
  code_dir <- paths$hlw_code

  # Source HLW functions
  setwd(code_dir)
  source("kalman.log.likelihood.R")
  source("kalman.states.R")
  source("kalman.standard.errors.R")
  source("median.unbiased.estimator.stage1.R")
  source("median.unbiased.estimator.stage2.R")
  source("kalman.states.wrapper.R")
  source("log.likelihood.wrapper.R")
  source("calculate.covariance.R")
  source("run.hlw.estimation.R")
  source("unpack.parameters.stage1.R")
  source("unpack.parameters.stage2.R")
  source("unpack.parameters.stage3.R")
  source("rstar.stage1.R")
  source("rstar.stage2.R")
  source("rstar.stage3.R")
  source("utilities.R")
  source("format.output.R")

  setwd(working_dir)

  # Settings
  sample.start <- sample_start
  sample.end <- sample_end
  data.start <- shiftQuarter(sample.start, -4)

  xi.00.stage1 <- NA
  xi.00.stage2 <- NA
  xi.00.stage3 <- NA
  P.00.stage1 <- NA
  P.00.stage2 <- NA
  P.00.stage3 <- NA

  a.r.constraint <- a_r_constraint
  b.y.constraint <- b_y_constraint

  g.pot.start.index <- 1 + ti(shiftQuarter(sample.start, -3), 'quarterly') - ti(data.start, 'quarterly')

  run.se <- run_se
  niter <- 5000

  use.kappa <- use_kappa
  fix.phi <- fix_phi

  # Kappa inputs
  kappa.inputs <- data.frame(
    'name' = c('kappa2020Q2-Q4', 'kappa2021', 'kappa2022'),
    'year' = c(2020, 2021, 2022),
    'T.start' = c(NA, NA, NA),
    'T.end' = c(NA, NA, NA),
    'init' = c(1, 1, 1),
    'lower.bound' = c(1, 1, 1),
    'upper.bound' = c(Inf, Inf, Inf),
    'theta.index' = c(NA, NA, NA),
    't.stat.null' = c(1, 1, 1)
  )

  if (use.kappa) {
    n.kappa <- dim(kappa.inputs)[1]
    for (k in 1:n.kappa) {
      covid.variance.start.yq <- c(kappa.inputs$year[k], 1) - sample.start
      kappa.inputs$T.start[k] <- max(covid.variance.start.yq[1] * 4 + covid.variance.start.yq[2] + 1, 0)
      covid.variance.end.yq <- c(kappa.inputs$year[k], 4) - sample.start
      kappa.inputs$T.end[k] <- max(covid.variance.end.yq[1] * 4 + covid.variance.end.yq[2] + 1, 0)
      if (kappa.inputs$year[k] == 2020) {
        kappa.inputs$T.start[k] <- kappa.inputs$T.start[k] + 1
      }
    }
  }

  # Read data based on country
  if (country == "US") {
    data <- read.xlsx("inputData/Holston_Laubach_Williams_estimates.xlsx",
                      sheet = "US input data", na.strings = ".", colNames = TRUE, rowNames = FALSE, detectDates = TRUE)
  } else if (country == "EA") {
    data <- read.xlsx("inputData/Holston_Laubach_Williams_estimates.xlsx",
                      sheet = "EA input data", na.strings = ".", colNames = TRUE, rowNames = FALSE, detectDates = TRUE)
  }

  log.output <- data$gdp.log
  inflation <- data$inflation
  inflation.expectations <- data$inflation.expectations
  nominal.interest.rate <- data$interest
  real.interest.rate <- nominal.interest.rate - inflation.expectations
  covid.indicator <- data$covid.ind

  cat("Running HLW estimation for", country, "...\n")

  estimation <- run.hlw.estimation(
    log.output = log.output,
    inflation = inflation,
    real.interest.rate = real.interest.rate,
    nominal.interest.rate = nominal.interest.rate,
    covid.indicator = covid.indicator,
    a.r.constraint = a.r.constraint,
    b.y.constraint = b.y.constraint,
    g.pot.start.index = g.pot.start.index,
    use.kappa = use.kappa,
    kappa.inputs = kappa.inputs,
    fix.phi = fix.phi,
    xi.00.stage1 = xi.00.stage1,
    xi.00.stage2 = xi.00.stage2,
    xi.00.stage3 = xi.00.stage3,
    P.00.stage1 = P.00.stage1,
    P.00.stage2 = P.00.stage2,
    P.00.stage3 = P.00.stage3,
    run.se = run.se,
    sample.end = sample.end
  )

  # One-sided estimates
  one.sided.est <- cbind(
    estimation$out.stage3$rstar.filtered,
    estimation$out.stage3$trend.filtered,
    estimation$out.stage3$z.filtered,
    estimation$out.stage3$output.gap.filtered
  )

  # Format output
  output <- format.output(
    country.estimation = estimation,
    one.sided.est.country = one.sided.est,
    real.rate.country = real.interest.rate,
    start = sample.start,
    end = sample.end,
    run.se = run.se
  )

  results <- list(
    output = output,
    estimation = estimation,
    one_sided = one.sided.est,
    input_data = data,
    settings = list(
      country = country,
      sample_start = sample_start,
      sample_end = sample_end,
      a_r_constraint = a_r_constraint,
      b_y_constraint = b_y_constraint,
      use_kappa = use_kappa,
      fix_phi = fix_phi
    )
  )

  return(results)
}

# Run HLW for US
cat("\nRunning HLW US estimation...\n")
tryCatch({
  # Check for input data
  hlw_input_file <- file.path(paths$hlw_code, "inputData", "Holston_Laubach_Williams_estimates.xlsx")
  if (!file.exists(hlw_input_file)) {
    source_file <- file.path(paths$root, "HLW_model", "Holston_Laubach_Williams_current_estimates.xlsx")
    if (file.exists(source_file)) {
      dir.create(file.path(paths$hlw_code, "inputData"), showWarnings = FALSE, recursive = TRUE)
      file.copy(source_file, hlw_input_file)
      cat("Copied input data to HLW code folder\n")
    }
  }

  hlw_us_baseline <- run_hlw_model(
    country = "US",
    sample_start = c(1961, 1),
    sample_end = c(2025, 2),
    a_r_constraint = -0.0025,
    b_y_constraint = 0.025,
    use_kappa = TRUE,
    fix_phi = NA,
    run_se = FALSE
  )

  cat("\nHLW US Baseline Results:\n")
  cat("  Lambda_g:", hlw_us_baseline$estimation$lambda.g, "\n")
  cat("  Lambda_z:", hlw_us_baseline$estimation$lambda.z, "\n")
  cat("  Final r* (filtered):", tail(hlw_us_baseline$estimation$out.stage3$rstar.filtered, 1), "\n")

  save(hlw_us_baseline, file = file.path(paths$output_base, "hlw_us_baseline.RData"))
  write.csv(hlw_us_baseline$output, file.path(paths$output_base, "hlw_us_baseline_output.csv"), row.names = FALSE)

}, error = function(e) {
  cat("Error running HLW US:", e$message, "\n")
})

# =============================================================================
# RUN HLW FOR EURO AREA
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("RUNNING BASELINE HLW MODEL (EURO AREA)\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

tryCatch({
  hlw_ea_baseline <- run_hlw_model(
    country = "EA",
    sample_start = c(1972, 1),  # EA has later start date
    sample_end = c(2025, 2),
    a_r_constraint = -0.0025,
    b_y_constraint = 0.025,
    use_kappa = TRUE,
    fix_phi = NA,
    run_se = FALSE
  )

  cat("\nHLW EA Baseline Results:\n")
  cat("  Lambda_g:", hlw_ea_baseline$estimation$lambda.g, "\n")
  cat("  Lambda_z:", hlw_ea_baseline$estimation$lambda.z, "\n")
  cat("  Final r* (filtered):", tail(hlw_ea_baseline$estimation$out.stage3$rstar.filtered, 1), "\n")

  save(hlw_ea_baseline, file = file.path(paths$output_base, "hlw_ea_baseline.RData"))
  write.csv(hlw_ea_baseline$output, file.path(paths$output_base, "hlw_ea_baseline_output.csv"), row.names = FALSE)

}, error = function(e) {
  cat("Error running HLW EA:", e$message, "\n")
  cat("  (Euro Area data may not be available in input file)\n")
})

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("BASELINE ESTIMATION COMPLETE\n")
cat(paste(rep("=", 60), collapse=""), "\n")
cat("\nOutput files saved to:", paths$output_base, "\n")

# Store functions for later use
save(run_lw_model, run_hlw_model, file = file.path(paths$code, "model_functions.RData"))

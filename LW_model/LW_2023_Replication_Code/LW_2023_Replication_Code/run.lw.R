rm(list=ls())

# =================
# DEFINE DIRECTORIES
# =================

# This directory should contain
#   - an 'inputData' folder with data from the FRBNY site
#   - an 'output' folder to store estimation results
working.dir <- ''

# Location of model code files
code.dir    <- ''

if ((working.dir=='') | (code.dir=='')) {
  stop("Must specify working.dir and code.dir locations in run.lw.R file")
}

# =================
# LOAD R PACKAGES
# =================

if (!require("tis")) {install.packages("tis"); library("tis")} ## Time series package
if (!require("nloptr")) {install.packages("nloptr"); library("nloptr")} ## Optimization
if (!require("mFilter")) {install.packages("mFilter"); library("mFilter")} # HP filter
if (!require("openxlsx")) {install.packages("openxlsx"); library("openxlsx")} # Read from and write to Excel

# ==================
# LOAD CODE PACKAGES
# ==================

setwd(code.dir)
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

# Set working directory back to output location
setwd(working.dir)

# =================
# DEFINE VARIABLES (See Technical Note)
# =================

# NOTE: the sample dates MUST correspond to data in input file

# Set the start and end dates of the estimation sample as well as the data start date (format is c(year,quarter))
sample.start <- c(1961,1)
sample.end   <- c(2025,2)

# The estimation process uses data beginning 8 quarters prior to the sample start
est.data.start    <- shiftQuarter(sample.start,-8)

# Initialization of state vector and covariance matrix
# Set as NA to follow procedure in HLW (2017) paper
# Or can input values manually
xi.00.stage1 <- NA
xi.00.stage2 <- NA
xi.00.stage3 <- NA

P.00.stage1 <- NA
P.00.stage2 <- NA
P.00.stage3 <- NA

# Upper bound on a_3 parameter (slope of the IS curve)
a.r.constraint <- -0.0025

# Lower bound on b_2 parameter (slope of the Phillips curve)
b.y.constraint <- 0.025

# Because the MC standard error procedure is time consuming, we include a run switch
# Set run.se to TRUE to run the procedure
run.se <- TRUE

# Set number of iterations for Monte Carlo standard error procedure
niter <- 5000

# =================
# COVID-ADJUSTED MODEL SETTINGS
# =================

# Set to TRUE if using time-varying volatility; FALSE if not
# Must specify kappa.inputs
use.kappa <- TRUE

# fix.phi must be set at NA or a numeric value
# Set as NA to estimate the COVID indicator coefficient
# Set at a numeric value to fix phi at that value
fix.phi <- NA


# =================
# VARIANCE SCALE PARAMETERS
# =================

# SETTINGS:
# kappa.inputs: DESCRIPTIONS
# name: used as label in param.num
# year: assumes kappa applies to full year, unless manually corrected
# T.start: time series index start; will be set in subsequent loop for YYYY:Q1
# T.end: time series index end; will be set in subsequent loop for YYYY:Q4
# init: value to initialize kappa in parameter estimation; default of 1
# lower.bound : lower bound for kappa in maximum likelihood estimation; default 1
# upper.bound : upper bound for kappa in maximum likelihood estimation; default Inf (no bound)
# theta.index: leave as NA; will be filled in within each stage

# NOTE: fix kappa to value by setting lower.bound=upper.bound=value

kappa.inputs <- data.frame('name'=c('kappa2020Q2-Q4','kappa2021','kappa2022'),
                           'year'=c(2020,2021,2022),
                           'T.start'=c(NA,NA,NA),
                           'T.end'=c(NA,NA,NA),
                           'init'=c(1,1,1),
                           'lower.bound'=c(1,1,1),
                           'upper.bound'=c(Inf,Inf,Inf),
                           'theta.index'=c(NA,NA,NA),
                           't.stat.null'=c(1,1,1))

# NOTE: Sets Q1-Q4 of years provided
if (use.kappa) {

  # Number of kappas introduced
  n.kappa <- dim(kappa.inputs)[1]
  for (k in 1:n.kappa) {
    # Indexing to start of y_t vector
    covid.variance.start.yq <- c(kappa.inputs$year[k],1) - sample.start

    kappa.inputs$T.start[k] <- max(covid.variance.start.yq[1]*4 + covid.variance.start.yq[2] +1,0)

    covid.variance.end.yq <- c(kappa.inputs$year[k],4) - sample.start

    kappa.inputs$T.end[k] <- max(covid.variance.end.yq[1]*4 + covid.variance.end.yq[2] +1,0)

    rm(covid.variance.start.yq, covid.variance.end.yq)

    # Manual adjustment to start Kappa_2020 in second quarter
    # Comment out under alternative specifications
    if (kappa.inputs$year[k]==2020) {
      kappa.inputs$T.start[k] <- kappa.inputs$T.start[k] + 1
    }
  }
}

# =================
# INPUT DATA
# =================

# Read input data from FRBNY website
data <- read.xlsx("inputData/Laubach_Williams_current_estimates.xlsx", sheet="input data",
                  na.strings = ".", colNames=TRUE, rowNames=FALSE, detectDates = TRUE)

# Read series beginning at data.start
log.output                      <- data$gdp
inflation                       <- data$inflation
relative.oil.price.inflation    <- data$oil.price.inflation - inflation
relative.import.price.inflation <- data$import.price.inflation - inflation
nominal.interest.rate           <- data$interest
inflation.expectations          <- data$inflation.expectations
covid.dummy                     <- data$covid.ind

real.interest.rate              <- nominal.interest.rate - inflation.expectations

# =================
# ESTIMATION
# =================

# Run stage 1
out.stage1 <- rstar.stage1(log.output=log.output,
                           inflation=inflation,
                           relative.oil.price.inflation=relative.oil.price.inflation,
                           relative.import.price.inflation=relative.import.price.inflation,
                           covid.dummy=covid.dummy,
                           sample.end=sample.end,
                           b.y.constraint=b.y.constraint,
                           xi.00=xi.00.stage1,
                           P.00=P.00.stage1,
                           use.kappa=use.kappa,
                           kappa.inputs=kappa.inputs,
                           fix.phi=fix.phi)

# Median unbiased estimate of lambda_g
lambda.g <- median.unbiased.estimator.stage1(out.stage1$potential.smoothed)

# Run stage 2
out.stage2 <- rstar.stage2(log.output=log.output,
                           inflation=inflation,
                           relative.oil.price.inflation=relative.oil.price.inflation,
                           relative.import.price.inflation=relative.import.price.inflation,
                           real.interest.rate=real.interest.rate,
                           covid.dummy=covid.dummy,
                           lambda.g=lambda.g,
                           sample.end=sample.end,
                           a.r.constraint=a.r.constraint,
                           b.y.constraint=b.y.constraint,
                           xi.00=xi.00.stage2,
                           P.00=P.00.stage2,
                           use.kappa=use.kappa,
                           kappa.inputs=kappa.inputs,
                           fix.phi=fix.phi)

# Median unbiased estimate of lambda_z
lambda.z <- median.unbiased.estimator.stage2(out.stage2$y, out.stage2$x, out.stage2$kappa.vec)

# Run stage 3
out.stage3 <- rstar.stage3(log.output=log.output,
                           inflation=inflation,
                           relative.oil.price.inflation=relative.oil.price.inflation,
                           relative.import.price.inflation=relative.import.price.inflation,
                           real.interest.rate=real.interest.rate,
                           covid.dummy=covid.dummy,
                           lambda.g=lambda.g,
                           lambda.z=lambda.z,
                           sample.end=sample.end,
                           a.r.constraint=a.r.constraint,
                           b.y.constraint=b.y.constraint,
                           run.se=run.se,
                           xi.00=xi.00.stage3,
                           P.00=P.00.stage3,
                           use.kappa=use.kappa,
                           kappa.inputs=kappa.inputs,
                           fix.phi=fix.phi)

# =================
# OUTPUT
# =================

# Save output to CSV
output.us <- format.output(estimation=out.stage3,
                           real.rate=real.interest.rate,
                           covid.dummy=covid.dummy,
                           start=sample.start,
                           end=sample.end,
                           run.se=run.se)

write.table(output.us, 'output/LW_output.csv', quote=FALSE, row.names=FALSE, sep = ',', na = '')

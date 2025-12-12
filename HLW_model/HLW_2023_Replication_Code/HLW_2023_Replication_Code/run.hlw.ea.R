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
  stop("Must specify working.dir and code.dir locations in run.hlw.ea.R file")
}

# =================
# LOAD R PACKAGES
# =================

if (!require("tis")) {install.packages("tis"); library("tis")} # Time series package
if (!require("mFilter")) {install.packages("mFilter"); library("mFilter")} # HP filter
if (!require("nloptr")) {install.packages("nloptr"); library("nloptr")} # Optimization
if (!require("openxlsx")) {install.packages("openxlsx"); library("openxlsx")} # Input from and write to Excel

# =================
# LOAD CODE PACKAGES
# =================

setwd(code.dir)
source("kalman.log.likelihood.R")
source("kalman.states.R")
source("median.unbiased.estimator.stage1.R")
source("median.unbiased.estimator.stage2.R")
source("utilities.R")
source("run.hlw.estimation.R")
source("unpack.parameters.stage1.R")
source("unpack.parameters.stage2.R")
source("unpack.parameters.stage3.R")
source("kalman.states.wrapper.R")
source("log.likelihood.wrapper.R")
source("calculate.covariance.R")
source("rstar.stage1.R")
source("rstar.stage2.R")
source("rstar.stage3.R")
source("format.output.R")
source("kalman.standard.errors.R")

# Set working directory back to output location
setwd(working.dir)

# =================
# DEFINE VARIABLES (See Technical Note)
# =================

# NOTE: the sample dates MUST correspond to data in input file

# Set the start and end dates of the estimation sample (format is c(year,quarter))
sample.start <- c(1972,1)
sample.end   <- c(2025,2)

# The estimation process uses data beginning 4 quarters prior to the sample start
data.start    <- shiftQuarter(sample.start,-4)

# Initialization of state vector and covariance matrix
# Set as NA to follow procedure in HLW paper
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

# Set start index for g.pot series; used in state vector initialization
g.pot.start.index <- 1 + ti(shiftQuarter(sample.start,-3),'quarterly')-ti(data.start,'quarterly')

# Set number of iterations for Monte Carlo standard error procedure
niter <- 5000

# Because the MC standard error procedure is time consuming, we include a run switch
# Set run.se to TRUE to run the procedure
run.se <- TRUE


# =================
# COVID-ADJUSTED MODEL SETTINGS
# =================

# Set to TRUE if using time-varying volatility; FALSE if not
# Must specify kappa.inputs if TRUE
use.kappa <- TRUE

# fix.phi must be set at NA or a numeric value
# Set as NA to estimate the COVID indicator coefficient
# Set at a numeric value to fix phi at that value (e.g. 0)
fix.phi <- NA


# =================
# VARIANCE SCALE PARAMETERS
# =================

# SETTINGS:
# kappa.inputs: DESCRIPTION
# name: used as label in param.num
# year: assumes kappa applies to full year, unless manually corrected
# T.start: time series index start; will be set in subsequent loop for YYYY:Q1
# T.end: time series index end; will be set in subsequent loop for YYYY:Q4
# init: value to initialize kappa in parameter estimation; default of 1
# lower.bound : lower bound for kappa in maximum likelihood estimation; default 1
# upper.bound : upper bound for kappa in maximum likelihood estimation; default Inf (no bound)
# theta.index: leave as NA; will be filled in within each stage

# NOTE: fix kappa at value by setting lower.bound=upper.bound=value

kappa.inputs <- data.frame('name'=c('kappa2020Q2-Q4','kappa2021','kappa2022','kappa2023'),
                           'year'=c(2020,2021,2022,2023),
                           'T.start'=c(NA,NA,NA,NA),
                           'T.end'=c(NA,NA,NA,NA),
                           'init'=c(1,1,1,1),
                           'lower.bound'=c(1,1,1,1),
                           'upper.bound'=c(Inf,Inf,Inf,Inf),
                           'theta.index'=c(NA,NA,NA,NA),
                           't.stat.null'=c(1,1,1,1))

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
ea.data <- read.xlsx("inputData/Holston_Laubach_Williams_estimates.xlsx", sheet="EA input data",
                      na.strings = ".", colNames=TRUE, rowNames=FALSE, detectDates = TRUE)

ea.log.output             <- ea.data$gdp.log
ea.inflation              <- ea.data$inflation
ea.inflation.expectations <- ea.data$inflation.expectations
ea.nominal.interest.rate  <- ea.data$interest
ea.real.interest.rate     <- ea.nominal.interest.rate - ea.inflation.expectations
ea.covid.indicator        <- ea.data$covid.ind


# =================
# ESTIMATION
# =================

ea.estimation <- run.hlw.estimation(log.output=ea.log.output,
                                    inflation=ea.inflation,
                                    real.interest.rate=ea.real.interest.rate,
                                    nominal.interest.rate=ea.nominal.interest.rate,
                                    covid.indicator=ea.covid.indicator,
                                    a.r.constraint=a.r.constraint,
                                    b.y.constraint=b.y.constraint,
                                    g.pot.start.index=g.pot.start.index,
                                    use.kappa=use.kappa,
                                    kappa.inputs=kappa.inputs,
                                    fix.phi=fix.phi,
                                    xi.00.stage1=xi.00.stage1,
                                    xi.00.stage2=xi.00.stage2,
                                    xi.00.stage3=xi.00.stage3,
                                    P.00.stage1=P.00.stage1,
                                    P.00.stage2=P.00.stage2,
                                    P.00.stage3=P.00.stage3,
                                    run.se=run.se,
                                    sample.end=sample.end)

# One-sided (filtered) estimates
one.sided.est.ea <- cbind(ea.estimation$out.stage3$rstar.filtered,
                          ea.estimation$out.stage3$trend.filtered,
                          ea.estimation$out.stage3$z.filtered,
                          ea.estimation$out.stage3$output.gap.filtered)

# Two-sided (smoothed) estimates
two.sided.est.ea <- cbind(ea.estimation$out.stage3$rstar.smoothed,
                          ea.estimation$out.stage3$trend.smoothed,
                          ea.estimation$out.stage3$z.smoothed,
                          ea.estimation$out.stage3$output.gap.smoothed)

# =================
# OUTPUT
# =================

# Set up output for export
output.ea <- format.output(country.estimation=ea.estimation,
                           one.sided.est.country=one.sided.est.ea,
                           real.rate.country=ea.real.interest.rate,
                           start=sample.start,
                           end=sample.end,
                           run.se=run.se)

# Save output to CSV
write.table(output.ea, 'output/output.ea.csv', col.names=TRUE, quote=FALSE, row.names=FALSE, sep = ',', na = '')

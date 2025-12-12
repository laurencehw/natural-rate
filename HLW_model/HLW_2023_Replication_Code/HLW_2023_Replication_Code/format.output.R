#------------------------------------------------------------------------------#
# File:        format.output.R
#
# Description: This generates a dataframe to be written to a CSV containing
#              one-sided estimates, parameter values, standard errors,
#              and other statistics of interest.
#------------------------------------------------------------------------------#
format.output <- function(country.estimation, one.sided.est.country, real.rate.country, start, end, run.se = TRUE) {
    output.country <- data.frame(matrix(NA,dim(one.sided.est.country)[1],28)) # if adding additional parameters, will need to adjust the output size

    output.country[,1]   <- seq(from = (as.Date(ti(shiftQuarter(start,-1),'quarterly'))+1), to = (as.Date(ti(shiftQuarter(end,-1),tif='quarterly'))+1), by = 'quarter')
    output.country[,2:4] <- one.sided.est.country[,1:3]

    # Output gap: In model with COVID indicator, this is the COVID-adjusted output gap
    output.country[,5] <- one.sided.est.country[,4]

    # Real rate gap: ex ante real interest rate - r*
    output.country[,6] <- real.rate.country[5:length(real.rate.country)] - country.estimation$out.stage3$rstar.filtered

    # Starting index for next section of columns
    sec2 <- 9

    # Store parameter values
    output.country[1,sec2]    <- "Parameter Point Estimates"
    output.theta.end    <- length(country.estimation$out.stage3$theta)+sec2-1

    output.country[2,sec2:(output.theta.end+1)] <- c(names(country.estimation$out.stage3$param.num), "a_y1 + a_y2")
    output.country[3,sec2:output.theta.end] <- country.estimation$out.stage3$theta
    output.country[3,(output.theta.end+1)]   <- country.estimation$out.stage3$theta[1] + country.estimation$out.stage3$theta[2]

    # Include standard errors in output only if run.se switch is TRUE
    if (run.se) {

        # Parameter t-stats
        output.country[4,sec2]    <- "T Statistics"
        output.country[5,sec2:output.theta.end] <- country.estimation$out.stage3$se$t.stats

        # Average SEs of latent variables
        output.country[8,sec2]    <- "Average Standard Errors"
        output.country[9,sec2:(sec2+2)]  <- c("y*","r*","g")
        output.country[10,sec2:(sec2+2)] <- country.estimation$out.stage3$se$se.mean

        # Count of discarded draws during SE procedure
        output.country[12,sec2]   <- "Restrictions on MC draws: a_3 < -0.0025; b_2 > 0.025; a_1 + a_2 < 1"
        output.country[13,sec2]   <- "Draws excluded:"; output.country[13,(sec2+2)] <- country.estimation$out.stage3$se$number.excluded
        output.country[13,(sec2+3)]  <- "Total:"; output.country[13,(sec2+4)] <- niter
        output.country[14,sec2]   <- "Percent excluded:"; output.country[14,(sec2+2)] <- as.numeric(output.country[13,(sec2+2)]) / (as.numeric(output.country[13,(sec2+2)]) + as.numeric(output.country[13,(sec2+4)]))
        output.country[15,sec2]   <- "Draws excluded because a_r > -0.0025:"; output.country[15,(sec2+4)] <- country.estimation$out.stage3$se$number.excluded.a.r
        output.country[16,sec2]   <- "Draws excluded because b_y <  0.025:"; output.country[16,(sec2+4)] <- country.estimation$out.stage3$se$number.excluded.b.y
        output.country[17,sec2]   <- "Draws excluded because a_y1 + a_y2 < 1:"; output.country[17,(sec2+4)] <- country.estimation$out.stage3$se$number.excluded.a1a2
    }

    # Signal-to-noise ratios
    output.country[19,sec2] <- "Signal-to-noise Ratios"
    output.country[20,sec2] <- "lambda_g"; output.country[20,(sec2+1)] <- country.estimation$lambda.g
    output.country[21,sec2] <- "lambda_z"; output.country[21,(sec2+1)] <- country.estimation$lambda.z
    output.country[19,(sec2+4)] <- "Log Likelihood"; output.country[20,(sec2+4)] <- country.estimation$out.stage3$log.likelihood

    # Initialization of state vector and covariance matrix
    output.country[24,sec2] <- "State vector: [y_{t}* y_{t-1}* y_{t-2}* g_{t} g_{t-1} g_{t-2} z_{t} z_{t-1} z_{t-2}]"
    output.country[25,sec2] <- "Initial State Vector"
    output.country[26,sec2:(sec2+8)] <- country.estimation$out.stage3$xi.00
    output.country[28,sec2] <- "Initial Covariance Matrix"
    output.country[29:37,sec2:(sec2+8)] <- country.estimation$out.stage3$P.00

    # Full time series of SEs
    if (run.se) {
        output.country[,(sec2+16)]   <- seq(from = (as.Date(ti(shiftQuarter(start,-1),'quarterly'))+1), to = (as.Date(ti(shiftQuarter(end,-1),tif='quarterly'))+1), by = 'quarter')
        output.country[,(sec2+17):(sec2+19)] <- country.estimation$out.stage3$se$se
    }

    # Output column names -- will need to adjust if adding additional parameters
    colnames(output.country) <- c("Date","rstar","g","z","output gap","real rate gap","","","All results are output from the Stage 3 model.",rep("",14),"Standard Errors","Date","y*","r*","g")

    return(output.country)
}
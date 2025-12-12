##------------------------------------------------------------------------------##
## File:        format.output.R
##
## Description: This generates a dataframe to be written to a CSV containing
##              one- and two-sided estimates, parameter values, standard errors,
##              and other statistics of interest.
##
##------------------------------------------------------------------------------##
format.output <- function(estimation, real.rate, covid.dummy, start, end, run.se = TRUE, estimate.theta=TRUE) {
    
    # One-sided (filtered) estimates
    one.sided.est <- cbind(estimation$rstar.filtered,
                           estimation$trend.filtered,
                           estimation$z.filtered,
                           estimation$output.gap.filtered)
    
    # Two-sided (smoothed) estimates
    two.sided.est <- cbind(estimation$rstar.smoothed,
                           estimation$trend.smoothed,
                           estimation$z.smoothed,
                           estimation$output.gap.smoothed)
    
    output <- data.frame(matrix(NA,dim(one.sided.est)[1],34))

    output[,1]   <- seq(from = (as.Date(ti(shiftQuarter(start,-1),'quarterly'))+1), to = (as.Date(ti(shiftQuarter(end,-1),tif='quarterly'))+1), by = 'quarter')
    output[,2:5] <- one.sided.est[,1:4] # adj. output gap

    # Real rate gap: ex ante real interest rate - r*
    output[,6] <- real.rate[9:length(real.rate)] - estimation$rstar.filtered
    
    # starting index for next section of columns
    sec2 <- 9
    
    output[1,sec2]    <- "Parameter Point Estimates"
    output.theta.end <- length(estimation$theta)+sec2-1
    
    output[2,sec2:output.theta.end] <- names(sort(estimation$param.num))
    output[3,sec2:output.theta.end] <- estimation$theta

    # Include standard errors in output only if run.se switch is TRUE
    if (run.se) {
      
        # Parameter t-stats
        output[4,sec2]    <- "T Statistics"
        output[5,sec2:output.theta.end] <- estimation$se$t.stats

        # Average SEs of latent variables
        output[8,sec2]    <- "Average Standard Errors"
        output[9,sec2:(sec2+2)]  <- c("y*","r*","g")
        output[10,sec2:(sec2+2)] <- estimation$se$se.mean
        
        # Count of discarded draws during SE procedure
        output[12,sec2]   <- "Restrictions on MC draws: a_3 < -0.0025; b_2 > 0.025; a_1 + a_2 < 1"
        output[13,sec2]   <- "Draws excluded:"; output[13,(sec2+2)] <- estimation$se$number.excluded
        output[13,(sec2+3)]  <- "Total:"; output[13,(sec2+4)] <- niter
        output[14,sec2]   <- "Percent excluded:"; output[14,(sec2+2)] <- as.numeric(output[13,(sec2+2)]) / (as.numeric(output[13,(sec2+2)]) + as.numeric(output[13,(sec2+4)]))
        output[15,sec2]   <- "Draws excluded because a_r > -0.0025:"; output[15,(sec2+4)] <- estimation$se$number.excluded.a.r
        output[16,sec2]   <- "Draws excluded because b_y <  0.025:"; output[16,(sec2+4)] <- estimation$se$number.excluded.b.y
        output[17,sec2]   <- "Draws excluded because a_y1 + a_y2 < 1:"; output[17,(sec2+4)] <- estimation$se$number.excluded.a1a2
    }

    # Signal-to-noise ratios
    output[19,sec2] <- "Signal-to-noise Ratios"
    output[20,sec2] <- "lambda_g"; output[20,(sec2+1)] <- estimation$lambda.g
    output[21,sec2] <- "lambda_z"; output[21,(sec2+1)] <- estimation$lambda.z
    output[19,(sec2+4)] <- "Log Likelihood"; output[20,(sec2+4)] <- estimation$log.likelihood

    # Initialization of state vector and covariance matrix
    output[24,sec2] <- "State vector: [y_{t}* y_{t-1}* y_{t-2}* g_{t} g_{t-1} g_{t-2} z_{t} z_{t-1} z_{t-2}]"
    output[25,sec2] <- "Initial State Vector"
    output[26,sec2:(sec2+8)] <- estimation$xi.00
    output[28,sec2] <- "Initial Covariance Matrix"
    output[29:37,sec2:(sec2+8)] <- estimation$P.00

    # Full time series of SEs
    if (run.se) {
        output[,(sec2+16)]    <- seq(from = (as.Date(ti(shiftQuarter(start,-1),'quarterly'))+1), to = (as.Date(ti(shiftQuarter(end,-1),tif='quarterly'))+1), by = 'quarter')
        output[,(sec2+17):(sec2+19)] <- estimation$se$se
    }

    output[,(sec2+22):(sec2+25)] <- two.sided.est[,1:4]

    colnames(output) <- c("Date","rstar","g","z","output gap","real rate gap","","","All results are output from the Stage 3 model.",rep("",14),"Standard Errors","Date","y*","r*","g",rep("",2),"rstar (smoothed)","g (smoothed)","z (smoothed)","output gap (smoothed)")
    
    return(output)
}

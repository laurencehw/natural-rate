#------------------------------------------------------------------------------#
# File:        kalman.standard.errors.R
#
# Description: This file computes confidence intervals and corresponding
#              standard errors for the estimates of the states using
#              Hamilton's (1986) Monte Carlo procedure that accounts for
#              both filter and parameter uncertainty. See footnote 7 in HLW 2017.
#------------------------------------------------------------------------------#
kalman.standard.errors <- function(t.end, states, theta, y.data, x.data, stage,
                                   lambda.g, lambda.z, xi.00, P.00, theta.lb, theta.ub, niter = 5000,
                                   a.r.constraint=NA, b.y.constraint=NA, sample.end,
                                   use.kappa=FALSE, kappa.inputs=NA, param.num, params.exclude.add=NA) {

    print('Computing Standard Errors')

    # Set a.r.constraint to -0.0025 if a constraint is not specified in stage 3
    if (is.na(a.r.constraint)) {
        a.r.constraint <- -0.0025
    }
    # Set b.y.constraint to 0.025 if a constraint is not specified in stage 3
    if (is.na(b.y.constraint)) {
        b.y.constraint <- 0.025
    }

    print("Standard Error Procedure: a.r.constraint")
    print(a.r.constraint)

    print("Standard Error Procedure: b.y.constraint")
    print(b.y.constraint)

    ###################################
    # DEFINE EXCLUDED PARAMETERS
    #
    # Parameters should only be excluded from SE computation if they are
    # fixed in the model; e.g. phi=0 if sample.end < 2020:Q1
    ###################################

    # Sets date for inclusion of COVID indicator in HLW model. If 2020:Q1 or later, compute SE for phi; otherwise, will omit
    start.phi.se <- ti(c(2020,1), tif='quarterly')  # start computing SE for phi in 2020:Q1
    date.se      <- ti(sample.end, tif='quarterly') # current date in tis format for comparison

    # Create list of parameters to exclude in computing SE
    params.exclude.list <- character(0) # empty vector

    # exclude any parameter for which theta.lb == theta.ub -- this will capture phi and kappa if they are fixed 
    if (length(theta.lb)!=length(param.num)) {
        # throw an error if the bounds and param.num are not the same length
        stop("Error in SE procedure: theta.lb and param.num are different lengths")
    } 

    # if theta.lb[ii]==theta.ub[ii], then the parameter is fixed and we do not compute the SE 
    for (ii in 1:length(param.num)) {
        if (theta.lb[ii]==theta.ub[ii]) { params.exclude.list <- c(params.exclude.list, names(param.num[ii])) } # If a parameter is fixed (upper and lower bound are the same), exclude from SE computation
    }

    if (!is.na(params.exclude.add)) {  params.exclude.list <- c(params.exclude.list, params.exclude.add) } # option to input list of additional parameters to exclude
    params.exclude.list <- unique(params.exclude.list) # remove duplicates

    if (length(params.exclude.list)==0) {
      params.exclude <- NA
    } else {
      params.exclude <- unname(param.num[names(param.num) %in% params.exclude.list]) # indices of excluded parameters
    }

    print('params.exclude.list:')
    print(params.exclude.list)

    print('params.exclude:')
    print(params.exclude)

    params.se.list <- param.num[! names(param.num) %in% params.exclude.list] # list of indices to use, with names
    params.se <- unname(params.se.list) # indices without names

    n.params <- length(theta) # number of total parameters
    n.params.se <- length(params.se) # number of parameter for which to compute SEs

    print('params.se.list:')
    print(params.se.list)

    print('params.se:')
    print(params.se)

    print('n.params:')
    print(n.params)

    print('n.params.se:')
    print(n.params.se)


    ###################################
    # PROCEDURES FOR INFORMATION MATRIX AND T-STATS
    ###################################

    # Number of state variables
    n.state.vars <- length(xi.00)

    # Return vector of log likelihood values at each time t
    log.likelihood.estimated.vector <- log.likelihood.wrapper(parameters=theta, y.data=y.data, x.data=x.data, stage=3,
                                                              lambda.g=lambda.g, lambda.z=lambda.z, xi.00=xi.00, P.00=P.00,
                                                              use.kappa=use.kappa, kappa.inputs=kappa.inputs,
                                                              param.num=param.num)$ll.vec
    stin <- states$smoothed$xi.tT[1,] # First smoothed state vector
    pp1  <- states$filtered$P.ttm1[1:n.state.vars,] # First covariance matrix
    eigenstuff.pp1   <- eigen(pp1)
    eigenvectors.pp1 <- eigenstuff.pp1$vectors # Eigenvectors of first covariance matrix
    # Eigenvectors without a positive first entry are multiplied by -1 to ensure
    # consistency across different versions of R, which choose the sign differently
    for (l in 1:n.state.vars) {
        if (eigenvectors.pp1[1,l] < 0 ) { eigenvectors.pp1[,l] <- -eigenvectors.pp1[,l] }
    }
    eigenvalues.pp1  <- eigenstuff.pp1$value   # Eigenvalues of first covariance matrix
    dg   <- diag(x = eigenvalues.pp1)
    hh2  <- eigenvectors.pp1 %*% sqrt(dg)

    # Compute information matrix from difference in gradients of the likelihood function
    likelihood.gradient <- matrix(NA,t.end,n.params) # use n.params = full length of theta

    # Loop over all estimated parameters (excludes parameters that are fixed, e.g. fix.phi=0, or otherwise removed from params.se)
    for (i in params.se) {
      # i corresponds to index position in theta
      delta   <- max(theta[i]*1e-6, 1e-6)
      d.theta <- theta
      d.theta[i] <- theta[i] + delta

      # Fill in corresponding column of likelihood.gradient
      likelihood.gradient[,i] <-  (log.likelihood.wrapper(parameters=d.theta, y.data=y.data, x.data=x.data, stage=3,
                                                          lambda.g=lambda.g, lambda.z=lambda.z, xi.00=xi.00, P.00=P.00,
                                                          use.kappa=use.kappa, kappa.inputs=kappa.inputs,
                                                          param.num=param.num)$ll.vec - log.likelihood.estimated.vector)/delta
    }

    # Get information matrix

    # Likelihood gradient is filled in for cols associated with params.se (all estimated parameters)
    # It is NA for cols associated with params.exclude

    # To invert, drop cols associated with params.exclude
    if (any(!is.na(params.exclude))) {
      likelihood.gradient.se <- likelihood.gradient[, -params.exclude]
    } else {
      likelihood.gradient.se <- likelihood.gradient # if no excluded parameters
    }

    # likelihood.gradient.se - number of cols is n.params.se

    info <- solve(t(likelihood.gradient.se) %*% likelihood.gradient.se) # Information matrix: dim n.params.se x n.params.se
    bse <- sqrt(diag(info)) # length n.params.se

    # Assign t-stats to correct theta position
    t.stats <- c(rep(NA, n.params)) # length of total theta vector
    t.stats.null <- c(rep(NA, n.params)) # length of total theta vector - this is the null value for each t stat

    # for the params SE parameters, fill in 0s (the default null value)
    t.stats.null[params.se] <- 0

    # for the kappas, fill in 1s (or whatever is in kappa.inputs$t.stat.null)
    if (use.kappa) {
      t.stats.null[kappa.inputs$theta.index] <- kappa.inputs$t.stat.null
    }
    
    print('param.num in SE run:')
    print(param.num)
    print('t.stats.null in SE run:')
    print(t.stats.null)

    t.stats[params.se] <- abs(theta[params.se] - t.stats.null[params.se]) / bse # vector of length theta; NA for params.exclude

    print('t.stats')
    print(t.stats)

    # Smoothed estimates
    g      <- 4 * states$smoothed$xi.tT[,4]
    ypot   <- states$smoothed$xi.tT[,1] # potential output; not COVID-adjusted
    z      <- states$smoothed$xi.tT[,7]
    rstar  <- theta[param.num["c"]]*g + z

    # cum1 cumulates terms for parameter uncertainty;
    # cum2 cumulates terms for filter uncertainty
    cum1 <- matrix(0,t.end,3)
    cum2 <- matrix(0,t.end,3)
    eigenstuff.info   <- eigen(info) # Information matrix: dim n.params.se x n.params.se
    eigenvectors.info <- eigenstuff.info$vectors # Eigenvectors of information matrix
    # Eigenvectors without a positive first entry are multiplied by -1 to ensure
    # consistency across different versions of R, which choose the sign differently
    for (l in 1:(n.params.se)) {
        if (eigenvectors.info[1,l] < 0 ) { eigenvectors.info[,l] <- -eigenvectors.info[,l] }
    }
    eigenvalues.info  <- eigenstuff.info$value # Eigenvalues of information matrix - vector of length n.params.se
    dg <- diag(x = eigenvalues.info)     # dim n.params.se x n.params.se
    hh <- eigenvectors.info %*% sqrt(dg) # dim n.params.se x n.params.se

    set.seed(50)

    # Store the number of draws excluded for violating constraints
    good.draws                  <- 0
    excluded.draw.counter.kappa <- 0 # kappa constraint is tabulated separately from "main" constraints on model parameters
    excluded.draw.counter.main  <- 0
    excluded.draw.counter.a.r   <- 0
    excluded.draw.counter.b.y   <- 0
    excluded.draw.counter.a1a2 <- 0

    # See HLW 2017, footnote 7 for description of procedure
    # niter is the number of iterations; we discard draws that violate constraints
    while (good.draws < niter) {
      # Perturb the parameters in n.params.se (all estimated parameters)
      # Any fixed parameters are not perturbed, i.e. they remain fixed in SE computations
      param.perturbation <- c(rep(0, n.params)) # length of total theta vector
      param.perturbation[params.se] <- as.vector(hh %*% rnorm(n.params.se))
      theta.i <- param.perturbation + theta

      # Check if kappa >= 1 constraint is violated for any value of kappa; if yes, skip to next theta.i
      if (use.kappa) {
        kappa.lb.violated <- FALSE

        # if any kappa constraint violated, change flag to TRUE
        for (kappa in kappa.inputs$name) {
          if (theta.i[param.num[kappa]] < 1) {
            kappa.lb.violated <- TRUE
          }
        }

        if (kappa.lb.violated) {
          excluded.draw.counter.kappa <- excluded.draw.counter.kappa + 1
          next
        }
      }

      # Proceed if kappa constraint is not violated; check model constraints
      if ( (theta.i[param.num["a_r"]] <= a.r.constraint) &
           (theta.i[param.num["b_y"]] >= b.y.constraint) &
           (theta.i[param.num["a_y1"]] + theta.i[param.num["a_y2"]] < 1) ){


          xi.00.i  <- c(t(hh2 %*% rnorm(n.state.vars) + stin))
          states.i <- kalman.states.wrapper(parameters=theta.i, y.data=y.data, x.data=x.data, stage=3,
                                            lambda.g=lambda.g, lambda.z=lambda.z, xi.00=xi.00.i, P.00=pp1,
                                            use.kappa=use.kappa, kappa.inputs=kappa.inputs, param.num=param.num)

          g.i    <- 4 * states.i$smoothed$xi.tT[,4]
          ypot.i <- states.i$smoothed$xi.tT[,1] # potential output; not COVID-adjusted
          z.i    <- states.i$smoothed$xi.tT[,7]
          r.i    <- theta.i[param.num["c"]]*g.i + z.i

          cum1[,1] <- cum1[,1]+(ypot.i-ypot)^2
          cum1[,2] <- cum1[,2]+(r.i-rstar)^2
          cum1[,3] <- cum1[,3]+(g.i-g)^2

          P.tT.i   <- states.i$smoothed$P.tT
          P.ttm1.i.f <- states.i$filtered$P.tt
          for (j in 1:(t.end-1)){
              cum2[j,1]  <- cum2[j,1] + P.tT.i[(j * n.state.vars +1),1]
              cum2[j,2]  <- cum2[j,2] + 16 * P.tT.i[(j*n.state.vars+4),4] + P.tT.i[(j*n.state.vars+7),7]
              cum2[j,3]  <- cum2[j,3] + P.tT.i[(j*n.state.vars+4),4]
          }
          cum2[t.end,1] <- cum2[t.end,1] + P.ttm1.i.f[((t.end-1)*n.state.vars+1),1]
          cum2[t.end,2] <- cum2[t.end,2] + (16 * P.ttm1.i.f[((t.end-1)*n.state.vars+4),4] + P.ttm1.i.f[((t.end-1)*n.state.vars+7),7])
          cum2[t.end,3] <- cum2[t.end,3] + P.ttm1.i.f[((t.end-1)*n.state.vars+4),4]
          good.draws <- good.draws + 1
      } else {
          excluded.draw.counter.main <- excluded.draw.counter.main + 1
          if (theta.i[param.num["a_r"]] > a.r.constraint) {
              excluded.draw.counter.a.r <- excluded.draw.counter.a.r + 1
          }
          if (theta.i[param.num["b_y"]] < b.y.constraint) {
              excluded.draw.counter.b.y <- excluded.draw.counter.b.y + 1
          }
          if ((theta.i[param.num["a_y1"]] + theta.i[param.num["a_y2"]]) >= 1) {
              excluded.draw.counter.a1a2 <- excluded.draw.counter.a1a2 + 1
          }
      }

    } # end of while loop


    cum1 <- cum1/niter # Measure of parameter uncertainty
    cum2 <- cum2/niter # Measure of filter uncertainty
    cum2[,3] <- 16*cum2[,3] # Variance for growth at an annualized rate

    # Standard errors for estimates of the states
    # Order: y*, r*, g
    se <- sqrt(cum1 + cum2)

    return(list("se.mean"=colMeans(se),
                "se"=se,"t.stats"=t.stats,
                "number.excluded"=excluded.draw.counter.main,
                "number.excluded.a.r"=excluded.draw.counter.a.r,
                "number.excluded.b.y"=excluded.draw.counter.b.y,
                "number.excluded.a1a2"=excluded.draw.counter.a1a2,
                "number.excluded.kappa"=excluded.draw.counter.kappa))
}

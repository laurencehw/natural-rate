#------------------------------------------------------------------------------#
# File:        calculate.covariance.R
#
# Description: This function calculates the covariance matrix of the
#              initial state from the gradients of the likelihood function.
#------------------------------------------------------------------------------#
calculate.covariance <- function(initial.parameters, theta.lb, theta.ub,
                                 y.data, x.data, stage,
                                 lambda.g=NA,lambda.z=NA, xi.00,
                                 use.kappa=FALSE, kappa.inputs=NA, param.num){

  # Number of state variables
  n.state.vars <- length(xi.00)

  # Set covariance matrix equal to 0.2 times the identity matrix
  P.00 <- diag(0.2,n.state.vars,n.state.vars)

  # Get parameter estimates via maximum likelihood
  f <- function(theta) {return(-log.likelihood.wrapper(theta, y.data=y.data, x.data=x.data, stage=stage,
                                                       lambda.g=lambda.g, lambda.z=lambda.z,
                                                       xi.00=xi.00, P.00=P.00,
                                                       use.kappa=use.kappa, kappa.inputs=kappa.inputs,
                                                       param.num=param.num)$ll.cum)}
  nloptr.out <- nloptr(initial.parameters, f, eval_grad_f=function(x) {gradient(f, x)},
                       lb=theta.lb,ub=theta.ub,opts=list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8,"maxeval"=5000))
  theta <- nloptr.out$solution

  if (nloptr.out$status==-1 | nloptr.out$status==5) {
      print(paste0("Look at the termination conditions for nloptr in calculate.covariance, Stage ",as.character(stage)))
      stop(nloptr.out$message)
  } else {
    print(paste0("Stage ",as.character(stage),", calculate.covariance: The terminal conditions in nloptr are"))
    print(nloptr.out$message)
  }

  # Run Kalman filter with above covariance matrix and corresponding parameter estimates
  states <- kalman.states.wrapper(parameters=theta, y.data=y.data, x.data=x.data, stage=stage,
                                  lambda.g=lambda.g, lambda.z=lambda.z, xi.00=xi.00, P.00=P.00,
                                  use.kappa=use.kappa, kappa.inputs=kappa.inputs, param.num=param.num)

  # Save initial covariance matrix
  P.00 <- states$filtered$P.ttm1[1:n.state.vars,]

  return(P.00)
}
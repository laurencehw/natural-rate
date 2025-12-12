#------------------------------------------------------------------------------#
# File:        kalman.states.wrapper.R
#
# Description: This is a wrapper function for kalman.states.R that specifies
#              inputs based on the estimation stage.
#------------------------------------------------------------------------------#
kalman.states.wrapper <- function(parameters, y.data, x.data, stage = NA,
                                  lambda.g=NA, lambda.z=NA, xi.00=NA, P.00=NA,
                                  use.kappa=FALSE, kappa.inputs=NA, param.num){

    if (stage == 1) {
        out <- unpack.parameters.stage1(parameters=parameters, y.data=y.data, x.data=x.data,
                                        xi.00=xi.00, P.00=P.00,
                                        use.kappa=use.kappa, kappa.inputs=kappa.inputs, param.num=param.num)
    } else if (stage == 2) {
        out <- unpack.parameters.stage2(parameters=parameters, y.data=y.data, x.data=x.data,
                                        lambda.g=lambda.g, xi.00=xi.00, P.00=P.00,
                                        use.kappa=use.kappa, kappa.inputs=kappa.inputs, param.num=param.num)
    } else if (stage == 3) {
        out <- unpack.parameters.stage3(parameters=parameters, y.data=y.data, x.data=x.data,
                                        lambda.g=lambda.g, lambda.z=lambda.z, xi.00=xi.00, P.00=P.00,
                                        use.kappa=use.kappa, kappa.inputs=kappa.inputs, param.num=param.num)
    } else {
        stop('You need to enter a stage number in kalman.states.wrapper.')
    }

  for (n in names(out)) {
      eval(parse(text=paste0(n, "<-out$", n)))
  }
  t.end <- dim(y.data)[1]
  # Run Kalman filter and smoother 
  states <- kalman.states(xi.tm1tm1=xi.00, P.tm1tm1=P.00, F=F, Q=Q, A=A, H=H, R=R, kappa=kappa.vec, y=y.data, x=x.data)
  if (stage == 1) {
      states$filtered$xi.tt <- states$filtered$xi.tt + cbind(1:t.end,0:(t.end-1),-1:(t.end-2)) * parameters[param.num["g"]]
      states$smoothed$xi.tT <- states$smoothed$xi.tT + cbind(1:t.end,0:(t.end-1),-1:(t.end-2)) * parameters[param.num["g"]]
  }
  return(states)
}
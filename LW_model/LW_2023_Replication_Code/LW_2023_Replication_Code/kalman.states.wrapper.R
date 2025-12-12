##------------------------------------------------------------------------------##
## File:        kalman.states.wrapper.R
##
## Description: This is a wrapper function for kalman.states.R that specifies
##              inputs based on the estimation stage.
##------------------------------------------------------------------------------##
kalman.states.wrapper <- function(parameters, y.data, x.data, stage = NA,
                                  lambda.g=NA, lambda.z=NA, xi.00=NA, P.00=NA,
                                  use.kappa=FALSE, kappa.inputs=NA, param.num){

    if (stage == 1) {
        out <- unpack.parameters.stage1(parameters, y.data, x.data,
                                        xi.00, P.00,
                                        use.kappa=use.kappa, kappa.inputs=kappa.inputs, param.num=param.num)
    } else if (stage == 2) {
        out <- unpack.parameters.stage2(parameters, y.data, x.data,
                                        lambda.g, xi.00, P.00,
                                        use.kappa=use.kappa, kappa.inputs=kappa.inputs, param.num=param.num)
    } else if (stage == 3) {
        out <- unpack.parameters.stage3(parameters, y.data, x.data,
                                        lambda.g, lambda.z, xi.00, P.00,
                                        use.kappa=use.kappa, kappa.inputs=kappa.inputs, param.num=param.num)
    } else {
        stop('You need to enter a stage number in kalman.states.wrapper.')
    }

  for (n in names(out)) {
      eval(parse(text=paste0(n, "<-out$", n)))
  }
  T <- dim(y.data)[1]
  states <- kalman.states(xi.tm1tm1=xi.00, P.tm1tm1=P.00, F=F, Q=Q, A=A, H=H, R=R, kappa=kappa.vec, cons=cons, y=y.data, x=x.data)
  return(states)
}

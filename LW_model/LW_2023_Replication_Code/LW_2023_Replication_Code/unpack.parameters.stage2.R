#------------------------------------------------------------------------------#
# File:        unpack.parameters.stage2.R
#
# Description: This file generates coefficient matrices for the stage 2
#              state-space model for the given parameter vector.
#
# Stage 2 parameter vector: [a_y,1, a_y,2, a_r, a_0, a_g, b_{pi,1}, b_{pi,2-4}, b_y, b_oil, b_import, sigma_y~, sigma_pi, sigma_y*, phi] + [kappas]  # length: 14 + kappas
#------------------------------------------------------------------------------#
unpack.parameters.stage2 <- function(parameters, y.data, x.data, lambda.g, xi.00=NA, P.00=NA, use.kappa, kappa.inputs, param.num) {

  n.state.vars <- 6

  A         <- matrix(0, 2, 13)
  A[1, 1]   <- parameters[param.num["a_1"]] # a_y,1
  A[1, 2]   <- parameters[param.num["a_2"]] # a_y,2
  A[1, 3:4] <- parameters[param.num["a_3"]]/2 # a_r/2
  A[1, 10]  <- parameters[param.num["a_4"]] # a_0
  A[2, 1]   <- parameters[param.num["b_3"]] # b_y
  A[2, 5]   <- parameters[param.num["b_1"]] # b_{pi,1}
  A[2, 6]   <- parameters[param.num["b_2"]] # b_{pi,2-4}
  A[2, 7]   <- 1 - parameters[param.num["b_1"]] - parameters[param.num["b_2"]] # 1 - b_{pi,1} - b_{pi,2-4}
  A[2, 8]   <- parameters[param.num["b_4"]] # b_oil
  A[2, 9]   <- parameters[param.num["b_5"]] # b_import
  A[1, 11]  <- parameters[param.num["phi"]]  # phi
  A[1, 12]  <- -parameters[param.num["a_1"]]*parameters[param.num["phi"]] # -a_1*phi
  A[1, 13]  <- -parameters[param.num["a_2"]]*parameters[param.num["phi"]] # -a_2*phi
  A[2, 12]  <- -parameters[param.num["b_3"]]*parameters[param.num["phi"]] # -b_3*phi
  A         <- t(A)

  H         <- matrix(0, 2, 6)
  H[1, 1]   <- 1
  H[1, 2]   <- -parameters[param.num["a_1"]] # a_y,1
  H[1, 3]   <- -parameters[param.num["a_2"]] # a_y,2
  H[1, 5:6] <- parameters[param.num["a_5"]]/2 # a_g/2 # 2 lags: (a_g/2)*(g_{t-1} + g_{t-2})
  H[2, 2]   <- -parameters[param.num["b_3"]] # b_y
  H         <- t(H)

  R         <- diag(c(parameters[param.num["sigma_1"]]^2, parameters[param.num["sigma_2"]]^2)) # sigma_y~,sigma_pi

  Q         <- matrix(0, 6, 6)
  Q[1, 1]   <- parameters[param.num["sigma_4"]]^2 # sigma_y*
  Q[4, 4]   <- (lambda.g * parameters[param.num["sigma_4"]])^2 # sigma_g = lambda_g * sigma_y*

  F <- matrix(0, 6, 6)
  F[1, 1] <- F[1, 4] <- F[2, 1] <- F[3, 2] <- F[4,4] <- F[5,4] <- F[6,5] <- 1

  # Kappa_t vector:
  kappa.vec <- rep(1, dim(y.data)[1])
  if (use.kappa) {
    n.kappa <- dim(kappa.inputs)[1]
    for (k in 1:n.kappa) {
      T.kappa.start <- kappa.inputs$T.start[k]
      T.kappa.end   <- kappa.inputs$T.end[k]
      ind <- kappa.inputs$theta.index[k]
      kappa.vec[T.kappa.start:T.kappa.end] <- parameters[ind]
      rm(T.kappa.start, T.kappa.end, ind)
    }
  }

  cons <- matrix(0, n.state.vars, 1)

  return(list("xi.00"=xi.00, "P.00"=P.00, "F"=F, "Q"=Q, "A"=A, "H"=H, "R"=R, "kappa.vec"=kappa.vec, "cons"=cons, "x.data"=x.data, "y.data"=y.data))
}

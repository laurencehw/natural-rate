#------------------------------------------------------------------------------#
# File:        unpack.parameters.stage2.R
#
# Description: This file generates coefficient matrices for the stage 2
#              state-space model for the given parameter vector.
#
# Stage 2 parameter vector: [a_y,1, a_y,2, a_r, a_0, a_g, b_pi, b_y, sigma_y~, sigma_pi, sigma_y*, phi, kappas]
#------------------------------------------------------------------------------#
unpack.parameters.stage2 <- function(parameters, y.data, x.data, lambda.g, xi.00=NA, P.00=NA, use.kappa, kappa.inputs, param.num) {
  A         <- matrix(0, 2, 10)
  A[1, 1]   <- parameters[param.num["a_y1"]]
  A[1, 2]   <- parameters[param.num["a_y2"]]
  A[1, 3:4] <- parameters[param.num["a_r"]]/2
  A[1, 7]   <- parameters[param.num["a_0"]]
  A[2, 1]   <- parameters[param.num["b_y"]]
  A[2, 5]   <- parameters[param.num["b_pi"]]
  A[2, 6]   <- 1 - parameters[param.num["b_pi"]]
  A[1, 8]   <- parameters[param.num["phi"]]
  A[1, 9]   <- -parameters[param.num["a_y1"]]*parameters[param.num["phi"]]
  A[1, 10]  <- -parameters[param.num["a_y2"]]*parameters[param.num["phi"]]
  A[2, 9]   <- -parameters[param.num["b_y"]]*parameters[param.num["phi"]]
  A         <- t(A)

  H         <- matrix(0, 2, 6)
  H[1, 1  ] <- 1
  H[1, 2]   <- -parameters[param.num["a_y1"]]
  H[1, 3]   <- -parameters[param.num["a_y2"]]
  H[1, 5:6] <- parameters[param.num["a_g"]]/2
  H[2, 2]   <- -parameters[param.num["b_y"]]
  H         <- t(H)

  R         <- diag(c(parameters[param.num["sigma_ygap"]]^2, parameters[param.num["sigma_pi"]]^2))

  Q         <- matrix(0, 6, 6)
  Q[1, 1]   <- parameters[param.num["sigma_ystar"]]^2
  Q[4, 4]   <- (lambda.g * parameters[param.num["sigma_ystar"]])^2

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

  return(list("xi.00"=xi.00, "P.00"=P.00, "F"=F, "Q"=Q, "A"=A, "H"=H, "R"=R, "kappa.vec"=kappa.vec, "x.data"=x.data, "y.data"=y.data))
}
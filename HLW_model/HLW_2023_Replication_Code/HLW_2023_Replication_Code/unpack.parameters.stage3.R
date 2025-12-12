#------------------------------------------------------------------------------#
# File:        unpack.parameters.stage3.R (annual.kappa)
#
# Description: This file generates coefficient matrices for the stage 3
#              state-space model for the given parameter vector.
#
# Stage 3 parameter vector: [a_y,1, a_y,2, a_r, b_pi, b_y, sigma_y~, sigma_pi, sigma_y*, phi, c] + [kappas]
#
# Stage 3 state vector: [y^*_t, y^*_{t-1}, y^*_{t-2}, g_t, g_{t-1}, g_{t-2}, z_t, z_{t-1}, z_{t-2}]
#
#------------------------------------------------------------------------------#
unpack.parameters.stage3 <- function(parameters, y.data, x.data, lambda.g, lambda.z, xi.00, P.00, use.kappa, kappa.inputs, param.num) {
  A         <- matrix(0, 2, 9)
  A[1, 1]   <- parameters[param.num["a_y1"]]
  A[1, 2]   <- parameters[param.num["a_y2"]]
  A[1, 3:4] <- parameters[param.num["a_r"]]/2
  A[2, 1]   <- parameters[param.num["b_y"]]
  A[2, 5]   <- parameters[param.num["b_pi"]]
  A[2, 6]   <- 1 - parameters[param.num["b_pi"]]
  A[1, 7]   <- parameters[param.num["phi"]]
  A[1, 8]   <- -parameters[param.num["a_y1"]]*parameters[param.num["phi"]]
  A[1, 9]   <- -parameters[param.num["a_y2"]]*parameters[param.num["phi"]]
  A[2, 8]   <- -parameters[param.num["b_y"]]*parameters[param.num["phi"]]
  A         <- t(A)

  H         <- matrix(0, 2, 9)
  H[1, 1]   <- 1
  H[1, 2]   <- -parameters[param.num["a_y1"]]
  H[1, 3]   <- -parameters[param.num["a_y2"]]
  H[1, 5:6] <- -parameters[param.num["c"]]*parameters[param.num["a_r"]]*2
  H[1, 8:9] <- -parameters[param.num["a_r"]]/2
  H[2, 2]   <- -parameters[param.num["b_y"]]
  H         <- t(H)

  R         <- diag(c(parameters[param.num["sigma_ygap"]]^2, parameters[param.num["sigma_pi"]]^2))

  Q         <- matrix(0, 9, 9)
  Q[1, 1]   <- parameters[param.num["sigma_ystar"]]^2
  Q[4, 4]   <- (lambda.g*parameters[param.num["sigma_ystar"]])^2
  Q[7, 7]   <- (lambda.z*parameters[param.num["sigma_ygap"]]/parameters[param.num["a_r"]])^2

  F <- matrix(0, 9, 9)
  F[1, 1] <- F[1, 4] <- F[2, 1] <- F[3, 2] <- F[4,4] <- F[5,4]<- F[6,5] <- F[7,7] <- F[8,7] <- F[9,8] <- 1

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
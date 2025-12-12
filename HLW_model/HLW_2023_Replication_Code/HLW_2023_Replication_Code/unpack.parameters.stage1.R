#------------------------------------------------------------------------------#
# File:        unpack.parameters.stage1.R
#
# Description: This file generates coefficient matrices for the stage 1
#              state-space model for the given parameter vector.
#
# Stage 1 parameter vector: [a_y,1, a_y,2, b_pi, b_y, g, sigma_y~, sigma_pi, sigma_y*, phi, kappas]
#------------------------------------------------------------------------------#
unpack.parameters.stage1 <- function(parameters, y.data, x.data, xi.00, P.00,
                                     use.kappa, kappa.inputs, param.num) {
  A         <- matrix(0, 7, 2)
  A[1, 1]   <- parameters[param.num["a_y1"]]
  A[2, 1]   <- parameters[param.num["a_y2"]]
  A[1, 2]   <- parameters[param.num["b_y"]]
  A[3, 2]   <- parameters[param.num["b_pi"]]
  A[4, 2]   <- 1-parameters[param.num["b_pi"]]
  A[5, 1]   <- parameters[param.num["phi"]]
  A[6, 1]   <- -parameters[param.num["a_y1"]]*parameters[param.num["phi"]]
  A[6, 2]   <- -parameters[param.num["b_y"]]*parameters[param.num["phi"]]
  A[7, 1]   <- -parameters[param.num["a_y2"]]*parameters[param.num["phi"]]

  H         <- matrix(0, 3, 2)
  H[1, 1]   <- 1
  H[2, 1]   <- -parameters[param.num["a_y1"]]
  H[3, 1]   <- -parameters[param.num["a_y2"]]
  H[2, 2]   <- -parameters[param.num["b_y"]]

  R         <- diag(c(parameters[param.num["sigma_ygap"]]^2, parameters[param.num["sigma_pi"]]^2))
  Q         <- matrix(0, 3, 3)
  Q[1, 1]   <- parameters[param.num["sigma_ystar"]]^2

  F <- matrix(0, 3, 3)
  F[1, 1] <- F[2, 1] <- F[3, 2] <- 1

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

  # Make the data stationary
  y.data[, 1] <- y.data[, 1] - 1:dim(y.data)[1] * parameters[param.num["g"]]
  x.data[, 1] <- x.data[, 1] - 0:(dim(x.data)[1]-1) * parameters[param.num["g"]]
  x.data[, 2] <- x.data[, 2] - -1:(dim(x.data)[1]-2) * parameters[param.num["g"]]

  return(list("xi.00"=xi.00, "P.00"=P.00, "F"=F, "Q"=Q, "A"=A, "H"=H, "R"=R, "kappa.vec"=kappa.vec, "x.data"=x.data, "y.data"=y.data))
}
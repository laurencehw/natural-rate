#------------------------------------------------------------------------------#
# File:        unpack.parameters.stage1.R
#
# Description: This file generates coefficient matrices for the stage 1
#              state-space model for the given parameter vector.
#
# Stage 1 parameter vector: [a_1, a_2, b_1, b_2, b_3, b_4, b_5, g, sigma_1, sigma_2, sigma_4, phi] + [kappas]  # length: 12 + kappas
#
# Stage 1 parameter vector: [a_y,1, a_y,2, b_pi,1, b_{pi,2-4}, b_y, b_oil, b_import, g, sigma_y~, sigma_pi, sigma_y*, phi] +  [kappas] # length: 12 + kappas
#------------------------------------------------------------------------------#
unpack.parameters.stage1 <- function(parameters, y.data, x.data, xi.00=NA, P.00=NA, use.kappa, kappa.inputs, param.num) {

  n.state.vars <- 3

  A         <- matrix(0, 10, 2)
  A[1, 1]   <- parameters[param.num["a_1"]] # a_y,1
  A[2, 1]   <- parameters[param.num["a_2"]] # a_y,2
  A[1, 2]   <- parameters[param.num["b_3"]]   # b_y
  A[3, 2]   <- parameters[param.num["b_1"]] # b_{pi,1}
  A[4, 2]   <- parameters[param.num["b_2"]] # b_{pi,2-4}
  A[5, 2]   <- 1-sum(A[3:4, 2]) # 1 - b_{pi,1} - b_{pi,2-4}
  A[6, 2]   <- parameters[param.num["b_4"]] # b_oil
  A[7, 2]   <- parameters[param.num["b_5"]] # b_import
  A[8, 1]   <- parameters[param.num["phi"]] # phi
  A[9, 1]   <- -parameters[param.num["a_1"]]*parameters[param.num["phi"]] # -a_y,1*phi
  A[9, 2]   <- -parameters[param.num["b_3"]]*parameters[param.num["phi"]] # -b_y*phi
  A[10,1]   <- -parameters[param.num["a_2"]]*parameters[param.num["phi"]] # -a_y,2*phi

  H         <- matrix(0, 3, 2)
  H[1, 1]   <- 1
  H[2, 1]   <- -parameters[param.num["a_1"]] # -a_y,1
  H[3, 1]   <- -parameters[param.num["a_2"]] # -a_y,2
  H[2, 2]   <- -parameters[param.num["b_3"]] # -b_y

  R         <- diag(c(parameters[param.num["sigma_1"]]^2, parameters[param.num["sigma_2"]]^2)) # sigma_y~, sigma_pi

  Q         <- matrix(0, 3, 3)
  Q[1, 1]   <- parameters[param.num["sigma_4"]]^2 # sigma_y*

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

  cons <- matrix(0, 3, 1)
  cons[1, 1] <- parameters[param.num["g"]] # g

  return(list("xi.00"=xi.00, "P.00"=P.00, "F"=F, "Q"=Q, "A"=A, "H"=H, "R"=R, "kappa.vec"=kappa.vec, "cons"=cons, "x.data"=x.data, "y.data"=y.data))
}

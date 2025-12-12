#------------------------------------------------------------------------------#
# File:        unpack.parameters.stage3.R
#
# Description: This file generates coefficient matrices for the stage 3
#              state-space model for the given parameter vector.
#
# Stage 3 parameter vector: [a_y,1, a_y,2, a_r, b_{pi,1}, b_{pi,2-4}, b_y, b_oil, b_import, c, sigma_y~, sigma_pi, sigma_y*, phi] +  [kappas]  # length: 13 + kappas
#------------------------------------------------------------------------------#
unpack.parameters.stage3 <- function(parameters, y.data, x.data, lambda.g, lambda.z, xi.00=NA, P.00=NA, xi.00.gpot=NA, P.00.gpot=NA, use.kappa, kappa.inputs, param.num) {

  n.state.vars <- 9

  A         <- matrix(0, 2, 12)
  A[1, 1]   <- parameters[param.num["a_1"]] # a_y,1
  A[1, 2]   <- parameters[param.num["a_2"]] # a_y,2
  A[1, 3:4] <- parameters[param.num["a_3"]]/2 # a_r
  A[2, 1]   <- parameters[param.num["b_3"]]   # b_y
  A[2, 5]   <- parameters[param.num["b_1"]] # b_{pi,1}
  A[2, 6]   <- parameters[param.num["b_2"]] # b_{pi,2-4}
  A[2, 7]   <- 1 - parameters[param.num["b_1"]] - parameters[param.num["b_2"]] # 1 - b_{pi,1} - b_{pi,2-4}
  A[2, 8]   <- parameters[param.num["b_4"]] # b_oil
  A[2, 9]   <- parameters[param.num["b_5"]] # b_import
  A[1, 10]  <- parameters[param.num["phi"]] # phi
  A[1, 11]  <- -parameters[param.num["a_1"]]*parameters[param.num["phi"]] # -a_y,1*phi
  A[1, 12]  <- -parameters[param.num["a_2"]]*parameters[param.num["phi"]] # -a_y,2*phi
  A[2, 11]  <- -parameters[param.num["b_3"]]*parameters[param.num["phi"]] # -b_y*phi
  A         <- t(A)

  H         <- matrix(0, 2, 9)
  H[1, 1  ] <- 1
  H[1, 2]   <- -parameters[param.num["a_1"]] # a_y,1,a_y,2
  H[1, 3]   <- -parameters[param.num["a_2"]] # a_y,1,a_y,2
  H[1, 5:6] <- -parameters[param.num["c"]]*parameters[param.num["a_3"]]*2 # c, a_r (annualized)
  H[1, 8:9] <- -parameters[param.num["a_3"]]/2 # a_r
  H[2, 2]   <- -parameters[param.num["b_3"]]  # b_y
  H         <- t(H)

  R         <- diag(c(parameters[param.num["sigma_1"]]^2, parameters[param.num["sigma_2"]]^2)) # sigma_y~,sigma_pi

  Q         <- matrix(0, 9, 9)
  Q[1, 1]   <- parameters[param.num["sigma_4"]]^2 # sigma_y*
  Q[4, 4]   <- (lambda.g*parameters[param.num["sigma_4"]])^2 # sigma_g = lambda_g * sigma_y*
  Q[7, 7]   <- (lambda.z*parameters[param.num["sigma_1"]]/parameters[param.num["a_3"]])^2 # sigma_z = lambda_z * sigma_y~ / a_r

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


  cons <- matrix(0, n.state.vars, 1)

  return(list("xi.00"=xi.00, "P.00"=P.00, "F"=F, "Q"=Q, "A"=A, "H"=H, "R"=R, "kappa.vec"=kappa.vec, "cons"=cons, "x.data"=x.data, "y.data"=y.data))
}
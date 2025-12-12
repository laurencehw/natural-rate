#------------------------------------------------------------------------------#
# File:        kalman.states.R
#
# Description: The function kalman.states() calls the functions
#              kalman.states.filtered() and kalman.states.smoothed() to
#              apply the Kalman filter and smoother.
#              It takes as input the coefficient matrices for the given
#              state-space model, with notation matching Hamilton (1994),
#              as well as conditional expectation and covariance matrix
#              of the initial state, xi.tm1tm1 and P.tm1tm1 respectively.
#------------------------------------------------------------------------------#
kalman.states <- function(xi.tm1tm1, P.tm1tm1, F, Q, A, H, R, kappa, y, x) {
  filtered <- kalman.states.filtered(xi.tm1tm1, P.tm1tm1, F, Q, A, H, R, kappa, y, x)
  smoothed <- kalman.states.smoothed(filtered$xi.ttm1, filtered$P.ttm1, filtered$xi.tt, filtered$P.tt,
                                     F, Q, A, H, R, kappa, y, x)
  return(list("filtered"=filtered, "smoothed"=smoothed))
}
kalman.states.filtered <- function(xi.tm1tm1, P.tm1tm1, F, Q, A, H, R, kappa, y, x, t.i=1) {
  xi.ttm1 <- as.vector(F %*% xi.tm1tm1)
  P.ttm1 <- F %*% P.tm1tm1 %*% t(F) + Q
  prediction.error <- (as.vector(y[t.i,]) - as.vector(t(A) %*% as.vector(x[t.i,])) - as.vector(t(H) %*% xi.ttm1))
  HPHR <- t(H) %*% P.ttm1 %*% H + (kappa[t.i]^2) * R
  xi.tt <- xi.ttm1 + as.vector(P.ttm1 %*% H %*% solve(HPHR, prediction.error))
  P.tt <- P.ttm1 - P.ttm1 %*% H %*% solve(HPHR, t(H) %*% P.ttm1)
  kalman.gain <- P.ttm1 %*% H %*% solve(HPHR) # dim(xi.tt) x 2 matrix
  if (t.i == dim(y)[1]) {
      return(list("xi.ttm1"=xi.ttm1, "P.ttm1"=P.ttm1, "xi.tt"=xi.tt, "P.tt"=P.tt,
                  "prediction.error"=prediction.error, "kalman.gain"=kalman.gain))
  } else {
      tmp <- kalman.states.filtered(xi.tt, P.tt, F, Q, A, H, R, kappa, y, x, t.i+1)
      return(list("xi.ttm1"=rbind(xi.ttm1, tmp$xi.ttm1),
                  "P.ttm1"=rbind(P.ttm1, tmp$P.ttm1),
                  "xi.tt"=rbind(xi.tt, tmp$xi.tt),
                  "P.tt"=rbind(P.tt, tmp$P.tt),
                  "prediction.error"=rbind(prediction.error, tmp$prediction.error),
                  "kalman.gain"=rbind(kalman.gain, tmp$kalman.gain)))
  }
}
kalman.states.smoothed <- function(xi.ttm1.array, P.ttm1.array, xi.tt.array, P.tt.array,
                                   F, Q, A, H, R, kappa, y, x, t.i=dim(y)[1], xi.tp1T=NA, P.tp1T=NA) {
  n <- dim(xi.ttm1.array)[2]
  if (t.i == dim(y)[1]) {
    xi.tT <- xi.tt.array[t.i,]
    P.tT <- P.tt.array[((t.i-1)*n+1):(t.i*n),]
    tmp <- kalman.states.smoothed(xi.ttm1.array, P.ttm1.array, xi.tt.array, P.tt.array,
                                  F, Q, A, H, R, kappa, y, x, t.i-1, xi.tT, P.tT)
    return(list("xi.tT"=rbind(tmp$xi.tT, xi.tT),
                "P.tT" =rbind(tmp$P.tT, P.tT)))
  } else {
    P.tt <- P.tt.array[((t.i-1)*n+1):(t.i*n),]
    P.tp1t <- P.ttm1.array[(t.i*n+1):((t.i+1)*n),]
    J.t <- P.tt %*% t(F) %*% solve(P.tp1t)
    xi.tt <- xi.tt.array[t.i,]
    xi.tp1t <- xi.ttm1.array[t.i+1,]
    xi.tT <- xi.tt + as.vector(J.t %*% (xi.tp1T - xi.tp1t))
    P.tT <- P.tt + J.t %*% (P.tp1T - P.tp1t) %*% t(J.t)
    if (t.i > 1) {
      tmp <- kalman.states.smoothed(xi.ttm1.array, P.ttm1.array, xi.tt.array, P.tt.array,
                                    F, Q, A, H, R, kappa, y, x, t.i-1, xi.tT, P.tT)
      return(list("xi.tT"=rbind(tmp$xi.tT, xi.tT),
                  "P.tT" =rbind(tmp$P.tT, P.tT)))
    } else {
      return(list("xi.tT"=xi.tT, "P.tT"=P.tT))
    }
  }
}
#' @title iOLS_path
#'
#' @description See the notebook at: https://www.davidbenatia.com/.
#'
#'
#' @param y the dependent variable, a vector.
#' @param X the regressors matrix x with a column of ones added.
#' @param deltainf numeric, the lowest hyper-parameter delta we want to apply iOLS with. The default value is 10^-5.
#' @param deltasup numeric, the highest hyper-parameter delta we want to apply iOLS with. The default value is 10000.
#' @param nbre_delta integer, the number of hyper-parameters delta we want between deltainf and deltasup.
#' @param epsi since the estimated parameters are obtained by converging, we need a convergence criterion epsi (supposed to be small, usually around 10^-5), to make the program stop once the estimations are near their limits. A numeric.
#' @param b_init the point from which the solution starts its converging trajectory. A vector that has the same number of elements as there are parameters estimated in the model.
#' @param error_type a character string specifying the estimation type of the covariance matrix. Argument of the vcovHC function, then click this link for details. "HC0" is the default value, this the White's estimator.
#'
#' @return a iOLS_path fitted model object

#'
#' @export
iOLS_path <- function(y, X, deltainf = 10^-5, deltasup = 10^4, nbre_delta = 20, epsi = 10^-3, b_init, error_type = "HC0") {

  delta <- exp(seq(log(deltainf), log(deltasup), length.out = nbre_delta))


  tX <- t(X)
  VX <- inv(tX %*% X)

  delta0 <- 1/mean(exp(X %*% c(b_init)))

  b <- NULL
  b[[1]] <-
    iOLS(y, X, VX, tX, delta0, epsi, b_init, error_type)
  pointsws <-
    t(b[[1]]$coef)



  deltaused <- delta0
  x_axis2 <- c(delta0)
  x_axis1 <- NULL
  i <- 1
  for (delta_i in delta) {
    if (delta_i > deltaused) {
      deltaused <- delta_i
      x_axis2 <- c(x_axis2, deltaused)
      b[[i+1]] <- iOLS(y, X, VX, tX, deltaused, epsi, pointsws[i,], error_type)
      pointsws <- rbind(pointsws, t(b[[i]]$coef))
      i <- i+1
    }
  }

  a<-NULL
  j<-1
  deltaused <- delta0
  for (delta_i in rev(delta)) {
    if (delta_i < deltaused) {
      deltaused <- delta_i
      x_axis1 <- c(deltaused, x_axis1)
      a[[j]] <- iOLS(y, X, VX, tX, deltaused, epsi, pointsws[1,], error_type)
      pointsws <- rbind(t(a[[j]]$coef), pointsws)
      j <- j+1
    }
  }

  x_axis <- c(x_axis1, x_axis2)

  regs <- append(rev(a), b)


  z <-
    list(
      iols = regs,
      coef_iter = pointsws,
      delta_list = x_axis,
      deltainf = deltainf,
      deltasup = deltasup,
      nbre_delta = nbre_delta,
      init = b_init,
      epsi = epsi,
      y = y,
      x = X[, -1],
      X = X,
      error_type = error_type,
      data = data.frame(y, X[, -1])
    )
  class(z) <- "iOLS_path"
  z
}

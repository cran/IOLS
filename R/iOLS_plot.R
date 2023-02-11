#' @title iOLS_plot
#'
#' @description Function that plots an \code{iOLS} fitted model object.
#'
#' @param m An \code{iOLS} fitted model object.
#' @param ... other parameters.
#' @param plot_beta If you want to see the trajectory of one
#' estimated parameter beta only, just precise plot_beta = k
#' (k=0 if you want to see the intercept's trajectory for example).
#' Otherwise, write plot_beta = "" (the default value),
#' and you will see all parameters' trajectory.
#' In this case, the colors of each curve is assigned randomly,
#' but by precising which parameters' trajectory you want to see,
#' it will be drawn in black.
#'
#' @return a plot of an \code{iOLS} fitted model object.
#'
#' @examples
#' data(DATASET)
#' y = DATASET$y
#' x = as.matrix(DATASET[,c("X1","X2")])
#' lm = lm(log(y+1) ~ x)
#' lm_coef = c(coef(lm))
#' X = cbind(rep(1, nrow(x)), x)
#' tX = t(X)
#' library(matlib) ; VX = inv(tX %*% X)
#' f = iOLS(y, X, VX, tX, 20, b_init = lm_coef)
#'
#' iOLS_plot(f)
#'
#' #Only one of the estimated parameters, for example k=0 (the intercept):
#' iOLS_plot(f, plot_beta = 0)
#'
#' @export
iOLS_plot <- function(m, ..., plot_beta = "") {
  if (plot_beta == "") {
    for (i in 1:(dim(cbind(m$x))[2]+1)) {
      plot(
        m$beta_iter[, i] ~ seq(1, m$n_iter, 1),
        ylim = c(-5, 5),
        col = randomColor(),
        ylab = 'beta',
        xlab = 'iteration rank',
        pch = 20
      )
      oldpar <- par(no.readonly = TRUE)
      on.exit(par(oldpar))
      par(new = TRUE)
    }
  }
  else {
    plot(
      m$beta_iter[, plot_beta + 1] ~ seq(1, m$n_iter, 1),
      ylim = c(-5, 5),
      col = "black",
      ylab = str_c('beta', "_", plot_beta),
      xlab = 'iteration rank',
      pch = 20
    )
  }
}

#' @title plot.iOLS
#'
#'
#' @description See the notebook at: https://www.davidbenatia.com/.
#'
#'
#' @param m An "iOLS" fitted model object.
#' @param ... other parameters.
#' @param plot_beta If you want to see the trajectory of one estimated parameter beta only, just precise plot_beta = k (k=0 if you want to see the intercept's trajectory for example). Otherwise, write plot_beta = "" (the default value), and you will see all parameters' trajectory. In this case, the colors of each curve is assigned randomly, but by precising which parameters' trajectory you want to see, it will be drawn in black.
#'
#' @return a plot of a iOLS fitted model object.

#' @export
plot <-
  function(m, plot_beta, ...){
    UseMethod("plot")
  }
#' @rdname plot
#' @method plot iOLS
#' @S3method plot iOLS
plot.iOLS <- function(m, ..., plot_beta = "") {
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

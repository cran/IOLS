#' @title iOLS_path_plot
#'
#' @description Function that plots an \code{iOLS_path} fitted model object.
#'
#' @param m An "iOLS_path" fitted model object.
#' @param delta_rank Among all the hyper-parameters delta, we can choose to plot the "iOLS_path" fitted model object corresponding to the chosen delta_rank. If a value is not precised, the default value is NULL and the function will only display the estimated parameter(s) in function of log(delta).
#' @param plot_beta If you want to see the trajectory of one estimated parameter beta only, just precise plot_beta = k (k=0 if you want to see the intercept's trajectory for example). Otherwise, write plot_beta = "" (the default value), and you will see all parameters' trajectory. In this case, the colors of each curve is assigned randomly, but by precising which parameters' trajectory you want to see, it will be drawn in black.
#' @param ... other parameters.
#'
#' @return a plot of a iOLS_path fitted model object.
#'
#' @export
iOLS_path_plot <- function(m, delta_rank = NULL, plot_beta = "", ...) {

  if (is.null(delta_rank)) {
    if (plot_beta == "") {
      for (i in 1:(dim(cbind(m$x))[2]+1)) {
        plot(
          m$coef_iter[, i] ~ log(m$delta_list),
          ylim = c(-5, 5),
          col = randomColor(),
          ylab = 'beta',
          xlab = 'log(delta)',
          pch = 20
        )
        oldpar <- par(no.readonly = TRUE)
        on.exit(par(oldpar))
        par(new = TRUE)
      }
    }
    else {
      plot(
        m$coef_iter[, plot_beta + 1] ~ log(m$delta_list),
        ylim = c(-5, 5),
        col = "black",
        ylab = str_c('beta', plot_beta),
        xlab = 'log(delta)',
        pch = 20
      )
    }
  }
  else {
    if (plot_beta == "") {
      plot(m$iols[[delta_rank]])
    }
    else {
      plot(m$iols[[delta_rank]], plot_beta)
    }
  }
}

#' @examples
#' data(DATASET)
#' y = DATASET$y
#' x = as.matrix(DATASET[,c("X1","X2")])
#' lm = lm(log(y+1) ~ x)
#' lm_coef = c(coef(lm))
#' X = cbind(rep(1, nrow(x)), x)
#' k = iOLS_path(y, X, b_init = lm_coef, deltainf = 10^-5,
#' deltasup = 10^4, nbre_delta = 20,
#' epsi = 10^-3, error_type = "HC0")
#'
#' #All the parameters, as a function of log(delta) (ie. each triplet from an iOLS regression) :
#' iOLS_path_plot(k)
#'
#' #All the parameters from the 6th iOLS regression :
#' iOLS_path_plot(k, delta_rank = 6)
#'
#' #Intercept from the 6th iOLS regression :
#' iOLS_path_plot(k, delta_rank = 6, plot_beta = 0)
#'

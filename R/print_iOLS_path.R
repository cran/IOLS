#' @title print.iOLS_path
#'
#' @description See the notebook at: https://www.davidbenatia.com/.
#' @param m An "iOLS_path" fitted model object.
#' @param delta_rank Among all the hyper-parameters delta, we can choose to plot the "iOLS_path" fitted model object corresponding to the chosen delta_rank. If a value is not precised, the default value is NULL and the function will only display the estimated parameter(s) in function of log(delta).
#' @param ... other parameters.
#'
#' @return a display of a iOLS_path fitted model object.


#' @export

print <-
  function(m, ...){
    UseMethod("print")
  }
#' @rdname print
#' @method print iOLS_path
#' @S3method print iOLS_path
#'
print.iOLS_path <- function(m, delta_rank = NULL, ...) {
  if (is.null(delta_rank)) {
    cat("\nCall: iOLS_path(y ~ x)\n")
    cat("\nCoefficients as a function of delta:\n\n")
    mat <-cbind(m$delta_list, m$coef_iter)
    colnames(mat)<-c("Delta", "Intercept", colnames(m$X)[-1])
    print(mat)
  } else{
    m$iols[[delta_rank]]
  }
}

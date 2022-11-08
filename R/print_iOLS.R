#' @title print.iOLS
#'
#' @description See the notebook at: https://www.davidbenatia.com/.
#'
#'
#' @param m An "iOLS" fitted model object.
#' @param ... other parameters.
#'
#' @return a display of a iOLS fitted model object.



#' @export

print <-
  function(m, ...){
    UseMethod("print")
  }
#' @rdname print
#' @method print iOLS
#' @S3method print iOLS
print.iOLS <- function(m, ...) {

  if (is.null(colnames(m$X)) == FALSE) {
    cat("\nCall: iOLS(y ~ x)\ndelta =", m$delta, ", \npoint initial =", m$init)
    cat("\n")
    cat("\nCoefficients:\n")
    displaym <- cbind(m$coef, m$sd, c(m$tvalue), c(m$pvalue))
    rownames(displaym) <- c("(Intercept)", colnames(m$X)[-1])
    colnames(displaym) <-
      c("Coefficients", "Std. Error", "t value", "Pr(>|t|)")
    print(displaym)
  } else{
    cat("name your variables (X columns except the first one !)")
  }
}

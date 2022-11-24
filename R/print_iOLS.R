#' @title print.iOLS
#'
#' @description Function that prints an \code{iOLS} fitted model object.
#'
#' @param m An "iOLS" fitted model object.
#' @param ... other parameters.
#'
#' @return a display of a iOLS fitted model object.
#'
#' @export
print <-
  function(m, ...){
    UseMethod("print")
  }

#' @rdname print.iOLS
#'
#' @method {print} {iOLS}
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
#' print(k)
#'

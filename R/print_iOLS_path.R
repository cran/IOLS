#' @title print.iOLS_path
#'
#' @description Function that prints an \code{iOLS_path} fitted model object.
#' @param m An "iOLS_path" fitted model object.
#' @param delta_rank Among all the hyper-parameters delta, we can choose to plot the "iOLS_path" fitted model object corresponding to the chosen delta_rank. If a value is not precised, the default value is NULL and the function will only display the estimated parameter(s) in function of log(delta).
#' @param ... other parameters.
#'
#' @return a display of a iOLS_path fitted model object.
#'
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
#' #Printing of all the iOLS regression:
#' print(k)
#'
#' #Printing of the 6th iOLS regression :
#' print(k, delta_rank = 6)
#'
#' @export
print <-
  function(m, delta_rank = NULL, ...){
    UseMethod("print")
  }

#' @rdname print.iOLS_path
#'
#' @method {print} {iOLS_path}
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

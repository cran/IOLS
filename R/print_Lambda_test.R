#' @title print.lambda_test
#'
#' @description See the notebook at: https://www.davidbenatia.com/.
#' @param m  A "lambda_test" object.
#' @param ... other parameters.
#'
#' @return a display and a plot of a lambda_test object.
#
#' @export
print <-
  function(m, ...){
    UseMethod("print")
  }

#' @rdname print.lambda_test
#'
#' @method {print} {lambda_test}
print.lambda_test <- function(m, ...){
  cat(
    "Lambda-test:\n\nLambda 1 :",
    m$lambda1,
    ",  SE =",
    m$t_test_lambda1$stderr,
    " ,  t =",
    m$t_test_lambda1$statistic,
    ",  p =",
    m$t_test_lambda1$p.value,
    "\nLambda 2 :",
    m$lambda2,
    ",  SE =",
    m$t_test_lambda2$stderr,
    " ,  t =",
    m$t_test_lambda2$statistic,
    ",  p =",
    m$t_test_lambda2$p.value,
    "\n\ndelta(lambda_1) =",
    m$delta_lambda1,
    "\ndelta(lambda_2) =",
    m$delta_lambda2
  )

  plot(
    m$lambda_list ~ log(m$iols_path_delta_list),
    ylim = c(0, 5),
    col = ifelse(m$lambda_list %in% m$lambda1, 'red', ifelse(m$lambda_list %in% c(m$lambda2), 'blue', 'black')),
    ylab = 'lambda',
    xlab = 'log(delta)',
    pch = 20
  )
  text(log(m$iols_path_delta_list[m$rank_1]),m$lambda1, "Lambda 1", col = "red", cex = 0.8, pos = 3, font = 2)
  text(log(m$iols_path_delta_list[m$rank_2]),m$lambda2, "Lambda 2", col = "blue", cex  = 0.8, pos = 1, font = 2)

  abline(h = 1)
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
#' L = lambda_test(k, nB = 5)
#'
#' print(L)
#'

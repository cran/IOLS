#' @title iOLS
#'
#' @description \code{iOLS} used to fit log-linear model/log-log model, adressing the "log of zero" problem,
#' based on the theoretical results developed in the following paper : <https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3444996>.
#'
#' @param y the dependent variable, a vector.
#' @param X the regressors matrix x with a column of ones added.
#' @param VX a matrix that MUST be equal to (X'X)^-1.
#' @param tX a matrix that MUST be equal to X^t (the transpose of X).
#' @param d the value of the hyper-parameter delta, numeric.
#' @param epsi since the estimated parameters are obtained by converging, we need a convergence criterion epsi (supposed to be small, usually around 10^-5), to make the program stop once the estimations are near their limits. A numeric.
#' @param b_init the point from which the solution starts its converging trajectory. A vector that has the same number of elements as there are parameters estimated in the model.
#' @param error_type a character string specifying the estimation type of the covariance matrix. Argument of the vcovHC function, then click this link for details. "HC0" is the default value, this the White's estimator.
#'
#' @importFrom stats binomial glm lm pt t.test
#' @importFrom utils tail
#' @importFrom sandwich vcovHC
#' @importFrom matlib inv
#' @importFrom boot boot
#' @importFrom graphics abline par text
#' @importFrom randomcoloR randomColor
#' @importFrom stringr str_c
#'
#' @return a iOLS fitted model object
#'
#' @export
iOLS <- function(y, X, VX, tX, d, epsi = 10^-5, b_init, error_type = "HC0") {

  beta <- rbind(b_init)

  crit <- 1000
  i <- 1
  while (crit > epsi) {
    ty <-
      log(y + d * exp(X %*% cbind(beta[i,]))) - c_(d, beta[i,], y, X)
    beta <-
      rbind(beta, c(VX %*% tX %*% ty))
    i <- i + 1
    crit <- norm(c(beta[i,] - beta[i - 1,]), type = "2")
  }
  bhat <- tail(beta, 1)

  yhat<- exp(X%*%t(bhat))

  uhat <- y * exp(-X %*% t(bhat))
  G <- tX%*%diag(c(uhat/(d+uhat)))%*%X
  ty <- log(y + d * exp(X %*% t(bhat))) - c_(d, t(bhat), y, X)
  lm <- lm(ty ~ X[,-1])
  Omeg <- vcovHC(lm, type = error_type, sandwich = TRUE)
  S <- inv(G) %*% tX %*% X %*% Omeg %*% tX %*% X %*% inv(G)
  SE <- sqrt(diag(S))

  t_value <- bhat/SE
  p_value <- 2*pt(abs(t_value), dim(X)[1] - dim(X)[2], lower.tail = FALSE)

  z <-
    list(
      init = b_init,
      delta = d,
      coef = t(bhat),
      epsi = epsi,
      sd = SE,
      residuals = uhat,
      fitted.values = yhat,
      beta_iter = beta,
      n_iter = i,
      tvalue = t_value,
      pvalue = p_value,
      X = X,
      x = X[,-1],
      error_type = error_type
    )
  class(z) <- "iOLS"
  z
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

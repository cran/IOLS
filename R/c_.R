c_ <- function (d, beta, y, X) {

  beta_1 <- log(mean(y * exp(-X %*% cbind(beta))))

  mean(log(y + d * exp(beta_1 + X %*% cbind(beta))) - beta_1 - X %*% cbind(beta))
}

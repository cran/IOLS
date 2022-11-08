#' @title lambda_test
#'
#' @description See the notebook at: https://www.davidbenatia.com/.
#'
#' @param f An "iOLS_path" fitted model object that you want to apply this test on.
#' @param nB The number of iteration that you want to be done in the bootstrap process used in the function.
#'
#' @return a lambda_test object
#'
#' @export
lambda_test <- function(f, nB){

  data <- f$data

  calcul_lambda <- function(data, ind) {
    dB <- data[ind, ]
    lambda <- NULL
    ydB <- dB[, 1]
    xdB <- data.matrix(dB[, -1])
    XdB <- cbind(rep(1, dim(xdB)[1]), xdB)
    y0 <- rep(0, length(ydB))
    y0[ydB > 0] <- 1
    p_hat <-
      glm(y0 ~ xdB, family = binomial(link = "logit"))$fitted.values

    mB <- iOLS_path(ydB, XdB, f$deltainf, f$deltasup, f$nbre_delta, f$epsi, f$init, f$error_type)

    for (i in 1:length(mB$delta_list)) {
      c_hat <- c_(mB$delta_list[i], mB$iols[[i]]$coef, ydB, XdB)
      w <- (c_hat - log(mB$delta_list[i])) / p_hat
      u_hat_pos <- mB$iols[[i]]$residuals[ydB > 0]
      lm_test <-
        lm( log(mB$delta_list[i] + u_hat_pos)-log(mB$delta_list[i]) ~ w[ydB > 0])
      lambda <- c(lambda, lm_test$coefficients[2])
    }
    lambda
  }

  cL <- calcul_lambda(data)

  rank_1 <- which.min(abs(cL-1))
  lambda_1 <- cL[rank_1]

  B <- boot(data, calcul_lambda, R = nB)
  RMSE <- NULL
  for (i in 1:dim(B$t)[2]){
    RMSE[i] <- sqrt(sum((B$t[,i]-1)^2)/nB)
  }
  rank_2 <- which.min(RMSE)
  lambda_2 <- cL[rank_2]

  t1 <- t.test(B$t[,rank_1], mu = 1)
  t2 <- t.test(B$t[,rank_2], mu = 1)

  z <-
    list(
      lambda_list = B$t0,
      lambda_list_boot = B$t,
      lambda1 = lambda_1,
      lambda2 = lambda_2,
      t_test_lambda1 = t1,
      t_test_lambda2 = t2,
      iols_path_delta_list = f$delta_list,
      rank_1 = rank_1,
      rank_2 = rank_2,
      delta_lambda1 = f$delta_list[rank_1],
      delta_lambda2 = f$delta_list[rank_2]
    )
  class(z) <- "lambda_test"
  z
}

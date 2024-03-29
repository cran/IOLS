% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iOLS_path_print.R
\name{print.iOLS_path}
\alias{print.iOLS_path}
\title{print.iOLS_path}
\usage{
\method{print}{iOLS_path}(m, delta_rank = NULL, ...)
}
\arguments{
\item{m}{An \code{iOLS_path} fitted model object.}

\item{delta_rank}{Among all the hyper-parameters delta,
we can choose to plot the "iOLS_path" fitted model object
corresponding to the chosen delta_rank.
If a value is not precised, the default value is NULL
and the function will only display the estimated parameter(s)
in function of log(delta).}

\item{...}{other parameters.}
}
\value{
a display of a \code{iOLS_path} fitted model object.
}
\description{
Function that prints an \code{iOLS_path}
fitted model object.
}
\examples{
data(DATASET)
y = DATASET$y
x = as.matrix(DATASET[,c("X1","X2")])
lm = lm(log(y+1) ~ x)
lm_coef = c(coef(lm))
X = cbind(rep(1, nrow(x)), x)
k = iOLS_path(y, X, b_init = lm_coef, deltainf = 10^-5,
deltasup = 10^4, nbre_delta = 20,
epsi = 10^-3, error_type = "HC0")

#Printing of all the iOLS regression:
print(k)

#Printing of the 6th iOLS regression :
print(k, delta_rank = 6)

}

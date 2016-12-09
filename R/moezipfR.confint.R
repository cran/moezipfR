#' Confidence Intervals
#'
#' Computes the confidence intervals for the maximum likelihood estimation of the parameters.
#'
#' @param data Matrix of count data.
#' @param alpha Maximum likelihood estimation of the parameter \eqn{\alpha}.
#' @param beta Maximum likelihood estimation of the parameter \eqn{\beta}.
#' @param level Confidence level used to calculate the intervals.
#' @return An object with the intervals of each parameter.
#'  \item{alpha.lowerB}{Lower bound of the \eqn{\alpha} parameter.}
#'  \item{alpha.upperB}{Upper bound of the \eqn{\alpha} parameter.}
#'  \item{beta.lowerB}{Lower bound of the \eqn{\beta} parameter.}
#'  \item{beta.upperB}{Upper bound of the \eqn{\beta} parameter.}
#' @details The argument \code{data} is a matrix where the first column corresponds to the level,
#' and the second column contains the frequency for each level.
#' @examples
#' data <- rmoezipf(n = 1000, alpha = 2.5, beta = 1.3)
#' data <- moezipfR.utils.getDataMatrix(data)
#' estimation <- moezipfR.fit(data = data, init_alpha = 1.001, init_beta = 0.001)
#' moezipfR.confint(data, alpha = estimation$alpha, beta = estimation$beta, level = 0.95)
.moezipfR.confint <- function(data, alpha, beta, level=0.95){
  res <- NULL
  # dataInfo <- .getSpectrumValues(data)
  hess <- numDeriv::hessian(.mloglikelihood, c(alpha, beta), nSize = sum(data[, 2]),
                  freq = data[, 2], values = data[, 1])
  normValue <- round(stats::qnorm(1-((1-level)/2)), 2)

  inc <- normValue * sqrt(diag(solve(hess)))
  alowerB <- alpha - inc[1]
  aupperB <- alpha + inc[1]
  blowerB <- beta - inc[2]
  bupperB <- beta + inc[2]
  res <- list(alpha.lowerB = alowerB, alpha.upperB = aupperB,
              beta.lowerB = blowerB, beta.upperB = bupperB)
  return(res)
}

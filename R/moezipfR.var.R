#' Variance.
#'
#' Computes the variance of the MOEZipf distribution for given values of \eqn{\alpha} and \eqn{\beta}.
#' @param alpha Value of the \eqn{\alpha} parameter (\eqn{\alpha > 3}).
#' @param beta Value of the \eqn{\beta} parameter (\eqn{\beta > 0}).
#' @param tolerance Tolerance used in the calculations. (default = \eqn{10^{-4}})
#' @return A positive real value corresponding to the variance of the distribution.
#'
#' @details
#' The variance of the distribution only exists for \eqn{\alpha} strictly greater than 3.
#' It is calculated as:
#' \deqn{Var[Y] = E[Y^2] - (E[Y])^2}
#'
#' @examples
#' moezipfR.var(3.5, 1.3)
#' @export
#'
moezipfR.var <- function(alpha, beta, tolerance = 10^(-4)){
  moment1 <- moezipfR.moments(1, alpha, beta, tolerance)
  moment2 <- moezipfR.moments(2, alpha, beta, tolerance)

  return(moment2 - (moment1^2))
}

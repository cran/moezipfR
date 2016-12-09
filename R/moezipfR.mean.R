#' Expected value.
#'
#' Computes the expected value of the MOEZipf distribution for given parameters
#' \eqn{\alpha} and \eqn{\beta}.
#'
#' @param alpha Value of the \eqn{\alpha} parameter (\eqn{\alpha > 2}).
#' @param beta Value of the \eqn{\beta} parameter (\eqn{\beta > 0}).
#' @param tolerance Tolerance used in the calculations (default = \eqn{10^{-4}}.
#'
#' @return A positive real value corresponding to the mean value of the distribution.
#'
#' @details
#' The expected value of the MOEZipf distribution only exists for \eqn{\alpha} values strictly greater than 2.
#' In that case, if Y is a random variable that follows a MOEZipf distribution with parameters \eqn{\alpha}
#' and \eqn{\beta}, the expected values is computed as:
#' \deqn{E(Y) = \sum_{x = 1} ^\infty \frac{\beta \zeta(\alpha) x^{-\alpha + 1}}{[\zeta(\alpha) - \bar{\beta}\zeta(\alpha, x)][\zeta(\alpha) - \bar{\beta}\zeta(\alpha, x + 1)]}, \alpha > 2,  \beta > 0}
#'
#' The mean is computed by calculating the partial sums of the serie. When two
#' consecutive partial sums differs less than the \code{tolerance} value, the process
#' stops and the partial sum is returned.
#'
#' @examples
#' moezipfR.mean(2.5, 1.3)
#' moezipfR.mean(2.5, 1.3, 10^(-3))
#' @export
moezipfR.mean <- function(alpha, beta, tolerance = 10^(-4)){
  return(moezipfR.moments(1, alpha, beta, tolerance = tolerance))
}


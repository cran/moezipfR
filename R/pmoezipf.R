#' Cumulative function.
#'
#' Cumulative distribution function for the MOEZipf distribution
#' with parameters \eqn{\alpha} and \eqn{\beta}.
#'
#' @details The cumulative distribution function, \eqn{F(x)}, at a given positive real value \eqn{x},
#'  is calcuted from the survival function \eqn{S(x)} as:
#'
#'
#' \deqn{F(x) = 1 - S(x), }
#'
#' the survival function \eqn{S(x)} is equal to:
#'
#' \deqn{S(x) = \frac{\beta \zeta(\alpha, x + 1)}{\zeta(\alpha) - \bar{\beta}\zeta(\alpha, x + 1)}, \forall x > 0}
#'
#' @param x Vector of positive values.
#' @param alpha Value of the \eqn{\alpha} parameter (\eqn{\alpha > 1}).
#' @param beta Value of the \eqn{\beta} parameter (\eqn{\beta > 0}).
#' @param show.plot logical; if TRUE shows the plot of the distibution (default = FALSE).
#'
#' @return The cumulative probability of each value in vector \code{x}.
#'
#' @examples
#' pmoezipf(1:10, 2.5, 1.3)
#' pmoezipf(1:10, 2.5, 1.3, show.plot = TRUE)
#'
#' @seealso \code{\link{smoezipf}} for the survival probability function.
#' @export
pmoezipf <- function(x, alpha, beta, show.plot=F){
  srvvl <- sapply(x, .survival.default, alpha = alpha, beta = beta, simplify = T)
  cmltv <- 1 - srvvl
  if(show.plot){
    graphics::barplot(cmltv)
  }
  return(cmltv)
}

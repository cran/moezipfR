.dmoezipf.default <- function(x, alpha, beta){
  num <- beta * VGAM::zeta(alpha) * x^(-alpha)
  den <- (VGAM::zeta(alpha) - (1 - beta)*.zeta_x(alpha, x))*(VGAM::zeta(alpha) - (1 - beta) * .zeta_x(alpha, x + 1))
  values <- num/den

  return(values)
}

#' Density function.
#'
#' Density function for the Marshall-Olkin Extended Zipf distribution
#' with parameters \eqn{\alpha} and \eqn{\beta}.
#'
#' @details The probability mass function at x of the MOEZipf distribution with
#' parameters \eqn{\alpha} and \eqn{\beta} is computed as follows:
#'
#' \deqn{p(x | \alpha, \beta) = \frac{x^{-\alpha} \beta \zeta(\alpha) }{[\zeta(\alpha) - \bar{\beta} \zeta (\alpha, x)] [\zeta (\alpha) - \bar{\beta} \zeta (\alpha, x + 1)]}, \alpha > 1, \beta > 0, }
#'
#' where \eqn{\zeta(\alpha)} is the Riemann-zeta function at \eqn{\alpha}, \eqn{\zeta(\alpha, x)}
#' is the Hurtwitz zeta function with arguments \eqn{\alpha} and x, and \eqn{\bar{\beta} = 1 - \beta}.
#'
#' @param x Vector of positive integer values.
#' @param alpha Value of the \eqn{\alpha} parameter (\eqn{\alpha > 1} ).
#' @param beta Value of the \eqn{\beta} parameter (\eqn{\beta > 0} ).
#' @param show.plot logical; if TRUE shows the plot of the distibution (default = FALSE).
#' @return The probability associated to each value in vector \code{x}.
#' @examples
#' dmoezipf(1:10, 2.5, 1.3)
#' dmoezipf(1:10, 2.5, 1.3, show.plot = TRUE)
#' @export
dmoezipf <- function(x, alpha, beta, show.plot=F){
  values <- sapply(x, .dmoezipf.default, alpha = alpha, beta = beta)
  if(show.plot){
    graphics::barplot(values)
  }
  return(values)
}

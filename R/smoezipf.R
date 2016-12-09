.survival.default <- function(x, alpha, beta){
  num <- beta * (.zeta_x(alpha, x + 1))
  den <- (VGAM::zeta(alpha) - (1 - beta)*(.zeta_x(alpha, x + 1)))
  return(num/den)
}

#' Survival function.
#'
#' Survival function for the MOEZipf with parameters \eqn{\alpha}, \eqn{\beta}.
#'
#' @details The survival function at value x is computed as follows:
#' \deqn{S(x) = \frac{\beta \zeta(\alpha, x + 1)}{\zeta(\alpha) - \bar{\beta}\zeta(\alpha, x + 1)}}
#'
#' @param x Vector of positive integer values.
#' @param alpha Value of the \eqn{\alpha} parameter (\eqn{\alpha > 1}).
#' @param beta Value of the \eqn{\beta} parameter (\eqn{\beta > 0}).
#' @param show.plot logical; if TRUE shows the plot of the distibution (default = FALSE).
#' @return The survival probability value associated to each component in vector \code{x}.
#'
#' @examples
#' smoezipf(1:10, 2.5, 1.3)
#' smoezipf(1:10, 2.5, 1.3, show.plot = TRUE)
#' @export
smoezipf <- function(x, alpha, beta, show.plot=F){
  values <- sapply(x, .survival.default, alpha = alpha, beta = beta, simplify = T)
  if(show.plot){
    graphics::barplot(values)
  }
  return(values)
}

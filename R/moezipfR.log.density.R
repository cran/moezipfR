#' Log density function.
#'
#' Computes the log of the probability mass function of the MOEZipf distribution.
#'
#' @param x Vector of positive integer values.
#' @param alpha Value of the \eqn{\alpha} parameter (\eqn{\alpha > 1}).
#' @param beta Value of the \eqn{\beta} parameter (\eqn{\beta > 0}).
#' @param show.plot logical; if TRUE shows the plot of the distibution (default = FALSE).
#' @return The log of the probability associated to each value in vector \code{x}.
#' @examples
#' moezipfR.log.density(1:10, 2.5, 1.3)
#' moezipfR.log.density(1:10, 2.5, 1.3, show.plot = TRUE)
#' @export
moezipfR.log.density <- function(x, alpha, beta, show.plot=F){
  values <- sapply(x, .dmoezipf.default, alpha = alpha, beta = beta)
  values <- log(values)
  if(show.plot){
    graphics::barplot(values)
  }
  return(values)
}

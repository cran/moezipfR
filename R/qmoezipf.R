.qmoezipf.default <- function(x, beta){
  p <- (x*beta)/(1 + x*(beta-1))
  return(p)
}

#' Quantile function of the distribution.
#'
#' Computes the inverse cumulative function for a given vector of probabilities p.
#' It requires the \code{\link{qzipfman}} function implemented into the
#' \code{\link{tolerance}} package refered below.
#'
#' The calculus of the quantiles of a MOEZipf distribution for a given probability
#' vector \code{p}, is obtained by computing the quartiles of a Zipf distribution with
#' the same parameter \eqn{\alpha}, and probability vector equal to:
#'
#' \deqn{p\prime = \frac{p \beta}{1 + p(\beta - 1)}}
#'
#' @param p Vector of probabilities.
#' @param alpha Value of the \eqn{\alpha} parameter (\eqn{\alpha} > 1).
#' @param beta Value of the \eqn{\beta} parameter (\eqn{\beta} > 0).
#' @return Quantiles associated to a given probability vector \code{p}.
#' @references{ Young, D. S. (2010). \emph{Tolerance: an R package for estimating tolerance intervals}. Journal of Statistical Software, 36(5), 1-39.
#'  \url{https://core.ac.uk/download/pdf/6340270.pdf?repositoryId=153}}
#' @examples
#' qmoezipf(0.56, 2.5, 1.3)
#' @export
qmoezipf <- function(p, alpha, beta){
  if(!is.numeric(alpha) || !is.numeric(beta)){
    stop('Wrong values for the parameters.')
  }

  if(length(p) < 1){
    stop('Wrong values for the p parameter.')
  }

  if(length(which(p > 1 || p < 0 )) > 0){
    stop('There is a wrong value(s) in the p parameter.')
  }

  u <- sapply(p, .qmoezipf.default, beta = beta)
  data <- tolerance::qzipfman(u, s = alpha, b = NULL, N = Inf)
  return(data)
}

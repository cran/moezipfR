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
#' The quantiles of a MOEZipf distribution for a given probability
#' vector \code{p}, are obtained by computing the quantiles associated to a Zipf distribution with
#' the same parameter \eqn{\alpha}, and probability vector equal to:
#'
#' \deqn{p\prime = \frac{p\,\beta}{1 + p\,(\beta - 1)}}
#'
#' @param p Vector of probabilities.
#' @param alpha Value of the \eqn{\alpha} parameter (\eqn{\alpha} > 1).
#' @param beta Value of the \eqn{\beta} parameter (\eqn{\beta} > 0).
#' @param log.p Logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail Logical; if TRUE (default), probabilities are \eqn{P[X \leq x]}, otherwise, \eqn{P[X > x]}.
#' @return Quantiles associated to a given probability vector \code{p}.
#' @references{ Young, D. S. (2010). \emph{Tolerance: an R package for estimating tolerance intervals}. Journal of Statistical Software, 36(5), 1-39.}
#' @examples
#' qmoezipf(0.56, 2.5, 1.3)
#' @export
qmoezipf <- function(p, alpha, beta, log.p = FALSE, lower.tail = TRUE){
  if(!is.numeric(alpha) || !is.numeric(beta)){
    stop('Wrong values for the parameters.')
  }

  if(length(p) < 1){
    stop('Wrong values for the p parameter.')
  }

  if(log.p && lower.tail){
    p <- exp(p)
  } else{
    if(log.p && !lower.tail){
      p <- 1-exp(p)
    } else{
      if(!log.p && !lower.tail){
        p <- 1-p
      }
    }
  }

  if(length(which(p > 1 || p < 0 )) > 0){
    stop('There is a wrong value(s) in the p parameter.')
  }

  u <- sapply(p, .qmoezipf.default, beta = beta)
  data <- tolerance::qzipfman(u, s = alpha, b = NULL, N = Inf)
  return(data)
}

#' Random number generator.
#'
#' Generates random numbers from a MOEZipf distribution with
#' fixed parameters \eqn{\alpha} and \eqn{\beta}.
#' @param n Number of random numbers to return.
#' @param alpha Value of the \eqn{\alpha} parameter (\eqn{\alpha > 1}).
#' @param beta Value of the \eqn{\beta} parameter (\eqn{\beta > 0}).
#' @return Vector containing  the n generated numbers.
#' @references{ Young, D. S. (2010). \emph{Tolerance: an R package for estimating tolerance intervals}. Journal of Statistical Software, 36(5), 1-39.
#'  \url{https://core.ac.uk/download/pdf/6340270.pdf?repositoryId=153}}
#' @examples
#' rmoezipf(10, 2.5, 1.3)
#' @export
rmoezipf <- function(n, alpha, beta){
  if(!is.numeric(alpha) || !is.numeric(beta)){
    stop('Wrong values for the parameters.')
  }

  uValues <- stats::runif(n, 0, 1)
  data <- qmoezipf(uValues, alpha, beta)
  lvl <- names(sort(table(data), decreasing = TRUE))
  uniqVal <- 1:length(unique(data))
  data <- uniqVal[match(data, lvl)]
  return(data)
}

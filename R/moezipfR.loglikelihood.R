 #' Log-likelihood.
#'
#' Computes the value of the log-likelihood function for a given data set and parameter values.
#' @param data Matrix of count data.
#' @param alpha Value of the \eqn{\alpha} parameter \eqn{(\alpha > 1)}.
#' @param beta Value of the \eqn{\beta} parameter \eqn{(\beta > 0)}.
#' @return The log-likelihood value.
#' @details The argument \code{data} is a matrix where, for each row, the first column corresponds to a count,
#' and the second column contains its corresponding frequency.
#'
#' The log-likelihood function is computed by means of the following equation:
#'
#' \deqn{l(\alpha, \beta; x) = -\alpha \sum_{i = 1} ^m f_{a}(x_{i}) log(x_{i}) + N (log(\beta) + \log(\zeta(\alpha)))}
#' \deqn{- \sum_{i = 1} ^m f_a(x_i) log[(\zeta(\alpha) - \bar{\beta}\zeta(\alpha, x_i)(\zeta(\alpha) - \bar{\beta}\zeta(\alpha, x_i + 1)))], }
#' where \eqn{N} is the sample size \eqn{N = \sum_{i = 1} ^m x_i f_a(x_i)}, \eqn{m} is the number of different values \eqn{x_{i}} in the sample,
#' and \eqn{f_{a}(x_i)} is the absolute frequency of \eqn{x_i}.
#'
#' @examples
#' data <- rmoezipf(100, 2.5, 1.3)
#' data <- moezipfR.utils.getDataMatrix(data)
#' moezipfR.loglikelihood(data, 2.5, 1.3)
#' @export
moezipfR.loglikelihood <- function(data, alpha, beta){
  .mloglikelihood(c(alpha, beta), sum(data[,2]), data[, 2], data[, 1])
}

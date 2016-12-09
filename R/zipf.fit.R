#' Zipf parameter estimation.
#'
#' Estimates the parameter of the Zipf distribution by means of the maximum likelihood
#' method, for a given count data set.
#' @param data Matrix of count data.
#' @param init_alpha Initial value for the \eqn{\alpha} parameter (\eqn{\alpha > 1}).
#' @param show.plot logical; if TRUE shows the plot of the fitting (default = FALSE).
#' @param ... Further arguments to pass to \link{optim}.
#' @return Returns an object that contains:
#'  \item{alpha}{Estimated value of the \eqn{\alpha} parameter.}
#'  \item{mloglik}{Value of the maximum log-likelihood.}
#'  \item{fitted.values}{Estimated counts at given levels.}
#' @details
#' The argument \code{data} is a matrix where the first column corresponds to the count,
#' and the second column contains its corresponding.
#'
#' The log-likelihood function is computed by means of the following equation:
#' \deqn{l(\alpha; x) = -\alpha \sum_x ^{m} log(x) f_{a}(x_i) - N log(\zeta(\alpha))}
#'
#' where \eqn{N} is the sample size (\eqn{N = \sum_{i = 1} ^m x_i f_a(x_i)}), \eqn{m} the different values \eqn{x_{i}}, each one of them with absolute frequency equal to \eqn{f_{a}(x_i)}.
#'
#' The function \emph{\link{optim}} is used to estimate the parameters.
#' @examples
#' data <- rmoezipf(1000, 2.5, 1 )
#' data <- moezipfR.utils.getDataMatrix(data)
#' zipf.fit(data, 1.0001)
#' @export
zipf.fit <- function(data, init_alpha, show.plot = FALSE, ...){
  if(!is.numeric(init_alpha)){
    stop('Wrong intial value for the alpha parameter.')
  }

  estResults <- .paramEstimationBase(data, c(init_alpha), .zeta_Distribution, ...)
  zipf.fitted <- estResults$stats$nSize*sapply(estResults$stats$values, .zipf_pmf, alpha = as.numeric(estResults$results$par[1]))

  if(show.plot){
    graphics::plot(estResults$stats$values, estResults$stats$frequencies, log="xy",
                   xlab="Observation", ylab="Frequency",
                   main="Fitting Zipf Distribution")

    graphics::lines(estResults$stats$values, zipf.fitted, col="red")

    graphics::legend("topright",  legend = c('Observations', 'Zipf Distribution'),
           col=c('black', 'red'),  pch=c(21,NA),
           lty=c(NA, 1), lwd=c(NA, 2))
  }

  return(list(alpha = estResults$results$par[1],
              mlogLik = estResults$results$value,
              fitted.values = zipf.fitted))
}

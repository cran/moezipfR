#' MOEZipf parameters estimation.
#'
#' For a given count data set, estimates the parameters of the MOEZipf distribution by means of the maximum likelihood
#' method.
#' @param data Matrix of count data.
#' @param init_alpha Initial value of \eqn{\alpha} parameter (\eqn{\alpha > 1}).
#' @param init_beta Initial value of \eqn{\beta} parameter (\eqn{\beta > 0}).
#' @param show.plot logical; if TRUE shows the plot of the fitting. (default = FALSE)
#' @param ... Further arguments to pass to \link{optim}.
#' @return Returns an object containing four fields:
#'  \item{alpha}{Estimate for the \eqn{\alpha} parameter.}
#'  \item{beta}{Estimate for the \eqn{\beta} parameter.}
#'  \item{mloglik}{Value of the maximum log-likelihood.}
#'  \item{fitted.values}{Estimated counts at a given level.}
#' @details
#' The argument \code{data} is a matrix where the first column corresponds to the count,
#' and the second column contains its corresponding frequency.
#'
#' The log-likelihood function is computed by means of the following equation:
#'
#' \deqn{l(\alpha, \beta; x) = -\alpha \sum_{i = 1} ^m f_{a}(x_{i}) log(x_{i}) + N (log(\beta) + \log(\zeta(\alpha)))}
#' \deqn{ - \sum_{i = 1} ^m f_a(x_i) log[(\zeta(\alpha) - \bar{\beta}\zeta(\alpha, x_i)(\zeta(\alpha) - \bar{\beta}\zeta(\alpha, x_i + 1)))], }
#' where \eqn{N} is the sample size (\eqn{N = \sum_{i = 1} ^m x_i f_a(x_i)}), \eqn{m} the different values \eqn{x_{i}}, each one of them with absolute frequency equal to \eqn{f_{a}(x_i)}.
#'
#' The function \emph{\link{optim}} is used to estimate the parameters.
#'
#' Based on the paper refered below, the authors suggest on one side to use \eqn{\alpha = log_2(\frac{f_1}{f_2})},
#' where \eqn{f_1} and \eqn{f_2} are the absolute frequency associated to the two first counts in the \code{data} matrix.
#' Additionally, is suggested to use \eqn{\beta = 1} because it corresponds to the Zipf distribution.
#' To obtain more accurate initial values is suggested to fit the data by means of the Zipf distribution and to use
#' the mle of the Zipf as initial value for the \eqn{\alpha} parameter when fitting a MOEZipf.
#'
#' @examples
#' data <- rmoezipf(1000, 2.5, 1.3)
#' data <- moezipfR.utils.getDataMatrix(data)
#' moezipfR.fit(data, 1.001, 0.001)
#' @seealso \code{\link{zipf.fit}} for fitting the data by the Zipf distribution.
#' @references{ Güney, Y., Tuaç, Y., & Arslan, O. (2016). Marshall–Olkin distribution: parameter estimation and
#' application to cancer data. Journal of Applied Statistics, 1-13.}
#' @export
moezipfR.fit <- function(data, init_alpha, init_beta, show.plot = FALSE, ...){
  if(!is.numeric(init_alpha) || !is.numeric(init_beta)){
    stop('Wrong intial values for the parameters.')
  }

  estResults <- .paramEstimationBase(data, c(init_alpha, init_beta), .mloglikelihood, ...)
  estAlpha <- as.numeric(estResults$results$par[1])
  estBeta <- as.numeric(estResults$results$par[2])

  moezipf.fitted <- estResults$stats$nSize*sapply(estResults$stats$values, dmoezipf,
                                               alpha = estAlpha, beta = estBeta)

  if(show.plot && !is.null(estAlpha) && !is.null(estBeta)){

    graphics::plot(estResults$stats$values, estResults$stats$frequencies, log="xy",
                   xlab="Observation", ylab="Frequency",
                   main="Fitting MOEZipf Distribution")

    graphics::lines(estResults$stats$values, moezipf.fitted, col="blue")

    graphics::legend("topright",  legend = c('Observations', 'MOEZipf Distribution'),
           col=c('black', 'blue'), pch=c(21,NA),
           lty=c(NA, 1), lwd=c(NA, 2))
  }
  return(list(alpha = estAlpha, beta = estBeta, mlogLik = estResults$results$value,
              fitted.values = moezipf.fitted))
}







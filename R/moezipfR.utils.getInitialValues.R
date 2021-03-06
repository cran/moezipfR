#' Calculates initial values for the \eqn{\alpha} and \eqn{\beta} parameters.
#'
#' The initial value of the parameters are computed using the empirical absolute frequencies of values
#' one and two. The selection of robust initial values allows to reduce the number
#' of iterations which in turn, reduces the computation time.
#' In the case where one of the two first positive integer values does not appear in the data
#' set, the default values are set equal to \eqn{\alpha} = 1.0001 and \eqn{\beta} = 0.0001.
#'
#' @param data Matrix of count data.
#'
#' @details
#'
#' The argument \code{data} is a matrix where, for each row, the first column corresponds to a count,
#' and the second column contains its corresponding frequency.
#'
#' To obtain the initial value for \eqn{\alpha} and \eqn{\beta}, one will assume that
#' the data come from a Zipf(\eqn{\alpha}) distribution. Thus, the initial value for \eqn{\beta}
#' is set equal to one, and the inital value for \eqn{\alpha}, denoted by \eqn{\alpha_0}, is obtained
#' equating the ratio of the theoretical probabilities at one and two to the corresponding emprirical
#' ratio. Thus,
#'
#' \deqn{\alpha_0 = log_2 \big (\frac{f_1}{f_2} \big)}
#' where \eqn{f_1} and \eqn{f_2} are the absolute frequencies of one and two in the sample.
#' @return Returns the initial value for parameters \eqn{\alpha} and \eqn{\beta}.
#' @examples
#' data <- rmoezipf(100, 2.5, 1.3)
#' data <- moezipfR.utils.getDataMatrix(data)
#' initials <- moezipfR.utils.getInitialValues(data)
#' @seealso \code{\link{moezipfR.utils.getDataMatrix}}
#' @references{ Güney, Y., Tuaç, Y., & Arslan, O. (2016). Marshall–Olkin distribution: parameter estimation and
#' application to cancer data. Journal of Applied Statistics, 1-13.}
#' @export
moezipfR.utils.getInitialValues <- function(data){
  freq1 <- data[which(data[,1] == 1),][2]
  freq2 <- data[which(data[,1] == 2),][2]
  if(is.na(freq1) || is.na(freq2)){
    return(list(init_alpha = 1.0001, init_beta = 0.0001))
  }
  alpha0 <- max(log2(freq1/freq2), 1.0001, na.rm = TRUE)
  # K <- freq2/freq1
  # beta0 <- (-(K * (2^(alpha0) + 1)))/(VGAM::zeta(alpha0)*((2^(alpha0) * K) - 1) - K*(2^(alpha0) + 1))
  # beta0 <- max(beta0, 0.0001, na.rm = TRUE)
  return(list(init_alpha = round(alpha0, 4), init_beta = 1))
}

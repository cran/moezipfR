.QValue <- function(alpha, beta, x){
  log(VGAM::zeta(alpha) - (1-beta)*.zeta_x(alpha, x))
}

.mloglikelihood <- function(param, nSize, freq, values){
  a <- param[1]
  b <- param[2]
  -(nSize*log(b) + nSize * log(VGAM::zeta(a)) - a * sum(freq * log(values))
    - sum(freq * sapply(values, .QValue, alpha = a, beta = b))
    - sum(freq * sapply(values + 1, .QValue, alpha = a, beta = b)))
}

.zipf_pmf <- function(k, alpha){
  (k^(-alpha))/VGAM::zeta(alpha)
}

.zeta_Distribution <- function(alpha, nSize, freq, values){
  -( -alpha*sum(freq * log(values)) - nSize*log(VGAM::zeta(alpha)))
}

.getSpectrumValues <- function(data){
  frequencies <- as.numeric(data[, 2])
  values <- as.numeric(data[, 1])
  nSize <- sum(frequencies)
  return(list(values = values, frequencies = frequencies,
              nSize = nSize))
}

.paramEstimationBase <- function(x, initValues, likelihoodFunc, ...){
  result <- NULL

  tryCatch({
    # statistcs <- .getSpectrumValues(x)
    statistics <- list(nSize = sum(x[, 2]), values = x[, 1], frequencies = x[, 2])
    result <- stats::optim(par = initValues, likelihoodFunc,
                           nSize = statistics$nSize, freq = statistics$frequencies,
                           values = statistics$values, ...)

    return(list(results = result, stats=statistics))
  },
  error = function(e) {
    print(e$message)
  })
}

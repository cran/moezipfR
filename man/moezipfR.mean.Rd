% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moezipfR.mean.R
\name{moezipfR.mean}
\alias{moezipfR.mean}
\title{Expected value.}
\usage{
moezipfR.mean(alpha, beta, tolerance = 10^(-4))
}
\arguments{
\item{alpha}{Value of the \eqn{\alpha} parameter (\eqn{\alpha > 2}).}

\item{beta}{Value of the \eqn{\beta} parameter (\eqn{\beta > 0}).}

\item{tolerance}{Tolerance used in the calculations (default = \eqn{10^{-4}}).}
}
\value{
A positive real value corresponding to the mean value of the distribution.
}
\description{
Computes the expected value of the MOEZipf distribution for given values of parameters
\eqn{\alpha} and \eqn{\beta}.
}
\details{
The expected value of the MOEZipf distribution only exists for \eqn{\alpha} values strictly greater than 2.
In this case, if Y is a random variable that follows a MOEZipf distribution with parameters \eqn{\alpha}
and \eqn{\beta}, the expected value is computed as:
\deqn{E(Y) = \sum_{x = 1} ^\infty \frac{\beta \zeta(\alpha) x^{-\alpha + 1}}{[\zeta(\alpha) - \bar{\beta}\zeta(\alpha, x)][\zeta(\alpha) - \bar{\beta}\zeta(\alpha, x + 1)]}\,, \alpha > 2\,,  \beta > 0}

The mean is computed calculating the partial sums of the serie, and it stops when two
consecutive partial sums differs less than the \code{tolerance} value.
The last partial sum is returned.
}
\examples{
moezipfR.mean(2.5, 1.3)
moezipfR.mean(2.5, 1.3, 10^(-3))
}


\name{moore.test}
\alias{moore.test}
\title{Moore's Uniformity Test}
\description{
Performs the Moore's test of circular uniformity for vector data or weighted angular data.
}
\arguments{
  \item{x}{vector of angles}
  \item{w}{vector of lengths or weights}
  \item{p.value}{method of calculating the p-value. \code{asymptotic} calculates the p-value using the formula in Moore (1980). With \code{simulated}, the p-value is simulated with 9,999 repetitions.}
}
\value{An \code{"htest"} object which contains:
  \item{statistic}{the value of the chi-square statistic.}
  \item{p.value}{the p-value of the test.}
  \item{parameter}{degrees of freedom. 2 if the p-value is asymptotic, NA if the p-value is simulated.}
  \item{method}{a character string indicating what type of test was
    performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
  }
\examples{
library(circular)
x <- rcircularuniform(100)
w <- runif(100, 0.3, 1)
moore.test(x = x, w = w)

x <- sort(rcircularuniform(100))
w <- sort(runif(100, 0.3, 1))
moore.test(x = x, w = w)
}
\references{
Moore, B. R. (1980). A Modification of the Rayleigh Test for Vector Data. Biometrika, 67(1), 175–180.
}

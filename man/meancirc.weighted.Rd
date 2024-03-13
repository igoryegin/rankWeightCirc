\name{meancirc.weighted}
\alias{meancirc.weighted}
\title{
Mean circular direction for rank-weighted data
}
\description{
This function calculates mean direction for angular data with rank weights, i.e. weights that take integer values from 1 to N (N = sample size)
}
\usage{
meancirc.weighted(theta, w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
a vector of angular observations or an object of class \emph{circular}
}
  \item{w}{
a numeric vector of weights
}
}
\details{
Weighted angular mean is formally defined as \eqn{\mu_{w}=atan2({b_{w}, a_{w}})}, where \eqn{atan2} is the angle between a circular observation and the x-axis.
}
\value{
Returns the weighted mean direction of the data
}
\references{
}
\author{
Igor Yegin (MSc Social Statistics and Social Research)
}
\note{
}
\seealso{
\code{\link{trig.moment.weighted}}
}
\examples{
library(circular)
VM <- rvonmises(300, circular(0), 1)
weights <- sample(1:300)
mean.circular(VM)
meancirc.weighted(VM, weights)
}

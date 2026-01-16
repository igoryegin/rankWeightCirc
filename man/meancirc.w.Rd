\name{meancirc.w}
\alias{meancirc.w}
\title{
Mean circular direction for rank-weighted data
}
\description{
This function calculates the average direction for angular data with rank weights, i.e. weights that take integer values from 1 to N (N = sample size)
}
\usage{
meancirc.w(theta, w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
a vector of angular observations or an object of class \emph{circular}
}
  \item{w}{
a numeric vector of weights; these are automatically converted into ranks
}
}
\details{
Weighted angular mean is formally defined as \deqn{\overline{\theta^{(w)}}=\text{atan2}\left({b_1^{(w)}, a_1^{(w)}}\right)}
where \eqn{\text{atan2}} is the two-valued arctangent function which returns the angle between a circular observation and the x-axis.
}
\value{
Returns the weighted mean direction of the data
}
\references{
}
\author{
Igor Yegin
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
meancirc.w(VM, weights)
}

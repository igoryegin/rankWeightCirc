\name{rho.w}
\alias{rho.w}
\title{
Mean resultant vector length (circular rho) for rank-weighted data
}
\description{
This function calculates mean resultant vector length in the presence of rank weights, i.e. weights that take integer values from 1 to N (N = sample size)
}
\usage{
rho.w(theta, w)
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
Weighted rho is formally defined as \deqn{\rho_{w}=\sqrt{(a^{(w)})^2+(b^{(w)})^2}}
}
\value{
Returns the weighted circular rho of a vector
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
rho.circular(VM)
rho.w(VM, weights)
}

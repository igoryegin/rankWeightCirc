\name{rho.weighted}
\alias{rho.weighted}
\title{
Mean resultant vector length (circular rho) for rank-weighted data
}
\description{
This function calculates mean resultant vector length in the presence of rank weights, i.e. weights that take integer values from 1 to N (N = sample size)
}
\usage{
rho.weighted(theta, w)
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
Weighted rho is formally defined as \eqn{\rho_{w}=\sqrt{a_{w}^2+b_{w}^2}}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Igor Yegin (MSc Social Statistics and Social Research)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{trig.moment.weighted}}
}
\examples{
library(circular)
VM <- rvonmises(300, circular(0), 1)
weights <- sample(1:300)
rho.circular(VM)
rho.weighted(VM, weights)
}

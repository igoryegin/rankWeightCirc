\name{biascor.CI}
\alias{biascor.CI}
\title{
Bias-Corrected Confidence Intervals For Some Rank-Weighted Summary Statistics
}
\description{
Normally, the point estimates of mean direction and mean resultant length are biased.
The bias term increases substantially if the population counterpart is
close to zero (i.e., if the angles are scattered around the circle quasi-uniformly), so failure to account for the bias may lead to inaccure confidence interval bands.

This function computes an approximately unbiased confidence interval for a chosen circular summary statistic.
}
\usage{
biascor.CI(stat = c("mu", "rho"), theta, w, CI.level)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stat}{
Summary statistic for which a CI is produced.
Can be either rank-weighted mean angular direction ("mu") or rank-weighted mean resultant vector length ("rho").
Other values are not allowed and will result in an error.
}
  \item{theta}{
a vector of angular observations or an object of class \emph{circular}
}
  \item{w}{
a numeric vector of weights; these are automatically converted into ranks
}
  \item{CI.level}{
confidence level; a number between 0 and 1
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(circular)
library(rankWeightCirc)
set.seed(2025)
theta <- rcardioid(60, mu = circular(pi/2), rho = 0.2)
w <- rexp(60)
biascor.CI(theta = theta, w = w, CI.level = 0.9)
biascor.CI(theta = theta, w = w, CI.level = 0.95)
biascor.CI(theta = theta, w = w, CI.level = 0.99)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

\name{biascor.CI}
\alias{biascor.CI}
\title{
Bias-Corrected Confidence Intervals For Some Rank-Weighted Summary Statistics
}
\description{
Normally, the point estimates of mean direction and mean resultant length are biased.
The bias term increases substantially if the population counterpart is
close to zero (i.e., if the angles are scattered around the circle almost uniformly), so failure to account for the bias may lead to inaccure confidence interval bands.

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
Pewsey (2004) derives the asymptotic distribution of four summary statistics including their respective bias terms.
He then demonstrates how subtracting the bias component yields a 'better' estimate of a statistic.

Pewsey's results were adapted for rank-weighted statistics. In particular, the weighted mean direction is approximately normally distributed:
\deqn{\overline{\theta^{(w)}} \sim N \left(\mu - M \frac{\overline{\beta_2}}{2n\rho^2}, \frac{M}{2n} \frac{1 - \overline{\alpha_2}}{\rho^2} \right)}

The weighted mean resultant length is also aprroximately normal:
\deqn{\overline{R^{(w)}} \sim N \left(\rho + M \frac{1 - \overline{\alpha_2}}{4n\rho}, \frac{M}{2n} (1 - 2 \rho^2 + \overline{\alpha_2}) \right)}

\eqn{M = \frac{4n + 2}{3n + 2}} is the inflation factor due to the presence of rank weights.

Both quantities are estimated by plugging in corresponding rank-weighted sample statistics for \eqn{\mu, \rho, \overline{\alpha_2}, \overline{\beta_2}}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Pewsey, A. (2004) The large-sample joint distribution of key circular statistics. \emph{Metrika}, 60(1).
}
\author{
Igor Yegin
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

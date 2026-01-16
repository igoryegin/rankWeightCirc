\name{trig.moment.w}
\alias{trig.moment.w}
\title{
Trigonometric moments for rank-weighted circular data
}
\description{
This function computes trigonometric (cosine and sine) moments in the presence of weights. Each of the N weights is transformed into its respective rank, i.e., an integer from 1 to N.
}
\usage{
trig.moment.w(theta, w, p = 1, central = FALSE)
}
\arguments{
  \item{theta}{
a vector of angular observations, or an object of class \emph{circular}
}
  \item{w}{
a numeric vector of weights
}
  \item{p}{
a number specifying moment order
}
  \item{central}{
a boolean variable (TRUE/FALSE); if TRUE, central moments are computed, otherwise raw moments are computed
}
}
\details{
A \eqn{p}-th trigonometric moment is defined as an average of complex random observations with unit radius: \eqn{t_{p,0}=\frac{1}{n}\sum_{j=1}^{n}{e^{ip\theta_{j}}}}. In a similar fashion, we define a weighted trigonometric moment as weighted arithmetic mean (notice that weights are equivalent to radii of complex numbers in this formulation): \eqn{t_{p,0}^{w}=\frac{\sum_{j=1}^{n}{w_{j}e^{ip\theta_{j}}}}{\sum_{j=1}^{n}{w_{j}}}=\frac{\sum_{j=1}^{n}{w_{j}(\cos{p\theta_{j}}+i\sin{p\theta_{j}})}}{\sum_{j=1}^{n}{w_{j}}}}.
From these formulae, it is easy to derive \eqn{a_{p,0}^{w}=\frac{\sum_{j=1}^{n}{w_{j}\cos{p\theta_{i}}}}{\sum_{j=1}^{n}{w_{j}}}} and \eqn{b_{p,0}^{w}=\frac{\sum_{j=1}^{n}{w_{j}\sin{p\theta_{j}}}}{\sum_{j=1}^{n}{w_{j}}}}, which stand for \eqn{p}-th cosine and \eqn{p}-th sine moment, respectively.

Furthermore, if we define weighted mean direction as \eqn{\bar{\theta}_{w}=atan2(b_{p}^{w}, a_{p}^{w})}, then \eqn{\bar{a}_{p,0}^{w}=\frac{\sum_{j=1}^{n}{w_{j}\cos{p(\theta_{i}-\bar{\theta}_{w})}}}{\sum_{j=1}^{n}{w_{j}}}} and \eqn{\bar{b}_{p,0}^{w}=\frac{\sum_{j=1}^{n}{w_{j}\sin{p(\theta_{j}-\bar{\theta}_{w})}}}{\sum_{j=1}^{n}{w_{j}}}}

This function transforms weights into rank-weights (\eqn{w_{i}=1,...,n}); this ensures that the distribution of radii does not influence circular summary statistics.
}
\value{
Returns a list with two elements: \code{C} (cosine moment), and \code{S} (sine moment)
}
\references{
Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular Statistics in R. OUP Oxford
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

CU <- rcircularuniform(150)
W <- rexp(150, 2)
trig.moment.w(CU, W, p = 1, central = FALSE)

VM <- rvonmises(150, mu = circular(pi/2), kappa = 2/3)
W <- runif(150)

trig.moment.w(CU, W, p = 1, central = TRUE)
trig.moment.w(CU, W, p = 2, central = TRUE)

VM <- sort(VM)
W <- sort(W)
trig.moment.w(CU, W, p = 2, central = TRUE)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

\name{meanplot}
\alias{meanplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rank-weighted mean direction diagram
}
\description{
Outputs a circular scatter diagram with an arrow indicating debiased mean direction and (optionally)
an asymptotic confidence band around the circular mean.
}
\usage{
meanplot(theta, w, CI.level, arrow.col = "black", ci.band = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
%%     ~~Describe \code{theta} here~~
}
  \item{w}{
%%     ~~Describe \code{w} here~~
}
  \item{CI.level}{
%%     ~~Describe \code{CI.level} here~~
}
  \item{arrow.col}{
%%     ~~Describe \code{arrow.col} here~~
}
  \item{ci.band}{
%%     ~~Describe \code{ci.band} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (theta, w, CI.level, arrow.col = "black", ci.band = TRUE,
    ...)
{
    colarg <- rank(w)/max(rank(w))
    rhoW <- biascor.CI("rho", theta, w, CI.level)
    muW <- biascor.CI("mu", theta, w, CI.level)
    plot(cos(theta), sin(theta), pch = 16, axes = FALSE, col = rgb(0,
        0, 0, alpha = colarg), xlim = c(-1, 1), ylim = c(-1,
        1), xlab = "", ylab = "", ...)
    abline(h = 0)
    abline(v = 0)
    arrows.circular(muW$mu, pch = 4, lwd = 1.5, angle = 10, col = arrow.col)
    if (ci.band) {
        segments(x0 = 0, y0 = 0, x1 = cos(muW$lower), y1 = sin(muW$lower),
            lty = 2, col = arrow.col)
        segments(x0 = 0, y0 = 0, x1 = cos(muW$upper), y1 = sin(muW$upper),
            lty = 2, col = arrow.col)
    }
  }
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

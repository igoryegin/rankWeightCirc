###########################################################
#                                                         #
# BIAS-CORRECTED CONFIDENCE INTERVAL FOR                  #
# WEIGHTED CIRCULAR MEAN OR                               #
# WEIGHTED MEAN RESULTANT VECTOR LENGTH                   #
#                                                         #
# Author: Igor Yegin                                      #
#                                                         #
###########################################################

#' @export
#' @importFrom stats qnorm pchisq
biascor.CI <- function(stat = c("mu", "rho"), theta, w, CI.level = 0.95) {
  if(!is.numeric(w))
    stop("'w' must be numeric")
  if(CI.level <= 0 | CI.level >= 1)
    stop("'CI.level' must be in (0, 1)")
  stat <- match.arg(stat)
  w <- rank(w, ties.method = "first")
  n <- length(theta)
  a <- sum(w * cos(theta)) / sum(w)
  a2 <- sum(w * cos(2 * theta)) / sum(w)
  b <- sum(w * sin(theta)) / sum(w)
  b2 <- sum(w * sin(2 * theta)) / sum(w)
  a2bar <- sum(w * cos(2 * (theta - atan2(b, a)))) / sum(w)
  b2bar <- sum(w * sin(2 * (theta - atan2(b, a)))) / sum(w)
  mu <- atan2(b, a)
  rhobar <- sqrt(a^2 + b^2)
  if(stat == "rho") {
    rhocorr <- rhobar - (4 * n + 2) / (3 * n + 3) * (1 - a2bar) / (4 * n * rhobar)
    rhosd <- sqrt((2 * n + 1) / (3 * n * (n + 1)) * (1 - 2 * rhobar ^ 2 + a2bar))
    list(rho = rhocorr, stdev = rhosd,
         lower = rhocorr - qnorm(1/2 + CI.level/2) * rhosd,
         upper = rhocorr + qnorm(1/2 + CI.level/2) * rhosd)
  }
  else if(stat == "mu") {
    mucorr <- mu + (4 * n + 2) / (3 * n + 3) * b2bar / (2 * n * rhobar ^ 2)
    musd <- sqrt((2 * n + 1) / (3 * n * (n + 1)) * (1 - a2bar) / rhobar ^ 2)
    list(mu = mucorr, stdev = musd,
         lower = mucorr - qnorm(1/2 + CI.level/2) * musd,
         upper = mucorr + qnorm(1/2 + CI.level/2) * musd)
  }
  else
    stop("Incorrect argument name. 'stat' must be either 'mu' or 'rho'")
}

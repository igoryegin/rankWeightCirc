###########################################################
#                                                         #
# BIAS-CORRECTED CONFIDENCE INTERVAL FOR                  #
# WEIGHTED CIRCULAR MEAN OR                               #
# WEIGHTED MEAN RESULTANT VECTOR LENGTH                   #
#                                                         #
# Author: Igor Yegin                                      #
#                                                         #
###########################################################

biascor.CI <- function(stat = c("mu", "rho"), theta, w, alpha) {
  stat <- match.arg(stat)
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
    rhocorr <- rhobar - (2 * n + 1) / (3 * n + 3) * (1 - a2bar) / (2 * n * rhobar)
    rhosd <- qnorm(1 - alpha/2) * sqrt((2 * n + 1) / (3 * n * (n + 1)) * (1 - 2 * rhobar ^ 2 + a2bar))
    c("Rho.corrected" = rhocorr, "Lower" = rhocorr - rhosd, "Upper" = rhocorr + rhosd)
  }
  else if(stat == "mu") {
    mucorr <- mu - (2 * n + 1) / (3 * n * (n + 1)) * b2bar / rhobar ^ 2
    musd <- qnorm(1 - alpha/2) * sqrt((2 * n + 1) / (3 * n * (n + 1)) * (1 - a2bar) / rhobar ^ 2)
    c("Mu.corrected" = mucorr, "Lower" = mucorr - musd, "Upper" = mucorr + musd)
  }
  else
    stop("Incorrect argument name. 'stat' must be either 'mu' or 'rho'")
}

###################################################################
#                                                                 #
# MEAN RESULTANT VECTOR LENGTH FOR CIRCULAR OBSERVATIONS          #
# WITH RANK WEIGHTS                                               #
#                                                                 #
# Author: Igor Yegin                                              #
#                                                                 #
###################################################################

rho.weighted <- function(theta, w) {
  w <- rank(w)
  a <- sum(w * cos(theta)) / sum(w)
  b <- sum(w * sin(theta)) / sum(w)
  sqrt(a^2 + b^2)
}

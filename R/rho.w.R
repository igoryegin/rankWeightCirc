###################################################################
#                                                                 #
# MEAN RESULTANT VECTOR LENGTH FOR CIRCULAR OBSERVATIONS          #
# WITH RANK WEIGHTS                                               #
#                                                                 #
# Author: Igor Yegin                                              #
#                                                                 #
###################################################################

rho.w <- function(theta, w) {
  w <- rank(w, ties.method = "first")
  a <- sum(w * cos(theta)) / sum(w)
  b <- sum(w * sin(theta)) / sum(w)
  sqrt(a^2 + b^2)
}

###################################################################
#                                                                 #
# MEAN DIRECTION FOR CIRCULAR OBSERVATIONS                        #
# WITH RANK WEIGHTS                                               #
#                                                                 #
# Author: Igor Yegin                                              #
#                                                                 #
###################################################################

mu.weighted <- function(theta, w) {
  w <- rank(w)
  a <- sum(w * cos(theta)) / sum(w)
  b <- sum(w * sin(theta)) / sum(w)
  atan2(b, a)
}

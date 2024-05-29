###################################################################
#                                                                 #
# MEAN DIRECTION FOR CIRCULAR OBSERVATIONS                        #
# WITH RANK WEIGHTS                                               #
#                                                                 #
# Author: Igor Yegin                                              #
#                                                                 #
###################################################################

meancirc.weighted <- function(theta, w) {
  w <- rank(w, ties.method = "first")
  a <- sum(w * cos(theta)) / sum(w)
  b <- sum(w * sin(theta)) / sum(w)
  atan2(b, a)
}

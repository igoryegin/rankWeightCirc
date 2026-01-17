###################################################################
#                                                                 #
# TRIGONOMETRIC MOMENTS (COSINE AND SINE)                         #
# FOR RANK-WEIGHTED CIRCULAR OBSERVATIONS                         #
#                                                                 #
# Author: Igor Yegin                                              #
#                                                                 #
###################################################################

#' @export
trig.moment.weighted <- function(theta, w, p = 1, central = FALSE) {
  if(central) {
    a <- sum(w * cos(theta)) / sum(w)
    b <- sum(w * sin(theta)) / sum(w)
    list(C = sum(w * cos(p * (theta - atan2(b, a)))) / sum(w),
         S = sum(w * sin(p * (theta - atan2(b, a)))) / sum(w))
  }
  else {
    list(C = sum(w * cos(p * theta)) / sum(w), S = sum(w * sin(p * theta)) / sum(w))
  }
}

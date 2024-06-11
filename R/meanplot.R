###########################################################
#                                                         #
# VISUALISING RANK-WEIGHTED CIRCULAR DATA                 #
#                                                         #
# Author: Igor Yegin                                      #
#                                                         #
###########################################################

meanplot <- function(theta, w, CI.level, arrow.col = "black", ci.band = TRUE, ...) {
  colarg <- rank(w) / max(rank(w))
  rhoW <- biascor.CI("rho", theta, w, CI.level)
  muW <- biascor.CI("mu", theta, w, CI.level)
  plot(cos(theta), sin(theta), pch = 16, axes = FALSE,
       col = rgb(0, 0, 0, alpha = colarg), xlim = c(-1, 1), ylim = c(-1, 1),
       xlab = "", ylab = "", ...)
  abline(h = 0)
  abline(v = 0)
  arrows.circular(muW$mu, pch = 4, lwd = 1.5, angle = 10, col = arrow.col)
  if(ci.band) {
    segments(x0 = 0, y0 = 0, x1 = cos(muW$lower), y1 = sin(muW$lower),
             lty = 2, col = arrow.col)
    segments(x0 = 0, y0 = 0, x1 = cos(muW$upper), y1 = sin(muW$upper),
             lty = 2, col = arrow.col)
  }
}

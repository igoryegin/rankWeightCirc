###########################################################
#                                                         #
# VISUALISING RANK-WEIGHTED CIRCULAR DATA                 #
#                                                         #
# Author: Igor Yegin                                      #
#                                                         #
###########################################################

meanplot <- function(theta, w, CI.level, ...) {
  colarg <- rank(w) / max(rank(w))
  rhoW <- biascor.CI("rho", theta, w, CI.level)
  muW <- biascor.CI("mu", theta, w, CI.level)
  plot(cos(theta), sin(theta), pch = 16, axes = FALSE,
       col = rgb(0, 0, 0, alpha = colarg), xlim = c(-1, 1), ylim = c(-1, 1),
       xlab = "", ylab = "", ...)
  lines(x = cos(seq(-pi, pi, pi/1000)), y = sin(seq(-pi, pi, pi/1000)),
        col = "lightgrey", type = "l")
  mtext("All", side = 1)
  mtext("Left", side = 2)
  mtext("None", side = 3)
  mtext("Right", side = 4)
  abline(h = 0)
  abline(v = 0)
  arrows.circular(muW$mu, pch = 4, lwd = 1.5, angle = 10)
  segments(x0 = 0, y0 = 0, x1 = cos(muW$lower), y1 = sin(muW$lower),
           lty = 2)
  segments(x0 = 0, y0 = 0, x1 = cos(muW$upper), y1 = sin(muW$upper),
           lty = 2)
}

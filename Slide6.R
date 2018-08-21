#Slide6.R
#=======
alpha = 1
NC <- 16

sigma1 = 4
sigma2 = 3

#=======
plotWithBands <- function(x, y, b, sigma_b, sigma
                          , main = "24- vs. 12-Month Data\nWith 1 se Prediction Bands"
                          , newpt = 90
) {
  yhatx <- x * b
  parrx <- x * sigma_b
  prorx <- round(sqrt(x) * sigma, 3)
  totrx <- sqrt(parrx^2 + prorx^2)
  plot(x, y, ylim=c(0, round(max(y)*1.2, -floor(log10(max(y))))), 
       xlim=c(0, round(max(x)*1.2, -floor(log10(max(x)))))
       , main = main)
  lines(x, yhatx)
  points(newpt, b*newpt, col = "red", pch = 15)
  axis(1, at = newpt, col.ticks = "red", col.axis = "red")
  lines(x[order(x)], (yhatx + parrx)[order(x)], lty = "dashed")
  lines(x[order(x)], (yhatx - parrx)[order(x)], lty = "dashed")
  lines(x[order(x)], (yhatx + totrx)[order(x)], lty = "dotted", col = "red")
  lines(x[order(x)], (yhatx - totrx)[order(x)], lty = "dotted", col = "red")
}
#=======
# Plot xseq rather than actual x's
plotWithBands2 <- function(x, y, b, sigma_b, sigma
                           , main = "24- vs. 12-Month Data\nWith 1 se Prediction Bands"
                           , newpt = 90
) {
  yhatx <- x * b
  plot(x, y, ylim=c(0, round(max(y)*1.2, -floor(log10(max(y))))), 
       xlim=c(0, round(max(x)*1.2, -floor(log10(max(x)))))
       , main = main)
  xseq <- seq(min(x), max(x), length.out = 100)
  yseq <- b*xseq
  lines(xseq, yseq)
  points(newpt, b*newpt, col = "red", pch = 15)
  axis(1, at = newpt, col.ticks = "red", col.axis = "red")
  parrx <- xseq * sigma_b
  prorx <- round(sqrt(xseq) * sigma, 3)
  totrx <- sqrt(parrx^2 + prorx^2)
  lines(xseq, (yseq + parrx), lty = "dashed")
  lines(xseq, (yseq - parrx), lty = "dashed")
  lines(xseq, (yseq + totrx), lty = "dotted", col = "red")
  lines(xseq, (yseq - totrx), lty = "dotted", col = "red")
}

plotWithBands3 <- function(x, y, b, sigma_b, sigma
                           , main = "24- vs. 12-Month Data\nWith 1 se Prediction Bands"
                           , newpt = 90
                           , ytop = round(max(y)*1.2, -floor(log10(max(y))))
                           , yext = 1.4
) {
  yhatx <- x * b
  plot(x, y, ylim=c(0, ytop), 
       xlim=c(0, round(max(x)*1.2, -floor(log10(max(x)))))
       , main = main)
  xseq <- seq(min(x), max(x), length.out = 100)
  yseq <- b*xseq
  lines(xseq, yseq)
  points(newpt, b*newpt, col = "red", pch = 15)
  axis(1, at = newpt, col.ticks = "red", col.axis = "red")
  parrx <- xseq * sigma_b
  prorx <- round(sqrt(xseq) * sigma, 3)
  totrx <- sqrt(parrx^2 + prorx^2)
  lines(xseq, (yseq + parrx), lty = "dashed")
  lines(xseq, (yseq - parrx), lty = "dashed")
  lines(xseq, (yseq + totrx), lty = "dotted", col = "red")
  lines(xseq, (yseq - totrx), lty = "dotted", col = "red")
}
plotWithBands4 <- function(x, y, b, sigma_b, sigma
                           , main = "24- vs. 12-Month Data\nWith 1 se Prediction Bands"
                           , newpt = 90
                           , ytop = round(max(y)*1.2, -floor(log10(max(y))))
                           , yext = 1.4
) {
  yhatx <- x * b
  plot(x, y, ylim=c(0, ytop), 
       xlim=c(0, round(max(x)*1.2, -floor(log10(max(x)))))
       , main = main,
       xlab = deparse(substitute(x)),
       ylab = deparse(substitute(y)))
  xseq <- seq(0, max(x), length.out = 100)
  yseq <- b*xseq
  lines(xseq, yseq)
  points(newpt, b*newpt, col = "red", pch = 15)
  axis(1, at = newpt, col.ticks = "red", col.axis = "red")
  parrx <- xseq * sigma_b
  prorx <- round(sqrt(xseq) * sigma, 3)
  totrx <- sqrt(parrx^2 + prorx^2)
  lines(xseq, (yseq + parrx), lty = "dashed")
  lines(xseq, (yseq - parrx), lty = "dashed")
  lines(xseq, (yseq + totrx), lty = "dotted", col = "red")
  lines(xseq, (yseq - totrx), lty = "dotted", col = "red")
}

#=======

plotWithBandsLinReg <- function(x, y
                                , main = "24- vs. 12-Month Data\nWith 1 se Prediction Bands\nLinear Regression"
                                , newpt = 90
) {
  plot(x, y, ylim=c(0, round(max(y)*1.2, -floor(log10(max(y))))), 
       xlim=c(0, round(max(x)*1.2, -floor(log10(max(x)))))
       , main = main)
  points(newpt, b*newpt, col = "red", pch = 15)
  axis(1, at = newpt, col.ticks = "red", col.axis = "red")
  L <- lm(y ~ x + 0)
  #abline(L)
  xseq <- seq(min(x), max(x), length.out = 10)
  P1 <- predict(L, newdata = data.frame(x = xseq)
                , interval = c("confidence"), se.fit = TRUE)
  P2 <- predict(L, newdata = data.frame(x = xseq)
                , interval = c("prediction"), se.fit = TRUE)
  print(P1)
  print("P1$fit")
  print(P1$fit[,3]- P1$fit[,1])
  print((P1$fit[,3]- P1$fit[,1])/P1$se.fit)
  print(P2)
  print("P2$fit")
  print(P2$fit[,3]- P2$fit[,1])
  print((P2$fit[,3]- P2$fit[,1])/(sqrt(P2$se.fit^2 + P2$residual.scale)))
  # yseq <- b*xseq
  lines(xseq, P1$fit[,"fit"])
  # parrx <- xseq * sigma_b
  # prorx <- round(sqrt(xseq) * sigma, 3)
  # totrx <- sqrt(parrx^2 + prorx^2)
  #  lines(xseq, P1[,"lwr"], lty = "dashed")
  #  lines(xseq, P1[,"upr"], lty = "dashed")
  #  lines(xseq, P2[,"lwr"], lty = "dotted", col = "red")
  #  lines(xseq, P2[,"upr"], lty = "dotted", col = "red")
}
plotWithBandsLinReg4 <- function(x, y
                                 , main = "24- vs. 12-Month Data\nWith 1 se Prediction Bands\nLinear Regression"
                                 , newpt = 90
                                 , ytop = round(max(y)*1.2, -floor(log10(max(y))))
                                 , yext = 1.4
) {
  plot(x, y, ylim=c(0, ytop), 
       xlim=c(0, round(max(x)*1.2, -floor(log10(max(x)))))
       , main = main)
  points(newpt, b*newpt, col = "red", pch = 15)
  axis(1, at = newpt, col.ticks = "red", col.axis = "red")
  L <- lm(y ~ x + 0)
  #abline(L)
  xseq <- seq(0, max(x), length.out = 10)
  pred.w.clim <- predict(L, newdata = data.frame(x = xseq)
                         , interval = c("confidence"))
  pred.w.plim <- predict(L, newdata = data.frame(x = xseq)
                         , interval = c("prediction"))
  OneSEconf <- (pred.w.clim[,3] - pred.w.clim[,1]) / qt(.975, summary(L)$df[2])
  OneSEpred <- (pred.w.plim[,3] - pred.w.plim[,1]) / qt(.975, summary(L)$df[2])
  
  lines(xseq, pred.w.clim[,1])
  lines(xseq, pred.w.clim[,1] + OneSEconf, 
        lty = "dashed")
  lines(xseq, pred.w.clim[,1] - OneSEconf, 
        lty = "dashed")
  lines(xseq, pred.w.plim[,1] + OneSEpred, lty = "dotted", col = "red")
  lines(xseq, pred.w.plim[,1] - OneSEpred, lty = "dotted", col = "red")
}
#=======

set.seed(12345)
x <- round(100 + rnorm(NC, 0, 50), sigma1)
ndx <- which(x <= 0)
while (n <- length(ndx)) {
  x[ndx] <- round(100 + rnorm(n, 0, 50), sigma1)
  ndx <- which(x <= 0)
}
hist(x)

newxs <- round(quantile(x, c(.2, .8)), 2)

y <- round(2 * x + rnorm(NC, 0, sqrt(x) * sigma1), 2) # rounded
ndx <- which(y <= 0)
while (n <- length(ndx)) {
  y[ndx] <- round(2 * x[ndx] + rnorm(n, 0, sqrt(x[ndx]) * sigma1), 2)
  ndx <- which(y <= 0)
}
hist(y)

png(file.path("CLRS_Session", "Images", "Slide6.png"),
    height = 6.47, width = 8, units = "in", res = 72)
plot(x,y, ylim=c(0, round(max(y)*1.2, -floor(log10(max(y))))), 
     xlim=c(0, round(max(x)*1.2, -floor(log10(max(x)))))
     , main = "24- vs. 12-Month Data\nWith Expected Value Line")
axis(1, at = newxs, col.ticks = "red", col.axis = "red")
#l <- lm(y~x+0)
#b <- coef(l)[1]
b <- sum(y) / sum(x)
#lines(x[order(x)], predict(l, newdata = data.frame(x = x))[order(x)])
lines(c(0, x[order(x)]), c(0, b * x[order(x)]), lty = "dotted")
lines(x[order(x)], b * x[order(x)])
?arrows
arrows(c(77.33, 131.5), c(0, 0), c(77.33, 131.5), c(160.4, 272.7),
       col = "red", lty = "dashed")
dev.off()

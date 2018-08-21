# WtdAvgIllustration

#=======
alpha = 1
NC <- 16

sigma1 = 4
sigma2 = 3

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


# linear regressionn with intercept
# Code from predict.lm help example
#predict(lm(y ~ x))
plot(x, y, ylim=c(0, round(max(y)*1.2, -floor(log10(max(y))))), 
     xlim=c(0, round(max(x)*1.2, -floor(log10(max(x)))))
#     , main = "12-24 Mo Development\nWith Intercept")
      , main = "12-24 Development\nLinear Regression\nZero Intercept")
L <- lm(y ~ x)
#L <- lm(y ~ x + 0)
xseq <- seq(min(x), max(x), length.out = 100)
new <- data.frame(x = xseq)
yseq <- predict(L, new)
lines(xseq, yseq)
points(newxs, predict(L, data.frame(x=newxs)), 
       col = "red", pch = 15)
axis(1, at = newxs, col.ticks = "red", col.axis = "red")
#predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.clim <- predict(L, new, interval = "confidence")
OneSEconf <- (pred.w.clim[,3] - pred.w.clim[,1]) / qt(.975, summary(L)$df[2])
#lines(xseq, (yseq + parrx), lty = "dashed")
#lines(xseq, (yseq - parrx), lty = "dashed")
#lines(xseq, pred.w.clim[,"upr"], lty = "dashed")
#lines(xseq, pred.w.clim[,"lwr"], lty = "dashed")
lines(xseq, pred.w.clim[,"fit"] + OneSEconf, lty = "dashed")
lines(xseq, pred.w.clim[,"fit"] - OneSEconf, lty = "dashed")
pred.w.plim <- predict(L, new, interval = "prediction")
OneSEpred <- (pred.w.plim[,3] - pred.w.plim[,1]) / qt(.975, summary(L)$df[2])
lines(xseq, pred.w.plim[,"fit"] + OneSEpred, lty = "dotted", col = "red")
lines(xseq, pred.w.plim[,"fit"] - OneSEpred, lty = "dotted", col = "red")
#lines(xseq, pred.w.plim[,"upr"], lty = "dotted", col = "red")
#lines(xseq, pred.w.plim[,"lwr"], lty = "dotted", col = "red")
#matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
#        lty = c(1,2,2,3,3), type = "l", # ylab = "predicted y",
#        col = c("black", "black", "black", "red", "red"),
#        main = "12-24 Mo Development\nWith Intercept",
#        xlab = "x", ylab = "y",
#        ylim=c(0, round(max(y)*1.2, -floor(log10(max(y))))), 
#        xlim=c(0, round(max(x)*1.2, -floor(log10(max(x))))))
#
# EXPORT AS PNG, RED CURVE LOOKS BETTER THAN AS JPG width=620, height=502
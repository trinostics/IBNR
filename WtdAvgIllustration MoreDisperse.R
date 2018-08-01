# WtdAvgIllustration

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

plot(x,y, ylim=c(0, round(max(y)*1.2, -floor(log10(max(y))))), 
     xlim=c(0, round(max(x)*1.2, -floor(log10(max(x)))))
     , main = "24- vs. 12-Month Data\nWith Expected Value Line")
axis(1, at = newxs, col.ticks = "red", col.axis = "red")
#l <- lm(y~x+0)
#b <- coef(l)[1]
b <- sum(y) / sum(x)
#lines(x[order(x)], predict(l, newdata = data.frame(x = x))[order(x)])
lines(x[order(x)], b * x[order(x)])

xp <- x / sqrt(x)
yp <- y / sqrt(x)
L <- lm(yp ~ xp + 0)
sL <- summary(L)
b <- round(sL$coefficient[1],3)
sigma_b <- round(sL$coefficient[2], 5)
sigma <- round(sL$sigma, 3)
tri <- round(
  cbind(`12` = c(x, newxs),
        `24` = c(y, NA, NA)),
  2)
rownames(tri) <- 1:(NC+2)
colnames(tri) <- 12*1:2
#tri
M <- suppressWarnings(ChainLadder:::MackChainLadder(
  tri, mse.method = "Independence", alpha = alpha))
#print(M)
sM <- summary(M)
compare <- data.frame(x = 90,
                      y = b * 90,
                      b = b,
                      sigma = round(M$sigma, 2),
  ParameterRisk = round(M$Mack.ParameterRisk[nrow(M$Mack.ParameterRisk),
                           ncol(M$Mack.ParameterRisk)], 2),
  ProcessRisk = round(M$Mack.ProcessRisk[nrow(M$Mack.ProcessRisk),
                         ncol(M$Mack.ProcessRisk)], 2),
  TotalRisk = round(M$Mack.S.E[nrow(M$Mack.S.E),
                 ncol(M$Mack.S.E)], 2),
  meanx = round(mean(x), 2),
  row.names = NC+1)
#print(compare)
#plotWithBands(x, y, b, sigma_b, sigma, newpt = newxs)
plotWithBands2(x, y, b, sigma_b, sigma, newpt = newxs)
plotWithBandsLinReg(x, y, newpt = newxs)
yhatx <- x * b
parrx <- x * sigma_b
prorx <- round(sqrt(x) * sigma, 3)
totrx <- sqrt(parrx^2 + prorx^2)
plot(x, y, ylim=c(0, round(max(y)*1.2, -floor(log10(max(y))))), 
     xlim=c(0, round(max(x)*1.2, -floor(log10(max(x)))))
, main = "24- vs. 12-Month Data\nWith 1 se Prediction Bands")
lines(x, yhatx)
points(newxs, b*newxs, col = "red", pch = 15)
axis(1, at = newxs, col.ticks = "red", col.axis = "red")
lines(x[order(x)], (yhatx + parrx)[order(x)], lty = "dashed")
lines(x[order(x)], (yhatx - parrx)[order(x)], lty = "dashed")
lines(x[order(x)], (yhatx + totrx)[order(x)], lty = "dotted", col = "red")
lines(x[order(x)], (yhatx - totrx)[order(x)], lty = "dotted", col = "red")



# Add another column

z <- round(1.1*y[1:(NC*3/4)] + rnorm(12, 0, sqrt(y[1:(NC*3/4)])*sigma2), 2)
ndx <- which(z <= 0)
while (n <- length(ndx)) {
  z[ndx] <- round(1.1*y[1:(NC*3/4)][ndx] + rnorm(n, 0, sqrt(y[1:(NC*3/4)][ndx])*sigma2), 2)
  ndx <- which(z <= 0)
}
hist(z)
ysave <- y
y <- y[1:(NC*3/4)]
plot(y, z, ylim=c(0, 1.2*round(max(z), -floor(log10(max(z))))), 
     xlim=c(0, 1.2*round(max(y), -floor(log10(max(y)))))
, main = "36- vs. 24-Month Data\nWith Expected Value Line")
l <- lm(z ~ y + 0)
lines(y[order(y)], predict(l, newdata = data.frame(y = y))[order(y)])
y <- ysave
(A2 <- round(sum(z)/sum(y[1:(NC*3/4)]), 3))
#not far from the theoretical value sigma2.

trisave <- tri
#tri
tri <- cbind(tri,
             `36` = c(z, rep(NA, NC/4+1)))
#tri
M <- ChainLadder::MackChainLadder(
  tri, mse.method = "Independence", est.sigma = "Mack", alpha = alpha)
#print(M)
print(M$Total.ParameterRisk)
print(M$Total.ProcessRisk)
tri <- trisave
#Let's add up the x's and y's into accident year totals.
#Here is the aggregated data.
dat <- data.frame(ay = c(rep(1:4, each = NC/4), 5)
                  , claimno=1:(NC+1)
                  , x = c(x, 90)
                  , y = c(y, NA)
                  , z = c(z, rep(NA, NC/4+1))
                  )
#dat
A <- aggregate(dat[c("x", "y", "z")], by = dat["ay"], FUN = sum)
A <- A[, 2:4]
#A
MA <- ChainLadder::MackChainLadder(A, mse.method = "Independence", 
                                   est.sigma = "Mack", alpha = alpha)
#print(MA)
print(M$Total.ParameterRisk)
print(M$Total.ProcessRisk)
print(MA$Total.ParameterRisk)
print(MA$Total.ProcessRisk)

plotWithBands(x=A[1:4, "x"], y=A[1:4, "y"], b=MA$f[1], sigma_b = MA$f.se[1]
              , sigma = MA$sigma[1],
              main = "12-24 Development Aggregated Data"
              , newpt = 90)

cbind(
    M$Total.ParameterRisk,M$Total.ProcessRisk,MA$Total.ParameterRisk,MA$Total.ProcessRisk
  )


# Mack_lm_compare_predict.R

# Show how the Mack risks can be found in
# predict.lm output when alpha = 2 -- linear regression

#=======

library(ChainLadder)

alpha = 1
delta = 2 - alpha
NC <- 16

# This is my way of generating some random data, positive values
sigma1 = 4
sigma2 = 3


set.seed(12345)
x <- round(100 + rnorm(NC, 0, 50), sigma1)
ndx <- which(x <= 0)
while (n <- length(ndx)) {
  x[ndx] <- round(100 + rnorm(n, 0, 50), sigma1)
  ndx <- which(x <= 0)
}
hist(x)
y <- round(2 * x + rnorm(NC, 0, sqrt(x) * sigma1), 2) # rounded
ndx <- which(y <= 0)
while (n <- length(ndx)) {
  y[ndx] <- round(2 * x[ndx] + rnorm(n, 0, sqrt(x[ndx]) * sigma1), 2)
  ndx <- which(y <= 0)
}
hist(y)

# Make a triangle with y, and x extended by a "new value" equal to its first value
M <- MackChainLadder(
  Triangle = cbind(`12` = c(x, x[1]), `24` = c(y, NA))
  , est.sigma = "Mack"
  , alpha = alpha
)
L <- {y <- y[1:16]; x<-x[1:16]; lm(y ~ x + 0, weights = 1 / x^delta)}
sL <- summary(L)
b <- round(sL$coefficient[1],3)
seb <- round(sL$coefficient[2], 5)
sigma <- round(sL$sigma, 3)

# show the estimated parameters are identical
cbind(
  `Mack` = c(
    `b` = M$f[1],
    `sigmab` = M$f.se[1],
    sigma = M$sigma[1]
  ),
  `L` = c(
    `b` = sL$coefficient[1],
    `sigmab` = sL$coefficient[2],
    `sigma` = sL$sigma
  )
  )

# Show that the errors for the prediction from the actual data
# are the same as with Mack

# Parameter risk
pc <- predict(L, interval = c("confidence"), se.fit = TRUE)
M$Mack.ParameterRisk[17, 2]
pc$se.fit[1] # easiest

# ... or ...
(pc$fit[,"upr"] - pc$fit[,"fit"])[1] / qt(0.975, df = sL$df[2])
M$Mack.ParameterRisk[17, 2]

# Total risk
pp <- predict(L, interval = c("prediction"), se.fit = TRUE) # suppress warnings
# per https://www.r-bloggers.com/prediction-interval-the-wider-sister-of-confidence-interval/
# All the warning is doing is saying we're now assuming the original
# data is "new" ("future") data -- Understanding what this warning
# meant was holding me back. I thought it might have been referring
# to a underlying cross-validation adjustment.

(pp$fit[,"upr"] - pp$fit[,"fit"])[1] / qt(0.975, df = sL$df[2])
M$Mack.S.E[17, 2] 

# ... also ...
sqrt(pc$se.fit^2 + summary(L)$sigma^2)[1]
M$Mack.S.E[17, 2]

# ... or using all pc output
sqrt(pc$se.fit^2 + pc$residual.scale^2)[1]
M$Mack.S.E[17, 2]

x <- rnorm(15)
y <- x + rnorm(15)
L <- lm(y~x+0)
summary(L)$coef[1,"Estimate"]->b
summary(L)$coef[1,"Std. Error"]->seb
pc <- predict(L, interval = "confidence", se.fit = TRUE)
all(round(seb*abs(x),6)==round(pc$se.fit,6))
sqrt(pc$se.fit^2 + summary(L)$sigma^2)
round((pc$fit[,"upr"] - pc$fit[,"fit"]) / pc$se.fit, 5) == round(qt(0.975, df = 14), 5)

pp <- predict(L, interval = "prediction", se.fit = TRUE)
round((pp$fit[,"upr"] - pp$fit[,"fit"]) / sqrt(pc$se.fit^2 + summary(L)$sigma^2),5) ==
  round(qt(0.975, df = 14), 5)

#x <- rnorm(15)
#y <- x + rnorm(15)
#L <- lm(y~x+0, weights = 1 / )
summary(L)$coef[1,"Estimate"]->b
summary(L)$coef[1,"Std. Error"]->seb
pc <- predict(L, interval = "confidence", se.fit = TRUE)
all(round(seb*abs(x),6)==round(pc$se.fit,6))
sqrt(pc$se.fit^2 + summary(L)$sigma^2)
round((pc$fit[,"upr"] - pc$fit[,"fit"]) / pc$se.fit, 5) == round(qt(0.975, df = 14), 5)

pp <- predict(L, interval = "prediction", se.fit = TRUE)
round((pp$fit[,"upr"] - pp$fit[,"fit"]) / sqrt(pc$se.fit^2 + summary(L)$sigma^2),5) ==
  round(qt(0.975, df = 14), 5)

# singular fit on second lm
x <- c(NA, NA, 2)
y <- c(1,2,4)
lm(y~x+0, singular.ok = F)
lm(y~x, singular.ok = F)
dat <- data.frame(
  e = c(100, 100, 100, 150, 100, 200),
  p1 = c(0, 10, 12, 10, 0, 15),
  p2 = c(5, 21, 21, 12, 10, 30)
)
fit <- lm(p2 ~ p1, dat)
print(fit)
with(dat, plot(p1, p2, ylim = c(0, max(p2))))
abline(fit)

fit <- lm(p2 ~ e, dat)
print(fit)
with(dat, plot(e, p2, ylim = c(0, max(p2))))
abline(fit)

fit <- lm(p1 ~ e, dat)
print(fit)
with(dat, plot(e, p1, ylim = c(0, max(p1, na.rm = TRUE))))
abline(fit)
p1fit <- predict(fit, newdata = data.frame(e = dat$e[is.na(dat$p1)]))
p1fit

m <- t(as.matrix(dat[2:3]))
matplot(m, type = "l")
matpoints(m)

rowMeans(m, na.rm = TRUE)
m <- cbind(m, rowMeans(m, na.rm = TRUE))
m
matplot(m, type = "l")
matpoints(m)

attach(dat)
y <- sum(p2, na.rm = TRUE)
x <- sum(p1, na.rm = TRUE)
ata <- y / x
ata
p1 * ata
sum(p1 * ata, na.rm = TRUE)
x
y
sum(p2[!is.na(p1)])
ataNoNA1 <- sum(p2[!is.na(p1)]) / x
ataNoNA1

plot(p1, p2, ylim = c(0, max(p2)))

reg1 <- lm(p2 ~ p1)
abline(reg1)
sum(residuals(reg1) ^ 2)

reg2 <- lm(p2 ~ p1 + 0)
abline(reg2, col = "red")
sum(residuals(reg2) ^ 2)

reg1e <- lm(p2 ~ p1 + e, data = dat)
sum(residuals(reg1e) ^ 2)
reg1e

# reg1e gives lowest sum of squared prediction errors
predict(reg1, newdata = dat)
predict(reg1, newdata = dat) - p2
(predict(reg1, newdata = dat) - p2) ^ 2
sum((predict(reg1, newdata = dat) - p2) ^ 2)

predict(reg2, newdata = dat)
predict(reg2, newdata = dat) - p2
(predict(reg2, newdata = dat) - p2) ^ 2
sum((predict(reg2, newdata = dat) - p2) ^ 2)

predict(reg1e, newdata = dat)
predict(reg1e, newdata = dat) - p2
(predict(reg1e, newdata = dat) - p2) ^ 2
sum((predict(reg1e, newdata = dat) - p2) ^ 2)

# ? AIC
# When comparing models fitted by maximum likelihood to the same data, 
# the smaller the AIC or BIC, the better the fit.
AIC(reg1)
AIC(reg2)
AIC(reg1e)

BIC(reg1)
BIC(reg2)
BIC(reg1e)

sum(y)
sum(predict(reg1, newdata = dat))
sum(predict(reg2, newdata = dat))
sum(predict(reg1e, newdata = dat)) # same as y b/c all obs used

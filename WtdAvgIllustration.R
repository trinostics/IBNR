# WtdAvgIllustration
set.seed(12345)
(x <- 100 + rnorm(16, 0, 50))
(y <- 2*x + rnorm(16, 0, sqrt(x)*2))
set.seed(12345)
(x <- 100 + rnorm(16, 0, 50))
(yp2 <- 2*x/sqrt(x) + rnorm(16,0,2))
(y2 <- yp2 * sqrt(x)) # same as y
plot(x,y, ylim=c(0,500), xlim=c(0,200))
sum(y)/sum(x)
yp <- y/sqrt(x)
xp <- x/sqrt(x)
(slm<-summary(lm(yp~xp-1)))
(shat <- sqrt(sum((yp-xp*2.033364)^2)/15))
tbl <- slm$coefficients
tbl <- cbind(tbl, n = length(xp), df = slm$df[2])
sqrt(1.837865^2*sum(xp^2)/(sum(xp^2)^2))
dat <- data.frame(claimno=1:16,x=x,y=y,ay=4*(1:4))
A <- aggregate(dat[c("x", "y")], by = dat["ay"], FUN = sum)
X <- A$x; Y <- A$y
sum(Y) / sum(X)
Yp <- Y/sqrt(X)
Xp <- X / sqrt(X)
(Slm <- summary(lm(Yp~Xp-1)))
tbl <- rbind(tbl, 
             cbind(Slm$coefficients, n = length(Xp), df = Slm$df[2])
)
tbl

L2 <- lm(y ~ x - 1, w = sqrt(x))
print(summary(L2))

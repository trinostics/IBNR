library(mondate)
library(ChainLadder)
library(triouts)
library(link)
rdataoutputfile <- "output.Rdata"
load(file = rdataoutputfile)
# Make a loss rptd triangle
cd$age = as.numeric(mondate(cd$eval_date)-mondate.ymd(cd$ay-1))
z <- long2wide(subset(cd, metric == "loss_itdrptd"))
#View(z)
dim(z)
z <- z[,-1]
z <- z / 1000000
MackChainLadder(z, est.sigma = "log-linear", mse.method = "Independence")

cdlong <- wide2long(z, "ay", "age")
lr <- link_ratio(cdlong, ymetric = "value", timevar = "age", by = "ay")
summary(lr)
pg <- predict(lr, newdata = cdlong)
e1 <- exh1(pg)
e1$se = sqrt(e1$D^2 + e1$G^2)
e1$cv = e1$se / e1[[5]]
formact(e1, digits = c(rep(0, 8), 3))
dim(e1)

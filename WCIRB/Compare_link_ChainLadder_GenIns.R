library(ChainLadder)
library(link)
library(triouts)
View(GenInsLong)
genins <- GenInsLong
names(genins) <- c("ay", "age", "value")
M <- MackChainLadder(long2wide(genins), mse.method = "Independence")
lr <- link_ratio(genins, ymetric = "value", timevar = "age", by = "ay")
pg <- predict(lr, newdata = genins)
e1 <- exh1(pg)
e1$se = sqrt(e1$D^2 + e1$G^2)
e1$cv = e1$se / e1[[5]]
e1
M
formact(e1, digits = c(rep(0, 8), 3))

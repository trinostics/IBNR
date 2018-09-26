# Hai's GenIns
library(excelRio)
x <- readFromCsv("AggregatedClaims.csv")
library(mondate)
x$origin <- mondate::year(x$AccidentDate)
x$dev <- mondate(x$EvaluationDate) - mondate.ymd(x$origin - 1)
library(reshape2)
library(ChainLadder)
HY.GenIns <- acast(x, origin ~ dev, fun.aggregate = sum, fill = as.numeric(NA), value.var = "Paid")
mean(HY.GenIns - ChainLadder::GenIns)

tail(smcl(MackChainLadder(GenIns)), 1)
tail(smcl(MackChainLadder(HY.GenIns)), 1)


source("plotMackIBNR.r")
plotMackIBNR(GenIns)
plotMackIBNR(HY.GenIns)

dat <- x[c("ClaimNo", "dev", "Paid")]
dat <- as.triangle(dat, origin = "ClaimNo", value = "Paid")
# Mack doesn't like dividing by 0's
dat[abs(dat) < 1] <- NA
dim(dat)
keep <- sapply(1:nrow(dat), function(i) !all(is.na(dat[i,])))
dat <- dat[keep,]
dim(dat)
dat <- round(dat, 0)
# example of some data points
dummy <- dat[c("1", "2", "1445", "1446", "2560", "2561", "3303", "3304", "3305", "4330", "5538", "5911", "6289", "6300"),]
writeToExcel(dummy)

m <- MackChainLadder(dat)
atadat <- ata(dat)


writeToExcel(tail(summaryMackChainLadder(MackChainLadder(GenIns)), 1), "dummy.xlsx")
writeToExcel(tail(summaryMackChainLadder(MackChainLadder(HY.GenIns)), 1), "dummy.xlsx")
writeToExcel(tail(summaryMackChainLadder(MackChainLadder(dat)), 1), "dummy.xlsx")
tail(smcl.dat, 1)
writeToExcel(
round(rbind(
`GenIns` = attr(ata(GenIns), "vwtd")[1]
, `HY.GenIns` = attr(ata(HY.GenIns), "vwtd")[1]
, `HY.detail` = attr(ata(dat), "vwtd")[1]
), 3)
, "dummy.xlsx")


M <- MackChainLadder(dat)
b <- M$f[1]
sigma_b <- M$f.se[1]
sigma <- M$sigma[1]
newpt <- mean(dat[,1], na.rm = TRUE)
png("dat12.png", width = 800, height = 600)
ndx <- !is.na(dat[,1]) & !is.na(dat[,2])
plotWithBands4(dat[ndx,1], dat[ndx,2], b, sigma_b, sigma, newpt = newpt)
abline(lm(dat[ndx,2] ~ dat[ndx,1]))
dev.off()
lm(dat[ndx,2] ~ dat[ndx,1])

plotWithBands4 <- function(x, y, b, sigma_b, sigma
                           , main = "24- vs. 12-Month Data\nWith 1 se Prediction Bands"
                           , newpt = 90
                           , ytop = round(max(y)*1.2, -floor(log10(max(y))))
                           , yext = 1.4
) {
  L <- lm(y ~ x)
  
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
plotWithBandsLinReg <- function(x, y
  , main = "24- vs. 12-Month Data\nWith 1 se Prediction Bands\nLinear Regression"
                                , newpt = 90
) {
  plot(x, y, ylim=c(0, round(max(y)*1.2, -floor(log10(max(y))))), 
       xlim=c(0, round(max(x)*1.2, -floor(log10(max(x)))))
       , main = main)
  points(newpt, b*newpt, col = "red", pch = 15)
  axis(1, at = newpt, col.ticks = "red", col.axis = "red")
  L <- lm(y ~ x)
  #abline(L)
  #xseq <- seq(min(x), max(x), length.out = 10)
  xseq <- seq(0, max(x), length.out = 100)
  P1 <- predict(L, newdata = data.frame(x = xseq)
                , interval = c("confidence"), se.fit = TRUE)
  P2 <- predict(L, newdata = data.frame(x = xseq)
                , interval = c("prediction"), se.fit = TRUE)
  # yseq <- b*xseq
  lines(xseq, P1$fit[,"fit"])
  # parrx <- xseq * sigma_b
  # prorx <- round(sqrt(xseq) * sigma, 3)
  # totrx <- sqrt(parrx^2 + prorx^2)
    lines(xseq, P1$fit[,"lwr"], lty = "dashed")
    lines(xseq, P1$fit[,"upr"], lty = "dashed")
    lines(xseq, P2$fit[,"lwr"], lty = "dotted", col = "red")
    lines(xseq, P2$fit[,"upr"], lty = "dotted", col = "red")
}


library(ChainLadder)
M <- MackChainLadder(GenIns)
b <- M$f[1]
sigma_b <- M$f.se[1]
sigma <- M$sigma[1]
newpt <- GenIns[1,10]
png("GenIns12.png", width = 800, height = 600)
plotWithBands4(GenIns[1:9,1], GenIns[1:9,2], b, sigma_b, sigma, newpt = newpt)
dev.off()

png("GenIns12 linreg.png", width = 800, height = 600)
plotWithBandsLinReg(GenIns[1:9,1], GenIns[1:9,2], newpt = newpt)
dev.off()

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

M <- MackChainLadder(GenIns)
b <- M$f[1]
sigma_b <- M$f.se[1]
sigma <- M$sigma[1]
newpt <- GenIns[1,10]
png("GenIns12.png", width = 800, height = 600)
plotWithBands4(GenIns[1:9,1], GenIns[1:9,2], b, sigma_b, sigma, newpt = newpt)
dev.off()

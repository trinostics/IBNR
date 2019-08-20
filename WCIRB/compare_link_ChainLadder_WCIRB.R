library(mondate)
library(ChainLadder)
library(triouts)
rdataoutputfile <- "output.Rdata"
load(file = rdataoutputfile)
# Make a loss rptd triangle
cd$age = as.numeric(mondate(cd$eval_date)-mondate.ymd(cd$ay-1))
z <- long2wide(subset(cd, metric == "loss_itdrptd"))
View(z)
dim(z)
z <- z[,-1]
MackChainLadder(z)

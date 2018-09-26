#slide23
library(ChainLadder)
library(excelRio)
T <- pasteFromExcel(rowheader = TRUE, header = TRUE)
T
class(T)
# without y
MackChainLadder(T[,c(1,3)], est.sigma = "Mack")
# with y
MackChainLadder(T, est.sigma = "Mack")

# in total
# with y see above
# without y for development, but use for most recent diagonal
# for se, see above
# IBNR
summary(MackChainLadder(T[,c(1,3)], est.sigma = "Mack"))$Totals[4,1]+sum(T[13:16,1])-sum(T[13:16,2])
# cv
summary(MackChainLadder(T[,c(1,3)], est.sigma = "Mack"))$Totals[5,1]/(summary(MackChainLadder(T[,c(1,3)], est.sigma = "Mack"))$Totals[4,1]+sum(T[13:16,1])-sum(T[13:16,2]))

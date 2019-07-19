library(ChainLadder)
library(excelRio)
#CalifWCIndMedIncd <- pasteFromExcel(rowheader = TRUE, header = TRUE)
CalifWCIndMedIncd <- readFromCsv(file = "CalifWCIndMedIncd.csv",
                                 na.strings = "",
                                   rowheader = TRUE, header = TRUE)
M <- MackChainLadder(CalifWCIndMedIncd, est.sigma = "Mack", tail = 1.025)
M

summary(M)$Totals
writeToExcel(summary(M)$ByOrigin, "MackWCIRB.xlsx")
writeToExcel(summary(M)$Totals, "MackWCIRB.xlsx")

source(file.path("..", "summaryMackChainLadder.R"))
S <- summaryMackChainLadder(M)
S
writeToExcel(S, "MackWCIRB.xlsx")

source(file.path("..", "plotMackIBNR.R"))
plotMackIBNR(CalifWCIndMedIncd, est.sigma = "Mack", tail = 1.025)

mu <- S["sum", "IBNR"]
cv <- S["sum", "CV(IBNR)"]
sd <- mu * cv
lp <- lnormParms(mu, sd)
meanlog <- lp["mu"]
sdlog <- lp["sigma"]
carried <- 36196
conflevel <- plnorm(carried, meanlog, sdlog)
conflevel

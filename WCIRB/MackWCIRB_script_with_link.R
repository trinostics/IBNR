# MackWCIRB_script_with_link.R

library(ChainLadder)
library(excelRio)
library(link)
library(triouts)
#CalifWCIndMedIncd <- pasteFromExcel(rowheader = TRUE, header = TRUE)
CalifWCIndMedIncd <- readFromCsv(file = "CalifWCIndMedIncd.csv",
                                 na.strings = "",
                                   rowheader = TRUE, header = TRUE)
M <- MackChainLadder(CalifWCIndMedIncd, est.sigma = "log-linear",
                     mse.method = "Independence")
M

# CalifWCIndMedIncd has columns names X15, X27, etc.,
#   but MackChainladder doesn't seem to care.
# For link, let's set the ages to 15, 27, etc. before transforming to long format
colnames(CalifWCIndMedIncd) <- 3 + 12 * (1:32)

dat <- wide2long(CalifWCIndMedIncd, objsname = "ay",
                 timesname = "age",
                 valuesname = "value")

lr <- link_ratio(dat, ymetric = "value", timevar = "age", by = "ay")
pg <- predict(lr, newdata = dat)
e1 <- exh1(pg)
e1$se = sqrt(e1$D^2 + e1$G^2)
e1$cv = e1$se / e1[[5]]
e1
M
formact(e1, digits = c(rep(0, 8), 3))


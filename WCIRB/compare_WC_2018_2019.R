# compare_WC_2018_2019.R
library(ChainLadder)
library(excelRio)
library(link)
library(triouts)

CalifWCIndMedIncd <- readFromCsv(file = "CalifWCIndMedIncd.csv",
                                 na.strings = "",
                                 rowheader = TRUE, header = TRUE,
                                 check.names = FALSE)
op <- par(mfrow = c(2, 1))
plot(as.triangle(CalifWCIndMedIncd), ylim = c(1000, 12000), main = "2018")

rdataoutputfile <- "output.Rdata"
load(file = rdataoutputfile)
loss2019 <- long2wide(
  subset(cd, metric == "loss_itdrptd"), 
  valuename = "value", rowname = "ay", columnname = "age")
loss2019 <- loss2019[, -1]
plot(as.triangle(loss2019 / 1000000), ylim = c(1000, 12000), main = "2019")
par(op)
# Export the plot to PDF, Portrait, PDF size = US Letter

# 1990 in 2018 eval looks fishy
loss2019[5, ] / 1000000
CalifWCIndMedIncd[5,]
x <- cbind(eval2018 = CalifWCIndMedIncd[5,], eval2019 = loss2019[5, ] / 1000000)

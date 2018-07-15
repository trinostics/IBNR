library(excelRio)
library(reshape2)
library(mondate)
library(ChainLadder)
setwd("~/GitHub/IBNR/GenIns")
det <- readFromCsv("AggregatedClaims.csv", stringsAsFactors = FALSE)
det$EvaluationDate <- as.Date(det$EvaluationDate)
det$AccidentDate <- as.Date(det$AccidentDate)
det$ay <- year(det$AccidentDate)
det$age <- as.numeric(mondate(det$EvaluationDate) - 
                        mondate.ymd(det$ay - 1))
tri.agg <- acast(det, ay ~ age, value.var = "Paid", fun.aggregate = sum,
      fill = as.numeric(NA))
#copyToExcel(tri.det) # same aggregated triangle as Hai's
tri.agg.ata <- ata(tri.agg)
attr(tri.agg.ata, "vwtd")
#attr(ata(GenIns), "vwtd")
tri.det <- acast(det, ClaimNo ~ age, value.var = "Paid", fun.aggregate = sum,
             fill = as.numeric(NA))
tri.det.ata <- ata(tri.det)
attr(tri.det.ata, "vwtd")

# the vwtd average link ratios are not the same between detail and aggregate
# because of the NA's in the early age columns in the detail.
# To prove, let's first count the number of those NAs
sum(is.na(tri.det[,1])) # 1338
sum(is.na(tri.det[,1]) & is.na(tri.det[,2])) # 21
sum(is.na(tri.det[,1]) & is.na(tri.det[,2]) & is.na(tri.det[,3])) # 1
sum(is.na(tri.det[,1]) & is.na(tri.det[,2]) & is.na(tri.det[,3])
    & is.na(tri.det[,4])) # 0

# Now replace those NA's with zeros and the vwtd avg link ratios will be the same
# as with the agg triangle.
tri.det0 <- tri.det
tri.det0[is.na(tri.det[,1]), 1] <- 0
tri.det0[is.na(tri.det[,1]) & is.na(tri.det[,2]), 2] <- 0
tri.det0[is.na(tri.det[,1]) & is.na(tri.det[,2]) & is.na(tri.det[,3]), 3] <- 0
tri.det0.ata <- ata(tri.det0)
attr(tri.det0.ata, "vwtd") # same as attr(tri.agg.ata, "vwtd")

# 
summary(MackChainLadder(tri.agg))$Totals
summary(MackChainLadder(tri.det0))$Totals

# get errors with 0's in the denominator.
# replace them with 1's
tri.det1 <- tri.det
a <- 1000
tri.det1[is.na(tri.det[,1]), 1] <- a
tri.det1[is.na(tri.det[,1]) & is.na(tri.det[,2]), 2] <- a
tri.det1[is.na(tri.det[,1]) & is.na(tri.det[,2]) & is.na(tri.det[,3]), 3] <- a
tri.det1.ata <- ata(tri.det1)
attr(tri.det1.ata, "vwtd") # same as attr(tri.agg.ata, "vwtd") # very close
# 
summary(MackChainLadder(tri.agg, alpha = 2))$Totals
summary(MackChainLadder(tri.det1, alpha = 2))$Totals


library(excelRio)
library(mondate)
library(ChainLadder)
library(reshape2)

getwd() # setwd(".../GenIns") ... = path to GenIns folder

AggregatedClaims <- readFromCsv("AggregatedClaims.csv")

AggregatedClaims$ay <- year(AggregatedClaims$AccidentDate)
AggregatedClaims$age <- mondate(AggregatedClaims$EvaluationDate) - mondate.ymd(AggregatedClaims$ay - 1)

tri <- acast(AggregatedClaims, ay ~ age, 
             fun = sum, fill = as.numeric(NA),
             value.var = "Paid")
tri
tri - GenIns
(tri - GenIns) / GenIns
MackChainLadder(GenIns)
MackChainLadder(tri)

# Accident month -- eg, 200101
AggregatedClaims$amo <- format(AggregatedClaims$AccidentDate, "%Y%m")

triAmo <- acast(AggregatedClaims, amo ~ age, 
             fun = sum, fill = as.numeric(NA),
             value.var = "Paid")

MackChainLadder(triAmo)

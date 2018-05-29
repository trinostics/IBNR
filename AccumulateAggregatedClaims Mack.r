library(mondate) # for 'age' variable
library(reshape2) # for 'acast'
library(ChainLadder)

getwd() # setwd(".../GenIns") ... = path to GenIns folder

AggregatedClaims <- read.csv("AggregatedClaims.csv")
AggregatedClaims$AccidentDate <- as.Date(AggregatedClaims$AccidentDate)
AggregatedClaims$EvaluationDate <- as.Date(AggregatedClaims$EvaluationDate)

AggregatedClaims$ay <- year(AggregatedClaims$AccidentDate)

AggregatedClaims$age <- mondate(AggregatedClaims$EvaluationDate) - 
  mondate.ymd(AggregatedClaims$ay - 1)

tri <- acast(AggregatedClaims, ay ~ age, 
             fun = sum, fill = as.numeric(NA),
             value.var = "Paid")
tri
tri - GenIns
(tri - GenIns) / GenIns # agrees with Hai You

# Compare Mack Method on both triangles
MackChainLadder(GenIns)
MackChainLadder(tri) # cv of total IBNR is half that of GenIns

# Let's compare the Ultimates
summary(MackChainLadder(tri))$ByOrigin[,"Ultimate"]
summary(MackChainLadder(GenIns))$ByOrigin[,"Ultimate"]
summary(MackChainLadder(tri))$ByOrigin[,"Ultimate"] - 
  summary(MackChainLadder(GenIns))$ByOrigin[,"Ultimate"]
(summary(MackChainLadder(tri))$ByOrigin[,"Ultimate"] - 
  summary(MackChainLadder(GenIns))$ByOrigin[,"Ultimate"]) / 
  summary(MackChainLadder(GenIns))$ByOrigin[,"Ultimate"]
plot((summary(MackChainLadder(tri))$ByOrigin[,"Ultimate"] -
        summary(MackChainLadder(GenIns))$ByOrigin[,"Ultimate"]) /
       summary(MackChainLadder(GenIns))$ByOrigin[,"Ultimate"])

setwd("C:/Users/Dan/Desktop/GenIns")
library(ChainLadder)
library(mondate)
df <- as.data.frame(GenIns, na.rm = TRUE)
df$ay <- as.numeric(df$origin) + 2000 # give labels to the origin's
df$Accident_Year <- as.numeric(df$origin) + 2007 # give labels to the origin's
df$Evaluation_YearEnd <- df$ay + df$dev - 1
df[["Evaluation_Date"]] <- mondate.ymd(df$Evaluation_YearEnd)
tri <- as.triangle(df, origin = "Accident_Year", 
                   dev = "Evaluation_YearEnd", value = "value")
tri

rearrangeForScheduleP <- function(tri, originYear = 2007){
  df <- as.data.frame(tri, na.rm = TRUE)
#  df$ay <- as.numeric(df$origin) + 2000 # give labels to the origin's
  df$Accident_Year <- as.numeric(df$origin) + originYear # give labels to the origin's
  df$AsOf_YearEnd <- df$Accident_Year + as.numeric(df$dev) - 1
  df[["Evaluation_Date"]] <- mondate.ymd(df$AsOf_YearEnd)
  tri <- as.triangle(df, origin = "Accident_Year", 
                     dev = "AsOf_YearEnd", value = "value")
  tri
}

formatForScheduleP <- function(tri) {
  m <- round(tri/1000)
#  names(dimnames(m))[2L] <- "Evaluation_Date ($000 omitted)"
  #noquote(format(m, big.mark = ","))
  mf <- format(m, big.mark = ",")
  nams <- names(dimnames(m))
  dimnms <- dimnames(m)
  mfp <- paste("   ", mf)
  dim(mfp) <- dim(mf)
  dimnames(mfp) <- dimnms
  noquote(mfp)
  mfp[is.na(m)] <- "     XXX"
  #noquote(mfp)
  
  mfp2 <- rbind(Prior = c("     000", rep("", 9)), mfp)
  names(dimnames(mfp2)) <- nams
  mfp2
  }
formatForScheduleP(tri)

Part3 <- rearrangeForScheduleP(GenIns)
Part3Formatted <- formatForScheduleP(Part3)

##
ATA <- attr(ata(GenIns), "vwtd")
LDF <- rev(cumprod(rev(ATA)))
LDF <- c(LDF, `10-Ult` = 1)
LDF <- 1.05 * LDF
GenInsUlt <- t(t(GenIns)*LDF)
Part2 <- rearrangeForScheduleP(GenInsUlt)
Part2Formatted <- formatForScheduleP(Part2)
Part2 - Part3
colSums(Part2 - Part3, na.rm = TRUE)

# summaryMackChainLadder 
# Puts total row at the bottom of the data.frame
summaryMackChainLadder <- smcl <- function(object,...){
  ## Summarise my results
  Latest <- getLatestCumulative(object$Triangle)
  
  ex.origin.period <- Latest!=0
  
  Ultimate <- object[["FullTriangle"]][,ncol(object[["FullTriangle"]])]
  Dev.To.Date <- Latest/Ultimate
  IBNR <- Ultimate-Latest
  Mack.S.E <- object[["Mack.S.E"]][,ncol(object[["Mack.S.E"]])]
  CV <- Mack.S.E/(Ultimate-Latest)
  
  ByOrigin <- data.frame(Latest, Dev.To.Date, Ultimate, IBNR, Mack.S.E, CV)
  names(ByOrigin)[6]="CV(IBNR)"
  ByOrigin <- ByOrigin[ex.origin.period,]
  
  Totals <-  c(sum(Latest,na.rm=TRUE),
               NA, #sum(Latest,na.rm=TRUE)/sum(Ultimate,na.rm=TRUE),
               sum(Ultimate,na.rm=TRUE),
               sum(IBNR,na.rm=TRUE), object[["Total.Mack.S.E"]],
               object[["Total.Mack.S.E"]]/sum(IBNR,na.rm=TRUE)
  )
  rownams <- row.names(ByOrigin)
  sumnams <- c("sum", "somme", "suma")
  ByOrigin <- rbind(ByOrigin, Totals)
  if (length(w <- which(!sumnams %in% rownams))) 
    row.names(ByOrigin)[nrow(ByOrigin)] <- sumnams[w[1]]
  return(ByOrigin)
  
  
  # Totals <- c(Totals, round(x[["Total.Mack.S.E"]]/sum(res$IBNR,na.rm=TRUE),2))
  Totals <- as.data.frame(Totals)
  
  colnames(Totals)=c("Totals")
  rownames(Totals) <- c("Latest:","Dev:","Ultimate:",
                        "IBNR:","Mack S.E.:",
                        "CV(IBNR):")
  
  output <- list(ByOrigin=ByOrigin, Totals=Totals)
  return(output)
}
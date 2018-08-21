plotMackIBNR <- function(x, ...) {
  xlab <- deparse(substitute(x))
  titl <- paste0("MackChainLadder(", xlab, ") IBNR Distribution")
  M <- MackChainLadder(x, ...)
  source("../summaryMackChainLadder.r")
  S <- smcl(M)
  source("c:/Utilities/lognormal.r")
  mu <- S["sum", "IBNR"]
  cv <- S["sum", "CV(IBNR)"]
  sd <- mu * cv
  lp <- lnormParms(mu, sd)
  meanlog <- lp["mu"]
  sdlog <- lp["sigma"]
  
  p <- c(seq(.001, .95, by = .001), seq(.9501, .999, by = .0001))
  q <- qlnorm(p, meanlog, sdlog)
  y <- dlnorm(q, meanlog, sdlog)
  
  dat <- data.frame(q, y, 
                    Carried = cut(q, 
                                  breaks = 
                                    c(0, q[p==.4], q[p==.8], q[p==.99], Inf),
                                  labels = c("<=40", "40-80", "80-99", "Wow!"))
  )
  #dat <- cbind(dat, fill = cut(p, ))
  ## Color the area under the curve between -1.2 and 1.1
  library(ggplot2)
  P <- ggplot(data = dat, mapping = aes(x = q, y = y))
  #P + geom_line()
  #P + geom_area(aes(fill = fill), alpha = .8)
  #P + geom_area(aes(fill = fill, alpha = c(.8, .8, .8, .8)))
  library(scales)
  P <- P + geom_area(alpha = .8, aes(fill = Carried)) + 
    scale_fill_manual(values=c("tomato", "green", "steelblue", "goldenrod")) +
    ylab("density") + xlab("IBNR") +
    scale_x_continuous(labels = dollar) +
    ggtitle(titl) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
#  C:\Users\Dan\Documents\GitHub\IBNR\CLRS_Session\Images
#  return(file.path("C:", "Users", "Dan", "GitHub", "IBNR", "CLRS_Session", "Images", paste(titl, "png", sep = ".")))
#  png(file.path("C:", "Users", "Dan", "GitHub", "IBNR", "CLRS_Session", "Images", paste(titl, "png", sep = "."))
#                , width = 800, height = 600)
  png(paste(xlab, "png", sep = "."), width = 800, height = 600)
  print(P)
  dev.off()
  
}
# Examples
library(ChainLadder)
library(excelRio)
M <- MackChainLadder(GenIns)
S <- smcl(M)
writeToExcel(smcl(M), "Examples.xlsx")
A <- GenIns
class(A) <- "matrix"
writeToExcel(A, "Examples.xlsx")
A <- ata(GenIns)
class(A) <- "matrix"
writeToExcel(round(A, 3), "Examples.xlsx")
plot(GenIns, main = "GenIns Development By Origin", ylab = "")
png("CLRS_Session/Images/GenInsDevelopment.png", width = 800, height = 600)
plot(GenIns, main = "GenIns Development By Origin", ylab = "")
dev.off()

bmp("CLRS_Session/Images/GenInsDevelopment.bmp", width = 800, height = 600)
plot(GenIns, main = "GenIns Development By Origin", ylab = "")
dev.off()

smcl(MackChainLadder(GenIns[-1,1:9])) # cv drops only slightly, 13% to 12%

# lognormal
S <- smcl(M)
source("c:/Utilities/lognormal.r")
mu <- S["sum", "IBNR"]
cv <- S["sum", "CV(IBNR)"]
sd <- meanlog * cv
lp <- lnormParms(mu, sd)
meanlog <- lp["mu"]
sdlog <- lp["sigma"]

#p <- c(.01, .02, .025, seq(.05, .95, by = .001), .975, .98, .99, .995, .999)
p <- c(seq(.001, .99, by = .001), seq(.991, .999, by = .0001))
q <- qlnorm(p, meanlog, sdlog)
y <- dlnorm(q, meanlog, sdlog)
plot(q, y, type = "l"
     , main = "Mack(GenIns) IBNR Distribution"
     , xlab = "IBNR"
     , ylab = "")

library(ggplot2)
dat <- data.frame(q, y, 
  Carried = cut(q, 
             breaks = 
               c(0, mu, q[p==.8], q[p==.99], Inf),
           labels = c("<=mean", "80th", "99th", ">.99"))
#             labels = c("<=mean", "80th", NA))
)
#dat <- cbind(dat, fill = cut(p, ))
## Color the area under the curve between -1.2 and 1.1
P <- ggplot(data = dat, mapping = aes(x = q, y = y))
#P + geom_line()
#P + geom_area(aes(fill = fill), alpha = .8)
#P + geom_area(aes(fill = fill, alpha = c(.8, .8, .8, .8)))
library(scales)
P + geom_area(alpha = .5, aes(fill = Carried)) + 
  scale_fill_manual(values=c("red", "green", "blue", "gold")) +
  ylab("density") + xlab("IBNR") +
  scale_x_continuous(labels = dollar) +
  ggtitle("Mack(GenIns) IBNR Distribution") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())



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
P <- ggplot(data = dat, mapping = aes(x = q, y = y))
#P + geom_line()
#P + geom_area(aes(fill = fill), alpha = .8)
#P + geom_area(aes(fill = fill, alpha = c(.8, .8, .8, .8)))
library(scales)
P <- P + geom_area(alpha = .8, aes(fill = Carried)) + 
  scale_fill_manual(values=c("tomato", "green", "steelblue", "goldenrod")) +
  ylab("density") + xlab("IBNR") +
  scale_x_continuous(labels = dollar) +
  ggtitle("Mack(GenIns) IBNR Distribution") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

png("CLRS_Session/Images/Mack(GenIns).png", width = 800, height = 600)
print(P)
dev.off()




library(excelRio)
library(magrittr)
library(dplyr)
library(cigapkg)
library(nxt) 
library(link)
library(triouts)
library(mondate)

modelsinputfile <- "models.csv"

efaoutputfile <- "EFA.xlsx"
slinkoutputfile <- "slink.xlsx"
rdataoutputfile <- "output.Rdata"
link_ratio_filterfun = NULL
link_ratio_pass2fun = NULL
wteslink <- FALSE # FALSE = don't write exhibits to excel
wteexh1 <- TRUE # FALSE = don't write exhibits to excel
wtcpredictcd <- FALSE # FALSE = don't write predictions to csv files
wtcprdf <- FALSE

#WORKINGDIR <- file.path(getwd(), "output") # Assumes start in WCIRB folder 
#od <- setwd(WORKINGDIR) # change to that
#setwd(".."); getwd()



########################
#
# DATA

# See script_createlongtriangle_metric_2019.R. Run that. Then
load("wcirbindmedlt_2019.Rdata")
#
########################

###############
#
# Actuarial data
#
models <- read.csv(
  modelsinputfile,
  header = TRUE,
  check.names = FALSE,
  stringsAsFactors = FALSE)

# link_ratio_filterfun <- NULL
# link_ratio_pass2fun <- NULL


########################
#
# ESTIMATE/SELECT LINK RATIOS
#
########################

# Pass 1

LM <- list()
# "Selected" via filterfun
for (i in 1:nrow(models)) {
  ymetric <- models$ymetric[i]
  xmetric <- models$xmetric[i]
  cdsub <- subset(cd, metric %in% union(ymetric, xmetric))
  # convert amount column into metric column
  cdsubwide <- long2widemetric(cdsub, idvar = c("ay", "age", "eval_date"))
  # drop the eval_date column
  cdsubwide <- cdsubwide[-which(names(cdsubwide) == "eval_date")]
  
  LM[[models$modelname[i]]] <-
    link_ratio(data = cdsubwide,
               ymetric = models$ymetric[i],
               xmetric = models$xmetric[i],
               timevar = "age",
               by = "ay",
               xlag = 1,
               alpha = 1,
               filterfun = link_ratio_filterfun,
               trimzeros.lag = FALSE,
               link_ratio_class = 
                 models$link_ratio_class[i])
  stopifnot(!any(
    sapply(LM[[models$modelname[i]]], 
           function(x) inherits(x, "error"))))
}

slink <- LM

########################
#
# ESTIMATE TAIL FACTORS
#
########################

# The tails are selected in models.csv.

slink <- link_ratio_appendtail(slink, models)

if (wteslink) for (nm in names(slink)) {
  sm <- summary(slink[[nm]])
  # So Inf in age.nxt doesn't come across as #NA
  sm[[7]] <- as.character(sm[[7]]) 
  writeToExcel(sm, slinkoutputfile, sheet = nm)
}
########################
#
# PREDICT FUTURE VALUES
#
########################

# We will roll-forward from the entirety of our dataset,
#   filling in missing values as discovered. 
# The predictions occur recursively. Starting at the youngest
#   age in slink, pair cd with its next value in the age direction.
#   If data exists for that ay as of that next age, so be it. If not,
#   the usual behavior of appending NA values occurs.
#   Fill in the NA values with predictions, append that 
#   to newdata, and move to the next age.
#   Repeat across all ages in slink.
# latestdiagonal <- last(cd, timevar = "age", by = "ay")

# wauto <- which(sapply(slink, .is.autoregressive))
# Starting with our entire data set (newdata = cd),
#   loop through all our models (the individual link_models) 
#   and predict the future values of that model's ymetric.
# predictdetail now does the Sum step too, adding the 
#   predictlinkSum estimate as attribute 'aggregate'.
predictcd <- list()
w <- which(sapply(slink, class)[1,] == "link_ratio_itd")
for (nm in names(slink)[w]) { # itd and case models
  
  # for these 1988=Prior runs, must recreate wide data
  ymetric <- attr(slink[[nm]], "ymetric")
  xmetric <- attr(slink[[nm]], "xmetric")
  cdsub <- subset(cd, metric %in% union(ymetric, xmetric))
  cdsubwide <- long2widemetric(cdsub, idvar = c("ay", "age", "eval_date"))
  #  cdsubwide <- subtotal_priorto(cdsubwide, priorto = 1989)
  cdsubwide <- cdsubwide[-which(names(cdsubwide) == "eval_date")]
  
  predictcd[[nm]] <- predict(object = slink[[nm]], newdata = cdsubwide)
}
w <- which(sapply(slink, class)[1,] == "link_ratio_incryx")
for (nm in names(slink)[w]) { # casepaid models
  
  # for these 1988=Prior runs, must recreate wide data
  ymetric <- attr(slink[[nm]], "ymetric")
  xmetric <- attr(slink[[nm]], "xmetric")
  cdsub <- subset(cd, metric %in% union(ymetric, xmetric))
  cdsubwide <- long2widemetric(cdsub, idvar = c("ay", "age", "eval_date"))
  #  cdsubwide <- subtotal_priorto(cdsubwide, priorto = 1989)
  cdsubwide <- cdsubwide[-which(names(cdsubwide) == "eval_date")]
  
  predictcd[[nm]] <- predict(
    object = slink[[nm]], 
    historicaldata = cdsubwide, 
    futuredata = predictcd[[attr(slink[[nm]], "xmetric")]] )
}

########################
#
# CREATE OUTPUT
#
########################

# So far we only have
#   Exhibit 1: by valuesname, a dataframe
#      ay       latest    predicted            D            G          MSE
nms <- names(predictcd)
for (i in 1:length(predictcd)) {
  print(formact(attr(predictcd[[i]], "exh1")))
  if (wteexh1) writeToExcel(attr(predictcd[[i]], "exh1"), 
                            efaoutputfile, 
                            sheet = nms[i], 
                            rowheader = FALSE)
}


# Add evaluation date and output to 16 csv files
for (nm in names(predictcd)) {
  pr <- predictcd[[nm]]
  pr$eval_date <- mondate.ymd(pr$ay-1) + pr$age
  predictcd[[nm]] <- pr
  if (wtcpredictcd) write.csv(pr, paste0(nm, ".csv"))
}

# Return predictions into long format by metric
prdf <- lapply(predictcd, function(pr) {
  res <- pr[c("ay", "age", "eval_date", "D", "G")]
  ymetric <- attr(pr, "ymetric")
  res$metric <- ymetric
  res$amount <- pr[[ymetric]]
  res
})
prdf <- do.call(rbind, prdf)
if (wtcprdf) write.csv(prdf, "prdf.csv")

# save objects for later processing
save(cd,
     models, link_ratio_filterfun, link_ratio_pass2fun,
     slink,
     predictcd,
     prdf,
     file = rdataoutputfile
)

gc()

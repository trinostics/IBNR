library(dplyr)
library(excelRio)
library(triouts)
library(mondate)
library(nxt)

#############################
#
# Exhibit 1

# copy/paste exhibit 1, calculate case metrics, turn into long format,
# Assign eval_date, calculate cypd metrics

exhibit1 <- pasteFromExcel(
  stringsAsFactors = FALSE,
  header = TRUE,
  rowheader = TRUE
)
exhibit1 <- cbind(exhibit1, 
              ind_itdrptd = exhibit1[, "ind_itdpaid"] + exhibit1[, "ind_case"]
                    )
exhibit1 <- cbind(exhibit1, 
              med_itdrptd = exhibit1[, "med_itdpaid"] + exhibit1[, "med_case"]
)

# copy/paste the individual triangles

metric <- "ind_itdpaid"
wt <- pasteFromExcel(
  stringsAsFactors = FALSE,
  header = TRUE,
  rowheader = TRUE
)
colnames(wt) <- seq(15, 399, by = 12)
lt <- wide2long(wt, objsname = "ay", timesname = "age", 
                valuesname = "amount")
lt$metric <- metric
df <- lt

metric <- "ind_itdrptd"
wt <- pasteFromExcel(
  stringsAsFactors = FALSE,
  header = TRUE,
  rowheader = TRUE
)
colnames(wt) <- seq(15, 399, by = 12)
lt <- wide2long(wt, objsname = "ay", timesname = "age", 
                valuesname = "amount")
lt$metric <- metric
df <- rbind(df, lt)

metric <- "med_itdpaid"
wt <- pasteFromExcel(
  stringsAsFactors = FALSE,
  header = TRUE,
  rowheader = TRUE
)
colnames(wt) <- seq(15, 399, by = 12)
lt <- wide2long(wt, objsname = "ay", timesname = "age", 
                valuesname = "amount")
lt$metric <- metric
df <- rbind(df, lt)

metric <- "med_itdrptd"
wt <- pasteFromExcel(
  stringsAsFactors = FALSE,
  header = TRUE,
  rowheader = TRUE
)
colnames(wt) <- seq(15, 399, by = 12)
lt <- wide2long(wt, objsname = "ay", timesname = "age", 
                valuesname = "amount")
lt$metric <- metric
df <- rbind(df, lt)

df$eval_date <- as.Date(mondate.ymd(df$ay) + df$age)



save(exhibit1, df, file = "wcirbindmedlt_2019_step1.Rdata")
writeToCsv(df, "wcirbindmedlt_2019.csv", overwrite = TRUE)
writeToCsv(exhibit1, "exhibit1_2019.csv", overwrite = TRUE)
#
load("wcirbindmedlt_2019_step1.Rdata")
# 

# make long version of exhibit 1
e1long <- wide2long(exhibit1, objsname = "ay", timesname = "metric")
e1long$eval_date <- as.Date("2019-03-31")
e1long <- subset(e1long, metric %in%
                   c("ind_itdpaid", "ind_itdrptd",
                     "med_itdpaid", "med_itdrptd"))

# Since we "build the triangles" by decumulating exhibit 1,
# let's cull the ay's in e1long and df so they match up.
aymutual <- sort(intersect(df$ay, e1long$ay))
df <- subset(df, ay %in% aymutual)
e1long <- subset(e1long, ay %in% aymutual)
save(exhibit1, df, e1long, file = "wcirbindmedlt_2019.Rdata")


load("wcirbindmedlt_2019.Rdata")


##########################
#
# DECUMULATE
#
# "de-cumulate" Exhibit1 indemnity and medical using triangles' ratios
# At each step, take a copy of exhibit1, replace eval_date with the
#   next younger eval_date based on the triangles,
#   and replace the values with the previous values divided by the 
#   triangles' ratios (the df "amount" field).
dates <- sort(unique(df$eval_date), decreasing = TRUE)
L = list()
L[[as.character(dates[1])]] <- e1long
e1longcopy <- e1long
for (i in 2:length(dates)) {
  e1longdf = left_join(e1longcopy, df, by = 
                   c("metric", "ay", "eval_date"))
  e1longdf$value = e1longdf$value / e1longdf$amount
  e1longdf$eval_date = dates[i]
  # Save this in our list, but w/o the df ratios
  e1longcopy <- e1longdf[c("ay", "metric", "value", "eval_date")]
  L[[as.character(dates[i])]] <- e1longcopy
}
cd <- do.call(rbind, L)
rm(e1longcopy, e1longdf, L, dates, i)
# values are NA when, for that ay, there is no eval_date in df
#   with which to match eval_date in e1longcopy
sum(is.na(cd$value)) # 144 NA values
cd <- cd[!is.na(cd$value), ]

# Can we make an indemnity paid triangle
#cd$age = as.numeric(mondate(cd$eval_date)-mondate.ymd(cd$ay-1))
#z <- long2wide(subset(cd, metric == "ind_itdpaid"))
#View(z)

##############################
#
# Calculate metrics based on existing metrics
#
# Calculate "case" metrics
# indemnity

indpaid <- subset(cd, metric == "ind_itdpaid")
indrptd <- subset(cd, metric == "ind_itdrptd")
stopifnot(all(dim(indpaid) == dim(indrptd)))
indcase <- full_join(indrptd, indpaid, by = c("ay", "eval_date"))
indcase$metric = "ind_case"
indcase$value = indcase$value.x - indcase$value.y
indcase = indcase[c("ay", "metric", "value", "eval_date")]

# medical

medpaid <- subset(cd, metric == "med_itdpaid")
medrptd <- subset(cd, metric == "med_itdrptd")
stopifnot(all(dim(medpaid) == dim(medrptd)))
medcase <- full_join(medrptd, medpaid, by = c("ay", "eval_date"))
medcase$metric = "med_case"
medcase$value = medcase$value.x - medcase$value.y
medcase = medcase[c("ay", "metric", "value", "eval_date")]

# Calculate "loss" metrics
# rptd
temp <- rbind(indrptd, medrptd)
lossrptd <- aggregate(
  temp["value"],
  by = temp[c("ay", "eval_date")],
  FUN = sum
)
lossrptd$metric = "loss_itdrptd"
rm(temp)
# paid
temp <- rbind(indpaid, medpaid)
losspaid <- aggregate(
  temp["value"],
  by = temp[c("ay", "eval_date")],
  FUN = sum
)
losspaid$metric = "loss_itdpaid"
rm(temp)
# case
temp <- rbind(indcase, medcase)
losscase <- aggregate(
  temp["value"],
  by = temp[c("ay", "eval_date")],
  FUN = sum
)
losscase$metric = "loss_case"
rm(temp)

# calendar year paids
# indemnity
temp <- prior(indpaid, timevar = "eval_date", by = c("metric", "ay"))
temp <- full_join(temp, indpaid, by = c("ay", "metric", "eval_date"))
temp$value = temp$value.y - temp$value.x
temp$metric = "ind_cypd"
indcypd = temp[c("ay", "metric", "value", "eval_date")]
rm(temp)
# medical
temp <- prior(medpaid, timevar = "eval_date", by = c("metric", "ay"))
temp <- full_join(temp, medpaid, by = c("ay", "metric", "eval_date"))
temp$value = temp$value.y - temp$value.x
temp$metric = "med_cypd"
medcypd = temp[c("ay", "metric", "value", "eval_date")]
rm(temp)
# loss
temp <- prior(losspaid, timevar = "eval_date", by = c("metric", "ay"))
temp <- full_join(temp, losspaid, by = c("ay", "metric", "eval_date"))
temp$value = temp$value.y - temp$value.x
temp$metric = "loss_cypd"
losscypd = temp[c("ay", "metric", "value", "eval_date")]
rm(temp)

cd <- rbind(
  cd,
  indcase, medcase,
  losspaid, lossrptd, losscase,
  indcypd, medcypd, losscypd
)
cd$age = as.numeric(mondate(cd$eval_date) - mondate.ymd(cd$ay - 1))
rm(indrptd, indpaid, indcase,
   medrptd, medpaid, medcase,
   losspaid, lossrptd, losscase,
   indcypd, medcypd, losscypd
)
save(exhibit1, df, e1long, cd, file = "wcirbindmedlt_2019.Rdata")
load("wcirbindmedlt_2019.Rdata")

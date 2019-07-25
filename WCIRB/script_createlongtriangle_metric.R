library(excelRio)
library(triouts)
library(mondate)

metric <- "ind_itdpaid"
wt <- pasteFromExcel(
  stringsAsFactors = FALSE,
  header = TRUE,
  rowheader = TRUE
)
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
lt <- wide2long(wt, objsname = "ay", timesname = "age", 
                valuesname = "amount")
lt$metric <- metric
df <- rbind(df, lt)

df$eval_date <- mondate.ymd(df$ay - 1) + df$age

save(df, file = "wcirbindmedlt.Rdata")
writeToCsv(df, "wcirbindmedlt.csv")

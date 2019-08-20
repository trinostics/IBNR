# average_ibnr.R
library(link)
rdataoutputfile <- "output.Rdata"

load(file = rdataoutputfile)
length(predictcd)
dim(models) # 12 models
names(predictcd)
models[1]
lapply(predictcd, function(x) exh1(x)[1,])

ind_rptd.ibnr <- exh1(predictcd[["ind_itdrptd"]])[1, "ind_itdrptd.future"]

ind_paid.ibnr <- exh1(predictcd[["ind_itdpaid"]])[1, "ind_itdpaid.future"] - 
  exh1(predictcd[["ind_case"]])[1, "ind_case.latest"]

ind_casepaid.ibnr <- exh1(predictcd[["ind_casepaid"]])[1, "ind_cypd.future"] - 
  exh1(predictcd[["ind_case"]])[1, "ind_case.latest"]

med_rptd.ibnr <- exh1(predictcd[["med_itdrptd"]])[1, "med_itdrptd.future"]

med_paid.ibnr <- exh1(predictcd[["med_itdpaid"]])[1, "med_itdpaid.future"] - 
  exh1(predictcd[["med_case"]])[1, "med_case.latest"]

med_casepaid.ibnr <- exh1(predictcd[["med_casepaid"]])[1, "med_cypd.future"] - 
  exh1(predictcd[["med_case"]])[1, "med_case.latest"]

# indemnity plus medical
ipm_rptd.ibnr = ind_rptd.ibnr + med_rptd.ibnr
ipm_paid.ibnr = ind_paid.ibnr + med_paid.ibnr
ipm_casepaid.ibnr = ind_casepaid.ibnr + med_casepaid.ibnr

loss_rptd.ibnr <- exh1(predictcd[["loss_itdrptd"]])[1, "loss_itdrptd.future"]

loss_paid.ibnr <- exh1(predictcd[["loss_itdpaid"]])[1, "loss_itdpaid.future"] - 
  exh1(predictcd[["loss_case"]])[1, "loss_case.latest"]

loss_casepaid.ibnr <- exh1(predictcd[["loss_casepaid"]])[1, "loss_cypd.future"] - 
  exh1(predictcd[["loss_case"]])[1, "loss_case.latest"]

ibnr <- mean(c(
  ipm_rptd.ibnr,
  ipm_paid.ibnr,
  ipm_casepaid.ibnr,
  loss_rptd.ibnr,
  loss_paid.ibnr,
  loss_casepaid.ibnr
))

barplot(c(
  ipm_rptd.ibnr,
  ipm_paid.ibnr,
  ipm_casepaid.ibnr,
  loss_rptd.ibnr,
  loss_paid.ibnr,
  loss_casepaid.ibnr
))
abline(h=ibnr)
# Standard error
ind_rptd.se <- sqrt(
  exh1(predictcd[["ind_itdrptd"]])$D[1]^2 + 
  exh1(predictcd[["ind_itdrptd"]])$G[1]^2
)
ind_paid.se <- sqrt(
  exh1(predictcd[["ind_itdpaid"]])$D[1]^2 + 
    exh1(predictcd[["ind_itdpaid"]])$G[1]^2
)
# ind_casepaid.se <- sqrt(
#   exh1(predictcd[["ind_itdcasepaid"]])$D[1]^2 + 
#     exh1(predictcd[["ind_itdcasepaid"]])$G[1]^2
# )

med_rptd.se <- sqrt(
  exh1(predictcd[["med_itdrptd"]])$D[1]^2 + 
    exh1(predictcd[["med_itdrptd"]])$G[1]^2
)
med_paid.se <- sqrt(
  exh1(predictcd[["med_itdpaid"]])$D[1]^2 + 
    exh1(predictcd[["med_itdpaid"]])$G[1]^2
)
# med_casepaid.se <- sqrt(
#   exh1(predictcd[["med_itdcasepaid"]])$D[1]^2 + 
#     exh1(predictcd[["med_itdcasepaid"]])$G[1]^2
# )

# indemnity plus medical
corr_im_rptd = .5
ipm_rptd.se = sqrt(
  ind_rptd.se^2 + med_rptd.se^2 +
    2 * ind_rptd.se * med_rptd.se * corr_im_rptd
  )
corr_im_paid = .5
ipm_paid.se = sqrt(
  ind_paid.se^2 + med_paid.se^2 +
    2 * ind_paid.se * med_paid.se * corr_im_paid
)
# corr_im_casepaid = .5
# ipm_casepaid.se = sqrt(
#   ind_casepaid.se^2 + med_casepaid.se^2 +
#     2 * ind_casepaid.se * med_casepaid.se * corr_im_casepaid
# )

loss_rptd.se <- sqrt(
  exh1(predictcd[["loss_itdrptd"]])$D[1]^2 + 
    exh1(predictcd[["loss_itdrptd"]])$G[1]^2
)
loss_paid.se <- sqrt(
  exh1(predictcd[["loss_itdpaid"]])$D[1]^2 + 
    exh1(predictcd[["loss_itdpaid"]])$G[1]^2
)
# loss_casepaid.se <- sqrt(
#   exh1(predictcd[["loss_itdcasepaid"]])$D[1]^2 + 
#     exh1(predictcd[["loss_itdcasepaid"]])$G[1]^2
# )
corr_ipm_rptd_ipm_paid = .5
corr_ipm_rptd_loss_rptd = .5
corr_ipm_rptd_loss_paid = .5
corr_ipm_paid_loss_rptd = .5
corr_ipm_paid_loss_paid = .5
corr_loss_rptd_loss_paid = .5
se4methods = sqrt(
  (
    ipm_rptd.se^2 +
      ipm_paid.se^2 +
      loss_rptd.se^2 +
      loss_paid.se^2 +
      2 * ipm_rptd.se * ipm_paid.se * corr_ipm_rptd_ipm_paid +
      2 * ipm_rptd.se * loss_rptd.se * corr_ipm_rptd_loss_rptd +
      2 * ipm_rptd.se * loss_paid.se * corr_ipm_rptd_loss_paid +
      2 * ipm_paid.se * loss_rptd.se * corr_ipm_paid_loss_rptd +
      2 * ipm_paid.se * loss_paid.se * corr_ipm_paid_loss_paid +
      2 * loss_rptd.se * loss_paid.se * corr_loss_rptd_loss_paid
  )
) / 16

barplot(c(
  ipm_rptd.se,
  ipm_paid.se,
#  ipm_casepaid.se,
  loss_rptd.se,
  loss_paid.se
#  loss_casepaid.se
))
abline(h = se4methods)

ibnr4methods = mean(c(
  ipm_rptd.ibnr,
  ipm_paid.ibnr,
  loss_rptd.ibnr,
  loss_paid.ibnr
))
barplot(c(
  ipm_rptd.ibnr,
  ipm_paid.ibnr,
  loss_rptd.ibnr,
  loss_paid.ibnr
))

ipm_rptd.cv = ipm_rptd.se / ipm_rptd.ibnr
ipm_paid.cv = ipm_paid.se / ipm_paid.ibnr
loss_rptd.cv = loss_rptd.se / loss_rptd.ibnr
loss_paid.cv = loss_paid.se / loss_paid.ibnr
cv4methods = se4methods / ibnr4methods

barplot(c(
  ipm_rptd.cv,
  ipm_paid.cv,
  loss_rptd.cv,
  loss_paid.cv
))
abline(h=cv4methods)

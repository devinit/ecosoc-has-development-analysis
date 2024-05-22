#### Setup ####
list.of.packages <- c("rstudioapi", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- dirname(getActiveDocumentContext()$path) 
setwd(wd)
setwd("../")
#### End setup ####

dat = fread("data/merged_crs_iati.csv")
dat = subset(dat, !is.na(sector_code))
dat$humanitarian = ifelse(
  dat$sector_code %in% c(720, 730, 740),
  "Humanitarian",
  "Development"
)
dat = dat[,.(
  usd_disbursement_crs=sum(usd_disbursement_crs),
  usd_disbursement_iati=sum(usd_disbursement_iati)
),
by=.(year, humanitarian, recipient_name, sector_code)]

dat_train = subset(dat, year < 2022)
dat_test = subset(dat, year == 2022)

fit = lm(usd_disbursement_crs~0+usd_disbursement_iati+recipient_name+sector_code, data=dat_train)
summary(fit)
confidence = predict.lm(fit, newdata = dat_test, interval = "confidence")
dat_test$usd_disbursement_iati_fit = confidence[,1]
dat_test$usd_disbursement_iati_lwr = confidence[,2]
dat_test$usd_disbursement_iati_upr = confidence[,3]
plot(usd_disbursement_crs~usd_disbursement_iati, data=dat_test)
abline(0,1)
original_rmse = sqrt(mean((dat_test$usd_disbursement_crs - dat_test$usd_disbursement_iati)^2))
original_rmse
plot(usd_disbursement_crs~usd_disbursement_iati_fit, data=dat_test)
abline(0,1)
fit_rmse = sqrt(mean((dat_test$usd_disbursement_crs - dat_test$usd_disbursement_iati_fit)^2))
fit_rmse


dat_train = subset(dat, year < 2023)
dat_predict = subset(dat, year < 2024)
fit = lm(usd_disbursement_crs~0+usd_disbursement_iati+recipient_name+sector_code, data=dat_train)
summary(fit)
confidence = predict.lm(fit, newdata = dat_predict, interval = "confidence")
dat_predict$usd_disbursement_crs_fit = confidence[,1]
dat_predict$usd_disbursement_crs_lwr = confidence[,2]
dat_predict$usd_disbursement_crs_upr = confidence[,3]
dat_predict$usd_disbursement_crs[which(dat_predict$year==2023)] = dat_predict$usd_disbursement_crs_fit[which(dat_predict$year==2023)]
fwrite(dat_predict, "data/modeled_crs_iati.csv")
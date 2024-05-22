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
dat_train = subset(dat, year < 2022)
dat_test = subset(dat, year == 2022)
dat_predict = subset(dat, year == 2023)
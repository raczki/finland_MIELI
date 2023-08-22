

### Con todas las variables

rm(list=ls())

library(magrittr) 
library(dplyr) 

#install.packages("readxl")
library(readxl)
total <- read_excel("C:/Users/Karen/Desktop/Tesis_Maestría/Modelos_C/base.xlsx")

#install.packages("Synth")
library(Synth)

total$id <- as.numeric(factor(total$country, levels=unique(total$country)))
total <- as.data.frame(total)

options(scipen=999)

errorV <- vector("numeric", 24L)

actual <- matrix(nrow=21, ncol=24, dimnames = list(c(1995:2015), c("Austria", "Belgium",  "Croatia", "Czech", "Denmark",
                                                                   "Estonia", "Finland", "Germany", "Greece", "Hungary", "Iceland",
                                                                   "Italy", "Latvia", "Lithuania", "Luxembourg", "Netherlands",
                                                                   "Norway", "Romania", "Russia", "Slovenia", "Spain", "Sweden",
                                                                   "Switzerland", "UK")))
predicted <- matrix(nrow=21, ncol=24, dimnames = list(c(1995:2015), c("Austria", "Belgium",  "Croatia", "Czech", "Denmark",
                                                                      "Estonia", "Finland", "Germany", "Greece", "Hungary", "Iceland",
                                                                      "Italy", "Latvia", "Lithuania", "Luxembourg", "Netherlands",
                                                                      "Norway", "Romania", "Russia", "Slovenia", "Spain", "Sweden",
                                                                      "Switzerland", "UK")))


dataprep.out <-
  dataprep(foo = total,
           predictors = c("1995","1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004",
                          "2005", "2006", "2007"),
           predictors.op = "mean",
           dependent = "sui_rate",
           unit.variable = "id",
           time.variable = "year",
           treatment.identifier = 23,
           controls.identifier = c(1:22,24),
           time.predictors.prior = c(1995:2008),
           time.optimize.ssr = c(1995:2008),
           unit.names.variable = "country",
           time.plot = 1995:2015
  )

synth.out <- synth(dataprep.out, method = "BFGS")

gaps<- dataprep.out$Y1plot-(
  dataprep.out$Y0plot%*%synth.out$solution.w
) ; gaps
gaps <- as.data.frame(gaps)
year <- c(1995:2015)
gaps_f <- cbind(year,gaps)
colnames(gaps_f)[1] <- "years"
colnames(gaps_f)[2] <- "Austria"
errorV[23] <- synth.out$loss.v

actual[,23] <- dataprep.out$Y1plot
predicted[,23 <- dataprep.out$Y0plot%*%synth.out$solution.w

library(writexl)
gaps_f <- as.data.frame(gaps_f)
write_xlsx(gaps_f,"C:/Users/Karen/Desktop/Tesis_Maestría/Modelos_C/gapsC4extra.xlsx")
errorV <- as.data.frame(errorV)
write_xlsx(errorV,"C:/Users/Karen/Desktop/Tesis_Maestría/Modelos_C/errorC4extra.xlsx")

actual <- as.data.frame(actual)
predicted <- as.data.frame(predicted)
write_xlsx(actual,"C:/Users/Karen/Desktop/Tesis_Maestría/Modelos_C/actualsC4extra.xlsx")
write_xlsx(predicted,"C:/Users/Karen/Desktop/Tesis_Maestría/Modelos_C/predsC4extra.xlsx")

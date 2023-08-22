

rm(list=ls())

library(readxl)
total <- read_excel("C:/Users/Karen/Desktop/Tesis_Maestría/suiEU.xlsm")

#install.packages("Synth")
library(Synth)

total$id <- as.numeric(factor(total$country, levels=unique(total$country)))
total <- as.data.frame(total)

options(scipen=999)

#################### ESPECIFICACION SELECCIONADA

### MODELO C.1.

dataprep.out <-
  dataprep(foo = total,
           predictors = c("RUC_total", "noreligion_all", "vuln", "alcohol"),
           predictors.op = "mean",
           dependent = "sui_rate",
           unit.variable = "id",
           time.variable = "year",
           treatment.identifier = 7,
           controls.identifier = c(1:6, 8:24),
           time.predictors.prior = c(1995:2008),
           time.optimize.ssr = c(1995:2008),
           unit.names.variable = "country",
           time.plot = 1995:2015
  )

synth.out <- synth(dataprep.out, method = "BFGS")

round(synth.out$solution.w,2)
synth.out$solution.v 

gaps<- dataprep.out$Y1plot-(
  dataprep.out$Y0plot%*%synth.out$solution.w
) ; gaps

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
print(synth.tables)

x11()
path.plot(dataprep.res = dataprep.out, synth.res = synth.out,
          Ylab = "Suicidios", Xlab = "Año",
          Legend = c("Finlandia", "Finlandia sintético"), Legend.position = "bottomright", tr.intake = 2009,
          Ylim = c(10,30))

x11()
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out, tr.intake = 2009)


################## LOOP INFERENCIA

errorV <- vector("numeric", 24L)

actual <- matrix(nrow=21, ncol=24, dimnames= list(c(1995:2015), c("Austria", "Belgium", "Croatia",  "Czech", "Denmark",
                                                                  "Estonia", "Finland", "Germany", "Greece", "Hungary", "Iceland",
                                                                  "Italy","Latvia", "Lithuania", "Luxembourg", "Netherlands",
                                                                  "Norway", "Romania", "Russia", "Slovenia", "Spain", "Sweden",
                                                                  "Switzarland", "UK")))
predicted <- matrix(nrow=21, ncol=24, dimnames= list(c(1995:2015), c("Austria", "Belgium", "Croatia",  "Czech", "Denmark",
                                                                     "Estonia", "Finland", "Germany", "Greece", "Hungary", "Iceland",
                                                                     "Italy","Latvia", "Lithuania", "Luxembourg", "Netherlands",
                                                                     "Norway", "Romania", "Russia", "Slovenia", "Spain", "Sweden",
                                                                     "Switzarland", "UK")))
for(x in 1:24){
  if (x==1){
    dataprep.out <-
      dataprep(foo = total,
               predictors = c("RUC_total", "noreligion_all", "vuln", "alcohol"),
               predictors.op = "mean",
               dependent = "sui_rate",
               unit.variable = "id",
               time.variable = "year",
               treatment.identifier = x,
               controls.identifier = c((x+1):24),
               time.predictors.prior = c(1995:2008),
               time.optimize.ssr = c(1995:2008),
               unit.names.variable = "country",
               time.plot = 1995:2015)
    synth.out <- synth(dataprep.out, method = "BFGS")
    gaps<- dataprep.out$Y1plot-(dataprep.out$Y0plot%*%synth.out$solution.w) ; gaps
    gaps <- as.data.frame(gaps)
    year <- c(1995:2015)
    gaps_f <- cbind(year,gaps)
    colnames(gaps_f)[1] <- "years"
    colnames(gaps_f)[2] <- "Austria"
    errorV[x] <- synth.out$loss.v
    
    actual[,x] <- dataprep.out$Y1plot
    predicted[,x] <- dataprep.out$Y0plot%*%synth.out$solution.w
  }
  else if (x==2){
    dataprep.out <-
      dataprep(foo = total,
               predictors = c("RUC_total", "noreligion_all", "vuln", "alcohol"),
               predictors.op = "mean",
               dependent = "sui_rate",
               unit.variable = "id",
               time.variable = "year",
               treatment.identifier = x,
               controls.identifier = c(1,(x+1):24),
               time.predictors.prior = c(1995:2008),
               time.optimize.ssr = c(1995:2008),
               unit.names.variable = "country",
               time.plot = 1995:2015)
    synth.out <- synth(dataprep.out, method = "BFGS")
    gaps<- dataprep.out$Y1plot-(
      dataprep.out$Y0plot%*%synth.out$solution.w
    ) ; gaps
    gaps <- as.data.frame(gaps)
    year <- c(1995:2015)
    gaps_f <- cbind(gaps_f,gaps)
    errorV[x] <- synth.out$loss.v
    
    actual[,x] <- dataprep.out$Y1plot
    predicted[,x] <- dataprep.out$Y0plot%*%synth.out$solution.w
  }
  else if (x<24){
    dataprep.out <-
      dataprep(foo = total,
               predictors = c("RUC_total", "noreligion_all", "vuln", "alcohol"),
               predictors.op = "mean",
               dependent = "sui_rate",
               unit.variable = "id",
               time.variable = "year",
               treatment.identifier = x,
               controls.identifier = c(1:(x-1),(x+1):24),
               time.predictors.prior = c(1995:2008),
               time.optimize.ssr = c(1995:2008),
               unit.names.variable = "country",
               time.plot = 1995:2015)
    synth.out <- synth(dataprep.out, method = "BFGS")
    gaps<- dataprep.out$Y1plot-(
      dataprep.out$Y0plot%*%synth.out$solution.w
    ) ; gaps
    gaps <- as.data.frame(gaps)
    year <- c(1995:2015)
    gaps_f <- cbind(gaps_f,gaps)
    errorV[x] <- synth.out$loss.v
    
    actual[,x] <- dataprep.out$Y1plot
    predicted[,x] <- dataprep.out$Y0plot%*%synth.out$solution.w
  }
  else if (x==24){
    dataprep.out <-
      dataprep(foo = total,
               predictors = c("RUC_total", "noreligion_all", "vuln", "alcohol"),
               predictors.op = "mean",
               dependent = "sui_rate",
               unit.variable = "id",
               time.variable = "year",
               treatment.identifier = x,
               controls.identifier = c(1:(x-1)),
               time.predictors.prior = c(1995:2008),
               time.optimize.ssr = c(1995:2008),
               unit.names.variable = "country",
               time.plot = 1995:2015)
    synth.out <- synth(dataprep.out, method = "BFGS")
    gaps<- dataprep.out$Y1plot-(
      dataprep.out$Y0plot%*%synth.out$solution.w
    ) ; gaps
    gaps <- as.data.frame(gaps)
    year <- c(1995:2015)
    gaps_f <- cbind(gaps_f,gaps)
    errorV[x] <- synth.out$loss.v
    
    actual[,x] <- dataprep.out$Y1plot
    predicted[,x] <- dataprep.out$Y0plot%*%synth.out$solution.w
  }
}

colnames(gaps_f)[3] <- "Belgium"
colnames(gaps_f)[4] <- "Croatia"
colnames(gaps_f)[5] <- "Czech"
colnames(gaps_f)[6] <- "Denmark"
colnames(gaps_f)[7] <- "Estonia"
colnames(gaps_f)[8] <- "Finland"
colnames(gaps_f)[9] <- "Germany"
colnames(gaps_f)[10] <- "Greece"
colnames(gaps_f)[11] <- "Hungary"
colnames(gaps_f)[12] <- "Iceland"
colnames(gaps_f)[13] <- "Italy"
colnames(gaps_f)[14] <- "Latvia"
colnames(gaps_f)[15] <- "Lithuania"
colnames(gaps_f)[16] <- "Luxembourg"
colnames(gaps_f)[17] <- "Netherlands"
colnames(gaps_f)[18] <- "Norway"
colnames(gaps_f)[19] <- "Romania"
colnames(gaps_f)[20] <- "Russia"
colnames(gaps_f)[21] <- "Slovenia"
colnames(gaps_f)[22] <- "Spain"
colnames(gaps_f)[23] <- "Sweden"
colnames(gaps_f)[24] <- "Switzerland"
colnames(gaps_f)[25] <- "UK"


errorV <- as.data.frame(errorV)
library(writexl)
write_xlsx(gaps_f,"C:/Users/Karen/Desktop/Tesis_Maestría/Modelos_C/gapsC3.xlsx")
write_xlsx(errorV,"C:/Users/Karen/Desktop/Tesis_Maestría/Modelos_C/errorC3.xlsx")

actual <- as.data.frame(actual)
predicted <- as.data.frame(predicted)
write_xlsx(actual,"C:/Users/Karen/Desktop/Tesis_Maestría/Modelos_C/actualsC3.xlsx")
write_xlsx(predicted,"C:/Users/Karen/Desktop/Tesis_Maestría/Modelos_C/predsC3.xlsx")
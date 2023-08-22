
rm(list=ls())

library(readxl)
total <- read_excel("C:/Users/Karen/Desktop/Tesis_Maestría/suiEU.xlsm")

#install.packages("Synth")
library(Synth)

###############################################################
#Saco los siguientes para evitar sesgo de interpolacion
total <- total[!grepl("Greece", total$country),]
total <- total[!grepl("Italy", total$country),]
total <- total[!grepl("Spain", total$country),]
total <- total[!grepl("Croatia", total$country),]
total <- total[!grepl("Romania", total$country),]
total <- total[!grepl("Hungary", total$country),]

total <- total[!grepl("Czech Republic", total$country),]
total <- total[!grepl("Estonia", total$country),]
total <- total[!grepl("Iceland", total$country),]
total <- total[!grepl("Latvia", total$country),]
total <- total[!grepl("Russian Federation", total$country),]
total <- total[!grepl("Slovenia", total$country),]
total <- total[!grepl("Lithuania", total$country),]
############################################################

total$id <- as.numeric(factor(total$country, levels=unique(total$country)))
total <- as.data.frame(total)

options(scipen=999)

#################### ESPECIFICACION SELECCIONADA

### MODELO B.1.

dataprep.out <-
  dataprep(foo = total,
           predictors = c("RUC_total", "GDP_pc", "GDP", "rural", "neonatal"),
           predictors.op = "mean",
           dependent = "sui_rate",
           unit.variable = "id",
           time.variable = "year",
           treatment.identifier = 4,
           controls.identifier = c(1:3, 5:11),
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

errorV <- vector("numeric", 11L)

actual <- matrix(nrow=21, ncol=11, dimnames= list(c(1995:2015), c("Austria", "Belgium", "Denmark",
                                                                  "Finland", "Germany",
                                                                  "Luxembourg", "Netherlands",
                                                                  "Norway", "Sweden", "Switzerland", "UK")))
predicted <- matrix(nrow=21, ncol=11, dimnames= list(c(1995:2015), c("Austria", "Belgium", "Denmark",
                                                                     "Finland", "Germany",
                                                                     "Luxembourg", "Netherlands",
                                                                     "Norway", "Sweden", "Switzerland", "UK")))
for(x in 1:11){
  if (x==1){
    dataprep.out <-
      dataprep(foo = total,
               predictors = c("RUC_total", "GDP_pc", "GDP", "rural", "neonatal"),
               predictors.op = "mean",
               dependent = "sui_rate",
               unit.variable = "id",
               time.variable = "year",
               treatment.identifier = x,
               controls.identifier = c((x+1):11),
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
               predictors = c("RUC_total", "GDP_pc", "GDP", "rural", "neonatal"),
               predictors.op = "mean",
               dependent = "sui_rate",
               unit.variable = "id",
               time.variable = "year",
               treatment.identifier = x,
               controls.identifier = c(1,(x+1):11),
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
  else if (x<11){
    dataprep.out <-
      dataprep(foo = total,
               predictors = c("RUC_total", "GDP_pc", "GDP", "rural", "neonatal"),
               predictors.op = "mean",
               dependent = "sui_rate",
               unit.variable = "id",
               time.variable = "year",
               treatment.identifier = x,
               controls.identifier = c(1:(x-1),(x+1):11),
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
  else if (x==11){
    dataprep.out <-
      dataprep(foo = total,
               predictors = c("RUC_total", "GDP_pc", "GDP", "rural", "neonatal"),
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
colnames(gaps_f)[4] <- "Denmark"
colnames(gaps_f)[5] <- "Finland"
colnames(gaps_f)[6] <- "Germany"
colnames(gaps_f)[7] <- "Luxembourg"
colnames(gaps_f)[8] <- "Netherlands"
colnames(gaps_f)[9] <- "Norway"
colnames(gaps_f)[10] <- "Sweden"
colnames(gaps_f)[11] <- "Switzerland"
colnames(gaps_f)[12] <- "UK"

errorV <- as.data.frame(errorV)
library(writexl)
write_xlsx(gaps_f,"C:/Users/Karen/Desktop/Tesis_Maestría/Modelos_B/gapsB1.xlsx")
write_xlsx(errorV,"C:/Users/Karen/Desktop/Tesis_Maestría/Modelos_B/errorB1.xlsx")

actual <- as.data.frame(actual)
predicted <- as.data.frame(predicted)
write_xlsx(actual,"C:/Users/Karen/Desktop/Tesis_Maestría/Modelos_B/actualsB1.xlsx")
write_xlsx(predicted,"C:/Users/Karen/Desktop/Tesis_Maestría/Modelos_B/predsB1.xlsx")





### Con todas las variables

rm(list=ls())

library(magrittr) 
library(dplyr) 

#install.packages("readxl")
library(readxl)
total <- read_excel("C:/Users/Karen/Desktop/Tesis_Maestr�a/Modelos_C/base.xlsx")

#install.packages("Synth")
library(Synth)

total$id <- as.numeric(factor(total$country, levels=unique(total$country)))
total <- as.data.frame(total)

options(scipen=999)

dataprep.out <-
  dataprep(foo = total,
           predictors = c("1995","1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004",
                          "2005", "2006", "2007"),
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
          Ylab = "Suicidios", Xlab = "A�o",
          Legend = c("Finlandia", "Finlandia sint�tico"), Legend.position = "bottomright", tr.intake = 2009,
          Ylim = c(10,30))

x11()
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out, tr.intake = 2009)


year <- c(1995:2015)
MSPE <- (dataprep.out$Y1plot-(dataprep.out$Y0plot%*%synth.out$solution.w))^2
MSPE <- as.data.frame(MSPE)

MSPE <- cbind(year,MSPE)
MSPE_pre_x <- MSPE %>% filter(year<2009)
MSPE_post_x <- MSPE %>% filter(year>2008)

MSPE_total_x <- mean(MSPE$`7`)
MSPE_pre_x <- mean(MSPE_pre_x$`7`)  
MSPE_post_x <- mean(MSPE_post_x$`7`)  

MAPE <- abs((dataprep.out$Y0plot%*%synth.out$solution.w)-dataprep.out$Y1plot)/dataprep.out$Y1plot
MAPE <- as.data.frame(MAPE)

MAPE <- cbind(year,MAPE)
MAPE_pre_x <- MAPE %>% filter(year<2009)
MAPE_post_x <- MAPE %>% filter(year>2008)

MAPE_total_x <- mean(MAPE$w.weight)
MAPE_pre_x <- mean(MAPE_pre_x$w.weight)  
MAPE_post_x <- mean(MAPE_post_x$w.weight)  

########################################################## LOOP INFERENCIA

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
for(x in 1:24){
  if (x==1){
    dataprep.out <-
      dataprep(foo = total,
               predictors = c("1995","1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004",
                              "2005", "2006", "2007"),
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
    gaps<- dataprep.out$Y1plot-(
      dataprep.out$Y0plot%*%synth.out$solution.w
    ) ; gaps
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
               predictors = c("1995","1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004",
                              "2005", "2006", "2007"),
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
               predictors = c("1995","1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004",
                              "2005", "2006", "2007"),
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
               predictors = c("1995","1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004",
                              "2005", "2006", "2007"),
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

library(writexl)
gaps_f <- as.data.frame(gaps_f)
write_xlsx(gaps_f,"C:/Users/Karen/Desktop/Tesis_Maestr�a/Modelos_C/gapsC4.xlsx")
errorV <- as.data.frame(errorV)
write_xlsx(errorV,"C:/Users/Karen/Desktop/Tesis_Maestr�a/Modelos_C/errorC4.xlsx")

actual <- as.data.frame(actual)
predicted <- as.data.frame(predicted)
write_xlsx(actual,"C:/Users/Karen/Desktop/Tesis_Maestr�a/Modelos_C/actualsC4.xlsx")
write_xlsx(predicted,"C:/Users/Karen/Desktop/Tesis_Maestr�a/Modelos_C/predsC4.xlsx")






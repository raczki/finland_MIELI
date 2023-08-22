
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

#total <- total[!grepl("Luxembourg", total$country),]
#total <- total[!grepl("Netherlands", total$country),]
#total <- total[!grepl("Belgium", total$country),]
total <- total[!grepl("United Kingdom", total$country),]
total <- total[!grepl("Switzerland", total$country),]

############################################################

total$id <- as.numeric(factor(total$country, levels=unique(total$country)))
total <- as.data.frame(total)

options(scipen=999)

library("plyr")

var <- c("RUC_total","GDP_pc","GDP","life_exp","christianity_all","islam_all","noreligion_all","sun",
         "rural","maternal","infla","under5","vuln","neonatal","alcohol","health_exp")

all <- ldply(1:16, function(x)t(combn(var,x)))
all[seq(1,nrow(all),1),]

# 65535 combinaciones
# 16 variables

# min 10 variables por modelo me da 14893 combinaciones

lossV <- vector("numeric", 65535L)
lossW <- vector("numeric", 65535L)


for(x in 47633:50643) {
  list2 <- data.frame(all[x,])
  list3 = NULL
  for(k in 10:16) {
    if(is.na(list2[1,k])==TRUE && is.na(list2[1,k-1])==FALSE) {
      list3 <- data.frame(list2[1,c(1:k-1)])
    } else if(k==16 && is.na(list2[1,k])==FALSE) {
      list3 <- data.frame(list2)
    }
  }
  list4 <- data.frame(lapply(list3,as.character),stringsAsFactors = FALSE)
  list4 <- as.vector(list4)
  list_vars <- as.character(unlist(list4[1,]))
  
  dataprep.out <-
    dataprep(foo = total,
             predictors = list_vars,
             predictors.op = "mean",
             dependent = "sui_rate",
             unit.variable = "id",
             time.variable = "year",
             treatment.identifier = 6,
             controls.identifier = c(1:5, 7:16),
             time.predictors.prior = c(1995:2008),
             time.optimize.ssr = c(1995:2008),
             unit.names.variable = "country",
             time.plot = 1995:2015
    )
  
  synth.out <- synth(dataprep.out, method = "BFGS")
  lossV[x] <- synth.out$loss.v
  lossW[x] <- synth.out$loss.w
  
  print(x)
}

results <- cbind(all,lossV,lossW)
library(writexl)
write_xlsx(results,"C:/Users/Karen/Desktop/Tesis_Maestría/results13.xlsx")

############################################ Loop paises #####################################

tot_paises <- c(1:6,8:16)

all_paises <- ldply(2:15, function(x)t(combn(tot_paises,x)))
all_paises[seq(1,nrow(all_paises),1),]

all_paises$treated <- 7

col_order <- c("treated","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
all_paises <- all_paises[,col_order]

# 32752 combinaciones de paises con Finlandia como tratado
# y por lo menos 2 controles (lo pide synth)

lossV <- vector("numeric", 32752L)
lossW <- vector("numeric", 32752L)

for(x in 1:3) {
  list2 <- data.frame(all_paises[x,])
  list3 = NULL
  for(k in 1:16) {
    if(is.na(list2[1,k])==TRUE && is.na(list2[1,k-1])==FALSE) {
      list3 <- data.frame(list2[1,c(1:k-1)])
    } else if(k==16 && is.na(list2[1,k])==FALSE) {
      list3 <- data.frame(list2)
    }
  }
  list4 <- data.frame(lapply(list3,as.character),stringsAsFactors = FALSE)
  list4 <- as.vector(list4)
  list_paises <- as.character(unlist(list4[1,]))

  dataprep.out <-
    dataprep(foo = total,
             predictors = c("RUC_total",	"GDP",	"christianity_all", "noreligion_all",	"sun",	"rural",
                            "alcohol"),
             predictors.op = "mean",
             dependent = "sui_rate",
             unit.variable = "id",
             time.variable = "year",
             treatment.identifier = as.numeric(list_paises[1]),
             controls.identifier = as.numeric(list_paises[2:length(list_paises)]),
             time.predictors.prior = c(1995:2008),
             time.optimize.ssr = c(1995:2008),
             unit.names.variable = "country",
             time.plot = 1995:2015
    )
  
  synth.out <- synth(dataprep.out, method = "BFGS")
  lossV[x] <- synth.out$loss.v
  lossW[x] <- synth.out$loss.w
  
  print(x)
}


paises <- cbind(all_paises,lossV,lossW)
library(writexl)
write_xlsx(paises,"C:/Users/Karen/Desktop/Tesis_Maestría/paises1.xlsx")

######################################################################################

####### ESPECIFICACION ELEGIDA

################################### A.1

dataprep.out <-
  dataprep(foo = total,
           predictors = c("RUC_total", "GDP", "christianity_all", "noreligion_all", "sun", "rural", "alcohol"),
           predictors.op = "mean",
           dependent = "sui_rate",
           unit.variable = "id",
           time.variable = "year",
           treatment.identifier = 6,
           controls.identifier = c(1:5, 7:16),
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

###################################### A.2

dataprep.out <-
  dataprep(foo = total,
           predictors = c("RUC_total", "GDP_pc", "noreligion_all", "sun", "rural", "vuln", "alcohol"),
           predictors.op = "mean",
           dependent = "sui_rate",
           unit.variable = "id",
           time.variable = "year",
           treatment.identifier = 6,
           controls.identifier = c(1:5, 7:16),
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


################## LOOP INFERENCIA

errorV <- vector("numeric", 16L)

for(x in 1:16){
  if (x==1){
    dataprep.out <-
      dataprep(foo = total,
               predictors = c("RUC_total",	"GDP",	"christianity_all", "noreligion_all",	"sun",	"rural",
                              "alcohol"),
               predictors.op = "mean",
               dependent = "sui_rate",
               unit.variable = "id",
               time.variable = "year",
               treatment.identifier = x,
               controls.identifier = c((x+1):16),
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
  }
  else if (x==2){
    dataprep.out <-
      dataprep(foo = total,
               predictors = c("RUC_total",	"GDP",	"christianity_all", "noreligion_all",	"sun",	"rural",
                              "alcohol"),
               predictors.op = "mean",
               dependent = "sui_rate",
               unit.variable = "id",
               time.variable = "year",
               treatment.identifier = x,
               controls.identifier = c(1,(x+1):16),
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
  }
  else if (x<16){
    dataprep.out <-
      dataprep(foo = total,
               predictors = c("RUC_total",	"GDP",	"christianity_all", "noreligion_all",	"sun",	"rural",
                              "alcohol"),
               predictors.op = "mean",
               dependent = "sui_rate",
               unit.variable = "id",
               time.variable = "year",
               treatment.identifier = x,
               controls.identifier = c(1:(x-1),(x+1):16),
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
  }
  else if (x==16){
    dataprep.out <-
      dataprep(foo = total,
               predictors = c("RUC_total",	"GDP",	"christianity_all", "noreligion_all",	"sun",	"rural",
                              "alcohol"),
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
  }
}

colnames(gaps_f)[3] <- "Belgium"
colnames(gaps_f)[4] <- "Czech"
colnames(gaps_f)[5] <- "Denmark"
colnames(gaps_f)[6] <- "Estonia"
colnames(gaps_f)[7] <- "Finland"
colnames(gaps_f)[8] <- "Germany"
colnames(gaps_f)[9] <- "Iceland"
colnames(gaps_f)[10] <- "Latvia"
colnames(gaps_f)[11] <- "Lithuania"
colnames(gaps_f)[12] <- "Luxembourg"
colnames(gaps_f)[13] <- "Netherlands"
colnames(gaps_f)[14] <- "Norway"
colnames(gaps_f)[15] <- "Russia"
colnames(gaps_f)[16] <- "Slovenia"
colnames(gaps_f)[17] <- "Sweden"

library(ggplot2)

x11()
ggplot(gaps_f,aes(x=year)) +
  geom_line(aes(y = Austria, colour="steelblue"), size = 0.3) +
  geom_line(aes(y = Belgium), size = 0.3) +
  geom_line(aes(y = Czech), size = 0.3) +
  geom_line(aes(y = Denmark), size = 0.3) +
  geom_line(aes(y = Estonia), size = 0.3) +
  geom_line(aes(y = Finland), size = 1) +
  geom_line(aes(y = Germany), size = 0.3) +
  geom_line(aes(y = Iceland), size = 0.3) +
  geom_line(aes(y = Latvia), size = 0.3) +
  geom_line(aes(y = Lithuania), size = 0.3) +
  geom_line(aes(y = Luxembourg), size = 0.3) +
  geom_line(aes(y = Netherlands), size = 0.3) +
  geom_line(aes(y = Norway), size = 0.3) +
  geom_line(aes(y = Russia), size = 0.3) +
  geom_line(aes(y = Slovenia), size = 0.3) +
  geom_line(aes(y = Sweden), size = 0.3) +
  
 # geom_line(aes(y = Croatia, colour = "Croatia"), size = 0.3) +
 # geom_line(aes(y = Greece, colour = "Greece"), size = 0.3) +
#  geom_line(aes(y = Hungary, colour = "Hungary"), size = 0.3) +
  #geom_line(aes(y = Italy, colour = "Italy"), size = 0.3) +
  #geom_line(aes(y = Romania, colour = "Romania"), size = 0.3) +
  #geom_line(aes(y = Spain, colour = "Spain"), size = 0.3) +
  #geom_line(aes(y = Swtizerland, colour = "Swtizerland"), size = 0.3) +
  #geom_line(aes(y = UK, colour = "UK"), size = 0.3) +
  
  geom_vline(xintercept = 2009, linetype = "dashed") +
  theme(legend.position = "none") 

errorV <- as.data.frame(errorV)
library(writexl)
write_xlsx(gaps_f,"C:/Users/Karen/Desktop/Tesis_Maestría/gaps1.xlsx")
write_xlsx(errorV,"C:/Users/Karen/Desktop/Tesis_Maestría/error1.xlsx")



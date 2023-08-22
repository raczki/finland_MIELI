
library(readxl)
rdos <- read_excel("C:/Users/Karen/Desktop/Tesis_Maestría/Modelos_A/Results_total.xlsx")

#install.packages("viridis")
library(viridis)
library(ggplot2)


x11()
ggplot(rdos, aes(x=lossV, y=MSPE_post, color=`Cant vars`)) +
  geom_point() + 
  xlim(0,100) + ylim(0,65) +
  scale_color_viridis(option = "D")

x11()
ggplot(rdos, aes(x=lossV, y=MSPE_post, color=`Cant vars`)) +
  geom_point() + 
  xlim(0.69,1) + ylim(1.5,5.5) +
  scale_color_viridis(option = "D") +
  xlab("MSPE pre-intervención") +
  ylab("MSPE post-intervención")


x11()
ggplot(rdos, aes(x=MAPE_pre, y=MAPE_post, color=`Cant vars`)) +
  geom_point() + 
  xlim(0.03,0.035) + ylim(0.045,0.15) +
  scale_color_viridis(option = "D")

rdos2 <- rdos[which(rdos$lossV<1),]
write_xlsx(rdos2,"rdos2.xlsx")

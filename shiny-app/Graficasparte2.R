library(dplyr)
library(ggplot2)
library(plotly)

setwd("C:/Users/infrabyte/Documents/Github/Project-bedu/Bedu-ProyectoR-CA/data")


#Para poner el promedio diario por año de PM25
data_PM2524h<-read.csv("PDEstacion_PM25.csv",header=T)
ggplot(data = data_PM2524h[data_PM2524h$ZONA=="NO" & data_PM2524h$Year==2020,], aes(x = Date, y = PromDiario))+labs(x="Meses, Enero=1", y="Concentración promedio diaria PM25")+
  geom_line(aes(group = station, color = station))+labs(color= "Estación")+geom_hline(yintercept=45)+theme_bw()

#Para poner el promedio diario por año de PM10
data_PM1024h<-read.csv("PDEstacion_PM10.csv",header=T)
ggplot(data = data_PM1024h[data_PM1024h$ZONA=="NO" & data_PM1024h$Year==2019 & data_PM1024h$Month==1,], aes(x = Date, y = PromDiario))+labs(x="Meses, Enero=1", y="Concentración promedio diaria PM10")+
  geom_line(aes(group = station, color = station))+labs(color= "Estación")+geom_hline(yintercept=75)+theme_bw()

#Para poner el promedio mensual para ambos años por zona
#PM25
data_PM25Zona<-read.csv("PMZona_PM25.csv",header=T)
data_PM25Zona<-data_PM25Zona[-1]
data_PM25Zona$Year<-as.character(data_PM25Zona$Year)
pos<-which(colnames(data_PM25Zona) =="CE")
ggplot(data = data_PM25Zona, aes(x = Month, y = data_PM25Zona[,pos]))+
  geom_line(aes(group = Year, color = Year))+labs(x="Meses, Enero=1", y="Concentración promedio PM25", color= "Año")+theme_bw()
#PM10
data_PM10Zona<-read.csv("c:/DATA/PMZona_PM10.csv",header=T)
data_PM10Zona<-data_PM10Zona[-1]
data_PM10Zona$Year<-as.character(data_PM10Zona$Year)
pos<-which(colnames(data_PM10Zona) =="NO")
ggplot(data = data_PM10Zona, aes(x = Month, y = data_PM10Zona[,pos]))+
  geom_line(aes(group = Year, color = Year))+labs(x="Meses, Enero=1", y="Concentración promedio PM10", color= "Año")+theme_bw()

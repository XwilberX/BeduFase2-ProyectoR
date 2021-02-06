library(ggplot2)
library(scales) 
library(datasets)

PM10Mensual <- read.csv("c:/DATA/Sesion2/PromediosMensualZona_PM10.csv")
PM25Mensual <- read.csv("c:/DATA/PromediosMensualZona_PM25.csv")
PM25Diario <- read.csv("c:/DATA/PromediosDiariosZona_PM25.csv")
PM10Diario <- read.csv("c:/DATA/PromediosDiariosZona_PM10.csv")
names(PM10Mensual)
names(PM25Mensual)
names(PM25Diario)
names(PM10Diario)

head(PM10Diario); tail(PM10Diario)


#Grafica1 Promedio Mensual por zona PM10
ggplot(PM10Mensual, aes(x=Month, y=Prom_mensual, fill=Year )) + facet_grid((~ZONA))+
  geom_point() + xlab('Meses') + 
  ylab('Prom Concentracion Mensual')

#Series de Tiempo PM10
PM10zona<-read.csv("c:/DATA/PM10_1920_Zonas.csv")
PM10zona<-PM10zona[-1]
# Zona CE PM10
ggplot(data = PM10zona, aes(x = Month, y = PM10zona[,3]))+
  geom_line(aes(group = Year, color = Year))+labs(x="Meses, Enero=1", y="Concentración promedio PM10", color= "Año")+theme_bw()
#Zona NE PM10
ggplot(data = PM10zona, aes(x = Month, y = PM10zona[,4]))+
  geom_line(aes(group = Year, color = Year))+labs(x="Meses, Enero=1", y="Concentración promedio PM10", color= "Año")+theme_bw()
#Zona NO PM10
ggplot(data = PM10zona, aes(x = Month, y = PM10zona[,5]))+
  geom_line(aes(group = Year, color = Year))+labs(x="Meses, Enero=1", y="Concentración promedio PM10", color= "Año")+theme_bw()
#Zona SE PM10
ggplot(data = PM10zona, aes(x = Month, y = PM10zona[,6]))+
  geom_line(aes(group = Year, color = Year))+labs(x="Meses, Enero=1", y="Concentración promedio PM10", color= "Año")+theme_bw()
#Zona SO PM10
ggplot(data = PM10zona, aes(x = Month, y = PM10zona[,7]))+
  geom_line(aes(group = Year, color = Year))+labs(x="Meses, Enero=1", y="Concentración promedio PM10", color= "Año")+theme_bw()

#Series de Tiempo PM2.5
PM25zona<-read.csv("c:/DATA/PM25_1920_Zonas.csv")
PM25zona<-PM25zona[-1]
# Zona CE PM2.5
ggplot(data = PM25zona, aes(x = Month, y = PM25zona[,3]))+
  geom_line(aes(group = Year, color = Year))+labs(x="Meses, Enero=1", y="Concentración promedio PM2.5", color= "Año")+theme_bw()
# Zona NE PM2.5
ggplot(data = PM25zona, aes(x = Month, y = PM25zona[,4]))+
  geom_line(aes(group = Year, color = Year))+labs(x="Meses, Enero=1", y="Concentración promedio PM2.5", color= "Año")+theme_bw()
# Zona NO PM2.5
ggplot(data = PM25zona, aes(x = Month, y = PM25zona[,5]))+
  geom_line(aes(group = Year, color = Year))+labs(x="Meses, Enero=1", y="Concentración promedio PM2.5", color= "Año")+theme_bw()
# Zona SE PM2.5
ggplot(data = PM25zona, aes(x = Month, y = PM25zona[,6]))+
  geom_line(aes(group = Year, color = Year))+labs(x="Meses, Enero=1", y="Concentración promedio PM2.5", color= "Año")+theme_bw()
# Zona SO PM2.5
ggplot(data = PM25zona, aes(x = Month, y = PM25zona[,7]))+
  geom_line(aes(group = Year, color = Year))+labs(x="Meses, Enero=1", y="Concentración promedio PM2.5", color= "Año")+theme_bw()



#Grafica de barras niveles de calidad concentracion PM25 
PM25Calidad <- read.csv("c:/DATA/PromediosMensualZonaCalidad_PM25.csv")

ggplot(PM25Calidad, aes(x=Calidad.del.aire, y=Month)) + 
  geom_bar(stat = "identity") 

#Grafica de puntos Promedio Mensual PM25 por zona 

ggplot(PM25Mensual, aes(x=ZONA, y=Prom_mensual, color=Year)) + 
  geom_point(size=6) 
  

#Carga y DepuraciÃ³n a PM25

library(dplyr)
library(tidyr)
#Se descargaron los datos y se guardan en csv
#url2019 <- "http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/excel/RAMA/20RAMA.zip"
#url2020 <- "http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/excel/RAMA/21RAMA.zip"

#... /Proyecto R/Ficheros/PM25
setwd("C:/Users/jtoba/Documents/BEDU/Proyecto/ProyectoBEDU/PM25/Datos")

#Leer el nombre de los archivos
lista_PM25 <- lapply(dir(pattern = ".csv"), read.csv)
#Se cargan los 3 archivos, uno para cada año
#Para leer las estaciones que se encuentran en todos los aÃ±os se realiza lo siguiente
#Primero se compara con las estaciones del primer archivo y el segundo
dPM25_C<-colnames(lista_PM25[[1]])[colnames(lista_PM25[[1]])%in%colnames(lista_PM25[[2]])]

#Estaciones que no tienen valores para 2020, se revis� de forma gr�fica
eliminar<-c("MGH")
#Eliminar de la lectura
dPM25_C<-dPM25_C[!(dPM25_C%in%eliminar)]


#Leer las lecturas por hora de cada una de las estaciones que ocurren en los tres aÃ±os
lista_PM25 <- lapply(lista_PM25, select, Date = FECHA,Time = HORA,all_of(dPM25_C))
lista_PM25 <- lapply(lista_PM25, mutate,Date=as.Date(Date, "%d/%m/%Y"),)

data_PM25 <- do.call(rbind, lista_PM25)

# Reemplazamos los -99 (missing values) a NA
is.na(data_PM25) <- data_PM25 == -99

#Reacomodamos el dataframe para que la estacion y la medida sean un parÃ¡metro de mediciÃ³n
data_PM25<-gather(data = data_PM25, key= "station", value= "measurement", dPM25_C[-c(1,2)])

#Eliminamos los valores NA
data_PM25<-data_PM25%>%drop_na(measurement)


#Calcular el promedio diario y saber cuantas horas se usaron para el calculo
data_PM2524h<-data_PM25%>%group_by(Date,station)%>%
  summarise(PromDiario=round(mean(measurement),0),
            N=n())
#Eliminar los que son menores a 18 según la norma mexicana no son válidos para el cálculo
data_PM2524h<-as.data.frame(data_PM2524h)

data_PM2524h<-data_PM2524h%>%filter(N>17)
#Eliminar la fila del número de horas utilizadas para el promedio diario
data_PM2524h<-data_PM2524h[-4]

#Unir con los datos de las Zonas
Zonas<-read.csv("C:/Users/jtoba/Documents/BEDU/Proyecto/PM10/zonas_zmvm.csv",header=T)
data_PM2524h<-merge(data_PM2524h,Zonas, by.x="station",by.y="cve_estac")

#Separar meses años y días
Year<-format(as.POSIXct(strptime(data_PM2524h$Date ,"%Y-%m-%d",tz="")) ,format = "%Y")
Month<-format(as.POSIXct(strptime(data_PM2524h$Date ,"%Y-%m-%d",tz="")) ,format = "%m")
Day<-format(as.POSIXct(strptime(data_PM2524h$Date ,"%Y-%m-%d",tz="")) ,format = "%d")
data_PM2524h<-cbind(Year,Month,Day,data_PM2524h)


##Guardar los promedios diarios
write.csv(data_PM2524h,"C:/Users/jtoba/Documents/BEDU/Proyecto/PDEstacion_PM25.csv")
#Calcular los promedios mensuales por zona
Zona_ave25<-data_PM2524h%>%group_by(Year,Month,ZONA)%>%
  summarise(Prom_mensual=round(mean(PromDiario),1))

Zona_ave25<-as.data.frame(Zona_ave25)

Zona_ave25<-spread(Zona_ave25, key = ZONA, value=Prom_mensual)
##Guardar los promedios mensuales
write.csv(Zona_ave25, "C:/Users/jtoba/Documents/BEDU/Proyecto/PMZona_PM25.csv")

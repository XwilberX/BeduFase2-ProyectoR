#Carga y DepuraciÃƒÂ³n a PM10

library(dplyr)
library(tidyr)
#Se descargaron los datos y se guardan en csv
#url2019 <- "http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/excel/RAMA/20RAMA.zip"
#url2020 <- "http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/excel/RAMA/21RAMA.zip"

#... /Proyecto R/Ficheros/PM10
setwd("~DatosPM10")

#Leer el nombre de los archivos
lista_PM10 <- lapply(dir(pattern = ".csv"), read.csv)
#Se cargan los 3 archivos, uno para cada aÃ±o
#Para leer las estaciones que se encuentran en todos los aÃƒÂ±os se realiza lo siguiente
#Primero se compara con las estaciones del primer archivo y el segundo
dPM10_C<-colnames(lista_PM10[[1]])[colnames(lista_PM10[[1]])%in%colnames(lista_PM10[[2]])]
#Estaciones que no tienen valores para 2020, se revisó de forma gráfica
eliminar<-c("CHO","MGH")
#Eliminar de la lectura
dPM10_C<-dPM10_C[!(dPM10_C%in%eliminar)]


#Leer las lecturas por hora de cada una de las estaciones que ocurren en los tres aÃƒÂ±os
lista_PM10 <- lapply(lista_PM10, select, Date = FECHA,Time = HORA,all_of(dPM10_C))
lista_PM10 <- lapply(lista_PM10, mutate,Date=as.Date(Date, "%d/%m/%Y"),)

data_PM10 <- do.call(rbind, lista_PM10)

# Reemplazamos los -99 (missing values) a NA
is.na(data_PM10) <- data_PM10 == -99

#Reacomodamos el dataframe para que la estacion y la medida sean un parÃƒÂ¡metro de mediciÃƒÂ³n
data_PM10<-gather(data = data_PM10, key= "station", value= "measurement", dPM10_C[-c(1,2)])

#Eliminamos los valores NA
data_PM10<-data_PM10%>%drop_na(measurement)


#Calcular el promedio diario y saber cuantas horas se usaron para el calculo
data_PM1024h<-data_PM10%>%group_by(Date,station)%>%
  summarise(PromDiario=round(mean(measurement),0),
            N=n())
#Eliminar los que son menores a 18 segÃºn la norma mexicana no son vÃ¡lidos para el cÃ¡lculo
data_PM1024h<-as.data.frame(data_PM1024h)

data_PM1024h<-data_PM1024h%>%filter(N>17)
#Eliminar la fila del nÃºmero de horas utilizadas para el promedio diario
data_PM1024h<-data_PM1024h[-4]

#Unir con los datos de las Zonas
Zonas<-read.csv("zonas_zmvm.csv",header=T)
data_PM1024h<-merge(data_PM1024h,Zonas, by.x="station",by.y="cve_estac")

#Separar meses aÃ±os y dÃ­as
Year<-format(as.POSIXct(strptime(data_PM1024h$Date ,"%Y-%m-%d",tz="")) ,format = "%Y")
Month<-format(as.POSIXct(strptime(data_PM1024h$Date ,"%Y-%m-%d",tz="")) ,format = "%m")
Day<-format(as.POSIXct(strptime(data_PM1024h$Date ,"%Y-%m-%d",tz="")) ,format = "%d")
data_PM1024h<-cbind(Year,Month,Day,data_PM1024h)


##Guardar los promedios diarios para utilizar en las gráficas
#write.csv(data_PM1024h,"PDEstacion_PM10.csv")

#Calcular los promedios mensuales por zona
Zona_ave<-data_PM1024h%>%group_by(Year,Month,ZONA)%>%
  summarise(Prom_mensual=round(mean(PromDiario),1))

Zona_ave<-as.data.frame(Zona_ave)
Zona_ave<-spread(Zona_ave, key = ZONA, value=Prom_mensual)
##Guardar los promedios mensuales para las gráficas
#write.csv(Zona_ave, "PMZona_PM10.csv")





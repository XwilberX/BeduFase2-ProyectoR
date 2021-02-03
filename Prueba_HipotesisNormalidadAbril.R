##Pruebas de hipÃ³tesis para los valores de las 5 estaciones en el mes de 
##abril de 2019 y 2020 de PM10


#Cargar los datos diarios
PM10<-read.csv("~Resultados/PDEstacion_PM10.csv", header=T)
PM10<-PM10[-1]

#Filtrar los datos de abril de 2019
PM100419<-PM10%>%filter(Month==4 & Year==2019)

#Filtrar los datos de abril de 2020
PM100420<-PM10%>%filter(Month==4 & Year==2020)

#Visualizar la normalidad con qqnorm y una prueba de shapiro 2019
for (i in unique(PM10$ZONA)){
  qqnorm(PM100419[PM100419$ZONA==i,]$PromDiario, main =  paste0(i,"-2019"))
  qqline(PM100419[PM100419$ZONA==i,]$PromDiario)
  print(paste0(i,"-2019"))
  st19<-shapiro.test(PM100419[PM100419$ZONA==i,]$PromDiario)
  print(st19$p.value>=0.05)
  qqnorm(PM100420[PM100420$ZONA==i,]$PromDiario, main =  paste0(i,"-2020"))
  qqline(PM100420[PM100420$ZONA==i,]$PromDiario)
  st20<-shapiro.test(PM100420[PM100420$ZONA==i,]$PromDiario)
  print(paste0(i,"-2020"))
  print(st20$p.value>=0.05)
}

#Se realiza la prueba de hipÃ³tesis en cada zona, considerando la varianza distinta
#SÃ³lo se toman en cuenta las que superaron la prueba de normalidad en ambos aÃ±os
Zonas_PH<-c("NE", "NO","CE")

Hipotesis<-c() #Se acepta la hipÃ³tesis alternativa 2020<2019? True se acepta False se rechaza
for(i in Zonas_PH){
  a<-t.test(PM100420[PM100420$ZONA==i,]$PromDiario ,PM100419[PM100419$ZONA==i,]$PromDiario,
            alternative = "less",
            mu = 0, paired = FALSE, var.equal = FALSE)
  Hipotesis<-c(Hipotesis,a$statistic<a$p.value)

}


Hipotesis_04<-Hipotesis
#En todas las zonas estudiadas se muestra una diferencia significativa entre años para este mes


###Para los datos de PM25#####
#Cargar los datos diarios
PM25<-read.csv("~Resultados/PDEstacion_PM25.csv", header=T)
PM25<-PM25[-1]

#Filtrar los datos de abril de 2019
PM250419<-PM25%>%filter(Month==4 & Year==2019)

#Filtrar los datos de abril de 2020
PM250420<-PM25%>%filter(Month==4 & Year==2020)

#Visualizar la normalidad con qqnorm y una prueba de shapiro 2019
for (i in unique(PM25$ZONA)){
  qqnorm(PM250419[PM250419$ZONA==i,]$PromDiario, main =  paste0("PM25",i,"-2019"))
  qqline(PM250419[PM250419$ZONA==i,]$PromDiario)
  print(paste0(i,"-2019"))
  st19<-shapiro.test(PM250419[PM250419$ZONA==i,]$PromDiario)
  print(st19$p.value>=0.05)
  qqnorm(PM250420[PM250420$ZONA==i,]$PromDiario, main =  paste0("PM25",i,"-2020"))
  qqline(PM250420[PM250420$ZONA==i,]$PromDiario)
  st20<-shapiro.test(PM250420[PM250420$ZONA==i,]$PromDiario)
  print(paste0(i,"-2020"))
  print(st20$p.value>=0.05)
}

#Se realiza la prueba de hipÃ³tesis en cada zona, considerando la varianza distinta

#solamente se utilizan las zonas que mostraron normalidad en ambos aÃ±os
Zonas_PH25<-c("CE","NO","NE","SE")
#Se agrupan los datos por zona y se determinan los valores para las pruebas

Hipotesis<-c() #Se rechaza la hipÃ³tesis nula 2020<2019? True se rechaza False se acepta de que
for(i in Zonas_PH25){
  a<-t.test(PM250420[PM250420$ZONA==i,]$PromDiario ,PM250419[PM250419$ZONA==i,]$PromDiario,
            alternative = "less",
            mu = 0, paired = FALSE, var.equal = FALSE)
  Hipotesis<-c(Hipotesis,a$statistic<a$p.value)
  
}



Hipotesis_PM25<-Hipotesis
#En todas las zonas estudiadas se muestra una diferencia significativa entre años para este mes

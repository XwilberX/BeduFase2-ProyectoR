library(shiny)
library(dashboardthemes)
library(leaflet)
library(rgdal)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)


setwd("C:/Users/infrabyte/Documents/Github/Project-bedu/Bedu-ProyectoR-CA/data")

DataEsta_PM10 <- read.csv("PDEstacion_PM10.csv", header = T)
DataEsta_PM10 <- DataEsta_PM10[-1]
DataEsta_PM10 <- rename(DataEsta_PM10, Fecha = Date)
DataEsta_PM25 <- read.csv("PDEstacion_PM25.csv", header = T)
DataEsta_PM25 <- DataEsta_PM25[-1]
DataEsta_PM25 <- rename(DataEsta_PM25, Fecha = Date)

CordsZ <- read.csv("cat_estacion - cat_estacion.csv", header = T)

#### SHAPE
setwd("C:/Users/infrabyte/Documents/Github/Project-bedu/Bedu-ProyectoR-CA/data/shape")
shape <- readOGR("C:/Users/infrabyte/Documents/Github/Project-bedu/shiny-app/data2", "limite250_l")
shape <- spTransform(shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

shinyServer(function(input, output) {

    output$Table <- renderDT(
        if(input$contaminante == 10){
            if(input$MoD == 1){
                filter(DataEsta_PM10, Year == input$years, 
                       ZONA == input$zonas,
                       Month == input$Mes,
                       Fecha >= input$dateRange[1],
                       Fecha <= input$dateRange[2])
            }else{
                filter(DataEsta_PM10, Year == input$years, 
                       ZONA == input$zonas,
                       Fecha >= input$dateRange[1],
                       Fecha <= input$dateRange[2])
            }
        }else{
            if(input$MoD == 1){
                filter(DataEsta_PM25, Year == input$years, 
                       ZONA == input$zonas,
                       Month == input$Mes,
                       Fecha >= input$dateRange[1],
                       Fecha <= input$dateRange[2])
            }else{
                filter(DataEsta_PM25, Year == input$years, 
                       ZONA == input$zonas,
                       Fecha >= input$dateRange[1],
                       Fecha <= input$dateRange[2])
            }
        }
    )
    
    output$busmap <- renderLeaflet({
        
        dataM2 <- filter(CordsZ, ZONA == input$zonas)
        
        leaflet() %>%
            addTiles() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addMarkers(data = dataM2, lng = dataM2$longitud, lat = dataM2$latitud) %>%
            addPolygons(data = shape)
        
        
    })
    
    output$meses <- renderUI({
        if(input$MoD == 1){
            selectInput("Mes", "Elije el mes",
             choices = c(
                 "Enero" = 1,
                 "Febrero" = 2,
                 "Marzo" = 3,
                 "Abril" = 4,
                 "Mayo" = 5,
                 "Junio" = 6,
                 "Julio" = 7,
                 "Agosto" = 8,
                 "Septiembre" = 9,
                 "Octubre" = 10,
                 "Noviembre" = 11,
                 "Diciembre" = 12
             )
            )
        }else{
            p("Se estan visualizando resultados de muestras diarias")
        }
    })

    output$RangeDate <- renderUI({
        
        dateRangeInput('dateRange',
                       label = "Introcude un rango de fechas",
                       start = paste(as.character(input$years),  "01-01", sep = "-"), 
                       end = paste(as.character(input$years),  "12-01", sep = "-"), 
                       separator = " - ", language = "es"
        )
    })
    
    output$plot <- renderPlotly({
        
        if(input$contaminante2 == 10){
            if(input$MoD2 == 1){
                p <- ggplot(data = DataEsta_PM10[DataEsta_PM10$ZONA== input$zonas2 & DataEsta_PM10$Year==input$years2 & 
                                                     DataEsta_PM10$Month==input$Mes2,], 
                            aes(x = Fecha, y = PromDiario)) +
                    labs(x="Meses, Enero=1", y="Concentración promedio diaria PM25") +
                    geom_line(aes(group = station, color = station)) + 
                    labs(color= "Estación") + 
                    geom_hline(yintercept=45) + theme_bw() + theme(axis.text.x = element_text(angle = 90))
            }else{
                p <- ggplot(data = DataEsta_PM10[DataEsta_PM10$ZONA== input$zonas2 & DataEsta_PM10$Year==input$years2,], 
                            aes(x = Fecha, y = PromDiario)) +
                    labs(x="Meses, Enero=1", y="Concentración promedio diaria PM25") +
                    geom_line(aes(group = station, color = station)) + 
                    labs(color= "Estación") + 
                    geom_hline(yintercept=45) + theme_bw() + theme(axis.text.x =element_blank())
            }
        }else{
            if(input$MoD2 == 1){
                p <- ggplot(data = DataEsta_PM25[DataEsta_PM25$ZONA== input$zonas2 & DataEsta_PM25$Year==input$years2 & 
                                                     DataEsta_PM25$Month==input$Mes2,], 
                            aes(x = Fecha, y = PromDiario)) +
                    labs(x="Meses, Enero=1", y="Concentración promedio diaria PM25") +
                    geom_line(aes(group = station, color = station)) + 
                    labs(color= "Estación") + 
                    geom_hline(yintercept=45) + theme_bw() + theme(axis.text.x = element_text(angle = 90))
            }else{
                p <- ggplot(data = DataEsta_PM25[DataEsta_PM25$ZONA== input$zonas2 & DataEsta_PM25$Year==input$years2,], 
                            aes(x = Fecha, y = PromDiario)) +
                    labs(x="Meses, Enero=1", y="Concentración promedio diaria PM25") +
                    geom_line(aes(group = station, color = station)) + 
                    labs(color= "Estación") + 
                    geom_hline(yintercept=45) + theme_bw() + 
                    theme(axis.text.x =element_blank())
            }
        }
        
        
        ggplotly(p)
    })
    
    output$meses2 <- renderUI({
        if(input$MoD2 == 1){
            selectInput("Mes2", "Elije el mes",
                        choices = c(
                            "Enero" = 1,
                            "Febrero" = 2,
                            "Marzo" = 3,
                            "Abril" = 4,
                            "Mayo" = 5,
                            "Junio" = 6,
                            "Julio" = 7,
                            "Agosto" = 8,
                            "Septiembre" = 9,
                            "Octubre" = 10,
                            "Noviembre" = 11,
                            "Diciembre" = 12
                        )
            )
        }else{
            p("Se estan visualizando resultados de muestras diarias")
        }
    })
    
    output$RangeDate2 <- renderUI({
        
        dateRangeInput('dateRange2',
                       label = "Introcude un rango de fechas",
                       start = paste(as.character(input$years2),  "01-01", sep = "-"), 
                       end = paste(as.character(input$years2),  "12-01", sep = "-"), 
                       separator = " - ", language = "es"
        )
    })
    
    output$Table2 <- renderDT(
        if(input$contaminante2 == 10){
            if(input$MoD2 == 1){
                filter(DataEsta_PM10, Year == input$years2, 
                       ZONA == input$zonas2,
                       Month == input$Mes2,
                       Fecha >= input$dateRange2[1],
                       Fecha <= input$dateRange2[2])
            }else{
                filter(DataEsta_PM10, Year == input$years2, 
                       ZONA == input$zonas2,
                       Fecha >= input$dateRange2[1],
                       Fecha <= input$dateRange2[2])
            }
        }else{
            if(input$MoD2 == 1){
                filter(DataEsta_PM25, Year == input$years2, 
                       ZONA == input$zonas2,
                       Month == input$Mes2,
                       Fecha >= input$dateRange2[1],
                       Fecha <= input$dateRange2[2])
            }else{
                filter(DataEsta_PM25, Year == input$years2, 
                       ZONA == input$zonas2,
                       Fecha >= input$dateRange2[1],
                       Fecha <= input$dateRange2[2])
            }
        }
    )
})




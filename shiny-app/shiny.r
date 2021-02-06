library(shinydashboard)
library(shinythemes)
library(leaflet)
library(rgdal)
library(shiny)
library(dplyr)
library(DT)

setwd("C:/Users/infrabyte/Documents/Github/Project-bedu/shiny-app/data")

shape2 <- readOGR("C:/Users/infrabyte/Documents/Github/Project-bedu/shiny-app/data2", "limite250_l")
shape2 <- spTransform(shape2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

dataM <- read.csv("cat_estacion - cat_estacion.csv", header = T)

dataZ <- read.csv("data_PM10_Zona.csv", header = T)

grup <- group_by(dataZ, Year)
View(dataZ)
res <- subset(dataZ, Year == 2019)

filter(dataZ, station == "MON")


res <- select(res, station, Month, ZONA)

filter(res, ZONA == "NE")

res1 <- subset(dataZ, Year == 2020)

ui <- dashboardPage(
    dashboardHeader(title = "Calidad del aire"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard"),
            menuItem("Data", tabName = "rawdata")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("dashboard",
                fluidRow(
                    column(width = 9,
                        box(width = NULL, solidHeader = TRUE,
                            leafletOutput("busmap", height = 500)
                        ),
                        box(width = NULL,
                            DTOutput(outputId = "table")
                        )
                    ),
                    column(width = 3,
                        box(width = NULL, status = "warning",
                            uiOutput("routeSelect"),
                            selectInput("years", "Años",
                                choices = c(
                                    "2019" = 2019,
                                    "2020" = 2020
                                ),
                                selected = c(2019)
                            ),
                            selectInput("zonas", "Zonas",
                                choices = c(
                                    "Noroeste" = "NO",
                                    "Suroeste" = "SO",
                                    "Noreste" = "NE",
                                    "Sureste" = "SE",
                                    "CE" = "CE"
                                ),
                                selected = c("NE")
                            ),
                        )
                    )
                )
            ),
             tabItem("rawdata",
                numericInput("maxrows", "Rows to show", 25),
                verbatimTextOutput("rawtable"),
                downloadButton("downloadCsv", "Download as CSV")
            )
        )
    )
)


server <- function(input, output) {

   output$table <- renderDT(filter(dataZ, Year == input$years, ZONA == input$zonas))
    

    output$busmap <- renderLeaflet({
        dataM2 <- filter(dataM, ZONA == input$zonas)

        leaflet() %>%
            addTiles() %>%
            addProviderTiles(providers$Stamen.TonerLite,
            options = providerTileOptions(noWrap = TRUE)) %>%
            addMarkers(data = dataM2, lng = dataM2$longitud, lat = dataM2$latitud) %>%
            addPolygons(data = shape2)
    })
 }
shinyApp(ui, server)
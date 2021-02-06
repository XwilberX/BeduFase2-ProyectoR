library(shiny)
library(dashboardthemes)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(rgdal)
library(dplyr)
library(DT)

shinyUI(
    dashboardPage(skin = "purple",
         dashboardHeader(
           title = "Calidad del aire"
         ),
         #########################################################
         dashboardSidebar(
             sidebarMenu(
                 menuItem("Dashboard", tabName = "dashboard"),
                 menuItem("Graficos", tabName = "graficos")
             )
         ),
         ########################################################
         dashboardBody(
             tabItems(
                 tabItem("dashboard",
                         fluidRow(
                             column(width = 9,
                                    box(width = NULL, solidHeader = TRUE,
                                        leafletOutput("busmap", height = 500)
                                    ),
                                    box(width = NULL,
                                            DTOutput(outputId = "Table")
                                    )
                             ),
                             column(width = 3,
                                 box(title = "Filtros",
                                     width = NULL, status = "warning",
                                     uiOutput("routeSelect"),
                                     selectInput("contaminante", "Contaminante",
                                                 choices = c(
                                                   "PM10" = 10,
                                                   "PM25" = 25
                                                 ),
                                                 selected = c(2019)
                                     ),
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
                                     selectInput("MoD", "Meses o Diario",
                                          choices = c(
                                            "Meses" = 1,
                                            "Diario" = 2
                                          ),
                                          selected = c(1)
                                      ),
                                     uiOutput("meses"),
                                     uiOutput("RangeDate")
                                 )
                             )
                         )
                 ),
                 
                 tabItem("graficos",
                   fluidRow(
                     column(width = 9,
                       box( title = "Grafico",
                         width = NULL,
                         height = NULL,
                         plotlyOutput("plot")
                       ),
                       box(width = NULL,
                           DTOutput(outputId = "Table2")
                       )
                     ),
                     column(width = 3,
                        box(title = "Filtros",
                            width = NULL, status = "warning",
                            selectInput("contaminante2", "Contaminante",
                                        choices = c(
                                          "PM10" = 10,
                                          "PM25" = 25
                                        ),
                                        selected = c(2019)
                            ),
                            selectInput("years2", "Años",
                                        choices = c(
                                          "2019" = 2019,
                                          "2020" = 2020
                                        ),
                                        selected = c(2019)
                            ),
                            selectInput("zonas2", "Zonas",
                                        choices = c(
                                          "Noroeste" = "NO",
                                          "Suroeste" = "SO",
                                          "Noreste" = "NE",
                                          "Sureste" = "SE",
                                          "CE" = "CE"
                                        ),
                                        selected = c("NE")
                            ),
                            selectInput("MoD2", "Meses o Diario",
                                        choices = c(
                                          "Meses" = 1,
                                          "Diario" = 2
                                        ),
                                        selected = c(1)
                            ),
                            uiOutput("meses2"),
                            uiOutput("RangeDate2")
                        )
                     )
                   )
                 )
             )
         )
    )
    
)

Sys.Date()

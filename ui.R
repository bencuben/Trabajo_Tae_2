library(shiny)
library(leaflet)

shinyUI(div(
  tags$head(#Se incluye algunos estilos que usa la app
    includeCSS("styles.css")),
  navbarPage(
    "App-Familias",
    
    #Mapa: Tab para visualizar el mapa correspondiente
    #a la probabilidad dada por el modelo para cada uno
    #de los departamentos de Colombia (De acuerdo a los
    # paramentros de filtrado).
    
    tabPanel(title = "Mapa",
             div(
               class = "outer",
               
               #map: Mapa de calor por departamento en Colombia
               leafletOutput("map", width = "100%", height = "100%"),
               
               absolutePanel(
                 id = "controls",
                 class = "panel panel-default",
                 fixed = TRUE,
                 draggable = TRUE,
                 top = "70%",
                 left = "auto",
                 right = 20,
                 bottom = "auto",
                 width = 330,
                 height = "auto",
                 h3("Controles", class = "text-center"),
                 selectInput(inputId="year", label="Año", choices= c("2015", "2016", "2017")),
                 selectInput(inputId="year", label="Año", choices= c("2015", "2016", "2017"))
               )
             )),
    tabPanel(title = "Analisis Descriptivo"),
    tabPanel(title = "Acerca de")
  )
))

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
    
    tabPanel(title = "Por Departamento",
             div(
               class = "outer",
               
               #map: Mapa de calor por departamento en Colombia
               leafletOutput("mapDep", width = "100%", height = "100%"),
               
               absolutePanel(
                 id = "controls",
                 class = "panel panel-default",
                 fixed = TRUE,
                 draggable = TRUE,
                 top = 40,
                 left = 80,
                 right = "auto",
                 bottom = "auto",
                 width = 330,
                 height = "auto",
                 h3("Controles", class = "text-center"),
                 selectInput(inputId="P6077", label="Lugar donde habitaba anteriormente", 
                             choices= c("1 - El centro urbano donde está la alcaldía", 
                                        "2 - Un corregimiento, inspección de policía, caserío, vereda o campo")),
                 selectInput(inputId="P6080", label="De acuerdo con su cultura,pueblo o rasgos físicos, es o se reconoce comó",
                             choices= c("1 - Indígena","2 - Gitano","3 - Raizal del archipiélago","4 - Palenquero", 
                                        "5 - Negro, mulato o afrodescendiente","6 - Ninguno de los anteriores")),
                 selectInput(inputId="P6096", label="Cuál fue la razón principal para cambiar la residencia al municipio actual?",
                             choices= c("1 - Dificultad para encontrar trabajo o ausencia de medios de subsistencia", 
                                        "2 - Riesgo o consecuencia de desastre natural", 
                                        "3 - Amenaza o riesgo para su vida, su libertad o su integridad física, ocasionada por la violencia",
                                        "4 - Necesidad de educación", "5 - Porque se casó o formó pareja", "6 - Motivos de salud",
                                        "7 - Mejorar la vivienda o localización", "8 - Mejores  oportunidades laborales o de  negocio",
                                        "9 - Acompañar a otro(s) miembro(s) del hogar","10 - Otra")),
                 numericInput(inputId="P767", label="Cuántos años continuos hace que vive aquí en este municipio?", min = 1 , value=1),
                 selectInput(inputId="P6081", label="El padre vive en este hogar?",
                             choices=c("1 - Sí","2 - No", "3 - Fallecido"), selected="2 - No"),
                 selectInput(inputId="P5502", label="Situación sentimental actual", 
                             choices=c("1 - No está casado(a) y vive en pareja hace menos de dos años", 
                                      "2 - No está casado(a) y vive en pareja hace dos años o más", "3 - Está viudo(a)", 
                                      "4 - Está separado(a) o divorciado(a)", "5 - Está soltero(a)", "6 - Está casado(a)"))
                 
               )
             )),
    tabPanel(title = "Por región"),
    tabPanel(title = "Acerca de")
  )
))

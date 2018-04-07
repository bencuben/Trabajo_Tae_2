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
                 left = 60,
                 right = "auto",
                 bottom = "auto",
                 width = 500,
                 height = "auto",
                 h3("Filtros", class = "text-center"),
                 fluidRow(
                   column(width = 6,
                 selectInput(inputId="P6077", label="Lugar donde habitaba anteriormente",
                             choices= c("1 - El centro urbano donde está la alcaldía", 
                                        "2 - Un corregimiento, inspección de policía, caserío, vereda o campo")),
                 selectInput(inputId="P6080",
                             label="De acuerdo con su cultura,pueblo o rasgos físicos, es o se reconoce comó",
                             choices= c("1 - Indígena","4 - Palenquero", 
                                        "5 - Negro, mulato o afrodescendiente","6 - Ninguno de los anteriores")),
                 selectInput(inputId="P6096",
                             label="Cuál fue la razón principal para cambiar la residencia al municipio actual?",
                             choices= c("1 - Dificultad para encontrar trabajo o ausencia de medios de subsistencia", 
                                        "2 - Riesgo o consecuencia de desastre natural", 
                                        "3 - Amenaza o riesgo para su vida, su libertad o su integridad física, ocasionada por la violencia",
                                        "4 - Necesidad de educación", "5 - Porque se casó o formó pareja", "6 - Motivos de salud",
                                        "7 - Mejorar la vivienda o localización", "8 - Mejores  oportunidades laborales o de  negocio",
                                        "9 - Acompañar a otro(s) miembro(s) del hogar","10 - Otra"))),
                 column(width = 6, 
                 sliderInput("P767",
                             label="Cuántos años continuos hace que vive aquí en este municipio?", min = 1 , max= 100, value=1),
                 selectInput(inputId="P6081", label="El padre vive en este hogar?", width = 480,
                             choices=c("2 - No", "3 - Fallecido"), selected="2 - No"),
                 selectInput(inputId="P5502", label="Situación sentimental actual", width = 480,
                             choices=c("1 - No está casado(a) y vive en pareja hace menos de dos años", 
                                       "2 - No está casado(a) y vive en pareja hace dos años o más", 
                                       "6 - Está casado(a)")))
                 )
                 
               )
             )),
    tabPanel(title = "Por Región",
             div(
               class = "outer",
               
               #map: Mapa de calor por regiones en Colombia
               leafletOutput("mapReg", width = "100%", height = "100%"),
               absolutePanel(
                 id = "controls",
                 class = "panel panel-default",
                 fixed = TRUE,
                 draggable = TRUE,
                 top = 40,
                 left = 60,
                 right = "auto",
                 bottom = "auto",
                 width = 500,
                 height = "auto",
                 h3("Filtros", class = "text-center"),
                 fluidRow(
                  column(width =6,
                  selectInput(inputId="P6080_2", label="De acuerdo con su cultura, pueblo o rasgos físicos, es o se reconoce comó",
                             choices= c("1 - Indígena","3 - Raizal del archipiélago","4 - Palenquero", 
                                        "5 - Negro, mulato o afrodescendiente","6 - Ninguno de los anteriores")),
                 selectInput(inputId="P4567_2", label="Material predominante del techo o cubierta",
                             choices= c("1 - Plancha de concreto, cemento u hormigón", "2 - Tejas de barro",
                                        "3 - Teja de asbesto - cemento ", "4 - Teja metálica o lámina de zinc",
                                        "5 - Teja plástica", "6 - Paja, palma u otros vegetales", 
                                        "7 - Material de desecho (tela, cartón,latas, plástico, otros)")),
                 selectInput(inputId="CLASE_2", label="Clase",
                             choices= c("1 - Cabecera", "2 - Centros poblados y rural disperso")),
                 selectInput(inputId="P8520S1A1_2", label = "Estrato para tarifa de energía eléctrica",
                             choices = c("1 - Bajo - Bajo", "2 - Bajo", "3 - Medio - Bajo",  "4 - Medio", "5 Medio - Alto",
                                         "6 - Alto", "9 - No conoce el estrato o no cuenta con recibo de pago",
                                         "0 - Recibos sin estrato o el servicio es pirata")),
                 selectInput(inputId="P8520S3_2", label="Alcantarillado",
                             choices= c("1 - Sí" ,"2 - No"))),
                 column(width = 6,
                 selectInput(inputId="P6081_2", label="El padre vive en este hogar?",
                             choices=c("1 - Sí", "2 - No", "3 - Fallecido")),
                 selectInput(inputId="P5661S4_2", label="Contaminación del aire en los últimos 12 meses",
                             choices=c("1 - Nunca", "2 - Algunas veces", "3 - Muchas veces", "4 - Siempre")),
                 selectInput(inputId="P6077_2", label="Lugar donde habitaba anteriormente", 
                             choices= c("1 - El centro urbano donde está la alcaldía", 
                                        "2 - Un corregimiento, inspección de policía, caserío, vereda o campo")),
                 selectInput(inputId="P1070_2", label="Tipo de Vivienda", 
                             choices= c("1 - Casa", "2 - Apartamento", "3 - Cuarto(s)", "4 - Vivienda (casa)indigena",
                                        "5 Otro tipo de vivienda")),
                 sliderInput("P767_2", label="Cuántos años continuos hace que vive aquí en este municipio?", min = 1 , max= 100, value=1))
                 )
               )
               
             )
    ),
    
    tabPanel(title = "Acerca de",
             h3("Integrantes:"),
             br(),
             h4("Yeison Yovani Ocampo Naranjo - Estadística"),
             h4("Brahian Cano Urrego - Estadística"),
             h4("Daniel Alexander Cano Cuartas - Ingeniería de Sistemas e Informática"),
             h4("Sebastián Pino Sanchez - Ingeniería de Sistemas e Informática"),
             HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/PXBPiBdgG4M" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>')
    )
  )
))

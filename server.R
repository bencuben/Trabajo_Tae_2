
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(raster)
library(colorRamps)

shinyServer(function(input, output) {
  
  departamentos <- shapefile("Departamentos/departamentos.shp", encoding="UTF-8", use_iconv = TRUE)
  
  output$map <- renderLeaflet({
    
    # Generación de números aleatorios, suponiendo que son probabilidades
    departamentos@data$prob <- runif(33, min=0, max=100)
    
    #Creacion de los atributos del mapa
    pal <-colorNumeric(palette=blue2red(100),domain=c(0,100))
    popup<-paste(departamentos@data$DEPTO,paste("Probabilidad ",departamentos@data$prob," %"),sep="<br/>")
    
    #Definicion del mapa
    mapaDepartamentos <- leaflet(departamentos)
    mapaDepartamentos <- addProviderTiles(mapaDepartamentos, provider="OpenStreetMap.Mapnik")
    mapaDepartamentos <- addPolygons(mapaDepartamentos,
                                     popup = popup,
                                     color=pal(as.integer(departamentos@data$prob)),
                                     opacity = 0.5,
                                     fillOpacity = 0.5,
                                     weight = 1)
    mapaDepartamentos <-addLegend(mapaDepartamentos,"topright",pal=pal,values=departamentos@data$prob, 
                                  title="Probabilidad",
                                  opacity = 1)
    mapaDepartamentos
    
  })
})


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
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library (MASS)

shinyServer(function(input, output) {
  
  withProgress(message="Cargando datos", {
    departamentos <- shapefile("Departamentos/departamentos.shp", encoding="UTF-8", use_iconv = TRUE)
    incProgress(25)
    departamentos@data$DPTO_CCDGO <- as.integer(departamentos@data$DPTO_CCDGO)
    incProgress(25)
    departamentos <- departamentos[order(departamentos@data$DPTO_CCDGO),]
    incProgress(25)
    datos <- read.csv("BDs/Caracteristicas y composicion del hogar.csv")
    incProgress(25)
  })
  
  withProgress(message="Creando modelo", {
    #### Eliminando los valñores ausentes
    
    machetazo<- na.omit(datos)
    
    #### Eliminamos las columnas debido al alto número de valores "infinitos"
    
    machetazo<- machetazo[,-c(21,24,27)]
    
    
    #### Eliminamos todos los valores infinitos con la función is.finite y dejamos finalmente una basede datos limpia para trabajar
    
    guadanazo<-machetazo[which(is.finite(machetazo$P6071)
                               & is.finite(machetazo$P6071S1)
                               & is.finite(machetazo$P6088)
                               &  is.finite(machetazo$P1903)
                               & is.finite(machetazo$P6087)),  ]
      #### Procedemos a eliminar todas las variables que consideramos bno singinificativas o aportantes para este caso
    incProgress(25)
    
    
    #### recodificamos las variables para que sean de tipo categorico
    guadanazo<- guadanazo[,-c(1,2,3,4,5,6)]
    daticos<-guadanazo
    
    for(i in 1:28){
      daticos[,i]<- as.factor(guadanazo[,i])
    }
    incProgress(25)
    ### Todos los nombres
    
    
    
    daticos$P6040<- as.integer(daticos$P6040)
    daticos$P767<- as.integer(daticos$P767)
    daticos<-daticos[,-11]
    daticos<-daticos[,-c(5,7,9)]
    incProgress(25)
    daticos$P6076S1<-as.factor(daticos$P6076S1)
    
    #modelo luego del step aic
    modelo.fullhd<-multinom(formula = P6076S1 ~ P6077 + P6080 +  + P6096 + 
                              P767 + P6081 + P5502, data = daticos)
    incProgress(25)
  })
  
  output$mapDep <- renderLeaflet({
    
    vP6077 <- gsub("([0-9]+).*$", "\\1", input$P6077)
    vP6080 <- gsub("([0-9]+).*$", "\\1", input$P6080)
    vP6096 <- gsub("([0-9]+).*$", "\\1", input$P6096)
    vP767 <- as.integer(gsub("([0-9]+).*$", "\\1", input$P767))
    vP6081 <- gsub("([0-9]+).*$", "\\1", input$P6081)
    vP5502 <- gsub("([0-9]+).*$", "\\1", input$P5502)
    
    sujeto<-data.frame(P6077= vP6077,P6080=vP6080,P6096=vP6096,P767=vP767,P6081=vP6081,P5502=vP5502)
    probabilidades <- predict(modelo.fullhd,sujeto,type="prob")
    
    # Guanía no está en la base de datos original, probabilidad siempre será 0%
    probabilidades[30] <- 0
    
    departamentos@data$prob <- probabilidades
    # Generación de números aleatorios, suponiendo que son probabilidades
    # departamentos@data$prob <- runif(33, min=0, max=100)
    
    #Creacion de los atributos del mapa
    pal <-colorNumeric(palette=blue2red(max(departamentos@data$prob*100)),domain=c(0,departamentos@data$prob*100))
    popup<-paste(departamentos@data$DEPTO,paste("Probabilidad ", round(departamentos@data$prob*100,digits=2)," %"),sep="<br/>")
    
    #Definicion del mapa
    mapaDepartamentos <- leaflet(departamentos)
    mapaDepartamentos <- addProviderTiles(mapaDepartamentos, provider="OpenStreetMap.Mapnik")
    mapaDepartamentos <- addPolygons(mapaDepartamentos,
                                     popup = popup,
                                     color=pal(as.integer(departamentos@data$prob*100)),
                                     opacity = 0.5,
                                     fillOpacity = 0.5,
                                     weight = 1)
    mapaDepartamentos <-addLegend(mapaDepartamentos,"topright",pal=pal,values=departamentos@data$prob*100, 
                                  title="Probabilidad",
                                  opacity = 1)
    mapaDepartamentos 
    
  })
})

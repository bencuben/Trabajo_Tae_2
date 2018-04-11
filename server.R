
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(car)
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

  withProgress(message="Cargando datos y modelo", {
    incProgress(20)
    load("models.RData")
    incProgress(20)
  })
  
  output$mapDep <- renderLeaflet({

    vP6077 <- gsub("([0-9]+).*$", "\\1", input$P6077)
    vP6080 <- gsub("([0-9]+).*$", "\\1", input$P6080)
    vP6096 <- gsub("([0-9]+).*$", "\\1", input$P6096)
    vP767 <- as.integer(gsub("([0-9]+).*$", "\\1", input$P767))
    vP6081 <- gsub("([0-9]+).*$", "\\1", input$P6081)
    vP5502 <- gsub("([0-9]+).*$", "\\1", input$P5502)

    sujeto<-data.frame(P6077= vP6077,P6080=vP6080,P6096=vP6096,P767=vP767,P6081=vP6081,P5502=vP5502)
    probabilidades <- predict(model1,sujeto,type="prob")

    # Guanía no está en la base de datos original, probabilidad siempre será 0%
    probabilidades[30] <- 0

    # probabilidades <- seq(1, 1, length=33)
    departamentos@data$prob <- probabilidades

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


  output$mapReg <- renderLeaflet({
    vP6080_2 <- gsub("([0-9]+).*$", "\\1", input$P6080_2)
    vP4567_2 <- gsub("([0-9]+).*$", "\\1", input$P4567_2)
    vP8520S3_2 <- gsub("([0-9]+).*$", "\\1", input$P8520S3_2)
    vP8520S1A1_2 <- gsub("([0-9]+).*$", "\\1", input$P8520S1A1_2)
    vCLASE_2 <- gsub("([0-9]+).*$", "\\1", input$CLASE_2)
    vP767_2 <- as.integer(gsub("([0-9]+).*$", "\\1", input$P767_2))
    vP6081_2 <- as.integer(gsub("([0-9]+).*$", "\\1", input$P6081_2))
    vP5661S4_2 <- gsub("([0-9]+).*$", "\\1", input$P5661S4_2)
    vP6077_2 <- as.integer(gsub("([0-9]+).*$", "\\1", input$P6077_2))
    vP1070_2 <- gsub("([0-9]+).*$", "\\1", input$P1070_2)


    sujeto2<-data.frame(P6080=vP6080_2, P4567=vP4567_2, P8520S3 = vP8520S3_2, P8520S1A1 = vP8520S1A1_2, CLASE = vCLASE_2,
                        P767=vP767_2, P6081 = vP6081_2, P5661S4= vP5661S4_2, P6077 = vP6077_2, P1070= vP1070_2)

    probs <- as.vector(predict(model2,sujeto2,type="prob"))

    departamentos@data$prob2 <- seq(0, 0, length=33)

    departamentos@data[departamentos@data$region == "ANDINA",]$prob2 <- probs[2]
    departamentos@data[departamentos@data$region == "CARIBE",]$prob2 <- probs[3]
    departamentos@data[departamentos@data$region == "PACIFICA",]$prob2 <- probs[5]
    departamentos@data[departamentos@data$region == "ORINOQUIA",]$prob2 <- probs[4]
    departamentos@data[departamentos@data$region == "AMAZONIA",]$prob2 <- probs[1]

    #Creacion de los atributos del mapa 
    pal <-colorNumeric(palette=blue2red(max(departamentos@data$prob2*100)),domain=c(0,departamentos@data$prob2*100))
    popup<-paste(departamentos@data$region,paste("Probabilidad ", round(departamentos@data$prob2*100,digits=2)," %"),sep="<br/>")

    #Definicion del mapa
    mapaDepartamentos <- leaflet(departamentos)
    mapaDepartamentos <- addProviderTiles(mapaDepartamentos, provider="OpenStreetMap.Mapnik")
    mapaDepartamentos <- addPolygons(mapaDepartamentos,
                                     popup = popup,
                                     color=pal(as.integer(departamentos@data$prob2*100)),
                                     opacity = 0.5,
                                     fillOpacity = 0.5,
                                     weight = 1)
    mapaDepartamentos <-addLegend(mapaDepartamentos,"topright",pal=pal,values=departamentos@data$prob2*100,
                                  title="Probabilidad",
                                  opacity = 1)
    mapaDepartamentos

  })

})

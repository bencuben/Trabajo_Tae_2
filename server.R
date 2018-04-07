
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
  
  withProgress(message="Cargando datos", {
    departamentos <- shapefile("Departamentos/departamentos.shp", encoding="UTF-8", use_iconv = TRUE)
    incProgress(20)
    departamentos@data$DPTO_CCDGO <- as.integer(departamentos@data$DPTO_CCDGO)
    incProgress(20)
    departamentos <- departamentos[order(departamentos@data$DPTO_CCDGO),]
    incProgress(20)
    datos <- read.csv("BDs/Caracteristicas y composicion del hogar.csv")
    incProgress(20)
    datos2<- read.csv("BDs/baseUnida.csv")
    incProgress(20)
  })
  
  withProgress(message="Creando modelos", {
    ################################################################################## Modelo por departamento
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
    incProgress(10)
    daticos$P6076S1<-as.factor(daticos$P6076S1)
    
    #modelo luego del step aic
    modelo.fullhd<-multinom(formula = P6076S1 ~ P6077 + P6080 +  + P6096 + 
                              P767 + P6081 + P5502, data = daticos)
    incProgress(25)
    
    ################################################################################### Modelo por región
    datos2<-datos2[which(!is.na(datos2$P6076S1)),]
    
    #seleccion de variables a ojo experto
    
    # +  +  + + 
    
    datos2<- data.frame(FEX_C=datos2$FEX_C,P1070=datos2$P1070,CLASE=datos2$CLASE,P5010=datos2$P5010,CANT_PERSONAS_HOGAR=datos2$CANT_PERSONAS_HOGAR,
                        PERCAPITA=datos2$PERCAPITA,P1894=datos2$P1894,P6020=datos2$P6020,P6040=datos2$P6040,P5502=datos2$P5502,
                        P767=datos2$P767,P6080=datos2$P6080,P6240=datos2$P6240,P6081=datos2$P6081,
                        P6077=datos2$P6077,P6096=datos2$P6096,P6076S1=datos2$P6076S1,P4567=datos2$P4567,P8520S3=datos2$P8520S3,P8520S1A1 =datos2$P8520S1A1,
                        P5661S4=datos2$P5661S4)
    
    #quitar lo que estorba
    
    datos2<-na.omit(datos2)
    
    #deteccion de infinitos
    datos2<-datos2[which(is.finite(datos2$P5502)
                         &is.finite(datos2$P8520S1A1)        ),  ]
    incProgress(25)
    #eliminar variables con inf
    
    #recodificacion
    
    datos2$P1070<-as.factor(datos2$P1070)
    datos2$CLASE<- as.factor(datos2$CLASE)
    datos2$P1894<- as.factor(datos2$P1894)
    datos2$P6020<- as.factor(datos2$P6020)
    datos2$P5502<- as.factor(datos2$P5502)
    datos2$P6080<- as.factor(datos2$P6080)
    datos2$P6240<- as.factor(datos2$P6240)
    datos2$P6076S1<- as.factor(datos2$P6076S1)
    datos2$P4567<- as.factor(datos2$P4567)
    datos2$P8520S3<- as.factor(datos2$P8520S3)
    datos2$P8520S1A1<- as.factor(datos2$P8520S1A1)
    datos2$P5661S4<- as.factor(datos2$P5661S4)
    
    # Variable auxiliar
    var.resp<- as.factor(datos2$P6076S1)
    var2.res<-recode(var.resp,"c(5,11,15,17,18,25,41,54,63,66,68,73)='ANDINA'; c(8,20,23,44,47,70,88)='CARIBE';
                     c(13,19,27,52,76)='PACIFICA'; c(50,81,85,99)='ORINOQUIA'; c(86,91,94,95,97)='AMAZONIA'")
    
    #########
    
    mod.imaginario<-multinom(formula = var2.res ~ P6080 + P4567 + P8520S3+
                               P8520S1A1 + CLASE + P767 + P6081 + P5661S4 + P6077 + P1070, 
                             data = datos2)
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
    
    probs <- as.vector(predict(mod.imaginario,sujeto2,type="prob"))
    
    departamentos@data$prob2 <- seq(0, 0, length=33)
    
    departamentos@data[departamentos@data$region == "ANDINA",]$prob2 <- probs[1]
    departamentos@data[departamentos@data$region == "CARIBE",]$prob2 <- probs[2]
    departamentos@data[departamentos@data$region == "PACIFICA",]$prob2 <- probs[3]
    departamentos@data[departamentos@data$region == "ORINOQUIA",]$prob2 <- probs[4]
    departamentos@data[departamentos@data$region == "AMAZONIA",]$prob2 <- probs[5]
    
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

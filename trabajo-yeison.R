#dirección
#setwd("~/YEISON/MATERIAS/OCTAVO SEMESTRE/TAE/TRABAJO_2/BASES_DE_DATOS2/BDs")

# Lectura de la basede datos

datos <- read.csv("BDs/Caracteristicas y composicion del hogar.csv")

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

# Con esto observamos todos niveles de cada una de las variables y verificamos que no hay valoresinfinitos y nas.

apply(guadanazo[,-c(1,33,34,35,36)],2,table)


#### Procedemos a eliminar todas las variables que consideramos bno singinificativas o aportantes para este caso


### Consideremos 

#var.resp<- as.factor(guadanazo$P6076S1)


#### recodificamos las variables para que sean de tipo categorico
guadanazo<- guadanazo[,-c(1,2,3,4,5,6)]
daticos<-guadanazo

for(i in 1:28){
  daticos[,i]<- as.factor(guadanazo[,i])
}


## Librerias usadas

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library ( MASS )

### Todos los nombres



daticos$P6040<- as.integer(daticos$P6040)
daticos$P767<- as.integer(daticos$P767)
daticos<-daticos[,-11]
daticos<-daticos[,-c(5,7,9)]

paste0(names(daticos),collapse = "+")
daticos$P6076S1<-as.factor(daticos$P6076S1)
#regresion<- multinom(P6076S1~P6020+P6040+P6051+P5502+P6071S1+P767+P6077+P6096+P6081+P6087+P6083+P6088+P6080+P1895+P1896+P1897+P1898+P1899+P1901+P1902+P1903+P1904+P1905+LLAVEHOG+,
#                     data = daticos)
#exp(coef(regresion))
#head(pp <- fitted(regresion))



t#ail(predict(regresion,type="prob"),10)

#mod<-multinom(P6076S1~1,data=daticos)
#stepAIC(mod,direction = "forward",
#        scope=list(upper=~P6020+P6040+P6051+P5502+P6071S1+P767+P6077+P6096+P6081+P6087+P6083+P6088+P6080+P1895+P1896+P1897+P1898+P1899+P1901+P1902+P1903+P1904+P1905+LLAVEHOG+,lower=~1))


#modelo luego del step aic
modelo.fullhd<-multinom(formula = P6076S1 ~ P6077 + P6080 +  + P6096 + 
           P767 + P6081 + P5502, data = daticos)


#definicion de variables auxiliares
obs<-as.vector(daticos$P6076S1)
pre<-as.vector(predict(modelo.fullhd,type = "class"))

# Ciclo para tasa de mal clasificados
cont<-0
for (i in 1:4341){
  
  if(pre[i]==obs[i]){
    cont<-cont+1
    
  }
}
cont/4341

###################### shiny!!!!

#P6077 + P6080 +  + P6096 +P767 + P6081 + P5502

input=NULL
input$P6077<-"2"
input$P6080<-"4"
input$P6096<-"8"
input$P767<-46
input$P6081<-"1"
input$P5502<-"1"
sujeto<-data.frame(P6077=input$P6077,P6080=input$P6080,P6096=input$P6096,P767=input$P767,P6081=input$P6081,P5502=input$P5502)

####probabilidad para los departamentos que hay
predict(modelo.fullhd,sujeto,type="prob")


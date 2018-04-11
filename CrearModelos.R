require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library (MASS)

departamentos <- shapefile("Departamentos/departamentos.shp", encoding="UTF-8", use_iconv = TRUE)
departamentos@data$DPTO_CCDGO <- as.integer(departamentos@data$DPTO_CCDGO)
departamentos <- departamentos[order(departamentos@data$DPTO_CCDGO),]
datos <- read.csv("BDs/Caracteristicas y composicion del hogar.csv")
datos2<- read.csv("BDs/baseUnida.csv")

################################################################################## Modelo por departamento
#### Eliminando los valores ausentes

datos<- na.omit(datos)

#### Eliminamos las columnas debido al alto número de valores "infinitos"

datos<- datos[,-c(21,24,27)]


#### Eliminamos todos los valores infinitos con la función is.finite y dejamos finalmente una basede datos limpia para trabajar

datos<-datos[which(is.finite(datos$P6071)
                   & is.finite(datos$P6071S1)
                   & is.finite(datos$P6088)
                   &  is.finite(datos$P1903)
                   & is.finite(datos$P6087)),  ]
#### Procedemos a eliminar todas las variables que consideramos bno singinificativas o aportantes para este caso

#### recodificamos las variables para que sean de tipo categorico
datos <- datos[,-c(1,2,3,4,5,6)]
for(i in 1:28){
  datos[,i]<- as.factor(datos[,i])
}

### Todos los nombres



datos$P6040<- as.integer(datos$P6040)
datos$P767<- as.integer(datos$P767)
datos<-datos[,-11]
datos<-datos[,-c(5,7,9)]
datos$P6076S1<-as.factor(datos$P6076S1)

#modelo luego del step aic
model1<-multinom(formula = P6076S1 ~ P6077 + P6080 +  + P6096 +
                   P767 + P6081 + P5502, data = datos)

################################################################################### Modelo por región

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

########
model2<-multinom(formula = var2.res ~ P6080 + P4567 + P8520S3+
                   P8520S1A1 + CLASE + P767 + P6081 + P5661S4 + P6077 + P1070,
                 data = datos2)

save(model1,model2,departamentos, file = "models.RData")

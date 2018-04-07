####trabajo para campesinos
#setwd("~/YEISON/MATERIAS/OCTAVO SEMESTRE/TAE/TRABAJO_2/BASES_DE_DATOS2/BDs")

require(nnet)
library(car)
#lectura
datos2<- read.csv("BDs/baseUnida.csv")

#eliminar los NA´s en departamento
datos2<-datos2[which(!is.na(datos$P6076S1)),]

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
var2.res<-recode(var.resp,"c(5,11,15,17,18,25,41,54,63,66,68,73)='ANDINA';c(8,20,23,44,47,70,88)='CARIBE';c(13,19,27,52,76)='PACIFICA';c(50,81,85,99)='ORINOQUIA';c(86,91,94,95,97)='AMAZONIA'")


#paste0(names(datos2),collapse = "+")
#modelito<-multinom(var2.res~FEX_C+P1070+CLASE+P5010+CANT_PERSONAS_HOGAR+PERCAPITA+P1894+P6020+P6040+P5502+P767+P6080+P6240+P6081+P6077+P6096+P4567+P8520S3+P8520S1A1+P5661S4
#                   ,data=datos2)


#mod.imaginario<- stepAIC(vacio,direction = "forward",scope = list(upper=~FEX_C+P1070+CLASE+P5010+CANT_PERSONAS_HOGAR+PERCAPITA+P1894+P6020+P6040+P5502+P767+P6080+P6240+P6081+P6077+P6096+P4567+P8520S3+P8520S1A1+P5661S4,lower=~1))

#mod.imaginario$call
#########

mod.imaginario<-multinom(formula = var2.res ~ P6080 + P4567 + P8520S3+
                           P8520S1A1 + CLASE + P767 + P6081 + P5661S4 + P6077 + P1070, 
                         data = datos2)

input <- NULL
input$P6080_2 <- "1"
input$P4567_2 <- "1"
input$P8520S3_2 <- "1"
input$P8520S1A1_2<- "1"
input$CLASE_2<- "1"
input$P767_2<- 1
input$P6081_2 <- 1
input$P5661S4_2 <- "1"
input$P6077_2 <- 1
input$P1070_2 <- "1"

sujeto2<-data.frame(P6080=input$P6080_2, P4567=input$P4567_2, P8520S3 = input$P8520S3_2, 
                   P8520S1A1 =input$P8520S1A1_2, CLASE = input$CLASE_2, P767=input$P767_2, P6081 = input$P6081_2,
                   P5661S4= input$P5661S4_2, P6077 = input$P6077_2, P1070= input$P1070_2)


predict(mod.imaginario,sujeto2,type="prob")

####trabajo para campesinos
#setwd("~/YEISON/MATERIAS/OCTAVO SEMESTRE/TAE/TRABAJO_2/BASES_DE_DATOS2/BDs")

require(nnet)
library(car)
#lectura
datos<- read.csv("BDs/baseUnida.csv")

#eliminar los NA´s en departamento
datos2<-datos[which(!is.na(datos$P6076S1)),]

#seleccion de variables a ojo experto

# +  +  + + 

datos3<- data.frame(FEX_C=datos2$FEX_C,P1070=datos2$P1070,CLASE=datos2$CLASE,P5010=datos2$P5010,CANT_PERSONAS_HOGAR=datos2$CANT_PERSONAS_HOGAR,
                    PERCAPITA=datos2$PERCAPITA,P1894=datos2$P1894,P6020=datos2$P6020,P6040=datos2$P6040,P5502=datos2$P5502,
                    P767=datos2$P767,P6080=datos2$P6080,P6240=datos2$P6240,P6081=datos2$P6081,
                    P6077=datos2$P6077,P6096=datos2$P6096,P6076S1=datos2$P6076S1,P4567=datos2$P4567,P8520S3=datos2$P8520S3,P8520S1A1 =datos2$P8520S1A1,
                    P5661S4=datos2$P5661S4)

#quitar lo que estorba
summary(is.na(datos3))

datos3<-na.omit(datos3)

#deteccion de infinitos
apply(datos3[,-c(1,6)],2,table)

datos3<-datos3[which(is.finite(datos3$P5502)
                      &is.finite(datos3$P8520S1A1)        ),  ]

#eliminar variables con inf
#datos3<- datos3[,-c(15,13)]

#recodificacion

datos3$P1070<-as.factor(datos3$P1070)
datos3$CLASE<- as.factor(datos3$CLASE)
datos3$P1894<- as.factor(datos3$P1894)
datos3$P6020<- as.factor(datos3$P6020)
datos3$P5502<- as.factor(datos3$P5502)
datos3$P6080<- as.factor(datos3$P6080)
datos3$P6240<- as.factor(datos3$P6240)
datos3$P6076S1<- as.factor(datos3$P6076S1)
datos3$P4567<- as.factor(datos3$P4567)
datos3$P8520S3<- as.factor(datos3$P8520S3)
datos3$P8520S1A1<- as.factor(datos3$P8520S1A1)
datos3$P5661S4<- as.factor(datos3$P5661S4)

# Variable auxiliar
var.resp<- as.factor(datos3$P6076S1)
var2.res<-recode(var.resp,"c(5,11,15,17,18,25,41,54,63,66,68,73)='ANDINA';c(8,20,23,44,47,70,88)='CARIBE';c(13,19,27,52,76)='PACIFICA';c(50,81,85,99)='ORINOQUIA';c(86,91,94,95,97)='AMAZONIA'")


#paste0(names(datos3),collapse = "+")
#modelito<-multinom(var2.res~FEX_C+P1070+CLASE+P5010+CANT_PERSONAS_HOGAR+PERCAPITA+P1894+P6020+P6040+P5502+P767+P6080+P6240+P6081+P6077+P6096+P4567+P8520S3+P8520S1A1+P5661S4
#                   ,data=datos3)
vacio<- multinom(var2.res~1,data = datos3)


#mod.imaginario<- stepAIC(vacio,direction = "forward",scope = list(upper=~FEX_C+P1070+CLASE+P5010+CANT_PERSONAS_HOGAR+PERCAPITA+P1894+P6020+P6040+P5502+P767+P6080+P6240+P6081+P6077+P6096+P4567+P8520S3+P8520S1A1+P5661S4,lower=~1))

#mod.imaginario$call
#########

mod.imaginario<-multinom(formula = var2.res ~ P6080 + FEX_C + P4567 + P8520S3 + P6080+
                           P8520S1A1 + CLASE + P767 + P6081 + P5661S4 + P6077 + P1070, 
                         data = datos3)

obs<-as.vector(var2.res)
pre<-predict(mod.imaginario,type = "prob")

# Ciclo para tasa de bien clasificados
cont<-0
for (i in 1:3870){
  #print(c(i,pre[i],obs[i]))
  if(pre[i]==obs[i]){
    cont<-cont+1
    
  }
}
round(cont/3870*100,2)





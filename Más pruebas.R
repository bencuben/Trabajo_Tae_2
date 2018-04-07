library(raster)
library(car)
departamentos <- shapefile("Departamentos/departamentos.shp", encoding="UTF-8", use_iconv = TRUE)
departamentos@data$DPTO_CCDGO <- as.integer(departamentos@data$DPTO_CCDGO)
departamentos <- departamentos[order(departamentos@data$DPTO_CCDGO),]
departamentos@data$region <- seq(0, 0, length=33)
  

departamentos@data$region <- recode(departamentos@data$DPTO_CCDGO,"c(5,11,15,17,25,41,54,63,66,68,73)='ANDINA';c(8,13,20,23,44,47,70,88)='CARIBE';c(19,27,52,76)='PACIFICA';c(50,81,85,99)='ORINOQUIA';c(18,86,91,94,95,97)='AMAZONIA'")

departamentos@data$prob2 <- seq(0, 0, length=33)


prob <- c(5,10,30,40,80)
departamentos@data$prob <- recode(departamentos@data$DPTO_CCDGO,"c(5,11,15,17,25,41,54,63,66,68,73)=prob[1];c(8,13,20,23,44,47,70,88)=prob[2];c(19,27,52,76)=prob[3];c(50,81,85,99)=prob[4];c(18,86,91,94,95,97)=prob[5]")


library(maptools)
writeSpatialShape(departamentos, "departamentos")

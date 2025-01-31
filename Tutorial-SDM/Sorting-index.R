library(terra)

v <-  vect("Cuadricula-brad.gpkg")

byn <- rep(c(1, 0), 14)
byn1 <-  rep(c(0, 1), 14)

byn2 <- rep(c(byn, byn1), 15)

v$ByN <- byn2

writeVector(v, "Tutorial-SDM/Cuadricula-brad.gpkg", overwrite = T)

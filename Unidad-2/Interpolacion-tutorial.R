library(raster)

puntos <- read.csv("Datos-ejemplos/Datos-puntos-Moran.csv")

with(puntos, plot(Longitud, Latitud, cex = Mediciones/10))

vo <- dismo::voronoi(puntos)

vo$Mediciones <- puntos$Mediciones

r <- raster("Datos-ejemplos/Var-1.tif")

r.vo <- rasterize(x = vo, y = r,  field = "Mediciones")

plot(r.vo)

## Regresión sobre coordenadas

m1 <- lm(Mediciones ~ Latitud + Longitud, data = puntos)
plot(m1)

summary(m1)

nuevos <- data.frame(coordinates(r))

#Cambiar nombres de columnas
names(nuevos) <- c("Longitud", "Latitud")

predic <- predict(m1, newdata = nuevos)

nuevos$Prediccion <- predic

pred.r <- rasterFromXYZ(nuevos)

plot(pred.r)


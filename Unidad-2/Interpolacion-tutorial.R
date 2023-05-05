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

## Splines

library(mgcv)

spl <- gam(Mediciones ~ s(Longitud, Latitud, k = 25), data = puntos)

new.data <- r
new.data <- data.frame(coordinates(new.data))

names(new.data) <- c("Longitud", "Latitud")

spl.pred <- predict(spl, newdata = new.data)

r.spl <- rasterFromXYZ(data.frame(new.data, spl.pred))

plot(r.spl, main = "Interpolalción con splines")
points(puntos, pch = 20, col = "red", cex = 0.5)

## IDW

library(gstat); library(raster)

r.0 <- raster("Datos-ejemplos/Var-1.tif")
#Formato de puntos de mediciones
datos.sp <- puntos
coordinates(datos.sp) <- ~ Longitud + Latitud
proj4string(datos.sp) <- CRS(proj4string(r.0))
#Formato de ubicaciones a interpolar
new.data <- data.frame(rasterToPoints(r.0))[, 1:2]
names(new.data) <- c("Longitud", "Latitud")
coordinates(new.data) <- ~ Longitud + Latitud
proj4string(new.data) <- CRS(proj4string(r.0))
#Interpolación
inv.dist <- idw(formula = Mediciones ~ 1, locations = datos.sp, newdata = new.data)
#Creando raster interpolado
r.idw <- rasterFromXYZ(data.frame(coordinates(new.data), inv.dist$var1.pred))

#Gráfica
par(mfrow = c(1, 2))
plot(r.idw, main = "Inverso de la distancia")
plot(r.idw)
points(datos.sp, pch = 20, col = "red", cex = 0.5)

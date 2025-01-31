library(dismo); library(terra)

############# Preparación de datos

# Puntos de presencia

brad.p <- read.csv("Bradypus-sp.csv")

brad.sp <- vect(brad.p[, c("lon", "lat")])

#Capas raster

capas.r <- rast("Capas.tif")
capas.r <- scale(capas.r)

par(mar = c(1,1,1,1))
plot(capas.r[[1]]); points(brad.sp)

buf <- buffer(brad.sp, width = 10)

par(mar = c(1,1,1,1))
plot(capas.r[[1]]); points(brad.p$lon, brad.p$lat)
plot(buf, add = T)

capas.rm <- mask(capas.r, buf)

plot(capas.rm[[1]])
points(brad.sp)

########### Análisis exploratorio

dir.create("Tutorial-SDM")
png("Tutorial-SDM/Pares.png", width = 3000, height = 3000)
pairs(capas.rm)
dev.off()

## Modelo de puntos Poisson

source("imFromStack.R")
source("winFromRaster.R")
source("plotQuantIntens.R")

capas.im <- imFromStack(capas.rm)
w <- as.owin(capas.im[[1]])

brad.ppp <- ppp(x = brad.p$lon, y = brad.p$lat, window = w, check = F)

Q <- pixelquad(brad.ppp, W = w)

plotQuantIntens(imList = capas.im,
                noCuts = 10,
                Quad = Q,
                p.pp = brad.ppp,
                dir = "",
                name = "Bradypus")

## Datos completos en formato de tabla

k <- envelope(brad.ppp, nsim = 39, "Kest")
plot(k)

## Regresión Poisson

modelo <- ppm(brad.ppp ~ bio7 + bio8 + bio16 + bio17 +
                I(bio7^2) + I(bio8^2) + I(bio16^2) + I(bio17^2), 
              covariates = capas.im)

summary(modelo)

km <-  envelope(modelo, nsim = 39, "Kest")
plot(km)

modelo.1 <- ppm(brad.ppp ~ bio1 + bio12 + bio7 +
                  I(bio1^2) + I(bio12^2)+ I(bio7^2), 
                covariates = capas.im)

modelo.2 <- ppm(brad.ppp ~ bio8 + bio12 + bio7 +
                  I(bio8^2) + I(bio12^2)+ I(bio7^2), 
                covariates = capas.im)

modelo.3 <- ppm(brad.ppp ~ bio5 + bio12 + bio7 +
                  I(bio5^2) + I(bio12^2)+ I(bio7^2), 
                covariates = capas.im)

modelo.4 <- ppm(brad.ppp ~ bio1 + bio17 + bio16 + bio7 +
                  I(bio1^2) + I(bio17^2) + I(bio16^2)+ I(bio7^2), 
                covariates = capas.im)

AIC(modelo); AIC(modelo.1); AIC(modelo.2)
AIC(modelo.3); AIC(modelo.4)

modelo.4.step <- step(modelo.4)

summary(modelo.4.step)

km4 <-  envelope(modelo.4.step, nsim = 39, "Kest")
plot(km4)

pred.modelo <- predict(modelo.4.step, dimyx = c(192, 186))

plot(pred.modelo)

points(brad.ppp, pch = 20, cex = 0.1, col = "green")

## Mapas binarios

diagnose.ppm(modelo.4.step)

saveRDS(modelo.4.step, "Modelo-ppm.rds")

## Distribución binaria

pred.r <- rast(pred.modelo)

points(brad.sp)

# Valores de favorabilidad predichos en localidades de presencia:

val.pres <- extract(pred.r, brad.sp)

umbral.05 <- quantile(val.pres$lyr.1, 0.05)

dist.05 <-  pred.r > umbral.05

plot(dist.05)
points(brad.sp, pch = 20, cex = 0.5)

## Guardando los resultados

writeRaster(dist.05, "Distribucion.tif", overwrite = T)
writeRaster(pred.r, "Favorabilidad.tif", overwrite = T)


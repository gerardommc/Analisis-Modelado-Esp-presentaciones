library(dismo); library(terra)

############# Preparación de datos

# Puntos de presencia

brad.p <- read.csv("Bradypus-sp.csv")

brad.sp <- vect(brad.p[, c("lon", "lat")])

#Capas raster

capas.r <- rast("Capas.tif")

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

modelo <- ppm(brad.ppp ~ bio1 + bio12 + bio7 +
                I(bio1^2) + I(bio12^2) + I(bio7^2), 
              covariates = capas.im)

modelo.1 <- ppm(brad.ppp ~ bio1 + bio16 + bio7 +
                  I(bio1^2) + I(bio16^2)+ I(bio7^2), 
                covariates = capas.im)

modelo.2 <- ppm(brad.ppp ~ bio1 + bio17 + bio7 +
                  I(bio1^2) + I(bio17^2)+ I(bio7^2), 
                covariates = capas.im)

AIC(modelo); AIC(modelo.1); AIC(modelo.2)

mod.step <- step(modelo.1)

AIC(mod.step)

summary(mod.step)

pred.modelo <- predict(mod.step, dimyx = c(192, 186))

plot(pred.modelo)

points(brad.ppp, pch = 20, cex = 0.1, col = "green")

## Mapas binarios

k.mod <- envelope(mod.step, nsim = 39, "Kest")
plot(k.mod)

diagnose.ppm(mod.step)

saveRDS(mod.step, "Modelo-ppm.rds")

## Distribución binaria

pred.r <- rast(pred.modelo)

# Valores de favorabilidad predichos en localidades de presencia:

val.pres <- extract(pred.r, brad.sp)

umbral.05 <- quantile(val.pres$lyr.1, 0.05)

dist.05 <-  pred.r > umbral.05

plot(dist.05)

## Guardando los resultados

writeRaster(dist.05, "Distribucion.tif")
writeRaster(pred.r, "Favorabilidad.tif")


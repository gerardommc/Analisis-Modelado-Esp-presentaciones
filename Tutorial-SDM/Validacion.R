library(terra)

v <- vect("Cuadricula-brad-mascara-0.gpkg")

brad.p <- read.csv("Bradypus-sp.csv")

brad.sp <- vect(brad.p[, c("lon", "lat")])

#Capas raster

capas.r <- rast("Capas.tif")
capas.r <- scale(capas.r)

capas.m <- mask(capas.r, v, touches = T)

par(mar = c(1,1,1,1))
plot(capas.m[[1]]); points(brad.sp)

## Eliminando puntos en cuadros vacios

nas <- extract(capas.m[[1]], brad.sp)

brad.p$nas <- nas

brad.p <- na.omit(brad.p)

source("imFromStack.R")

capas.im <- imFromStack(capas.m)
w <- as.owin(capas.im[[1]])

brad.ppp <- ppp(x = brad.p$lon, y = brad.p$lat, window = w, check = F)

plot(capas.im[[1]]); points(brad.ppp, col = "green", pch = 20, cex = 0.5)

modelo <- readRDS("Modelo-ppm.rds")

mod.val <- ppm(brad.ppp ~ bio17 + bio16 + bio7 + I(bio1^2), 
               covariates = capas.im)

### Volviendo a crear lista de imagenes sin huecos

#Capas raster

capas.1 <- rast("Capas.tif")
capas.1 <- scale(capas.1)

buf <- buffer(brad.sp, width = 10)

capas.1 <- mask(capas.1, buf)

capas.im1 <- imFromStack(capas.1)

w2 <- as.owin(capas.im1[[1]])

#Predicción

mod.val.pred <- predict(mod.val, covariates = capas.im1, 
                        window = w2, dimyx = c(186, 192))
plot(mod.val.pred)

# Transformando a raster

pred.val.r <- rast(mod.val.pred)

# Puntos en 1s

v1 <- vect("Cuadricula-brad-mascara-1.gpkg")

brad.1 <- read.csv("Bradypus-sp.csv")[, -1]

capas.1v1 <- mask(capas.1, v1)

nas1 <- extract(capas.1v1[[1]], brad.1)

brad.1$nas <- nas1$bio1
brad.1 <- na.omit(brad.1)

## Graficando las predicciones del modelo de validación

plot(pred.val.r); points(brad.1)

global(pred.val.r, range, na.rm = T)

#Vector de valores humbral

umbrales <- seq(0, 0.25, len = 25)

umbrales.r <- pred.val.r > umbrales

valid.umb <- extract(umbrales.r, brad.1[, 1:2])

medias <- colMeans(valid.umb)
area <- global(umbrales.r, mean, na.rm = T)

plot(area$mean, medias[-1])

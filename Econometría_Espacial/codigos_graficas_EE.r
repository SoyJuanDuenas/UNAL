#Instalación de Spatmap desde GitHub
#install.packages("remotes")
#remotes::install_github("morrisonge/spatmap")

#Empezamos descargando librerias y archivos de interes

library(spdep)
library(rgeos)
library(rgdal)
library(spatialreg)
library(tmap)
library(sf)
library(ggplot2)
library(plotKML)
library(RColorBrewer)
library(cartogram)
library(stringr)
library(spatmap)
library(tidyverse)
library(leaflet)
library(dplyr)
library(maptools)
library(tmaptools)

setwd("C:/Users/PC/Downloads/Códigos/Códigos")
source("northarrow.R")
source("scalebar.R")
source("moranbi.test.R")
source("moran.cluster.R")
source("moran.bi.R")
source("moran.cluster.R")
source("localmoran.bi.R")
source("moranbi.plot.R")
source("quantile.e.R")
source("correlogram.d.R")
source("randomize_vector.R")
source("spcorrelogram.bi.R")
source("moranbi1.test.R")
source("moran.bi1.R")

setwd("C:/Users/PC/Downloads/UPZ_EM2017")
UPZ_Bog <- readOGR(dsn="ShapeTesis_Ocupa.shp")

#Datos importantes matriz tipo Rook
rook.fr<-poly2nb(UPZ_Bog, queen = FALSE)
summary(rook.fr)
#Graficar conexiones tipo Rook
plot(UPZ_Bog, border="grey")
plot(rook.fr,  coordinates(UPZ_Bog),  add=TRUE,  col="red")


#Datos importantes matriz tipo Queen
queen.fr <- poly2nb(UPZ_Bog, queen = TRUE)
summary(queen.fr)
#Graficar Conexiones tipo Queen
upz_nbr <- poly2nb(UPZ_Bog, queen=T)
plot(UPZ_Bog, col="white", border="grey", lwd=1)
plot(upz_nbr, coordinates(UPZ_Bog), col="red", lwd=1, add=TRUE)

#Simulación por MonteCarlo de Test de Moran para matriz reina
#Pendiente entender resultados e interpretación del test

Wq<-nb2listw(upz_nbr, style="W")
moran.test(UPZ_Bog$SharePetro, listw=Wq, randomisation = FALSE, alternative = "two.sided")
moran.mc(UPZ_Bog$SharePetro, Wq, nsim=99)

#cambiamos el sistema de coordenadas

UPZ_Bog_Geo <- spTransform(UPZ_Bog, "+proj=longlat")
coordsgeo <- coordinates(UPZ_Bog_Geo)

#Ahora creamos una matriz de vecinos basada en distancia, con esto podemos colocar en un vector cuantos vecinos tiene cada punto

k.poly <- knn2nb(knearneigh(coordsgeo, longlat = TRUE))
critical.threshold.poly <- max(unlist(nbdists(k.poly,coordsgeo, longlat = TRUE)))
nb.dist.band.poly <- dnearneigh(coordsgeo, 0, critical.threshold.poly, longlat = TRUE)

#Graficamos un histograma con el numero de vecinos por cada vecindario

poly.nb.card <- card(nb.dist.band.poly)
ggplot() +
  geom_histogram(aes(x=poly.nb.card)) +
  xlab("Number of Neighbors")

#Graficamos el mapa de Bogotá y sobre este las conexiones entre los vecinos de la matriz basada en distancia

plot(UPZ_Bog_Geo,col="gray", border="blue", lwd=1)
plot(nb.dist.band.poly, coordsgeo, add=T, lwd=.2, col="red", cex = .5)

# Uso matriz con banda de distancia para estimación del índice de moran via normalidad y via monte carlo

wkd <- nb2listw(neighbours=nb.dist.band.poly, style="W")
moran.test(UPZ_Bog$SharePetro, listw=wkd, randomisation = FALSE, alternative = "two.sided")
moran.mc(UPZ_Bog$SharePetro, wkd, nsim=99)

#podemos hacer este proceso tambien con un numero de k vecinos especificado

k6 <- knn2nb(knearneigh(coordsgeo, k = 6))
plot(UPZ_Bog_Geo,col="gray", border="blue", lwd=1)
plot(k6, coordsgeo, add=T, lwd=.2, col="red", cex = .5)

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

setwd("C:/Users/Rama IEEE UN/Downloads/Códigos")
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

setwd("C:/Users/Rama IEEE UN/Downloads/UPZ_EM2017")
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
plot(UPZ_Bog, col="white", border="grey", lwd=1)
plot(upz_nbr, coordinates(UPZ_Bog), col="red", lwd=1, add=TRUE)

#Simulación por MonteCarlo de Test de Moran para matriz reina
#Pendiente entender resultados e interpretación del test

Wq<-nb2listw(upz_nbr, style="W")
moran.test(UPZ_Bog$SharePetro, listw=Wq, randomisation = FALSE, alternative = "two.sided")
moran.mc(UPZ_Bog$SharePetro, Wq, nsim=99)

proj4string(UPZ_Bog)
projInfo(type = "proj")
UPZ_Bog_Geo <- spTransform(UPZ_Bog, "+proj=longlat")
UPZ_Bog_Geo@bbox
coordsgeo <- coordinates(UPZ_Bog_Geo)


#Empezamos limpiando el entorno
remove(list = ls())

#paquetes

library(forecast)    # Para hacer pronósticos con modelos arima
library(lmtest)      # Significancia individual de los coeficientes ARIMA
library(urca)        # Prueba de raíz unitaria
library(tseries)     # Para estimar modelos de series de tiempo y hacer pruebas de supuestos.
library(readxl)      # Para leer archivos de excel
library(stargazer)   # Para presentar resultados más estéticos.
library(psych)       # Para hacer estadísticas descriptivas
library(seasonal)    # Para desestacionalizar series
library(aTSA)        # Para hacer la prueba de efectos ARCH
library(astsa)       # Para estimar, validar y hacer pronósticos para modelos ARIMA/SARIMA  
library(xts)         # Para utilizar objetos xts 
library(tidyverse)   # Conjunto de paquetes (incluye dplyr y ggplot2)
library(readxl)      # Para leer archivos excel 
library(car)         # Para usar la función qqPlot

#Lo primero que hacemos es correr determinados modelos sencillos para entender su comportamiento
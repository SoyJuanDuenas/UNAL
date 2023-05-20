#Limpiamos el environment.
remove(list = ls())

#Activamos algunos paquetes que vamos a utilizar en toda la metodología

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

desempleo = read.csv("https://raw.githubusercontent.com/SoyJuanDuenas/UNAL/master/Econometr%C3%ADa_II/Desempleo_col.csv") 


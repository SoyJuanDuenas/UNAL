##____________________________
##____________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRÍA II - 2023-I
#                             Trabajo 1, Marzo 
##____________________________
##____________________________

remove(list = ls())

#Iniciamos importando librerias

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

# Para la realización del trabajo se uilizará el Índice de producción real de la 
# industria manufacturera colombiana

setwd("C:/Users/PC/Documents/Repos/UNAL/Econometría_II")
ipr =  read_excel("srea_027.xls", range= cell_rows(9:480))
colnames(ipr) <- c("Año", "Mes", "Total_industria", "Total_sin_trilla")


#Iniciamos el Data-cleaning y generamos el archivo ts

ipr = select(ipr, -"Total_sin_trilla")

for (i in 1:558){                   
  if (is.na((ipr[i,"Año"])) == FALSE){
    valor = ipr[i,"Año"]
  }else{
    ipr[i,"Año"] = valor
  }
}

ipr = na.omit(ipr)

ipr_ts = ts(ipr$Total_industria, start = 1980, frequency = 12)

#Identificación

plot(ipr_ts)
ipr_fac = acf(ipr_ts, plot = F)
ipr_facp = pacf(ipr_ts, plot = F)

plot(ipr_fac, main="Función de Autocorrelación (FAC) de la IPR", xlab="Retraso", ylab="Autocorrelación")
plot(ipr_facp, main="Función de Autocorrelación Parcial (FACP) de la IPR", xlab="Retraso", ylab="Autocorrelación Parcial")


#Nos damos cuenta que tenemos problemas de estacionareidad via grafica, por ende aplicamos transformaciones
#Especialmente, transformaciones logaritmica y luego diferencia para estabilizar media y varianza

ipr_diff = diff(log(ipr_ts))

ipr_fac_diff = acf(ipr_diff, plot = F)
ipr_facp_diff = pacf(ipr_diff, plot = F)

plot(ipr_fac_diff, main="Función de Autocorrelación (FAC) Log_Diff_IPR", xlab="Retraso", ylab="Autocorrelación")
plot(ipr_facp_diff, main="Función de Autocorrelación Parcial (FACP)_Log_Diff_IPR", xlab="Retraso", ylab="Autocorrelación Parcial")

adf.test(ipr_diff)
kpss.test(ipr_diff)

#Al no funcionar estas transformaciones realizaremos un promedio trimestral 
ipr_trimestral = aggregate(ipr_ts, nfrequency = 4, FUN = mean)

#verificamos via grafica

plot(ipr_trimestral)
ipr_fac_tri = acf(ipr_trimestral, plot = F)
ipr_facp_tri = pacf(ipr_trimestral, plot = F)

plot(ipr_fac_tri, main="Función de Autocorrelación (FAC) IPR_trimestral", xlab="Retraso", ylab="Autocorrelación")
plot(ipr_facp_tri, main="Función de Autocorrelación Parcial (FACP) IPR_trimestral", xlab="Retraso", ylab="Autocorrelación Parcial")

adf.test(ipr_trimestral)
kpss.test(ipr_trimestral)

#Nos damos cuenta que tenemos problemas de estacionareidad via grafica y formal, por ende aplicamos transformaciones
#Especialmente, transformacion logaritmica y diferencial para estabilizar media y varianza

ipr_tri_diff = diff(ipr_trimestral)

ipr_fac_tri_diff = acf(ipr_tri_diff, plot = F)
ipr_facp_tri_diff = pacf(ipr_tri_diff, plot = F)

plot(ipr_fac_tri_diff, main="Función de Autocorrelación (FAC) IPR (Trimestral)", xlab="Retraso", ylab="Autocorrelación")
plot(ipr_facp_tri_diff, main="Función de Autocorrelación Parcial (FACP) IPR (Trimestral)", xlab="Retraso", ylab="Autocorrelación Parcial")

adf.test(ipr_tri_diff)
kpss.test(ipr_tri_diff)

#Empezamos a identificar el modelo teniendo en cuenta que hicimos un diferencial, es decir que será de la forma ARIMA(p,1,q)

ipr_arima_1 = arima(ipr_ts, order = c(1,1,2))
ipr_arima_2 = arima(ipr_ts, order = c(2,1,1))

AIC(ipr_arima_2)
BIC(ipr_arima_2)
AIC(ipr_arima_1)
BIC(ipr_arima_1)

#elegimos el segundo modelo dado que es el que tiene un menor AIC y BIC

arima_211_fac = acf(ipr_arima_2$residuals, plot = F)
arima_211_facp = pacf(ipr_arima_2$residuals, plot = F)

plot(arima_211_fac, main="Función de Autocorrelación (FAC)", xlab="Retraso", ylab="Autocorrelación")
plot(arima_211_facp, main="Función de Autocorrelación Parcial (FACP)", xlab="Retraso", ylab="Autocorrelación Parcial")

#Verificamos supuestos

checkresiduals(ipr_arima_2)

#hacemos predicción

forecast_arima_211 <- forecast(ipr_arima_2, lead = 10)




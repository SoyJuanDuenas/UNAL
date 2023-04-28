##____________________________
##____________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRÍA II - 2023-I
#                             Trabajo 1, Marzo 
##____________________________
##____________________________

remove(list = ls())

#Iniciamos importando librerias
#install.packages("forecast")    # Para hacer pronósticos con modelos arima
#install.packages("lmtest")      # Significancia individual de los coeficientes ARIMA
#install.packages("urca")        # Prueba de raíz unitaria
#install.packages("tseries")     # Para estimar modelos de series de tiempo y hacer pruebas de supuestos.
#install.packages("readxl")      # Para leer archivos de excel
#install.packages("stargazer")   # Para presentar resultados más estéticos.
#install.packages("psych")       # Para hacer estadísticas descriptivas
#install.packages("seasonal")    # Para desestacionalizar series
#install.packages("aTSA")        # Para hacer la prueba de efectos ARCH
#install.packages("astsa")       # Para estimar, validar y hacer pronósticos para modelos ARIMA/SARIMA  
#install.packages("xts")         # Para utilizar objetos xts 
#install.packages("tidyverse")   # Conjunto de paquetes (incluye dplyr y ggplot2)
#install.packages("readxl")      # Para leer archivos excel 
#install.packages("car")         # Para usar la función qqPlot


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


ipr =  read_csv("https://raw.githubusercontent.com/SoyJuanDuenas/UNAL/master/Econometr%C3%ADa_II/srea_027.csv")
ipr <- slice(ipr, 9:n())
colnames(ipr) <- c("Año", "Mes", "Total_industria", "Total_sin_trilla")

#Iniciamos el Data-cleaning y generamos el archivo ts

for (i in 1:558){                   
  if (is.na((ipr[i,"Año"])) == FALSE){
    valor = ipr[i,"Año"]
  }else{
    ipr[i,"Año"] = valor
  }
}

ipr = select(ipr, Año, Mes, Total_industria)
ipr <- na.omit(ipr)
ipr$Total_industria = as.numeric(ipr$Total_industria)

#Identificación

ipr_fac = acf(ipr$Total_industria, plot = F)
ipr_facp = pacf(ipr$Total_industria, plot = F)

plot(ipr_fac, main="Función de Autocorrelación (FAC) de la IPR", xlab="Retraso", ylab="Autocorrelación")
plot(ipr_facp, main="Función de Autocorrelación Parcial (FACP) de la IPR", xlab="Retraso", ylab="Autocorrelación Parcial")


ipr_ts = ts(ipr$Total_industria, start = 1980, frequency = 12)
plot(ipr_ts)

#Nos damos cuenta que tenemos problemas de estacionareidad via grafica, por ende aplicamos transformaciones
#Especialmente, transformaciones logaritmica y luego diferencia para estabilizar media y varianza

ipr_diff = diff(log(ipr_ts))

ipr_fac_diff = acf(ipr_diff, plot = F)
ipr_facp_diff = pacf(ipr_diff, plot = F)

plot(ipr_fac_diff, main="Función de Autocorrelación (FAC) Log_Diff_IPR", xlab="Retraso", ylab="Autocorrelación")
plot(ipr_facp_diff, main="Función de Autocorrelación Parcial (FACP)_Log_Diff_IPR", xlab="Retraso", ylab="Autocorrelación Parcial")


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

plot(ipr_tri_diff)
plot(ipr_fac_tri_diff, main="Función de Autocorrelación (FAC) IPR (Trimestral)", xlab="Retraso", ylab="Autocorrelación")
plot(ipr_facp_tri_diff, main="Función de Autocorrelación Parcial (FACP) IPR (Trimestral)", xlab="Retraso", ylab="Autocorrelación Parcial")

adf.test(ipr_tri_diff)
kpss.test(ipr_tri_diff)

#Nos damos cuenta que hay problemas de varianza en 2020 y 2021 como consecuencia de los impactos en el
#indice que tuvo la pandemia

ipr_ts = ts(ipr$Total_industria, start = 1980, end=2019, frequency = 12)
ipr_trimestral = aggregate(ipr_ts, nfrequency = 4, FUN = mean)
ipr_tri_diff = diff(ipr_trimestral)
plot(ipr_tri_diff)

#Empezamos a identificar el modelo teniendo en cuenta que hicimos un diferencial, es decir que será de la forma ARIMA(p,1,q)

ipr_arima_1 = arima(ipr_ts, order = c(1,1,2))
ipr_arima_2 = arima(ipr_ts, order = c(2,1,1))

AIC(ipr_arima_2)
AIC(ipr_arima_1)
BIC(ipr_arima_2)
BIC(ipr_arima_1)

#elegimos el segundo modelo dado que es el que tiene un menor AIC y BIC

arima_211_fac = acf(ipr_arima_2$residuals, plot = F)
arima_211_facp = pacf(ipr_arima_2$residuals, plot = F)

plot(arima_211_fac, main="Función de Autocorrelación (FAC)", xlab="Retraso", ylab="Autocorrelación")
plot(arima_211_facp, main="Función de Autocorrelación Parcial (FACP)", xlab="Retraso", ylab="Autocorrelación Parcial")

#Verificamos supuestos

# Realizar prueba de Jarque-Bera
jb_test <- jarque.bera.test(ipr_arima_2$residuals)

# Imprimir resultados de la prueba
print(jb_test)

# Interpretar resultados de la prueba
if (jb_test$p.value < 0.05) {
  print("La serie no tiene una distribución normal")
} else {
  print("La serie tiene una distribución normal")
}

# Realizar prueba de Ljung-Box
lb_test <- Box.test(ipr_arima_2$residuals, type = "Ljung-Box")

# Imprimir resultados de la prueba
print(lb_test)

# Interpretar resultados de la prueba
if (lb_test$p.value < 0.05) {
  print("La serie esta correlacionada serialmente")
} else {
  print("La serie no esta correlacionada serialmente")
}

# Realizamos la prueba ARCH
arch_test = arch.test(ipr_arima_2)
print(arch_test)

#hacemos predicción

forecast_arima_211 <- forecast(ipr_arima_2, lead = 10)






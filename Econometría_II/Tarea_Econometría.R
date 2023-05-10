#Iniciamos descargando las librerias requeridas

## Limpiar el entorno de objetos
remove(list = ls())

# Se cargan los paquetes que vamos a utilizar. 
library(urca)
library(car)
library(rlang)
library(forecast)
library(tseries)
library(readxl)
library(dynlm)
library(broom)
library(aTSA)

#Importamos el DataSet, creamos los objetos time series

series = read.csv("https://raw.githubusercontent.com/SoyJuanDuenas/UNAL/master/Econometr%C3%ADa_II/pib_consumo.csv")
series <- na.omit(series)
consumo_ts = ts(series$Consumo.final..real)
pib_ts = ts(series$Producto.Interno.Bruto..PIB..real..Trimestral..base..2015)


#Analizamos graficamente y el spead

plot(merge(as.zoo(consumo_ts), as.zoo(pib_ts)))
plot(as.zoo(pib_ts-consumo_ts))

#Aplicamos ADF sobre cada una de las series

adf1 = ur.df(pib_ts, lags=6, type = "trend")
summary(adf1)
plot(adf1)

#El ADF sugiere que no hay presencia de una raiz unitaria en la serie
#la deriva es significativa y la tendencia es significativa 

#Ahora el Consumo:

adf2 = ur.df(consumo_ts, lags=6, type = "trend")
summary(adf2)
plot(adf2)
     
#El ADF sugiere que no hay presencia de una raiz unitaria en la serie
#la deriva es significativa y la tendencia es significativa 

#Ahora obtenemos la cointegración

coint.test(pib_ts, consumo_ts,nlag=6)

#Hay evidencia de cointegración en cada uno de los 3 casos, sin tendencia
#con tendencia lineal y con tendencia cuadratica
##____________________________
##____________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRÍA II - 2023-I
#                             Trabajo 1, Marzo 
##____________________________
##____________________________

remove(list = ls())

#Iniciamos importando librerias

library(tidyverse) 
library(readxl) 
library(dplyr)
library(ggplot2)

# Para la realización del trabajo se uilizará el Índice de producción real de la 
# industria manufacturera colombiana

setwd("C:/Users/PC/Documents/Repos/UNAL/Econometría_II")
ipr =  as.data.frame(read_excel("srea_027.xls", range= cell_rows(9:567)))

#Iniciamos el Data-cleaning

colnames(ipr) <- c("Año", "Mes", "Total_industria", "Total_sin_trilla")



for (i in 1:558){                       #se puede cambiar el 558 por la longitud del ds para mayor recursividad
  if (is.na((ipr[i,"Año"])) == TRUE){
    valor = ipr[i,"Año"]
  }else{
    ipr[i,"Año"] = valor
  }
}

## HAY PROBLEMAS EN VALOR, NO LO RECONOCE

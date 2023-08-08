## TRABAJO macro III

# Codigo para limpiar el Environment
remove(list = ls())
##Importamos librerias

library(readxl)
library(tseries)

#Importamos los datasets

paises_de_interes <- c("Colombia", "Francia", "Kenya", "Sudáfrica")
variables_de_interes <- c("pib", "pib_per_capita", "pt", "fbcf", "fbcf_per_capita", "pea")


datasets <- list(
  pib = read.csv("https://raw.githubusercontent.com/SoyJuanDuenas/UNAL/master/Macro_III/Trabajo_Final/PIB.csv", sep = ",", quote = "'", header = F),
  pib_per_capita = read.csv("https://raw.githubusercontent.com/SoyJuanDuenas/UNAL/master/Macro_III/Trabajo_Final/PIB%20per%20c%C3%A1pita%20(UMN%20a%20precios%20constantes).csv", sep = ",", quote = "'", header = F ),
  pt = read.csv("https://raw.githubusercontent.com/SoyJuanDuenas/UNAL/master/Macro_III/Trabajo_Final/poblacion%20total.csv", sep = ",", quote = "'", header = F ),
  fbcf = read.csv("https://raw.githubusercontent.com/SoyJuanDuenas/UNAL/master/Macro_III/Trabajo_Final/FBCF.csv", sep = ",", quote = "'", header = F ),
  fbcf_porcentaje = read.csv("https://raw.githubusercontent.com/SoyJuanDuenas/UNAL/master/Macro_III/Trabajo_Final/PIB%20per%20c%C3%A1pita%20(UMN%20a%20precios%20constantes).csv", sep = ",", quote = "'", header = F ),
  pea = read.csv("https://raw.githubusercontent.com/SoyJuanDuenas/UNAL/master/Macro_III/Trabajo_Final/pea.csv", sep = ",", quote = "'", header = F )

# Hacemos un datacleaning apropiado y creamos las TS

                   for (i in seq_along(variables_de_interes)) {
  datasets[[i]] <- subset(datasets[[i]], `Data.Source` %in% paises_de_interes)
  datasets[[i]] <- datasets[[i]][, -c(1,2,3,4)]
  rownames(datasets[[i]])[1] <- "Colombia"
  rownames(datasets[[i]])[2] <- "Francia"
  rownames(datasets[[i]])[3] <- "Kenya"
  rownames(datasets[[i]])[4] <- "Sudafrica"
  colnames(datasets[[i]]) <- c(1960:2021)
}



#Dado que no tenemos los datos de la PEA desde 1960 la estimamos a partir de la 
#PT



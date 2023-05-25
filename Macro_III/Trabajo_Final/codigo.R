## TRABAJO macro III

##Importamos librerias

library(readxl)
library(tseries)

#Importamos los datasets

paises_de_interes <- c("Colombia", "Francia", "Kenya", "Sudáfrica")
variables_de_interes <- c("pib", "pib_per_capita", "pt", "fbcf", "fbcf_per_capita", "pea")

datasets <- list(
  pib = read_excel("C:/Users/Rama IEEE UN/Downloads/PIB(UMN precios constantes).xls", sheet = 1),
  pib_per_capita = read_excel("C:/Users/Rama IEEE UN/Downloads/PIB per cápita (UMN a precios constantes).xls", sheet = 1),
  pt = read_excel("C:/Users/Rama IEEE UN/Downloads/Poblacion total.xls", sheet = 1),
  fbcf = read_excel("C:/Users/Rama IEEE UN/Downloads/Formación bruta de capital fijo (UMN a precios constantes).xls", sheet = 1),
  fbcf_per_capita = read_excel("C:/Users/Rama IEEE UN/Downloads/Formación bruta de capital fijo (% del PIB).xls", sheet = 1),
  pea = read_excel("C:/Users/Rama IEEE UN/Downloads/pea.xls", sheet = 1)
)

# Limitar los datasets a los países de interés
for (i in seq_along(variables_de_interes)) {
  datasets[[i]] <- subset(datasets[[i]], `Data Source` %in% paises_de_interes)
}


#convertimos a TS

ts(data = pib ,start = 1960, frequency = 1)
ts(data = pib_per_capita ,start = 1960, frequency = 1)
ts(data = pt ,start = 1960, frequency = 1)
ts(data = fbcf ,start = 1960, frequency = 1)
ts(data = fbcf_per_capita ,start = 1960, frequency = 1)
ts(data = pea ,start = 1960, frequency = 1)

for(i %in% c("Colombia", "Francia", "Kenya", "Sudáfrica")){
  
} 

#Dado que no tenemos los datos de la PEA desde 1960 la estimamos a partir de la 
#PT


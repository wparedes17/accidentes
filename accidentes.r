library(readxl)
library(tidyr)
library(dplyr)
library(plotly)

accidentes <- read_excel('accidentes.xlsx')
head(accidentes)

#=PROMEDIO(COLUMNA)
barplot(accidentes$MES)

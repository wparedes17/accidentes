library(readxl)
library(dplyr)
library(plotly)

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

accidentes <- read_excel('accidentes.xlsx')
head(accidentes)

accidentes <- accidentes %>% mutate('ACCI'=AUTOMOVIL+CAMPASAJ+MICROBUS+PASCAMION+OMNIBUS+TRANVIA+CAMIONETA+CAMION+TRACTOR+FERROCARRI+OTROVEHIC > 0)
accidentes_nobici <- accidentes %>% filter(ACCI == TRUE)
accidentes_bici <- accidentes %>% filter(BICICLETA > 0)

indicators <- colnames(accidentes)[52:70]

nobici_indicators <- accidentes_nobici %>% select(52:70)
bici_indicators <- accidentes_bici %>% select(52:70)

x <- apply(nobici_indicators, 2, function(x){
    median(x, na.rm = TRUE)
})

y <- apply(bici_indicators, 2, function(x){
    median(x, na.rm = TRUE)
})

datos_simple <- data.frame('CATS'=names(x), 'NO BICI'=x, 'BICI'=y)
datos_simple

fig <- plot_ly(datos_simple, x = ~CATS, y = ~`NO.BICI`, type = 'bar', name = 'Accidentes generales')
fig <- fig %>% add_trace(y = ~BICI, name = 'Accidentes bicicleta')
fig <- fig %>% layout(yaxis = list(title = 'Indicators'), barmode = 'group')
fig

peatones <- accidentes %>% mutate(AUX_BICI = if_else(BICICLETA > 0, 'BICICLETA', 'OTRO'))

peatones %>% group_by(AUX_BICI) %>% summarise(MUERTO = mean(PEATMUERTO),
    HERIDO = mean(PEATHERIDO), CICLISTA = mean(CICLMUERTO), ALCOHOL = mean(ALIENTO), SEXO_PREF = find_mode(SEXO), OLD = mean(EDAD),
    CAUSA = find_mode(CAUSAACCI))


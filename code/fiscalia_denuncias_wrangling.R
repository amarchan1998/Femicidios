# IMPORTE Y MANIPULACIÓN DE DATOS

# Alejandra Marchán
# El Quantificador
# Script para realizar el importe y limpieza de datos de la Fiscalía

# Librerías -----------------------------------------------------------------------------------------------

# Este código es para instalar las librerías que necesite el usuario para compilar el código

if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(labelled)) install.packages("labelled", repos = "http://cran.us.r-project.org")

# Datos ---------------------------------------------------------------------------------------------------

# Cargar bases de datos

fiscalia <- read_excel('data/fiscalia_denuncias.xlsx', sheet = "Datos_F1",
                       col_names = c('fecha','anio','mes','provincia','delito','tipo', 'victimas', 'denuncia'),
                       skip = 16)
fiscalia <-fiscalia[-35968,]

# Generar una base de femicidios solamente

femicidios_fiscalia <- fiscalia %>% filter(delito == 'FEMICIDIO')

# Generar una serie de tiempo anual

femicidios_fiscalia_yr<-
  femicidios_fiscalia %>% 
  group_by(anio) %>% 
  summarize(count=n())

sum(femicidios_fiscalia_yr$count)

# Generar cambios porcentuales ----------------------------------------------------------------------------

profit_pct_change <- function(x) {
  x <- x[order(x$anio, decreasing = TRUE), ] # Confirms ordered by decreasing year
  pct_change <- -diff(x$count)/x$count[-1] * 100 # Gets percent change in profit from preceding year
  data.frame(year = x$anio[-length(x$anio)], pct_change = pct_change) # Returns data frame
}

fem_pct_chg <-femicidios_fiscalia_yr %>% 
  do(profit_pct_change(.))



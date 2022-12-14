# IMPORTE Y MANIPULACIÓN DE DATOS

# Alejandra Marchán
# El Quantificador
# Script para realizar el importe y limpieza de datos.

# Librerías -----------------------------------------------------------------------------------------------

# Este código es para instalar las librerías que necesite el usuario para compilar el código

if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(labelled)) install.packages("labelled", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("labelled", repos = "http://cran.us.r-project.org")

source('code/reg_civil_wrangling.R') # Carga los datos del Registro Civil
# Datos ---------------------------------------------------------------------------------------------------

# Para poder cargar esta base de datos, fue necesario eliminar la imagen (logo del MinGob) para que R
# no me arroje un error

mingob <- read_excel('data/homicidios_intencionales_mingob.xls',
                      col_names = c('tipo', 'provincia', 'canton',
                                     'mes','anio','tipo_arma','edad_edad',
                                     'sexo','cantidad_hom'),
                                skip = 2)

# Manipulación --------------------------------------------------------------------------------------------

# Eliminar primera fila (que está vacía)

mingob <- mingob[-1,]

# Generar una base de femicidios solamente

femicidios_mingob <- 
  mingob %>% filter(tipo == 'FEMICIDIO')

# Cambiar el tipo de variables segun se requiera

femicidios_mingob$cantidad_hom <- as.numeric(femicidios_mingob$cantidad_hom)

# Generar una serie de tiempo anual

femicidios_mingob_yr<-
femicidios_mingob %>% 
  group_by(anio) %>% 
  summarize(femicidios=sum(cantidad_hom))

# Generar una serie de tiempo mensual

femicidios_mingob_month <-
  femicidios_mingob %>% 
  group_by(anio, mes) %>% 
  summarize(femicidios=sum(cantidad_hom))

# Unión de Bases de Datos ---------------------------------------------------------------------------------

# En esta sección se unen algunas bases de datos para construir gráficos conjuntos

## Base de Femicidios

# Se construye una base de Femicidios: 

# 1. MINGOB + FGE (obtenido desde pag web de la FGE)

# Cargamos la base de la FGE

fge<-
  read.csv('data/muertes_fem_fiscalia.csv') %>% 
  filter(tipo == 'Total') %>% 
  select(-tipo) %>% 
  mutate(fuente = 'FGE')

# Sacamos el porcentaje de cambio para los datos de la FGE

fge_change<- fge %>% as.data.frame()


fge_change<- change(fge, Var ='cantidad',
                     NewVar = 'pct_change',
                     slideBy = -1,
                     type='percent')

fge_change

# 2. ALDEA

# Cargamos la base de ALDEA

aldea <- 
  read_xlsx('data/femicidios_aldea.xlsx',
             col_names = c('año','cantidad'),
             skip = 1) %>% 
  mutate(fuente = 'ALDEA')

aldea

# Sacamos el porcentaje de cambio para los datos de Aldea

aldea_change<- aldea %>% as.data.frame()


aldea_change<- change(aldea_change, Var ='cantidad',
                    NewVar = 'pct_change',
                    slideBy = 1,
                    type='percent')

aldea_change

# 3. REGISTRO CIVIL

# Cargamos la base del Registro Civil

source('code/reg_civil_wrangling.R')

reg_civil <- 
  deaths_total_yearly_wom %>% 
  mutate(fuente = 'Registro Civil') %>% 
  rename('año' = year,
         'cantidad' = cant)

# Se construye la base conjunta

femicidios_conjunta<-
  fge %>%
  bind_rows(aldea) %>% 
  bind_rows(reg_civil)

# Filtro años que necesito

femicidios_conjunta <- 
  femicidios_conjunta %>% 
  filter(año %in% c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))

# Filtro 2014-2020 donde tengo datos para las tres fuentes

femicidios_conjunta2 <- 
  femicidios_conjunta %>% 
  filter(año %in% c(2014,2015,2016,2017,2018,2019,2020,2021))

# Solo Aldea y FGE

femicidios_conjunta3<-
  fge %>%
  bind_rows(aldea) %>% 
  filter(año %in% c(2014,2015,2016,2017,2018,2019,2020,2021,2022))


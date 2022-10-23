# ANÁLISIS DE DATOS
# Alejandra Marchán
# Femicidios Artículo El Quantificador

# Preliminares ------------------------------------------------------------

# Librerías

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# Datos

# Carga de Datos del Registro Civil

# Ejecutamos el script de de femicidios_wrangling.R

source('code/reg_civil_wrangling.R') # Carga los datos del Registro Civil
source('code/fiscalia_denuncias.R')
# Carga de datos del Fiscalía (Femicidios Oficiales + Otras Muertes)

muertes_fem<-read.csv('data/muertes_fem_fiscalia.csv')
fem_aldea <- read_xlsx('data/femicidios_aldea.xlsx')

# Formatos ------------------------------------------------------------------------------------------------

# Definimos algunos formatos para utilizar después

# Definimos colores en HEX para los gráficos

# Quantificador

quant_blue<-'#09A4CC'
quant_grey<-'#5C7C94'
quant_orange<-'#F8754D'
quant_red<-'#F44D54'

# Otros

purple_women <- "#88398a" # Color morado para estadísticas de mujeres

# Tema para los gráficos de mujeres

theme_women <-
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic'),
        legend.background = element_blank(),
        text =  element_text(color = 'black'))

# Análisis ------------------------------------------------------------------------------------------------

### Femicidios
### Análisis Anual

femicidios_col <- ggplot(muertes_fem, aes(x = as.character(año), y = cantidad, fill = tipo))+
  geom_col(width = 0.7,
           position = 'stack',
           color = 'black')+
  labs(x = 'Año',
       y = 'Número de muertes',
       title = 'Muertes de Mujeres en Ecuador 2014-2022',
       subtitle = 'Datos de la Fiscalía General del Estado: Muertes de Mujeres en Contexto Delictivo',
       fill = 'Tipo de muerte')+
  scale_fill_manual(values =  c('#52307c',purple_women))+
  theme_women

femicidios_col

 ### Muertes Violentas Hombres vs Mujeres: Datos Registro Civil

muertes_violentas_col <- ggplot(deaths_total_yearly_def, aes(x = as.character(year), y = cant, fill = mujer))+
  geom_col(width = 0.7,
           position = 'dodge',
           color = 'black')+
  labs(x = 'Año',
       y = 'Número de muertes',
       title = 'Muertes Violentas en Ecuador 2011-2020',
       subtitle = 'Datos del Registro Civil: Mujeres vs. Hombres',
       fill = 'Sexo')+
  scale_fill_manual(values =  c('#334d9e','#52307c'))+
  theme_women

muertes_violentas_col

 ### Femicidios: Datos Aldea

fem_aldea_col <- ggplot(fem_aldea, aes(x = as.character(año), y = num_fem))+
  geom_col(width = 0.7,
           color = 'black', fill = '#52307c')+
  labs(x = 'Año',
       y = 'Número de femicidios',
       title = 'Femicidios en Ecuador 2014-2022',
       subtitle = 'Datos Aldea')+
  theme_women

fem_aldea_col

 ### Femicidios: Datos Fiscalía General del Estado

fem_fiscalia_col <- ggplot(femicidios_fiscalia_yr, aes(x = as.character(anio), y = count))+
  geom_col(width = 0.7,
           color = 'black', fill = '#52307c')+
  labs(x = 'Año',
       y = 'Número de femicidios',
       title = 'Femicidios en Ecuador 2014-2022',
       subtitle = 'Datos de la Fiscalía General del Ecuador: Número de Denuncias')+
  theme_women

fem_fiscalia_col

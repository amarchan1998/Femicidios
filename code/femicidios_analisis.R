# ANÁLISIS DE DATOS
# Alejandra Marchán
# Femicidios Artículo El Quantificador

# Preliminares ------------------------------------------------------------

# Librerías

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")

# Datos

# Carga de Datos del Registro Civil

# Ejecutamos el script de de femicidios_wrangling.R

source('code/reg_civil_wrangling.R') # Carga los datos del Registro Civil

# Carga de Datos del Ministerio de Gobierno

source('code/min_gob_wrangling.R')

# Carga de datos del Fiscalía (Femicidios Oficiales + Otras Muertes)

muertes_fem<-
  read.csv('data/muertes_fem_fiscalia.csv') %>% 
  filter(tipo!= 'Total')

# Carga de datos de Fundación ALDEA 

fem_aldea <- 
  read_xlsx('data/femicidios_aldea.xlsx')

# Carga de datos de la Fiscalía (Denuncias)

source('code/fiscalia_denuncias_wrangling.R')

# Nota: Tanto datos de ALDEA como datos de Fiscalía tuvieron que ser manualmente copiados de la página web
# para poder  ser utilizados en el análisis. 
# Los datos de la fiscalía sobre denuncias se solicitaron a la dirección de correo electrónico de estadística@fge

# Formatos ------------------------------------------------------------------------------------------------

# Definimos algunos formatos para utilizarlos después en el análisis

# Definimos colores en HEX para los gráficos

# Quantificador

quant_blue<-'#09A4CC'
quant_grey<-'#5C7C94'
quant_orange<-'#F8754D'
quant_red<-'#F44D54'

# Otros

purple_women <- "#88398a" # Color morado para estadísticas de mujeres
purple_women2 <- '#52307c'  # Color morado más oscuro para estadísticas de mujeres
purple_women3 <- '#ca93d9'

# Tema para los gráficos de mujeres

theme_women <-
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic'),
        legend.background = element_blank(),
        text =  element_text(color = 'black'))

# Análisis Univariado ------------------------------------------------------------------------------------------------

## Femicidios
### Análisis Anual

femicidios_col <- ggplot(muertes_fem, aes(x = as.character(año), y = cantidad, fill = tipo))+
  geom_col(width = 0.7,
           position = 'stack',
           color = 'black')+
  labs(x = 'Año',
       y = 'Número de muertes',
       title = 'Muertes de Mujeres en Ecuador 2014-2022',
       subtitle = 'Fuente: Fiscalía General del Estado - Muertes de Mujeres en Contexto Delictivo',
       fill = 'Tipo de muerte')+
  scale_fill_manual(values =  c(purple_women, purple_women2))+
  theme_women

femicidios_col

## Femi(ni)cidios: Datos Aldea

fem_aldea_col <- ggplot(fem_aldea, aes(x = as.character(año), y = num_fem))+
  geom_col(width = 0.7,
           color = 'black', 
           fill = purple_women)+
  labs(x = 'Año',
       y = 'Número de femicidios',
       title = 'Femi(ni)cidios en Ecuador 2014-2022',
       subtitle = 'Fuente: Asociación Lationamericana para el Desarrollo Alternativo (ALDEA)')+
  theme_women

fem_aldea_col

## Femicidios conjunto

fem_conjunto <- 
  ggplot(femicidios_conjunta, aes(x = as.character(año), y = cantidad, fill = fuente))+
  geom_col(width = 0.7,
           color = 'black',
           position = 'dodge')+
  labs(x = 'Año',
       y = 'Número de femicidios',
       title = 'Femicidios en Ecuador 2011-2022',
       subtitle = 'Comparación entre fuentes Fundación ALDEA, Fiscalía General y Registro Civil',
       fill = 'Fuente')+
  scale_fill_manual(values =  c(purple_women, purple_women2, purple_women3))+
  theme_women

fem_conjunto

## Denuncias Femicidios: Datos Fiscalía General del Estado

fem_fiscalia_col <- ggplot(femicidios_fiscalia_yr, aes(x = as.character(anio), y = count))+
  geom_col(width = 0.7,
           color = 'black', 
           fill = purple_women)+
  labs(x = 'Año',
       y = 'Número de denuncias de femicidios',
       title = 'Denuncias de Femicidios en Ecuador 2014-2022',
       subtitle = 'Fuente: Fiscalía General del Ecuador')+
  theme_women

fem_fiscalia_col

### Cambio Porcentual en Femicidios: Datos Fiscalía General del Estado

fem_pchg <- 
  ggplot(fem_pct_chg, aes(x = as.character(year), y = pct_change, group = 1))+
  geom_line(size = 1,
            color = purple_women)+
  geom_point(color = purple_women2,
             size = 3)+
  labs(x = 'Año',
       y = 'Variación Anual (%)',
       title = 'Variación Anual (%) de Denuncias de Femicidios en Ecuador 2014-2022',
       subtitle = 'Fuente: Fiscalía General del Ecuador')+
  theme_women

fem_pchg

## Muertes Violentas Hombres vs Mujeres
#### Datos Registro Civil

muertes_violentas_col <- 
  ggplot(deaths_total_yearly_def, aes(x = as.character(year), y = cant, fill = mujer))+
  geom_col(width = 0.7,
           position = 'dodge',
           color = 'black')+
  labs(x = 'Año',
       y = 'Número de muertes',
       title = 'Muertes Violentas en Ecuador 2011-2020',
       subtitle = 'Fuente: Registro Civil',
       fill = 'Sexo')+
  scale_fill_manual(values =  c('#334d9e',purple_women))+
  theme_women

muertes_violentas_col

## Muertes Violentas Solo Mujeres
### Datos Registro Civil

muertes_violentas_col_wom <- 
  ggplot(deaths_total_yearly_wom, aes(x = as.character(year), y = cant))+
  geom_col(width = 0.7,
           position = 'dodge',
           color = 'black',
           fill = purple_women)+
  labs(x = 'Año',
       y = 'Número de muertes',
       title = 'Muertes Violentas en Ecuador 2011-2020',
       subtitle = 'Fuente: Registro Civil',
       fill = 'Sexo')+
  theme_women

muertes_violentas_col_wom

 ### Muertes hombre vs Mujeres: Cambio Porcentual Registro Civil

muertes_pc_col_muj <- 
  ggplot(muertes_pc_total, aes(x = as.character(year), y = pct_change, color = sexo, group = sexo))+
  geom_line(size = 1)+
  geom_point(size = 3)+
  labs(x = 'Año',
       y = 'Variación anual (%)',
       title = 'Variación Anual (%) en Muertes Violentas en Ecuador 2012-2020',
       subtitle = 'Fuente: Registro Civil')+
  labs(color = "Sexo")+scale_color_manual(values =  c('#334d9e', purple_women))
  theme_women

  muertes_pc_col_muj

 


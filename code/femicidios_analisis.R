# ANÁLISIS DE DATOS
# Alejandra Marchán
# Femicidios Artículo El Quantificador

# Preliminares ------------------------------------------------------------

# Librerías

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(survey)) install.packages("survey", repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")

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

# Se construye una base de solamente los totales

femicidios_totales <-
  read.csv('data/muertes_fem_fiscalia.csv') %>% 
  filter(tipo == 'Total')

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
        text =  element_text(color = 'black', size = 15),
        axis.ticks.x = element_blank())

# Análisis ------------------------------------------------------------------------------------------------

## Femicidios

# Caption largo para el gráfico 

caption_grafo1<-
  'El gráfico representa las muertes de mujeres en contexto delictivo desde agosto de 2014, cuando se tipificó
el delito de femicido en el COIP ecuatoriano, hasta finales de octubre del 2022, cuando los datos fueron extraídos
del panel de visualización del Consejo Nacional para la Igualdad de Género, localizado en la página web de la Fiscalía 
General del Estado. Las etiquetas muestran los totales de femicidios y otras muertes, que incluyen asesinatos,
homicidios intencionales, sicariatos, robos, ejecuciones extrajudiciales, entre otros.'

### Análisis Anual

femicidios_col <- 
  ggplot(muertes_fem, aes(x = as.character(año), y = cantidad, fill = tipo))+
  geom_col(width = 0.7,
           position = 'stack',
           color = 'black')+
  labs(x = '',
       y = 'Número de muertes',
       title = 'Muertes de Mujeres en Contexto Delictivo 2014-2022',
       subtitle = 'Fuente: Consejo Nacional para la Igualdad de Género',
       fill = 'Tipo de muerte',
       caption = str_wrap(caption_grafo1, 160))+
  scale_fill_manual(values =  c(purple_women, purple_women2),
                    limits = c('Femicidio', 'Otras'))+ # Utilizando el argumento "limits" no tengo que incluir el total en la leyenda
  geom_text(data = femicidios_totales,
            aes(label = cantidad),
            color = 'white',
            vjust = 1.5)+ # Incluyo texto para incluir la suma de ambos, que está en la base de datos.
  theme_women+
  theme(legend.position = c(0.08,0.9),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) # Incluir ediciones al tema después del tema predeterminado para que funcione

femicidios_col

# Guardar la imagen

png("images/graf1-femicidios-vs-otros.png", width = 900, height = 650, unit = 'px')

femicidios_col

dev.off()

## Femi(ni)cidios: Datos Aldea

fem_aldea_col <- ggplot(fem_aldea, aes(x = as.character(año), y = num_fem))+
  geom_col(width = 0.7,
           color = 'black', 
           fill = purple_women)+
  labs(x = '',
       y = 'Número de femicidios',
       title = 'Femi(ni)cidios en Ecuador 2014-2022',
       subtitle = 'Fuente: Asociación Lationamericana para el Desarrollo Alternativo (ALDEA)')+
  theme_women

fem_aldea_col

# Guardar la imagen

png("images/graf4-femicidios-aldea.png", width = 900, height = 650, unit = 'px')

fem_aldea_col

dev.off()

# Sacamos el porcentaje de cambio para estos datos

aldea_change<- fem_aldea %>% as.data.frame()

aldea_change<- change(aldea_change, Var ='num_fem',
                      NewVar = 'pct_change',
                      slideBy = 1,
                      type='percent')
aldea_change


## Femicidios conjunto

fem_conjunto <- 
  ggplot(femicidios_conjunta, aes(x = as.character(año), y = cantidad, fill = fuente))+
  geom_col(width = 0.7,
           color = 'black',
           position = 'dodge')+
  labs(x = 'Año',
       y = 'Número de femicidios',
       title = 'Femicidios en Ecuador 2014-2022',
       subtitle = 'Comparación entre fuentes Fundación ALDEA, Fiscalía General y Registro Civil',
       fill = 'Fuente')+
  scale_fill_manual(values =  c(purple_women, purple_women2, purple_women3))+
  theme_women+
  theme(legend.position = c(0.08,0.85),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())

fem_conjunto

## Guardo imagen

png("images/graf5-femicidios-compar.png", width = 900, height = 650, unit = 'px')

fem_conjunto

dev.off()

## Femicidios conjunto (solo fuentes con mismos años)

fem_conjunto2 <- 
  ggplot(femicidios_conjunta2, aes(x = as.character(año), y = cantidad, fill = fuente))+
  geom_col(width = 0.7,
           color = 'black',
           position = 'dodge')+
  labs(x = 'Año',
       y = 'Número de femicidios',
       title = 'Femicidios en Ecuador 2014-2020',
       subtitle = 'Comparación entre fuentes Fundación ALDEA, Fiscalía General y Registro Civil',
       fill = 'Fuente')+
  scale_fill_manual(values =  c(purple_women, purple_women2, purple_women3))+
  theme_women

fem_conjunto2

## Femicidios conjunto (solo fuentes con mismos años Aldea y FGE)

fem_conjunto3 <- 
  ggplot(femicidios_conjunta3, aes(x = as.character(año), y = cantidad, fill = fuente))+
  geom_col(width = 0.7,
           color = 'black',
           position = 'dodge')+
  labs(x = 'Año',
       y = 'Número de femicidios',
       title = 'Femicidios en Ecuador 2014-2022',
       subtitle = 'Comparación entre fuentes Fundación ALDEA y Fiscalía General',
       fill = 'Fuente')+
  scale_fill_manual(values =  c(purple_women, purple_women2))+
  theme_women

fem_conjunto3

## Denuncias Femicidios: Datos Fiscalía General del Estado

fem_fiscalia_col <- ggplot(femicidios_fiscalia_yr, aes(x = as.character(anio), y = count))+
  geom_col(width = 0.7,
           color = 'black', 
           fill = purple_women)+
  labs(x = '',
       y = 'Número de denuncias de femicidios')+
  theme_women

fem_fiscalia_col

# Sacamos el porcentaje de cambio para los datos de la FGE

fge_change<- femicidios_fiscalia_yr %>% as.data.frame()

fge_change<- change(fge_change, Var ='count',
                    NewVar = 'pct_change',
                    slideBy = -1,
                    type='percent')

fge_change

# Guardar la imagen

png("images/graf2-femicidios-fiscalia.png", width = 900, height = 650, unit = 'px')

fem_fiscalia_col

dev.off()

### Cambio Porcentual en Femicidios: Datos Fiscalía General del Estado

fem_pchg <- 
  ggplot(fem_pct_chg, aes(x = as.character(year), y = pct_change, group = 1))+
  geom_line(size = 1,
            color = purple_women)+
  geom_point(color = purple_women2,
             size = 3)+
  labs(x='',
       y = 'Variación Anual (%)')+
  theme_women

fem_pchg

### Dos gráficos de FGE

# Caption largo para el gráfico 

caption_fge_denuncias<- 'Se muestran las denuncias del delito de femicidio (art. 141 del COIP) anualmente en el panel derecho y 
la tasa de crecimiento anual de las mismas en el panel derecho. Los datos provienen de un pedido de información 
realizado a la FGE (estadistica@fiscalia.gob.ec) por los autores, presentando las denuncias desde agosto de 2014
hasta octubre de 2022. Se toman en cuenta el número de registros (filas) por año existentes en la base de datos, puesto que
un mayor número de víctimas o de noticias de delito (NDD) no necesariamente comprende un mayor número de delitos o denuncias en sí.'

grafge2<-
  fem_fiscalia_col + fem_pchg +
  plot_layout(ncol = 2) +
  plot_annotation(title = 'Denuncias de Femicidios en Ecuador 2014-2022',
                  subtitle = 'Fuente: Fiscalía General del Estado (FGE)',
                  caption = str_wrap(caption_fge_denuncias, 240),
                  theme = theme(plot.caption = element_text(hjust = 0, face = 'italic'),
                                plot.title = element_text(hjust = 0.5, size = 20))) +
                  theme_women
                 
grafge2<- 
  grafge2 + 
  labs( x = 'Año') + 
  theme(axis.title.x = element_text(hjust=-0.12))

grafge2

ggsave("images/graf6-fge-comparacion.png", device = "png", width = 12.5, height = 7, dpi = 900)

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
  geom_text(aes(label = cant),
            vjust = 1.5)+
  theme_women+
  theme(legend.position = c(0.85,0.86),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

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

# Guardar la imagen

png("images/graf3-femicidios-registro-civil.png", width = 900, height = 650, unit = 'px')

muertes_violentas_col_wom

dev.off()

### Muertes hombre vs Mujeres: Cambio Porcentual Registro Civil

muertes_pc_col_muj <- 
  ggplot(muertes_pc_total, aes(x = as.character(year), y = pct_change, color = sexo, group = sexo))+
  geom_line(size = 1)+
  geom_point(size = 3)+
  labs(x = 'Año',
       y = 'Variación anual (%)',
       title = 'Variación Anual (%) en Muertes Violentas en Ecuador 2012-2020',
       subtitle = 'Fuente: Registro Civil')+
  labs(color = "Sexo")+scale_color_manual(values =  c('#334d9e', purple_women))+
  theme_women

  muertes_pc_col_muj

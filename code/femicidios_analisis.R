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

# Carga de datos del Ministerio de Gobierno (Femicidios Oficiales)

femicidios <- read.csv("C:/Users/maria/OneDrive - Universidad San Francisco de Quito/Documentos/PERSONAL/ARTÍCULOS E INVESTIGACIÓN/LIDES ARTÍCULO/articulo-lide-femicidios/femicidios.csv")

femicidios_total <- read.csv("C:/Users/maria/OneDrive - Universidad San Francisco de Quito/Documentos/PERSONAL/ARTÍCULOS E INVESTIGACIÓN/LIDES ARTÍCULO/articulo-lide-femicidios/femicidios_total.csv")

# Formatos ------------------------------------------------------------------------------------------------

# Definimos algunos formatos para utilizar después

# Definimos colores en HEX para los gráficos

# Quantificador

quant_blue<-'#09A4CC'
quant_grey<-'#5C7C94'
quant_orange<-'#F8754D'
quant_red<-'#F44D54'

# Otros

purple_women <- "#52307c" # Color morado para estadísticas de mujeres

# Tema para los gráficos de mujeres

theme_women <-
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic'),
        legend.background = element_blank())

# Análisis ------------------------------------------------------------------------------------------------

### Femicidios
### Análisis Anual


femicidios_scatter <- ggplot(femicidios, aes(x = año, y = cantidad))+
  geom_line()+
  geom_line(aes(x = año, y = cantidad ), color = "#52307c", linetype = 'dotted') +
  geom_point()+
  labs(x = 'Año',
       y = 'Número de femicidios',
       title = 'Femicidios en Ecuador 2014 - 2022',
       color = '')+
  theme(legend.position = c(0.35,0.12),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 10)) +
  theme_women

femicidios_scatter

femicidios_col <- ggplot(femicidios, aes(x = as.character(año), y = cantidad))+
  geom_col(fill = "#52307c",
           color = "#52307c", 
           width = 0.7)+
  geom_text(aes(label = cantidad),
            size = 4,
            vjust = 4)+
  labs(x = 'Año',
       y = 'Número de Femicidios',
       title = 'Femicidios en Ecuador 2014-2022')+
  theme(axis.ticks = element_blank()) +
  theme_women

femicidios_col

### Análisis Mensual Total


fem_scatter_mens <-ggplot(femicidios_total, aes(x = anio, y = cantidad))+
  geom_line()+
  geom_line(aes(x = anio, y = cantidad ), color = "#52307c", linetype = 'dotted') +
  geom_point()+
  labs(x = 'Año',
       y = 'Número de femicidios',
       title = 'Femicidios en Ecuador 2014 - 2022 (Agregado Mensual)',
       color = '')+
  theme(legend.position = c(0.35,0.12),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 10)) + 
  theme_women
fem_scatter_mens

fem_total <-ggplot(femicidios_total, aes(x = anio, y = cantidad, group= 1 ))+
  geom_line(colour=purple_women) +
  theme_women

fem_total

### Mapa Esperanza de Vida

# Cargar base paises

esperanza_pais <- read.csv("esperanza_pais.csv")

map<-map_data('world')

write.xlsx(map, file = "C:/Users/maria/OneDrive - Universidad San Francisco de Quito/Documentos/PERSONAL/ARTÍCULOS E INVESTIGACIÓN/LIDES ARTÍCULO/articulo-lide-femicidios/maps.xlsx")

map<-left_join(map, esperanza_pais, by = 'region')



map$anio_2020_q <- cut(map$anio_2020, c("75", "80%", "84"),
                       labels = c("75", "80%", "84"),
                        include.lowest = TRUE)

mapa_esperanza <- ggplot(map,aes(x = long, y = lat,fill = anio_2020, group = group))+
  geom_polygon(color = 'black')+
  coord_fixed(xlim= c(-100,-35),
              ylim= c(-53,22),
              ratio = 1.2)+
  labs(fill = 'Esperanza de Vida (Número de Años)') 
mapa_esperanza
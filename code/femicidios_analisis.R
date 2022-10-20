# Graficos Alejandra Marchan

# Preliminares ------------------------------------------------------------

# Librerias

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(mapdata)
library(MAP)
library(maps)
library(dplyr)
library(ggplot2)
library(writexl)
library(openxlsx)
library(tmap)


# cargar datos

esperanza_vida <- read.csv("C:/Users/maria/OneDrive - Universidad San Francisco de Quito/Documentos/PERSONAL/ARTÍCULOS E INVESTIGACIÓN/LIDES ARTÍCULO/articulo-lide-femicidios/esperanza_vida.csv")

femicidios <- read.csv("C:/Users/maria/OneDrive - Universidad San Francisco de Quito/Documentos/PERSONAL/ARTÍCULOS E INVESTIGACIÓN/LIDES ARTÍCULO/articulo-lide-femicidios/femicidios.csv")

femicidios_total <- read.csv("C:/Users/maria/OneDrive - Universidad San Francisco de Quito/Documentos/PERSONAL/ARTÍCULOS E INVESTIGACIÓN/LIDES ARTÍCULO/articulo-lide-femicidios/femicidios_total.csv")

# Estilos 

purple_women <- "#52307c" # color morado para estadísticas de mujeres

theme_women <- # tema para ggplot de todos los graficos
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic'),
        legend.background = element_blank())

### Análisis de Esperanza de Vida

anios <- seq(1960,2020,5)
esp <- seq(0,85,5)

esperanza_anual <- ggplot(esperanza_vida, aes(x = as.character(anio), y = esperanza_vida, color = tipo, group =tipo))+
  geom_line(size = 1)+
  geom_point()+
  labs(x = 'Año',
       y = 'Esperanza de Vida (# de Años)',
       title = 'Esperanza de Vida en Ecuador (1960 - 2020)')+
  theme(axis.ticks = element_blank()) + 
  scale_color_manual(values=c("#94C9A9", "#777DA7", "#D5573B"))+ labs(color = "")+ 
  scale_x_discrete(breaks = anios)+
  scale_y_continuous(breaks = esp)+
  geom_hline(yintercept = 54.295, color= purple_women, linetype = 2)+
  geom_hline(yintercept = 80.02, color= purple_women, linetype = 2)+
  annotate('label', x = 51.2, y = 55.8, label = 'Esp. de Vida Mujeres 1960: 55 años', color = purple_women)+
  annotate('label', x = 10.9, y = 78.5, label = 'Esp. de Vida Mujeres 2020: 80 años', color = purple_women)+
  theme_women+
  theme(legend.position = c(.9, .5))

esperanza_anual 


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
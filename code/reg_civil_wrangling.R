# IMPORTE Y MANIPULACIÓN DE DATOS

# Alejandra Marchán
# El Quantificador
# Script para realizar el importe y limpieza de datos del Registro Civil

# Librerías -----------------------------------------------------------------------------------------------

# Este código es para instalar las librerías que necesite el usuario para compilar el código

if(!require(haven)) install.packages("survey", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(labelled)) install.packages("patchwork", repos = "http://cran.us.r-project.org")

# Datos ---------------------------------------------------------------------------------------------------

# Cargar bases de datos (descargadas del INEC) para la información del Registro Civil

deaths_2020 <- read_spss("data/EDG_2020.sav")
deaths_2019 <- read_spss("data/BDD_EDG_2019.sav")
deaths_2018 <- read_spss("data/EDG_2018.sav")
deaths_2017 <- read_spss("data/BBD_EDG_2017_spss.sav")
deaths_2016 <- read_spss("data/EDG_2016.sav")
deaths_2015 <- read_spss("data/EDG_2015.sav")
deaths_2014 <- read_spss("data/EDG_2014.sav")
deaths_2013 <- read_spss("data/EDG_2013.sav")
deaths_2012 <- read_spss("data/EDG_2012.sav")
deaths_2011 <- read_spss("data/EDG_2011.sav")

# Manipulación --------------------------------------------------------------------------------------------

# Manipulación de la base de datos del Registro Civil para la unión final.

deaths_2020 <- as_factor(deaths_2020)
deaths_2019 <- as_factor(deaths_2019)
deaths_2018 <- as_factor(deaths_2018)
deaths_2017 <- as_factor(deaths_2017)
deaths_2016 <- as_factor(deaths_2016)
deaths_2015 <- as_factor(deaths_2015)
deaths_2014 <- as_factor(deaths_2014)
deaths_2013 <- as_factor(deaths_2013)
deaths_2012 <- as_factor(deaths_2012)
deaths_2011 <- as_factor(deaths_2011)

# 2020
hom <- function(x) ifelse(x %in% "Homicidios", 1, 0)
deaths_2020$hom <- hom(deaths_2020$mor_viol)
deaths_2020 <- 
  deaths_2020 %>% 
  group_by(sexo) %>% 
  summarize(cant = sum(hom)) %>% 
  mutate(year = 2020) 

# 2019
deaths_2019$hom <- hom(deaths_2019$mor_viol)
deaths_2019 <- 
  deaths_2019 %>% 
  group_by(sexo) %>% 
  summarize(cant = sum(hom)) %>% 
  mutate(year = 2019) 

# 2018
deaths_2018$hom <- hom(deaths_2018$mor_viol)
deaths_2018 <- 
  deaths_2018 %>% 
  group_by(sexo) %>% 
  summarize(cant = sum(hom)) %>% 
  mutate(year = 2018) 

# 2017
deaths_2017$hom <- hom(deaths_2017$mor_viol)
deaths_2017<- 
  deaths_2017 %>%
  group_by(sexo) %>% 
  summarize(cant = sum(hom)) %>% 
  mutate(year = 2017)

# 2016
deaths_2016$hom <- ifelse(deaths_2016$mor_viol %in% "Agreciones (Homicidios)", 1, 0)
deaths_2016$anio_insc<-as.double(deaths_2016$anio_insc)
deaths_2016$dia_insc<-as.double(deaths_2016$dia_insc)

deaths_2016 <-
deaths_2016 %>% 
  group_by(sexo) %>% 
  summarize(cant = sum(hom)) %>% 
  mutate(year = 2016)

# 2015
deaths_2015$hom <- ifelse(deaths_2015$mor_viol %in% "Agreciones (Homicidios)", 1, 0)
deaths_2015$anio_insc<-as.double(deaths_2015$anio_insc)
deaths_2015$fecha_nac<-as.Date(deaths_2015$fecha_nac, format = '%Y-%m-%d')
deaths_2015$fecha_fall<-as.Date(deaths_2015$fecha_fall, format = '%Y-%m-%d')
deaths_2015$dia_insc<-as.double(deaths_2015$dia_insc)

deaths_2015 <-
  deaths_2015 %>% 
  group_by(sexo) %>% 
  summarize(cant = sum(hom)) %>% 
  mutate(year = 2015)

# 2014
deaths_2014$hom <- ifelse(deaths_2014$mor_viol %in% "Homicidio", 1, 0)
deaths_2014$anio_insc<-as.double(deaths_2014$anio_insc)
deaths_2014$dia_insc<-as.double(deaths_2014$dia_insc)
deaths_2014$fecha_nac<-as.Date(deaths_2014$fecha_nac, format = '%Y-%m-%d')
deaths_2014$fecha_fall<-as.Date(deaths_2014$fecha_fall, format = '%Y-%m-%d')

deaths_2014 <-
  deaths_2014 %>% 
  group_by(sexo) %>% 
  summarize(cant = sum(hom)) %>% 
  mutate(year = 2014)

# 2013
deaths_2013$hom <- ifelse(deaths_2013$mor_viol %in% " Homicidio", 1, 0)
deaths_2013$anio_insc<-as.double(deaths_2013$anio_insc)
deaths_2013$dia_insc<-as.double(deaths_2013$dia_insc)
deaths_2013$fecha_nac<-as.Date(deaths_2013$fecha_nac, format = '%Y-%m-%d')
deaths_2013$fecha_fall<-as.Date(deaths_2013$fecha_fall, format = '%Y-%m-%d')

deaths_2013 <-
  deaths_2013 %>% 
  group_by(sexo) %>% 
  summarize(cant = sum(hom)) %>% 
  mutate(year = 2013)

# 2012
deaths_2012$hom <- hom(deaths_2012$mu_violen)
deaths_2012$anio_insc<-as.double(deaths_2012$anio_insc)

deaths_2012 <-
  deaths_2012 %>% 
  group_by(sexo) %>% 
  summarize(cant = sum(hom)) %>% 
  mutate(year = 2012)

# 2011
deaths_2011$hom <- ifelse(deaths_2011$mu_violen %in% "Homicidio", 1, 0)
deaths_2011$anio_insc<-as.double(deaths_2011$anio_insc)
deaths_2011$anio_nac<-as.double(deaths_2011$anio_nac)
deaths_2011$anio_fall<-as.double(deaths_2011$anio_fall)
deaths_2011$ofi_insc<-as.double(deaths_2011$ofi_insc)
deaths_2011$acta_insc<-as.double(deaths_2011$acta_insc)

deaths_2011 <-
  deaths_2011 %>% 
  group_by(sexo) %>% 
  summarize(cant = sum(hom)) %>% 
  mutate(year = 2011)

# Total de todos los años

deaths_total<-
  deaths_2020 %>% 
  bind_rows(deaths_2019) %>% 
  bind_rows(deaths_2018) %>%
  bind_rows(deaths_2017) %>%
  bind_rows(deaths_2016) %>% 
  bind_rows(deaths_2015) %>% 
  bind_rows(deaths_2014) %>% 
  bind_rows(deaths_2013) %>% 
  bind_rows(deaths_2012) %>% 
  bind_rows(deaths_2011)

# Agrupar por años y sexo

deaths_total_yearly <-
  deaths_total %>%  
  group_by(year, sexo) %>%
  summarize(cant = sum(cant))

# Filtrando solo para mujeres

deaths_total_yearly_women <-
  deaths_total_yearly %>% 
  filter(sexo == 'Mujeres')


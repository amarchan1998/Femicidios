# IMPORTE Y MANIPULACIÓN DE DATOS

# Alejandra Marchán
# El Quantificador
# Script para realizar el importe y limpieza de datos.

# Librerías -----------------------------------------------------------------------------------------------

# Este código es para instalar las librerías que necesite el usuario para compilar el código

if(!require(haven)) install.packages("survey", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(labelled)) install.packages("patchwork", repos = "http://cran.us.r-project.org")

# Datos ---------------------------------------------------------------------------------------------------

# Para poder cargar esta base de datos, fue necesario eliminar la imagen (logo del MinGob) para que R
# no me arroje un error

femicidios_mingob <- read_excel('data/homicidios_intencionales_mingob.xls',
                                skip = 2)

# Manipulación --------------------------------------------------------------------------------------------

# Eliminar primera fila (que está vacía)

femicidios_mingob <- femicidios_mingob[-1]





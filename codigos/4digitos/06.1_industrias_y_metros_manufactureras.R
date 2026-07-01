#Código para encontrar las Industrias Manufactureras Intensivas en el Conocimiento

#Limpiar R
rm(list=ls (all=T))

### Paquetes  ----
library(pacman)
p_load(haven,      # Paquete para importar archivos de Stata, SAS y SPSS
       readxl,     # Paquete para importar archivos de Excel
       tidyverse)  # Metapaquete que incluye readr, paquete para importar achivos de texto plano

### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen = 999) # Prevenir notación científica

### Importar base de datos ----
# Cargar Datas----
library(readr)



#Integrar la Intensidad del Conocimiento con el índice creado----
indice_pc1 <- read_excel( "tablas_texto/sectores_intensivos_cluster_4digitos.xlsx")

# Extraer los primeros dos dígitos de la variable Subsector y crear la nueva variable cve_sector
indice_pc1$sector <- substr(indice_pc1$Subsector, 1, 2)

manufacturas<- c("31", "32", "33")

industrias_manufactureras<-indice_pc1 %>%
  filter(sector %in% manufacturas)

industrias_manu_filtradas<-industrias_manufactureras %>%
  select(Posicion, Subsector, PC1, Indice_Normalizado, top_cluster)

library(openxlsx)
# Guarda el dataframe en un archivo .xlsx
write.xlsx(industrias_manu_filtradas, "datos/4digitos/industrias_manu_filtradas.xlsx")


#Metrópolis con Industris Manufactureras----

#Cargar la Data Metropolitana----
#Cargar Datos del Censo Económico para todos los años, para todos los sectores, para todas las metropolis

intensidad_metro<-read_csv("datos/4digitos/data_metropoli_sectores_intensidad_4digitos.csv")


# Extraer los primeros dos dígitos de la variable Subsector y crear la nueva variable cve_sector

intensidad_metro<-intensidad_metro %>%
  mutate(sector = substr(cve_sector, 1, 2))

manufacturas<- c("31", "32", "33")

metropolis_manufactureras<-intensidad_metro %>%
  filter(sector %in% manufacturas)

metropolis_manufactureras<-metropolis_manufactureras %>%
  select(-sector)


#Guardar el Archivo de Metrópolis con datos de Industrias Manufactureras
write_csv(metropolis_manufactureras, "datos/4digitos/metropolis_manufactureras.csv")


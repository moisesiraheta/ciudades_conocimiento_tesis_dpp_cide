# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: #Evaluación de las Trayectorias Metropolitanas
# ==============================================================================

#Limpiar R
rm(list=ls (all=T))
### Paquetes  ----
library(pacman)
p_load(haven,      # Paquete para importar archivos de Stata, SAS y SPSS
       readxl,     # Paquete para importar archivos de Excel
       tidyverse,
       openxlsx,
       purrr,
       dplyr,
       stringr
)

### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen = 999) # Prevenir notación científica

#Cargar la Data----
tendencias<-read_excel("tablas_texto/tendencia_tc.xlsx")
tendencias<-tendencias %>% select(NOM_ZM, tendencia, cl_2003, cl_2018)
tendencias_completo<- read_excel("tablas_texto/TC_completo_c123.xlsx")
crecimiento<-read_excel("tablas_texto/base_heatmap.xlsx")#, locale = locale(encoding = "latin1"))

crecimiento_unir <- crecimiento %>% select (NOM_ZM, Contracción) %>%
  mutate(crecivscontra = case_when(
    Contracción >0 ~ "Contracción", # Condición 1: Mayor o igual a 1
    Contracción ==0 ~ "Crecimiento" )
) %>% select(NOM_ZM, crecivscontra)

tipologia <-tendencias  %>%
  inner_join(crecimiento_unir, by = "NOM_ZM")

#Guardar Archivo
write.xlsx(tipologia,
           file = "tablas_texto/tipologia.xlsx",
           rowNames = TRUE)



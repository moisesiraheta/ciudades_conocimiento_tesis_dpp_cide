# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: Construcción de la Base de Dataos para el Índice de Intensidad del Conocimiento por Rama
# ==============================================================================


#Limpiar R
rm(list=ls (all=T))
#Instalar Paquetes
library(pacman)
p_load(haven,      # Paquete para importar archivos de Stata, SAS y SPSS
       readxl,     # Paquete para importar archivos de Excel
       tidyverse,# Metapaquete que incluye readr, paquete para importar achivos de texto plano
       dplyr,
       openxlsx, #PAquete para guardar archivos en formato excel
       stats,  #Para análisis multivariado como ACP
       mice) #Para imputar valores
# Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen = 999) # Prevenir notación científica

#Cargar Base de datos
#Cargar Variables CE.1- CE.4
ce_4d_indicadores <- read.xlsx("datos/4digitos/ce_4d_indicadores.xlsx")

ce_4d_indicadores %>% filter(cve_sector == "1153") %>%
  select(nombre_sector)

ce_4d_indicadores<-ce_4d_indicadores %>%
  select(cve_sector, CE.1ProporcionPACD, CE.2IntensidadSP, CE.3Produc_inmaterial, CE.4Intensidad_TIC)

#Cargar Variables CE.5- CE.12
tabulado_indicadores_cyt<- read.xlsx("datos/4digitos/tabulado_indicadores_cyt_4digitos.xlsx")
tabulado_indicadores_cyt<- tabulado_indicadores_cyt %>%
  rename(cve_sector = cve_scian)

#Unir las Bases de Datos manteniendo unicamente establecimientos
library(dplyr)
# Unir los data frames usando left_join()
inten_conoc_sectores4d <-ce_4d_indicadores  %>%
  left_join(tabulado_indicadores_cyt, by = "cve_sector")

#Identificar sectores perdidos (¿Por qué no hay datos?)
sectores_vacios_1 <- inten_conoc_sectores4d %>%
  filter(is.na(nombre_sector))%>%
  select(cve_sector) #No tiene información en los Tabulados Temáticos

sectores_vacios_2 <- inten_conoc_sectores4d %>%
filter(nombre_sector == "") %>%
  select(cve_sector)

#Eliminar los sectores que no están presenten en las 2 bases de datos.
inten_conoc_sectores4d <- inten_conoc_sectores4d %>%
  filter(!is.na(nombre_sector) & nombre_sector != "")

#Ordenar las variables
inten_conoc_sectores4d<-inten_conoc_sectores4d %>%
  select(cve_sector, nombre_sector, CE.1ProporcionPACD, CE.2IntensidadSP, CE.3Produc_inmaterial, CE.4Intensidad_TIC, everything() )


#Imputar valores perdidos
data_intensidad_K<-inten_conoc_sectores4d

#Identificar los sectores con valores perdidos
# Identificar valores perdidos por sector y variable
valores_perdidos_por_sector <- data_intensidad_K %>%
  pivot_longer(cols = -c(cve_sector, nombre_sector), # Todas las columnas excepto los identificadores
               names_to = "variable",
               values_to = "valor") %>%
  filter(is.na(valor)) %>%
  group_by(cve_sector,nombre_sector, variable) %>%
  summarise(conteo_perdidos = n(), .groups = 'drop') %>%
  arrange(nombre_sector, variable)


#Imputación de Valores
colnames(data_intensidad_K)
library(mice)
library(tidyverse)

# 1. Identificar las variables a imputar (todas las numéricas excepto los identificadores)
variables_a_imputar <- data_intensidad_K %>%
  select_if(is.numeric) %>%
  colnames()

# 2. Realizar la imputación con MICE
# Establecer una semilla para la reproducibilidad
set.seed(123)

# Ejecutar el algoritmo MICE
# m: número de imputaciones
# maxit: número máximo de iteraciones
imputed_data_mice <- mice(data = data_intensidad_K %>% select(all_of(c("cve_sector", "nombre_sector", variables_a_imputar))),
                          m = 5,
                          maxit = 20,
                          printFlag = FALSE)

# 3. Obtener el conjunto de datos completado (usando la primera imputación)
data_intensidad_K_imputed <- complete(imputed_data_mice, 1)
#Evaluar la calidad de la imputación
plot(imputed_data_mice)

# Variables imputadas
variables_imputadas <- c("CE.9propor_per_innova", "CE.11gasto_percap_iyd", "CE.12gasto_percap_soft")

# Gráfico para variables imputadas
data_intensidad_K %>%
  select(all_of(variables_imputadas)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "original") %>%
  left_join(
    data_intensidad_K_imputed %>%
      select(all_of(variables_imputadas)) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "imputed"),
    by = "variable"
  ) %>%
  ggplot(aes(x = original, color = "Original")) +
  geom_density() +
  geom_density(aes(x = imputed, color = "Imputed")) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Comparación de Distribuciones Originales e Imputadas (MICE)",
       color = "Tipo de Dato")



write.xlsx(inten_conoc_sectores4d ,
           file = "datos/4digitos/inten_conoc_sectores4d.xlsx",
           rowNames = FALSE)

write.xlsx(data_intensidad_K_imputed ,
           file = "datos/4digitos/data_intensidad_K_imputed_4digitos.xlsx",
           rowNames = FALSE)


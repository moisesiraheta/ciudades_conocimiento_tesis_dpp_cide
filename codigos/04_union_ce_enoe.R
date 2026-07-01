# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: Unión Censo Económico y ENOE: Creación de Data para Índice de Intensidad del Conocimiento.
# ==============================================================================

# Limpiar entorno de trabajo para asegurar que no existan variables de sesiones previas
rm(list = ls(all = TRUE))

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
ce_3d_indicadores <- read.xlsx("datos/ce_3d_indicadores.xlsx")
ce_3d_indicadores<-ce_3d_indicadores %>%
  select(cve_sector, CE.1ProporcionPACD, CE.2IntensidadSP, CE.3Produc_inmaterial, CE.4Intensidad_TIC)

# Ver las primeras filas del data frame
head(ce_3d_indicadores)

#Cargar Variables CE.5- CE.12
tabulado_indicadores_cyt<- read.xlsx("datos/tabulado_indicadores_cyt.xlsx")
tabulado_indicadores_cyt<- tabulado_indicadores_cyt %>%
  rename(cve_sector = cve_scian)

#Cargar Variables ENOE.1-ENOE.4
enoe_conocimiento<- read.xlsx("datos/enoe_conocimiento.xlsx")

#Pongo el mismo nombre a la variable cve_sector y nombre_sector
enoe_conocimiento <- enoe_conocimiento %>%
  rename(cve_sector = scian_mx_3d)
enoe_conocimiento <- enoe_conocimiento %>%
  rename(nombre_sector = nombre_subsector_scian_ce_3digitos)

#Elimino nombre sector para quedarme únicamente con las cve_sector y las 4 variables de la ENOE
enoe_conocimiento_var<-enoe_conocimiento %>%
  select(cve_sector, ENOE.1intensidad_trab_conoc, ENOE.2porce_posgrado, ENOE.3porce_stem, ENOE.4porce_tic)

# Unir los data frames usando full_join() repetidamente
inten_conoc_sectores3d <-tabulado_indicadores_cyt  %>%
  full_join(ce_3d_indicadores, by = "cve_sector") %>%
  full_join(enoe_conocimiento_var, by = "cve_sector")

#Identificar sectores perdidos
sectores_vacios <- inten_conoc_sectores3d %>%
  filter(is.na(nombre_sector) | nombre_sector == "") %>%
  select(cve_sector)

# Unir sectores_vacios con enoe_conocimiento usando cve_sector como llave y mantener solo las filas de sectores_vacios
nombre_sectores_vacios <- sectores_vacios %>%
  left_join(enoe_conocimiento, by = "cve_sector") %>%
  select(cve_sector, nombre_sector) # Seleccionar solo las columnas cve_sector y nombre_sector

print(nombre_sectores_vacios)
#Eliminar los sectores que no están presenten en las 3 bases de datos.
inten_conoc_sectores3d <- inten_conoc_sectores3d %>%
  filter(!is.na(nombre_sector) & nombre_sector != "")

#Ordenar las variables
inten_conoc_sectores3d<-inten_conoc_sectores3d %>%
  select(cve_sector, nombre_sector, CE.1ProporcionPACD, CE.2IntensidadSP, CE.3Produc_inmaterial, CE.4Intensidad_TIC, everything() )

#Imputar valores perdidos
data_intensidad_K<-inten_conoc_sectores3d

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

valores_perdidos_por_sector

#Dimensión de la data
67*17

write.xlsx(valores_perdidos_por_sector ,
           file = "tablas_texto/valores_perdidos_por_sector.xlsx",
           rowNames = FALSE)

#Imputación de Valores
colnames(data_intensidad_K)
library(mice)
library(tidyverse)

# 1. Identificar las variables a imputar (todas las numéricas excepto los identificadores)
variables_a_imputar <- data_intensidad_K %>%
  select_if(is.numeric) %>%
   colnames()

# 2. Realizar la imputación con MICE
# Establecer una semilla para reproducibilidad de los resultados
set.seed(123)

# Ejecutar el algoritmo MICE
?mice
str(data_intensidad_K)
# Ver el patrón de datos faltantes antes de imputar
md.pattern(data_intensidad_K)

# m: número de imputaciones
# maxit: número máximo de iteraciones
imputed_data_mice <- mice(data = data_intensidad_K %>% select(all_of(c("cve_sector", "nombre_sector", variables_a_imputar))),
                          m = 5,
                          maxit = 20,
                          printFlag = FALSE)

# Ver qué pasó
imputed_data_mice$loggedEvents #No considera a las variables de identificación en la imputación, lo cual es correcto. Sólo es un mensaje de aviso.
# Revisar qué método usó MICE para cada variable
imputed_data_mice$method #Predictive Mean Matching: Una vez estimados los betas mediante el modelo de regresión y predicho el valor faltante, se selecciona el valor observado del vecino más cercano para realizar la imputación final.


# 3. Obtener el conjunto de datos completado (usando la primera imputación)
data_intensidad_K_imputed <- complete(imputed_data_mice, 1)

#Evaluar la calidad de la imputación
plot(imputed_data_mice)

# Variables de la ENOE
variables_enoe <- c("ENOE.1intensidad_trab_conoc", "ENOE.2porce_posgrado", "ENOE.3porce_stem", "ENOE.4porce_tic")

# Gráfico para variables de la ENOE
data_intensidad_K %>%
  select(all_of(variables_enoe)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "original") %>%
  left_join(
    data_intensidad_K_imputed %>%
      select(all_of(variables_enoe)) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "imputed"),
    by = "variable"
  ) %>%
  ggplot(aes(x = original, color = "Original")) +
  geom_density() +
  geom_density(aes(x = imputed, color = "Imputed")) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Comparación de Distribuciones Originales e Imputadas (MICE) - ENOE",
       color = "Tipo de Dato")

write.xlsx(inten_conoc_sectores3d ,
           file = "datos/inten_conoc_sectores3d.xlsx",
           rowNames = FALSE)

write.xlsx(data_intensidad_K_imputed ,
           file = "datos/data_intensidad_K_imputed.xlsx",
           rowNames = FALSE)


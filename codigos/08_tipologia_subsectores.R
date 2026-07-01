# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: Evaluación de las Trayectorias Metropolitanas Dos dimensiones y Construcción de Tipología con Subsectores
# ==============================================================================
#Limpiar R
rm(list=ls (all=T))
### Paquetes  ----
library(pacman)
p_load(haven,      # Paquete para importar archivos de Stata, SAS y SPSS
       readxl,     # Paquete para importar archivos de Excel
       tidyverse,openxlsx)  # Metapaquete que incluye readr, paquete para importar achivos de texto plano
library(writexl)
library(tidyverse)
### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen = 999) # Prevenir notación científica

# ==========================================
# Cargar la Data ----
# ==========================================
# --- SECCIÓN 1: Datos Subsectores -----------------------------------------

# CVIICAP_Subsectores
CVIICAP_Subsectores <- read_excel("tablas_texto/base_heatmap.xlsx")
CVIICAP_Subsectores <-CVIICAP_Subsectores %>% select(-...1, -Clasificación)

# TCCLPIIC_Subsectores
TCCLPIIC_Subsectores <- read_excel("tablas_texto/TC_completo_c123.xlsx")
TCCLPIIC_Subsectores <- TCCLPIIC_Subsectores %>% select(-...1)
# 2. Unir las tablas por la columna NOM_ZM
Trayectorias_Metro_Sub <- left_join(CVIICAP_Subsectores,
                                       TCCLPIIC_Subsectores,
                                       by = "NOM_ZM")
#Determinar Total CL por año
Trayectorias_Metro_Sub<-Trayectorias_Metro_Sub %>% mutate(
  Total_CL_2018_Mayor1 = rowSums(across(starts_with("CL") & ends_with("2018")) > 1, na.rm = TRUE),
  Total_CL_2003_Mayor1 = rowSums(across(starts_with("CL") & ends_with("2003")) > 1, na.rm = TRUE)
) %>% arrange(desc(Total_CL_2018_Mayor1)) %>%
  select(NOM_ZM, Contracción, Crecimiento, Tendencia_Nega, Tendencia_Post,  Total_CL_2003_Mayor1, Total_CL_2018_Mayor1, everything())


# Se determina la Mediana y la Media de los CL
TCCLPIIC_Clasificacion <- Trayectorias_Metro_Sub %>%
  rowwise() %>%
  mutate(
    # Medianas
    Mediana_CL_2003 = median(c_across(starts_with("CL_") & ends_with("_2003")), na.rm = TRUE),
    Mediana_CL_2018 = median(c_across(starts_with("CL_") & ends_with("_2018")), na.rm = TRUE),

    # Medias
    Media_CL_2003 = mean(c_across(starts_with("CL_") & ends_with("_2003")), na.rm = TRUE),
    Media_CL_2018 = mean(c_across(starts_with("CL_") & ends_with("_2018")), na.rm = TRUE),

    # Clasificación formal basada en el umbral teórico de 1
    Especialización_2003 = if_else(Mediana_CL_2003 >= 1, "CLPIIC>1*", "CLPIIC<1"),
    Especialización_2018 = if_else(Mediana_CL_2018 >= 1, "CLPIIC>1*", "CLPIIC<1")
  ) %>%
  ungroup()

TCCLPIIC_Clasificacion <- TCCLPIIC_Clasificacion %>%
  mutate(
    across(
      .cols = matches("^CV_|^CL_|^TasaC_|^Mediana_|^Media_"),
      .fns  = ~ round(.x, 2)
    )
  )



#Nueva Base Para análisis posterior por CL
clasificaciones_cl<-TCCLPIIC_Clasificacion %>% select(NOM_ZM, Contracción, Crecimiento,  Tendencia_Nega, Tendencia_Post, Total_CL_2003_Mayor1, Total_CL_2018_Mayor1, Mediana_CL_2003, Mediana_CL_2018, Media_CL_2003, Media_CL_2018,Especialización_2003, Especialización_2018, everything() )

#Metrópolis en Transición Hacia Ciudades del Conocimiento----
#Metrópolis en transición
en_transicion<-clasificaciones_cl %>%
  filter(Contracción == 0 & Tendencia_Nega == 0)

#Imprimir nombres de Metrópolis en Transición
metropolis_en_transición <- en_transicion %>%
  select(NOM_ZM, Contracción, Tendencia_Nega) %>%
  mutate(
    texto = paste0(NOM_ZM, " (", Contracción, "-", Tendencia_Nega, ")")
  ) %>%
  pull(texto) %>%
  paste(collapse = ", ")
metropolis_en_transición

#Metropolis en distanciamiento----

en_distanciamiento<-clasificaciones_cl %>%
  filter(Contracción >= 4 & Tendencia_Nega >= 5)
#Nombres de las ZM en distanciamiento
metropolis_en_distanciamiento<- en_distanciamiento %>%
  select(NOM_ZM, Contracción, Tendencia_Nega) %>%
  mutate(
    texto = paste0(NOM_ZM, " (", Contracción, "-", Tendencia_Nega, ")")
  ) %>%
  pull(texto) %>%
  paste(collapse = ", ")

metropolis_en_distanciamiento

#Análisis de CL para Metrópolis en Transición-----

analisis_cl<-en_transicion %>% select(NOM_ZM, Total_CL_2003_Mayor1, Total_CL_2018_Mayor1, CL_pot_c1234_2003, CL_pot_c1234_2018, Mediana_CL_2003, Mediana_CL_2018,Media_CL_2003, Media_CL_2018, Especialización_2003, Especialización_2018) %>%
  arrange(desc(Total_CL_2018_Mayor1), desc(Mediana_CL_2018))

write.xlsx(analisis_cl,
           file = "tablas_texto/analisis_cl_subsector.xlsx",
           rowNames = T)

#Análisis de CL para Metrópolis en Distanciamiento-----

analisis_cl_distanciamiento<-en_distanciamiento %>% select(NOM_ZM, Total_CL_2003_Mayor1, Total_CL_2018_Mayor1, CL_pot_c1234_2003, CL_pot_c1234_2018, Mediana_CL_2003, Mediana_CL_2018,Media_CL_2003, Media_CL_2018, Especialización_2003, Especialización_2018) %>%
  arrange(desc(Total_CL_2018_Mayor1), desc(Mediana_CL_2018))

write.xlsx(analisis_cl_distanciamiento,
           file = "tablas_texto/analisis_cl_distanciamiento_subsector.xlsx",
           rowNames = T)


#Metrópolis  Inercia----
#Metrópolis inercia
en_inercia<-clasificaciones_cl %>%
  filter(Contracción == 0 & Tendencia_Nega > 0)

#Imprimir nombres de Metrópolis en inercia
metropolis_en_inercia <- en_inercia %>%
  select(NOM_ZM, Contracción, Tendencia_Nega) %>%
  mutate(
    texto = paste0(NOM_ZM, " (", Contracción, "-", Tendencia_Nega, ")")
  ) %>%
  pull(texto) %>%
  paste(collapse = ", ")
metropolis_en_inercia

# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moisés Israel Iraheta Ávila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: Procesamiento de los Tabulados de los Censos Económicos 2018 (INNCENCE)
#Tabulados Censos Económicos 4 dígitos
# ==============================================================================

### Preparación de R para trabajar  ----
#Limpiar R
rm(list=ls (all=T))

# Instalar Paquetes
library(pacman) # Pacman incluye la función p_load() que carga los paquetes que uno requiere, y
p_load(haven,         # Paquete para importar archivos de Stata, SAS y SPSS
       readxl,        # Paquete para importar archivos de Excel
       tidyverse,     # Metapaquete que incluye readr, paquete para importar archivos de texto plano, y tidyr, entre otros.
       dplyr, openxlsx)

# Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen = 999) # Prevenir notación científica

#Cargr Datas----

#UE INNOVAN----
#Calcular el Porcentaje de Unidades Económicas con Actividades de Innovación
#Cargar Archivo
tab_innoce_01<-read.xlsx("datos/4digitos/innoce19_01_4digitos.xlsx")

#Explorar datoas
str(tab_innoce_01)

#Limpiar data ue que innovan
tab_innoce_01$cve_scian <- gsub("Rama ", "", tab_innoce_01$cve_scian)

tab_innoce_01$nombre_scian <- gsub("Rama .....", "", tab_innoce_01$nombre_scian)


tab_innoce_01<-tab_innoce_01 %>%
  mutate(CE.7propor_ue_innovan = ue_si_innova/total_ue)

#Creo el vector que usaré al final
vector_propor_ue_innovan<- tab_innoce_01 %>%
  select(cve_scian, CE.7propor_ue_innovan)

#COORDINACION para la INNOVACION----
#Calcular el Porcentaje Actividades de Innovación realizadas de manera coordinada con universidad, centros de investigación, empresas, clientes o proveedores en los años 2016, 2017 y 2018
#Cargar Archivo
tab_innoce_02<-read.xlsx("datos/4digitos/innoce19_02_4digitos.xlsx")

#Explorar datoas
str(tab_innoce_02)

#Limpiar data Nivel Educativo
tab_innoce_02$cve_sector <- gsub("Rama ", "", tab_innoce_02$cve_sector)

tab_innoce_02$nombre_sector <- gsub("Rama .....", "", tab_innoce_02$nombre_sector)


#Calcular Variable de Interés
tab_innoce_02<-tab_innoce_02 %>%
  mutate(media_coord_uni =  (coor_univer_2016 + coor_univer_2017 + coor_univer_2018)/3 ) %>%
  mutate(media_coord_empresa =  (coor_empresa_2016 + coor_empresa_2017 + coor_empresa_2018)/3) %>%
  mutate( media_coord_cliente = (coor_clientes_2016 + coor_clientes_2017 + coor_clientes_2018)/3) %>%
  mutate(act_innova = media_coord_uni + media_coord_empresa + media_coord_cliente) %>%
  mutate( CE.8propor_act_coord = act_innova /total_ue  )

#Creo el vector que usaré al final
tab_innoce_02 <- tab_innoce_02 %>%
  rename(cve_scian = cve_sector)

vector_coordina_innova<- tab_innoce_02 %>%
  select(cve_scian, CE.8propor_act_coord)

#PERSONAS DEDICADAS A LA INNOVACION----
#Calcular el Personas dedicadas a actividades de innovación en productos, marketing, procesos, gestión, adaptación y documentación de tecnologías
#Cargar Archivo
tab_innoce_04<-read.xlsx("datos/4digitos/innoce19_04_4digitos.xlsx")

#Explorar datoas
str(tab_innoce_04)

#Limpiar data de personas dedicadas a actividades de innovación
tab_innoce_04$cve_scian <- gsub("Rama ", "", tab_innoce_04$cve_scian)

tab_innoce_04$nombre_sector <- gsub("Rama .....", "", tab_innoce_04$nombre_sector)

#Calcular Variable de Interés
#Agregar el dato de personal ocupado total de la innoce_01
tab_innoce_04 <- tab_innoce_04 %>%
  left_join(select(tab_innoce_01, cve_scian, per_ocu_total), by = "cve_scian")

tab_innoce_04<-tab_innoce_04 %>%
  mutate(total_perso_innova = per_en_innova_producto + per_en_innova_proceso + per_en_innova_marketing + per_en_innova_gestion + per_en_innova_docu_tec) %>%
  mutate(CE.9propor_per_innova = total_perso_innova/per_ocu_total )

#Creo el vector que usaré al final
vector_propor_personas_innova<- tab_innoce_04 %>%
  select(cve_scian, CE.9propor_per_innova)


#PATENTES----
#Calcular Unidades Económicas que registraron o tramitaron patentes de marcas, productos
#Cargar Archivo
tab_innoce_06<-read.xlsx("datos/4digitos/innoce19_06_4digitos.xlsx")

#Explorar datoas
str(tab_innoce_06)

#Limpiar data de personas dedicadas a actividades de innovación
tab_innoce_06$cve_scian <- gsub("Rama ", "", tab_innoce_06$cve_scian)

tab_innoce_06$nombre_sector <- gsub("Rama .....", "", tab_innoce_06$nombre_sector)

#Calcular Variable de Interés
tab_innoce_06<- tab_innoce_06 %>%
  mutate( promedio_patentes = (pantente_al_menos_un_año + patente_2016 + patente_2017 + patento_2018)/4) %>%
  mutate(CE.10patentes_por_ue =  promedio_patentes /total_ue)

#Creo el vector que usaré al final
vector_patentes_por_ue<- tab_innoce_06 %>%
  select(cve_scian, CE.10patentes_por_ue)


#GASTO I+D y GASTO SOFTWARE----
#Calcular Gasto en I+D y el Gasto en Software
#Cargar Archivo
tab_innoce_07<-read.xlsx("datos/4digitos/innoce19_07_4digitos.xlsx")

#Explorar datoas
str(tab_innoce_07)

#Limpiar data de personas dedicadas a actividades de innovación
tab_innoce_07$cve_scian <- gsub("Rama ", "", tab_innoce_07$cve_scian)

tab_innoce_07$nombre_sector <- gsub("Rama .....", "", tab_innoce_07$nombre_sector)

#Calcular Variable de Interés
tab_innoce_01$total_ue==tab_innoce_07$total_ue

tab_innoce_07 <- tab_innoce_07 %>%
  left_join(select(tab_innoce_01, cve_scian, per_ocu_total), by = "cve_scian")

tab_innoce_07<-tab_innoce_07 %>%
  mutate(promedio_gasto_iyd = (gato_iyd_2016 + gato_iyd_2017 + gato_iyd_2018)/3 ) %>%
  mutate(CE.11gasto_percap_iyd = promedio_gasto_iyd /per_ocu_total) %>%
  mutate(promedio_gasto_soft = (gasto_soft_2016 + gasto_soft_2017 + gasto_soft_2018)/3) %>%
  mutate(CE.12gasto_percap_soft = promedio_gasto_soft /per_ocu_total)

#Creo el vector que usaré al final
vector_gasto_per_iyd_soft<- tab_innoce_07 %>%
  select(cve_scian, nombre_sector, CE.11gasto_percap_iyd, CE.12gasto_percap_soft)


#Reviso mis datas que he creado
vector_gasto_per_iyd_soft
vector_propor_personas_innova
vector_coordina_innova
vector_patentes_por_ue
vector_propor_ue_innovan



#Crear mi Base de Datos de los Tabulados----

library(dplyr)
# Unir los data frames usando full_join() repetidamente
tabulado_indicadores_cyt <- vector_gasto_per_iyd_soft %>%
  full_join(vector_propor_personas_innova, by = "cve_scian") %>%
  full_join(vector_coordina_innova, by = "cve_scian") %>%
  full_join(vector_patentes_por_ue, by = "cve_scian") %>%
  full_join(vector_propor_ue_innovan, by = "cve_scian")

#Ordeno los indicadores
tabulado_indicadores_cyt<-tabulado_indicadores_cyt %>%
  select(cve_scian, nombre_sector,  CE.7propor_ue_innovan, CE.8propor_act_coord, CE.9propor_per_innova, CE.10patentes_por_ue, CE.11gasto_percap_iyd, CE.12gasto_percap_soft )

#Guardar la data
write.xlsx(tabulado_indicadores_cyt,
           file = "datos/4digitos/tabulado_indicadores_cyt_4digitos.xlsx", rowNames = FALSE)

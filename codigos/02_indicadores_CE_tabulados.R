# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moisés Israel Iraheta Ávila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: Procesamiento de los Tabulados de los Censos Económicos 2018 (INNCENCE)
# ==============================================================================

# 1. PREPARACIÓN DEL ENTORNO ----
# rm(): Función que elimina objetos del entorno. Al usar list=ls(all=TRUE),
# limpiamos todo el historial de variables para evitar mezclar datos de otras sesiones.
#Limpiar R
rm(list=ls (all=T))

# Instalar Paquetes
library(pacman) # Pacman incluye la función p_load() que carga los paquetes que uno requiere, y
p_load(haven,         # Paquete para importar archivos de Stata, SAS y SPSS
       readxl,        # Paquete para importar archivos de Excel
       tidyverse,     # Metapaquete que incluye readr, paquete para importar archivos de texto plano, y tidyr, entre otros.
       dplyr,
       openxlsx)

# Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen = 999) # Prevenir notación científica

#Cargr Datas----
#1.- Nivel Educativo
#2.1.- Computadoras y 2.2.- Internet


#Nivel Educativo por Sector
tabla_educa_superior <- read.xlsx("datos/tab_educa_superior.xlsx")
#Explorar datoas
str(tabla_educa_superior)

#Limpiar data Nivel Educativo
tabla_educa_superior$cve_scian <- gsub("Subsector ", "", tabla_educa_superior$cve_scian)

tabla_educa_superior$nombre_sector<- gsub("Subsector ....", "", tabla_educa_superior$nombre_sector)

#Eliminar los Subsectores cuya Unidad de Observación no es el Establecimiento
no_establecimiento<- c("112","114","211", "212", "221", "236", "237","238", "481", "482", "483", "484", "485", "486", "487", "491", "492","521", "522", "523","524")#Sectores que no son establecimientos.
tabla_educa_superior<- tabla_educa_superior %>%
  filter(!cve_scian %in% no_establecimiento)#Eliminar subsectores cuya unidad de observación no fue el establecimiento.

#Calcular el Porcentaje de personal ocupado con educación superior por subsector
tabla_educa_superior<- tabla_educa_superior %>%
  mutate(CE.5porcent_educa_superior = edu_superior/per_ocupa_total)
#Creo el vector que usaré al final
vector_educa_superior <- tabla_educa_superior %>%
  select(cve_scian, CE.5porcent_educa_superior)

#Creo mi tabla para el escrito
rank_tabla_porcent_educa_superior<- tabla_educa_superior %>%
  select(cve_scian, nombre_sector,edu_superior, per_ocupa_total, CE.5porcent_educa_superior) %>%
  arrange(desc(CE.5porcent_educa_superior)) %>%
  mutate(ranking = row_number()) %>%
  select(ranking, everything())%>%
  slice(1:5, (n() - 4):n())

write.xlsx(rank_tabla_porcent_educa_superior,
           file = "tablas_texto/rank_tabla_porcent_educa_superior.xlsx", rowNames = FALSE)


#Calcular el Porcentaje de INTERNET y COMPUS----
#Cargar Archivo
tabla_compu_internet<-read.xlsx("datos/tab_compu_internet.xlsx")

#Explorar datos
str(tabla_compu_internet)

#Limpiar data Internet y computadoras
tabla_compu_internet$cve_scian <- gsub("Subsector ", "", tabla_compu_internet$cve_scian)

tabla_compu_internet$nombre_sector<- gsub("Subsector ....", "", tabla_compu_internet$nombre_sector)

tabla_compu_internet<- tabla_compu_internet %>%
  filter(!cve_scian %in% no_establecimiento)#Eliminar subsectores cuya unidad de observación no fue el establecimiento.
tabla_compu_internet<-tabla_compu_internet %>%
  rename(
    CE.6.1porcen_si_compu = porcen_si_compu,
    CE.6.2porcent_si_internet = porcent_si_internet
  )

#Creo el vector que usaré al final
vector_compu_internet<- tabla_compu_internet %>%
  select(cve_scian, CE.6.1porcen_si_compu, CE.6.2porcent_si_internet)

#Creo mi tabla para el escrito
rank_tabla_compu<- tabla_compu_internet %>%
  select(cve_scian, nombre_sector, CE.6.1porcen_si_compu) %>%
  arrange(desc(CE.6.1porcen_si_compu)) %>%
  mutate(ranking = row_number()) %>%
  select(ranking, everything())%>%
  slice(1:5, (n() - 4):n())

rank_tabla_internet<- tabla_compu_internet %>%
  select(cve_scian, nombre_sector, CE.6.2porcent_si_internet) %>%
  arrange(desc(CE.6.2porcent_si_internet)) %>%
  mutate(ranking = row_number()) %>%
  select(ranking, everything())%>%
  slice(1:5, (n() - 4):n())

write.xlsx(rank_tabla_compu,
           file = "tablas_texto/rank_tabla_compu.xlsx", rowNames = FALSE)

write.xlsx(rank_tabla_internet,
           file = "tablas_texto/rank_tabla_internet.xlsx", rowNames = FALSE)

#UE INNOVAN----
#Calcular el Porcentaje de Unidades Económicas con Actividades de Innovación
#Cargar Archivo
tab_innoce_01<-read.xlsx("datos/editado_innoce19_01.xlsx")

#Explorar datoas
str(tab_innoce_01)

#Limpiar data ue que innovan
tab_innoce_01$cve_scian <- gsub("Subsector ", "", tab_innoce_01$cve_scian)

tab_innoce_01$nombre_scian <- gsub("Subsector ....", "", tab_innoce_01$nombre_scian)

#Eliminar los Subsectores cuya Unidad de Observación no es el Establecimiento
tab_innoce_01<- tab_innoce_01 %>%
  filter(!cve_scian %in% no_establecimiento)#Eliminar subsectores cuya unidad de observación no fue el establecimiento.

tab_innoce_01<-tab_innoce_01 %>%
  mutate(CE.7propor_ue_innovan = ue_si_innova/total_ue)

#Creo el vector que usaré al final
vector_propor_ue_innovan<- tab_innoce_01 %>%
  select(cve_scian, CE.7propor_ue_innovan)

#Creo mi tabla para el escrito

rank_tab_innoce_01<- tab_innoce_01 %>%
  select(cve_scian, nombre_scian, ue_si_innova, total_ue, CE.7propor_ue_innovan) %>%
  arrange(desc(CE.7propor_ue_innovan)) %>%
  mutate(ranking = row_number()) %>%
  select(ranking, everything())%>%
  slice(1:5, (n() - 4):n())

write.xlsx(rank_tab_innoce_01,
           file = "tablas_texto/rank_tab_innoce_01.xlsx", rowNames = FALSE)


#COORDINACION para la INNOVACION----
#Calcular el Porcentaje Actividades de Innovación realizadas de manera coordinada con universidad, centros de investigación, empresas, clientes o proveedores en los años 2016, 2017 y 2018
#Cargar Archivo
tab_innoce_02<-read.xlsx("datos/editado_innoce19_02.xlsx")

#Explorar datos
str(tab_innoce_02)

#Limpiar data
tab_innoce_02$cve_sector <- gsub("Subsector ", "", tab_innoce_02$cve_sector)

tab_innoce_02$nombre_sector <- gsub("Subsector ....", "", tab_innoce_02$nombre_sector)

#Eliminar los subsectores cuya Unidad de Observación no es el Establecimiento
tab_innoce_02<- tab_innoce_02 %>%
  filter(!cve_sector %in% no_establecimiento)#Eliminar subsectores cuya unidad de observación no fue el establecimiento.

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

#Creo mi tabla para el escrito
rank_tab_innoce_02<- tab_innoce_02 %>%
  select(cve_scian, nombre_sector, act_innova,total_ue, CE.8propor_act_coord ) %>%
  arrange(desc(CE.8propor_act_coord)) %>%
  mutate(ranking = row_number()) %>%
  select(ranking, everything())%>%
  slice(1:5, (n() - 4):n())

write.xlsx(rank_tab_innoce_02,
           file = "tablas_texto/rank_tab_innoce_02.xlsx", rowNames = FALSE)



#PERSONAS DEDICADAS A LA INNOVACION----
#Calcular Personas dedicadas a actividades de innovación en productos, marketing, procesos, gestión, adaptación y documentación de tecnologías
#Cargar Archivo
tab_innoce_04<-read.xlsx("datos/editado_innoce19_04.xlsx")

#Explorar datos
str(tab_innoce_04)

#Limpiar data de personas dedicadas a actividades de innovación
tab_innoce_04$cve_scian <- gsub("Subsector ", "", tab_innoce_04$cve_scian)

tab_innoce_04$nombre_sector <- gsub("Subsector ....", "", tab_innoce_04$nombre_sector)

#Eliminar los sectores cuya Unidad de Observación no es el Establecimiento
tab_innoce_04<- tab_innoce_04 %>%
  filter(!cve_scian %in% no_establecimiento)#Eliminar subsectores cuya unidad de observación no fue el establecimiento.

#Calcular Variable de Interés
tab_innoce_04 <- tab_innoce_04 %>%
  left_join(select(tab_innoce_01, cve_scian, per_ocu_total), by = "cve_scian")

tab_innoce_04<-tab_innoce_04 %>%
  mutate(total_perso_innova = per_en_innova_producto + per_en_innova_proceso + per_en_innova_marketing + per_en_innova_gestion + per_en_innova_docu_tec) %>%
  mutate(CE.9propor_per_innova = total_perso_innova/per_ocu_total )

#Creo el vector que usaré al final
vector_propor_personas_innova<- tab_innoce_04 %>%
  select(cve_scian, CE.9propor_per_innova)


#Creo mi tabla para el escrito

rank_tab_innoce_04<- tab_innoce_04 %>%
  select(cve_scian, nombre_sector, total_perso_innova, per_ocu_total, CE.9propor_per_innova ) %>%
  arrange(desc(CE.9propor_per_innova)) %>%
  mutate(ranking = row_number()) %>%
  select(ranking, everything())%>%
  slice(1:5, (n() - 4):n())

write.xlsx(rank_tab_innoce_04,
           file = "tablas_texto/rank_tab_innoce_04.xlsx", rowNames = FALSE)


#PATENTES----
#Calcular Unidades Económicas que registraron o tramitaron patentes de marcas, productos
#Cargar Archivo
tab_innoce_06<-read.xlsx("datos/editado_innoce19_06.xlsx")

#Explorar datoas
str(tab_innoce_06)

#Limpiar data de personas dedicadas a actividades de innovación
tab_innoce_06$cve_scian <- gsub("Subsector ", "", tab_innoce_06$cve_scian)

tab_innoce_06$nombre_sector <- gsub("Subsector ....", "", tab_innoce_06$nombre_sector)

#Eliminar los sectores cuya Unidad de Observación no es el Establecimiento
tab_innoce_06<- tab_innoce_06 %>%
  filter(!cve_scian %in% no_establecimiento)#Eliminar subsectores cuya unidad de observación no fue el establecimiento.

#Calcular Variable de Interés
tab_innoce_06<- tab_innoce_06 %>%
  mutate( promedio_patentes = (pantente_al_menos_un_año + patente_2016 + patente_2017 + patento_2018)/4) %>%
  mutate(CE.10patentes_por_ue =  promedio_patentes /total_ue)

#Creo el vector que usaré al final
vector_patentes_por_ue<- tab_innoce_06 %>%
  select(cve_scian, CE.10patentes_por_ue)


#Creo mi tabla para el escrito
rank_tab_innoce_06<- tab_innoce_06 %>%
  select(cve_scian, nombre_sector, promedio_patentes, total_ue,CE.10patentes_por_ue ) %>%
  arrange(desc(CE.10patentes_por_ue)) %>%
  mutate(ranking = row_number()) %>%
  select(ranking, everything())%>%
  slice(1:5, (n() - 4):n())

write.xlsx(rank_tab_innoce_06,
           file = "tablas_texto/rank_tab_innoce_06.xlsx", rowNames = FALSE)


#GASTO I+D y GASTO SOFTWARE----
#Calcular Gasto en I+D y el Gasto en Software
#Cargar Archivo
tab_innoce_07<-read.xlsx("datos/editado_innoce19_07.xlsx")

#Explorar datoas
str(tab_innoce_07)

#Limpiar data de personas dedicadas a actividades de innovación
tab_innoce_07$cve_scian <- gsub("Subsector ", "", tab_innoce_07$cve_scian)

tab_innoce_07$nombre_sector <- gsub("Subsector ....", "", tab_innoce_07$nombre_sector)

#Eliminar los sectores cuya Unidad de Observación no es el Establecimiento
tab_innoce_07<- tab_innoce_07 %>%
  filter(!cve_scian %in% no_establecimiento)#Eliminar subsectores cuya unidad de observación no fue el establecimiento.

#Calcular Variable de Interés
tab_innoce_01$total_ue==tab_innoce_07$total_ue

tab_innoce_07 <- tab_innoce_07 %>%
  left_join(select(tab_innoce_01, cve_scian, per_ocu_total), by = "cve_scian")

tab_innoce_01$per_ocu_total ==tab_innoce_07$per_ocu_total

tab_innoce_07<-tab_innoce_07 %>%
  mutate(gasto_iyd = gato_iyd_2016 + gato_iyd_2017 + gato_iyd_2018) %>%
  mutate( promedio_gasto_iyd = gasto_iyd/3 ) %>%
  mutate(CE.11gasto_percap_iyd = promedio_gasto_iyd /per_ocu_total)


tab_innoce_07<-tab_innoce_07 %>%
  mutate( gasto_soft = gasto_soft_2016 +gasto_soft_2017 +gasto_soft_2018) %>%
  mutate(promedio_gasto_soft = gasto_soft/3 ) %>%
  mutate(CE.12gasto_percap_soft = promedio_gasto_soft/ per_ocu_total )

#Creo el vector que usaré al final
vector_gasto_per_iyd_soft<- tab_innoce_07 %>%
  select(cve_scian, nombre_sector, CE.11gasto_percap_iyd, CE.12gasto_percap_soft)


#Creo mi tabla para el escrito giyd
rank_tab_innoce_07<- tab_innoce_07 %>%
  select(cve_scian, nombre_sector,promedio_gasto_iyd, per_ocu_total, CE.11gasto_percap_iyd ) %>%
  arrange(desc(CE.11gasto_percap_iyd )) %>%
  mutate(ranking = row_number()) %>%
  select(ranking, everything())%>%
  slice(1:5, (n() - 4):n())

write.xlsx(rank_tab_innoce_07,
           file = "tablas_texto/rank_tab_innoce_07.xlsx", rowNames = FALSE)

#Creo mi tabla para el escrito gasto sofware
rank_tab_innoce_07.2<- tab_innoce_07 %>%
  select(cve_scian, nombre_sector, promedio_gasto_soft, per_ocu_total, CE.12gasto_percap_soft ) %>%
  arrange(desc(CE.12gasto_percap_soft )) %>%
  mutate(ranking = row_number()) %>%
  select(ranking, everything())%>%
  slice(1:5, (n() - 4):n())

write.xlsx(rank_tab_innoce_07.2,
           file = "tablas_texto/rank_tab_innoce_07.2.xlsx", rowNames = FALSE)


vector_gasto_per_iyd_soft
vector_propor_personas_innova
vector_coordina_innova
vector_compu_internet
vector_educa_superior
vector_patentes_por_ue
vector_propor_ue_innovan



#Crear mi Base de Datos de los Tabulados

library(dplyr)

# Unir los data frames usando full_join() repetidamente
tabulado_indicadores_cyt <- vector_gasto_per_iyd_soft %>%
  full_join(vector_propor_personas_innova, by = "cve_scian") %>%
  full_join(vector_coordina_innova, by = "cve_scian") %>%
  full_join(vector_compu_internet, by = "cve_scian") %>%
  full_join(vector_educa_superior, by = "cve_scian") %>%
  full_join(vector_patentes_por_ue, by = "cve_scian") %>%
  full_join(vector_propor_ue_innovan, by = "cve_scian")

#Ordeno los indicadores
tabulado_indicadores_cyt<-tabulado_indicadores_cyt %>%
  select(cve_scian, nombre_sector, CE.5porcent_educa_superior, CE.6.1porcen_si_compu, CE.6.2porcent_si_internet, CE.7propor_ue_innovan, CE.8propor_act_coord, CE.9propor_per_innova, CE.10patentes_por_ue, CE.11gasto_percap_iyd, CE.12gasto_percap_soft )

#Guardar la data
write.xlsx(tabulado_indicadores_cyt,
           file = "datos/tabulado_indicadores_cyt.xlsx", rowNames = FALSE)


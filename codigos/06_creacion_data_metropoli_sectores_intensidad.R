# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: Data de Municipios-Metrópolis
#Este código tiene por objeto: Crear la BD de los datos con los cuales se identifican a las metrópolis, principalmente los municipios que los constituyen, las claves que inegi asigna a los municipios, las claves de las Zonas Metroplitana (ZM), y otros datos que permitan asociar bases de datos a nivel municipal y construir datos a nivel metrópoli.
# ==============================================================================

#Limpiar R
rm(list=ls (all=T))

### Paquetes  ----
library(pacman)
p_load(haven,      # Paquete para importar archivos de Stata, SAS y SPSS
       readxl,     # Paquete para importar archivos de Excel
       tidyverse)  # Metapaquete que incluye readr, paquete para importar archivos de texto plano

### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen = 999) # Prevenir notación científica

### Importar base de datos ----
# Cargar Datas----
library(readr)

url <- "https://conapo.segob.gob.mx/work/models/CONAPO/Datos_Abiertos/Delimitacion_ZM/ZM_2015.csv"
zm_2015<- read_csv(url, col_types = cols(
  CVE_ZM = col_character(),
  CVE_ENT = col_character(),
  CVE_MUN = col_character()
))

#Texto para Tesis: Zonas Metropolitanas, Población y Municipios
zm_2015_ordenado <- zm_2015 %>%
  group_by(NOM_ZM) %>%
  arrange(desc(POB_2015), .by_group = TRUE) %>%
  ungroup()

zm_2015_mod <- zm_2015_ordenado  %>%
  mutate(
    NOM_MUN_MC = if_else(MC == 1, #Etiqueta a municipios centrales
                         paste0(NOM_MUN, " (MC)"),
                         NOM_MUN)
  ) %>%
  select(NOM_ENT, NOM_ZM, POB_2015, NOM_MUN_MC, MC)

#Resumen ZM, Poblción y Municipios
zm_2015_resumen <- zm_2015_mod  %>%
  group_by(NOM_ZM) %>%
  summarise(
    POB_2015_total = sum(POB_2015, na.rm = TRUE),
    Municipios = paste(unique(NOM_MUN_MC), collapse = " | ")
  ) %>%
  arrange(desc(POB_2015_total))


write_csv(zm_2015_resumen, "tablas_texto/zm_2015_resumen.csv")


zm_2015<- zm_2015 %>% #Selecciona las variables de interés para formar ZM a partir de municipios.
  select(CVE_ZM,
         NOM_ZM,
         CVE_ENT,
         NOM_ENT,
         CVE_MUN,
         NOM_MUN,
         MC,
         CF,
         DIST_CC,
         TIPO_MUN)

zm_2015 #Ya tengo la Clave Municipal a 5 dígitos de la Base de Metrópolis

#Guardar la Data
write_csv(zm_2015, "datos/clave_metro_2015.csv")

#Creación de la Data Metropolitana----
#Cargar Datos del Censo Económico para todos los años, para todos los sectores, para todos los municipios, con totales

ce_mun<-read_csv("datos/SAIC_Exporta_2025518_83148141.csv")

#Crear la Variable CVE_MUN para el Censo Económico----

#Crear la variable CVE_MUN con los primeros dos dígitos de la variable ce_mun$Entidad y los primeros tres dígitos de la variable ce_mun$Municipio
# Extraer los primeros dos dígitos de la variable Entidad
entidad_prefijo <- substr(ce_mun$Entidad, 1, 2)
# Extraer los primeros tres dígitos de la variable Municipio
municipio_prefijo <- substr(ce_mun$Municipio, 1, 3)
# Combinar los prefijos para crear la nueva variable CVE_MUN
ce_mun$CVE_MUN <- paste0(entidad_prefijo, municipio_prefijo)

#Crear la data con los datos a nivel nacional por año por cada subsector "ce_nac" ----
ce_nac <- ce_mun %>%
  filter(Entidad == "00 Total Nacional")
#Renombrar las variables del ce_nac
ce_nac<-ce_nac%>%
  rename(
    ue_nac_subsector_t=`UE Unidades económicas`
    )
ce_nac<-ce_nac%>%
  rename(
    pot_nac_subsector_t= `H001A Personal ocupado total`
    )
ce_nac<-ce_nac%>%
  rename(
    vapercap_nac_subsector_t= `A204A Valor agregado en promedio por persona ocupada (Pesos)`
    )
ce_nac<- ce_nac%>%
  rename(
    ingresos_nac_subsector_t=`M000A Total de ingresos por suministro de bienes y servicios (millones de pesos)`
    )
ce_nac<- ce_nac%>%
  rename(
    ingresos_percap_nac_subsector_t=`A436A Ingresos por suministro de bienes y servicios por persona ocupada (Pesos)`
    )
ce_nac<- ce_nac%>%
  rename(
   vacb_nac_subsector_t=`A131A Valor agregado censal bruto (millones de pesos)`
  )

colnames(ce_nac)
#Seleccionar Variables de interés----
ce_nac<-ce_nac %>% select(`Año Censal`, `Actividad económica`, ue_nac_subsector_t, pot_nac_subsector_t, vacb_nac_subsector_t, vapercap_nac_subsector_t, ingresos_nac_subsector_t, ingresos_percap_nac_subsector_t)
#####

#Crear la data con los datos totales municipales por año "ce_tot_mun" ----
ce_tot_mun<- ce_mun %>%
  filter(`Actividad económica` == "Total municipal")
#Renombrar variables para que indiquen que es el total municipal
ce_tot_mun<-ce_tot_mun %>%
  rename(tot_mun_ue=`UE Unidades económicas`)

ce_tot_mun<-ce_tot_mun %>%
  rename( tot_mun_pot= `H001A Personal ocupado total`)

ce_tot_mun<-ce_tot_mun %>%
  rename( tot_mun_vapercap= `A204A Valor agregado en promedio por persona ocupada (Pesos)`)

ce_tot_mun<-ce_tot_mun %>%
  rename( tot_mun_ingresos= `M000A Total de ingresos por suministro de bienes y servicios (millones de pesos)`)

ce_tot_mun<-ce_tot_mun %>%
  rename( tot_mun_ingresos_percap= `A436A Ingresos por suministro de bienes y servicios por persona ocupada (Pesos)`)

ce_tot_mun<-ce_tot_mun %>%
  rename( tot_mun_vacb= `A131A Valor agregado censal bruto (millones de pesos)`)

colnames(ce_tot_mun)
#Selecciona las variables de interés de totales municipales
ce_tot_mun<-ce_tot_mun %>%
  select(`Año Censal`, CVE_MUN, tot_mun_ue, tot_mun_pot, tot_mun_ingresos, tot_mun_vacb, tot_mun_ingresos_percap, tot_mun_vapercap)
#####

#Limpiar la Base de ce_mun y crear "ce_mun_filtrado"----
#Base de datos con el el valor de la variable por subsector por municipio por año "ce_mun_filtrado"
# Eliminar las observaciones donde la columna 'Entidad' es igual a "00 Total Nacional"
ce_mun_filtrado <- ce_mun %>%
  filter(Entidad != "00 Total Nacional")
# Luego, sobre el data frame resultante, eliminar las observaciones donde la columna 'Actividad económica' es igual a "Total municipal"
ce_mun_filtrado <- ce_mun_filtrado %>%
  filter(`Actividad económica` != "Total municipal")

#Aquí tengo Tres Bases de datos----

#1.- Base de datos con las variables a nivel subsector a nivel nacional por año "ce_nac"
#2.- Base de datos con los totales municipales por año por cada variable "ce_tot_mun"
#3.- Base de datos con el el valor de la variable por subsector por municipio por año "ce_mun_filtrado"

#Quiero en una sola base de datos tener, por año, por metrópoli y por subsector las UE (y todas las variables)
#Necesito primero, filtrar y quedarme sólo con municipios metropolitanos.

#Crear las Bases Metropolitanas----
#Tengo dos bases de datos una con año-municipio-sector y otra con total-año-municipio. En ambas tengo que agrupar por metrópoli

# 1.- Seleccionar solo los municipios que sean metropolitanos
# 2.- Calcular las variables a nivel metropolitano

#Voy a comenzar con la base de total-año-municipio "ce_tot_mun"----
# Para seleccionar los municipios metropolitanos necesito # i) La clave de zonas metro
#Afinar la Clave de Zonas Metro
zm_2015<-zm_2015 %>%
  select(-CVE_ENT, -NOM_ENT)
# Crear la data totales-año-municipio identificando a los municipios metropolitanos
totales_municipios_metro <- merge(zm_2015, ce_tot_mun,
                          by = "CVE_MUN",
                          all = TRUE)
# Eliminar municipio no metropolitanos
totales_municipios_metro <- totales_municipios_metro %>%
  filter(!is.na(CVE_ZM))
#Ordenar las variables comenzando por año censal, luego zona metro, luego las demás.
totales_municipios_metro<-totales_municipios_metro %>%
  select(`Año Censal`, NOM_ZM, everything())
# Agrupar por año y CVE_ZM
#Crear la primera data metropolitana: totales por metrópoli m en el año t "totales_metro_año"----
totales_metro_año <- totales_municipios_metro %>%
  group_by(`Año Censal`,NOM_ZM) %>%
  summarise(
    ue_m_t = sum(tot_mun_ue, na.rm = TRUE),
    pot_m_t = sum(tot_mun_pot, na.rm = TRUE),
    vacb_m_t = sum(tot_mun_vacb, na.rm = TRUE),
    ingresos_m_t = sum(tot_mun_ingresos, na.rm = TRUE),
    va_percap_m_t = sum(tot_mun_vapercap, na.rm = TRUE),
    ingresos_pecap_m_t  = sum(tot_mun_ingresos_percap, na.rm = TRUE))
#Tengo UE Totales (y todas las variables) por ZM por año (Suma de UE por Municipio).
#####

#Ahora Sigue calcular las variables por año, por ZM, por Subsector----

#1.- Identificar Municipio Metropolitanos en la base ce_mun_filtrado
municipios_metro_con_subsector_anio <- merge(zm_2015, ce_mun_filtrado,
                          by = "CVE_MUN",
                          all = TRUE)
# Eliminar municipios que no son metropolitanos
municipios_metro_con_subsector_anio <- municipios_metro_con_subsector_anio %>%
  filter(!is.na(CVE_ZM))
#Ordenar las variables
municipios_metro_con_subsector_anio<-municipios_metro_con_subsector_anio %>%
  select(`Año Censal`, NOM_ZM, `Actividad económica`, everything())
#Calcular las UE (y todas las variables) en la metrópoli en el año t, en la metrópoli m, para el sector i
# Crear data metropolitana año, CVE_ZM y Actividad económica "metro_sectores_año"----
metro_sectores_año <- municipios_metro_con_subsector_anio %>%
  group_by(`Año Censal`,NOM_ZM, `Actividad económica`) %>%
  summarise(
    ue_m_i_t = sum(`UE Unidades económicas`, na.rm = TRUE),
    pot_m_i_t = sum(`H001A Personal ocupado total`, na.rm = TRUE),
    vacb_m_i_t = sum(`A131A Valor agregado censal bruto (millones de pesos)`, na.rm = TRUE),
    ingresos_m_i_t = sum(`M000A Total de ingresos por suministro de bienes y servicios (millones de pesos)`, na.rm = TRUE),
    va_percap_m_i_t = sum(`A204A Valor agregado en promedio por persona ocupada (Pesos)`, na.rm = TRUE),
    ingresos_percap_m_i_t = sum(`A436A Ingresos por suministro de bienes y servicios por persona ocupada (Pesos)`, na.rm = TRUE)

    )
#####

#Aquí tengo dos bases metro: 1.- metro_sectores_año y 2.- totales_metro_año----

#Unir las dos datas
metro_anio_sector_totales <- merge(metro_sectores_año, totales_metro_año,
                      by.x = c("Año Censal", "NOM_ZM"),
                      by.y = c("Año Censal", "NOM_ZM"),
                      all = TRUE)
#Agregar tamaño nacional del sector en el año a la base "ce_nac"----

#Unir las dos datas
metro_anio_sector_totales_secnac <- merge(metro_anio_sector_totales,ce_nac ,
                                   by.x = c("Año Censal", "Actividad económica"),
                                   by.y = c("Año Censal", "Actividad económica"),
                                   all.x = TRUE)

# Extraer los primeros tres dígitos de la variable Subsector y crear la nueva variable cve_sector
metro_anio_sector_totales_secnac$cve_sector <- substr(metro_anio_sector_totales_secnac$`Actividad económica`, 11, 13)

#Ordenar las Variables
metro_anio_sector_totales_secnac<- metro_anio_sector_totales_secnac %>%
  select(`Año Censal`, NOM_ZM, cve_sector,`Actividad económica`, everything())

#Integrar la Intensidad del Conocimiento con el índice creado----
indice_pc1 <- read_excel( "tablas_texto/sectores_intensivos_cluster.xlsx")

# Extraer los primeros tres dígitos de la variable Subsector y crear la nueva variable cve_sector
indice_pc1$cve_sector <- substr(indice_pc1$Subsector, 1, 3)

data_anio_metro_sector_intensidad_k <- merge(metro_anio_sector_totales_secnac, indice_pc1 ,
                                          by.x = c("cve_sector"),
                                          by.y = c("cve_sector"),
                                          all.x = TRUE)

#Eliminar de la base metropolitana a los Subsectores que no forman parte del estudio, y que fueron descartados desde la elaboración del índice por no ser del tipo "establecimiento" o por carecer de datos en alguna de las bases utilizadas.
data_anio_metro_sector_intensidad_k_filtrado <- data_anio_metro_sector_intensidad_k %>%
  filter(!is.na(PC1))


#Seleccionar variables de interés
data_metropoli_sectores_intensidad<-data_anio_metro_sector_intensidad_k_filtrado %>% select(
  `Año Censal`, NOM_ZM, cve_sector, `Actividad económica`,
  ue_m_i_t, pot_m_i_t, vacb_m_i_t, ingresos_m_i_t, va_percap_m_i_t, ingresos_percap_m_i_t,
  ue_m_t, pot_m_t, vacb_m_t, ingresos_m_t, va_percap_m_t, ingresos_pecap_m_t,
  ue_nac_subsector_t, pot_nac_subsector_t, vacb_nac_subsector_t, ingresos_nac_subsector_t, vapercap_nac_subsector_t, ingresos_percap_nac_subsector_t,
  PC1, Indice_Normalizado, top_cluster,Media_Indice_Cluster)
                                                                  #Terminada la Data Metropolitana por año, por metrópoli, por subsector con intensidad del conocimiento----

#Guardar la base de datos----
write_csv(data_metropoli_sectores_intensidad, "datos/data_metropoli_sectores_intensidad.csv")


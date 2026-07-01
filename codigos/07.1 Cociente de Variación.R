# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: Evaluación de las Trayectorias Metropolitanas: Dimensión de Crecimiento y Función de CVIICAP
# ==============================================================================

#Limpiar R
rm(list=ls (all=T))
### Paquetes  ----
library(pacman)
p_load(haven,      # Paquete para importar archivos de Stata, SAS y SPSS
       readxl,     # Paquete para importar archivos de Excel
       tidyverse, #Conjunto de paquetes para manipulación, transformación, visualización y análisis de datos
       openxlsx, #Crea, edita y exporta archivos Excel con control de formatos y estilos.
       purrr, #Programación funcional: iteraciones elegantes con map(), evitando loops.
       dplyr, #Manipulación de datos con verbos como filter(), mutate(), summarise(), group_by()
       stringr, #Manejo consistente y limpio de cadenas de texto.
       pheatmap, #Genera heatmaps personalizables con anotaciones y escalas de color
       RColorBrewer #Paletas de colores cualitativas, secuenciales y divergentes para gráficos
       )


### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen = 999) # Prevenir notación científica

#Cargar la Data----
metro_data<-read_csv("datos/data_metropoli_sectores_intensidad.csv", locale = locale(encoding = "latin1"))
metro_data<- metro_data %>%
  rename(`Año Censal`=`AÃ±o Censal`)
metro_data<- metro_data %>%
  rename(`Actividad económica`=`Actividad econÃ³mica`)
#Actualizar Valores Monetarios de 2003 a Precios de 2018----
metro_EC_2018 <- metro_data %>%
  filter(`Año Censal` %in% c("2018"))
metro_EC_2003 <- metro_data %>%
  filter(`Año Censal` %in% c("2003"))

indice_de_precios<-read_csv("datos/indice_de_precios_base2018.csv", locale = locale(encoding = "latin1"))

indice_de_precios_listo <- indice_de_precios %>%
  # 1. Seleccionar, renombrar y filtrar NA en el IPI de 2003
  select(Concepto, IPI_2003 = `2003`) %>%
  filter(!is.na(IPI_2003)) %>%
  # 2. Crear la clave del subsector (cve_sector) de 3 dígitos
  mutate(
    Concepto_5d = substr(Concepto, 1, 5),
    # Extraer el código de 3 dígitos del SCIAN (el 'as.double' puede ser 'as.character' si no es necesario operar)
    cve_sector = as.double(substr(Concepto_5d, 1, 3))
  ) %>%
  # 3. Filtrar solo los registros que son subsectores de 3 dígitos (Ej. "115 -")
  filter(grepl("^[0-9]{3} -$", Concepto_5d)) %>%
  # 4. Seleccionar las columnas finales requeridas
  select(cve_sector, IPI_2003)

# Definir constantes para la imputación
#De acuerdo con las Notas Metodológicas del INEGI: i) El código y la denominación ´430 Comercio al por mayor´ se ha creado para concentrar todos los subsectores contemplados en el SCIAN 2018 dedicados a las actividades de distribución de bienes al por mayor (Con un IPI_2003 de 44.8) ii) El código y la denominación ´460 Comercio al por menor´ se ha creado para concentrar todos los subsectores contemplados en el SCIAN 2018 dedicados a las actividades de distribución de bienes al por menor. (Con un IPI_2003 de 43.3)
#Instituto Nacional de Estadística y Geografía [INEGI]. (s.f.). Cuentas de Bienes y Servicios (detallada). Año base 2018. Cuentas de producción, por actividad económica de origen/ Valor agregado bruto en valores básicos. Recuperado el 7 de agosto de 2025, de https://www.inegi.org.mx/app/tabulados/default.aspx?pr=1&vr=4&in=31&tp=20&wr=1&cno=1&idrt=3247&opc=p
# Definir los valores de imputación según la nota metodológica
IPI_MAYORISTA <- 44.8
IPI_MINORISTA <- 43.3

#Unir Base de Datos Metropolitanos de 2003 con el IPI y Deflactar
metro_EC_2003_deflactado <- metro_EC_2003 %>%
  # 1. Unir el IPI_2003 específico del subsector
  left_join(indice_de_precios_listo, by = "cve_sector") %>%
  # 2. Imputación Metodológica de Comercio (43# y 46#)
  mutate(
    Sector_2D = substr(cve_sector, 1, 2), # Clave para imputación
    IPI_2003_Corregido = case_when(
      # Caso 1: Imputar Mayorista (43) si IPI es NA
      is.na(IPI_2003) & Sector_2D == "43" ~ IPI_MAYORISTA,
      # Caso 2: Imputar Minorista (46) si IPI es NA
      is.na(IPI_2003) & Sector_2D == "46" ~ IPI_MINORISTA,
      # Caso 3: Mantener el IPI existente (o el NA si no es comercio)
      TRUE ~ IPI_2003
    )
  ) %>%
  # 3. Cálculo del Factor de Deflación y Aplicación a las Variables Monetarias
  mutate(
    Factor_Deflaccion = 100 / IPI_2003_Corregido,
    # Deflactar (pasar a Pesos Constantes de 2018)
    vacb_m_i_t = vacb_m_i_t * Factor_Deflaccion,
    ingresos_m_i_t = ingresos_m_i_t * Factor_Deflaccion
  ) %>%
  # 4. Seleccionar las columnas finales (Eliminar variables auxiliares)
  select(-IPI_2003, -IPI_2003_Corregido, -Factor_Deflaccion, -Sector_2D)
# Unir las dos bases de datos una encima de la otra
metro_EC <- bind_rows(
  metro_EC_2003_deflactado,
  metro_EC_2018
)
#Eliminación de variables no necesarias
metro_EC<-metro_EC %>% select( -va_percap_m_i_t, -ingresos_percap_m_i_t, -va_percap_m_t, -ingresos_pecap_m_t, -vapercap_nac_subsector_t, -ingresos_percap_nac_subsector_t )

#Guardar Data Economía del Conocimiento por Metrópoli ya Deflactada
write.xlsx(metro_EC,
           file = "tablas_texto/metro_EC_deflac.xlsx",
           rowNames = TRUE)

#Evaluar la presencia de valores negativos en vacb y en ingresos----
clusters_sectores<- c(1, 2, 3, 4)
colnames(metro_EC)
# Diagnóstico de valores negativos
# 1. Identificar todas las variables de interés (contienen 'vacb' o 'ingreso')
vars_diagnostico <- grep("vacb|ingreso", colnames(metro_EC), value = TRUE, ignore.case = TRUE)

# 2. Diagnóstico a nivel de registro (Subsectores del Conocimiento)
detalle_negativos <- metro_EC %>%
  filter(top_cluster %in% clusters_sectores) %>%
  # Creamos una columna que cuente cuántas de las variables seleccionadas son negativas
  mutate(n_negativos = rowSums(across(all_of(vars_diagnostico), ~ . < 0), na.rm = TRUE)) %>%
  filter(n_negativos > 0) %>%
  select(`Año Censal`, NOM_ZM, cve_sector, `Actividad económica`, all_of(vars_diagnostico), top_cluster)

####
# 1. Filtrar solo las metrópolis que identificamos con algún negativo en VACB
metros_en_riesgo <- unique(detalle_negativos$NOM_ZM)
print(metros_en_riesgo)

# 2. Calcular la sumatoria agregada ponderada para verificar signos finales
verificacion_cviicap <- metro_EC %>%
  filter(NOM_ZM %in% metros_en_riesgo, top_cluster %in% clusters_sectores) %>%
  mutate(var_metro_ponderada = vacb_m_i_t * Media_Indice_Cluster) %>%
  group_by(`Año Censal`, NOM_ZM) %>%
  summarise(
    EC_AyP_m_t = sum(var_metro_ponderada, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Pivotar para comparar t0 vs t1 por metrópoli
  pivot_wider(names_from = `Año Censal`, values_from = EC_AyP_m_t, names_prefix = "VACB_Sum_")

# 3. Detectar si alguna sumatoria total es negativa (lo que invalidaría el CVIICAP)
print(verificacion_cviicap)
#Tres casos. Se toma la decisión de colocar NA en esos casos cuando la EC Agregada y Ponderada en cualquier año sea negativa.

#EC AyP Bruta por Metrópoli y año----
#EC_AyP con POT y Todos los subsectores

EC_AyP_Bruta_pot <- metro_EC %>%
  filter(top_cluster %in% clusters_sectores) %>%
  mutate(var_metro_ponderada = pot_m_i_t * Media_Indice_Cluster) %>%
  group_by(`Año Censal`, NOM_ZM) %>%
  summarise(
    EC_AyP_m_t = sum(var_metro_ponderada, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Pivotar para comparar t0 vs t1 por metrópoli
  pivot_wider(names_from = `Año Censal`, values_from = EC_AyP_m_t, names_prefix = "POT_Sum_")

#EC Agregada y Ponderada por Año Ordenada por 2003
EC_AyP_Bruta_pot<-EC_AyP_Bruta_pot  %>% arrange(desc(POT_Sum_2003))
colnames(EC_AyP_Bruta_pot)

EC_AyP_Bruta_pot <- EC_AyP_Bruta_pot %>%
  mutate(
    Porcentaje_2003 = POT_Sum_2003 / sum(POT_Sum_2003, na.rm = TRUE) * 100,
    Porcentaje_2018 = POT_Sum_2018 / sum(POT_Sum_2018, na.rm = TRUE) * 100
  )%>% select(NOM_ZM, POT_Sum_2003, Porcentaje_2003, POT_Sum_2018, Porcentaje_2018)

EC_AyP_Bruta_pot <- EC_AyP_Bruta_pot %>%
  mutate(
    POT_Sum_2003 = round(POT_Sum_2003, 2),
    POT_Sum_2018 = round(POT_Sum_2018, 2),
    Porcentaje_2003 = round(Porcentaje_2003, 2),
    Porcentaje_2018 = round(Porcentaje_2018, 2)
  )

top_20_EC_AyP_Bruta_pot <- EC_AyP_Bruta_pot %>%
  slice(1:20)

down_20_EC_AyP_Bruta_pot <- EC_AyP_Bruta_pot %>%
  tail(20)

#Guardar Data Tamaño Absoluto y Relativo de la Economía del Conocimiento por Metrópoli con POT
write.xlsx(top_20_EC_AyP_Bruta_pot,
           file = "tablas_texto/top_20_EC_AyP_Bruta_pot.xlsx",
           rowNames = TRUE)

write.xlsx(down_20_EC_AyP_Bruta_pot,
           file = "tablas_texto/down_20_EC_AyP_Bruta_pot.xlsx",
           rowNames = TRUE)
#----

##### Cociente de Variación de Industrias Intensivas en el Conocimiento Agregadas Ponderadas----

#Fórmula General:
#Filtrar: Sólo Industrias Intensivas en el Conocimiento
#CVIICAP: (Suma (var_t1*k)/Suma (Var_t0*k))*100



#Función CVIICAP----
calcular_CVIICAP <- function(
    df,
    clusters_sectores,
    var_a_ponderar,
    t0_year = "2003",
    t1_year = "2018"
) {
  # Verificar existencia de la variable
  var_a_ponderar_sym <- rlang::enquo(var_a_ponderar)
  if (quo_name(var_a_ponderar_sym) %in% names(df) == FALSE) {
    stop(paste("La variable", quo_name(var_a_ponderar_sym), "no se encuentra en el dataframe."))
  }

  # --- 1. Ponderación y Filtrado ---
  df_ponderado <- df %>%
    filter(top_cluster %in% clusters_sectores) %>%
    mutate(
      var_metro_ponderada = {{var_a_ponderar}} * Media_Indice_Cluster
    )

  # --- 2. Agregación a Nivel Metropolitano ---
  CV_metro_EC <- df_ponderado %>%
    group_by(`Año Censal`, NOM_ZM) %>%
    summarise(
      EC_AyP_m_t = sum(var_metro_ponderada, na.rm = TRUE),
      .groups = 'drop'
    )

  # --- 3. Separación de Años y Unión Horizontal ---
  CV_metro_EC_t0 <- CV_metro_EC %>% filter(`Año Censal` == t0_year) %>% select(NOM_ZM, EC_AyP_m_t)
  CV_metro_EC_t1 <- CV_metro_EC %>% filter(`Año Censal` == t1_year) %>% select(NOM_ZM, EC_AyP_m_t)

  CV_final <- full_join(
    x = CV_metro_EC_t0,
    y = CV_metro_EC_t1,
    by = "NOM_ZM",
    suffix = c(paste0("_", t0_year), paste0("_", t1_year))
  )

  # --- 4. Cálculo del CVIICAP con Validación de Signos ---
  T0_col <- paste0("EC_AyP_m_t_", t0_year)
  T1_col <- paste0("EC_AyP_m_t_", t1_year)

  CV_final <- CV_final %>%
    mutate(
      # Discernimiento: Si cualquiera de los años es negativo o cero, el índice es NA
      # Esto previene crecimientos ficticios o errores de signo
      CVIICAP_m = ifelse(!!sym(T0_col) <= 0 | !!sym(T1_col) <= 0,
                         NA,
                         round(((!!sym(T1_col) / !!sym(T0_col)) * 100), digits = 2))
    ) %>%
    select(NOM_ZM, CVIICAP_m) %>%
    arrange(CVIICAP_m)

  return(CV_final)
}


#APLICACIÓN DE LA FUNCIÓN----
#Cluster a Considerar del 1 al 4. Puede ser dos cluster o un grupo de tres o cuatro clusters.
#Las posibles variables a ponderar "var_a_ponderar" son: "ue_m_i_t", "pot_m_i_t", "vacb_m_i_t" y "ingresos_m_i_t",

#Variable ue_m_i_t y Todos Clusters----
CV_ue_c1234 <- calcular_CVIICAP(
  df = metro_EC,
  clusters_sectores = c("1", "2", "3", "4"),
  var_a_ponderar = ue_m_i_t
)
#UE C123
CV_ue_c123 <- calcular_CVIICAP(
  df = metro_EC,
  clusters_sectores = c("1", "2", "3"),
  var_a_ponderar = ue_m_i_t
)
#UE C12
CV_ue_c12 <- calcular_CVIICAP(
  df = metro_EC,
  clusters_sectores = c("1", "2"),
  var_a_ponderar = ue_m_i_t
)

# 1. Base CV_ue_c1234
CV_ue_c1234 <- CV_ue_c1234 %>%
  mutate(CV_ue_c1234 = paste0(NOM_ZM, ": ", CVIICAP_m))
# 2. Base CV_ue_c123
CV_ue_c123 <- CV_ue_c123 %>%
  mutate(CV_ue_c123 = paste0(NOM_ZM, ": ", CVIICAP_m))
# 3. Base CV_ue_c12
CV_ue_c12 <- CV_ue_c12 %>%
  mutate(CV_ue_c12 = paste0(NOM_ZM, ": ", CVIICAP_m))
# Paso A: Unir la Base 1 y la Base 2
base_parcial <- CV_ue_c1234 %>%
  full_join(CV_ue_c123, by = "NOM_ZM")
# Paso B: Unir la Base Parcial con la Base 3 para obtener el resultado final
CV_ue_clusters <- base_parcial %>%
  full_join(CV_ue_c12, by = "NOM_ZM")

write.xlsx(CV_ue_clusters,
           file = "tablas_texto/CV_ue_clusters.xlsx",
           rowNames = T)

#Variable POT y Todos Clusters----
CV_pot_c1234 <- calcular_CVIICAP(
  df = metro_EC,
  clusters_sectores = c("1", "2", "3", "4"),
  var_a_ponderar = pot_m_i_t
)
#POT C123
CV_pot_c123 <- calcular_CVIICAP(
  df = metro_EC,
  clusters_sectores = c("1", "2", "3"),
  var_a_ponderar = pot_m_i_t
)
#POT C12
CV_pot_c12 <- calcular_CVIICAP(
  df = metro_EC,
  clusters_sectores = c("1", "2"),
  var_a_ponderar = pot_m_i_t
)
# 1. Base CV_pot_c1234
CV_pot_c1234 <- CV_pot_c1234 %>%
  mutate(CV_pot_c1234 = paste0(NOM_ZM, ": ", CVIICAP_m))
# 2. Base CV_pot_c123
CV_pot_c123 <- CV_pot_c123 %>%
  mutate(CV_pot_c123 = paste0(NOM_ZM, ": ", CVIICAP_m))
# 3. Base CV_pot_c12
CV_pot_c12 <- CV_pot_c12 %>%
  mutate(CV_pot_c12 = paste0(NOM_ZM, ": ", CVIICAP_m))
# --- 2. Integración: Unir Bases de Datos ---
# Paso A: Unir la Base 1 y la Base 2 usando NOM_ZM
base_parcial_pot <- CV_pot_c1234 %>%
  full_join(CV_pot_c123, by = "NOM_ZM")
# Paso B: Unir la Base Parcial con la Base 3 para obtener el resultado final
CV_pot_clusters <- base_parcial_pot %>%
  full_join(CV_pot_c12, by = "NOM_ZM")

# --- 3. Salida de Datos (Exportar a Excel) ---
write.xlsx(CV_pot_clusters,
             file = "tablas_texto/CV_pot_clusters.xlsx",
             rowNames = TRUE)

#Variable vacb_m_i_t y Todos Clusters----
CV_vacb_c1234 <- calcular_CVIICAP(
  df = metro_EC,
  clusters_sectores = c("1", "2", "3", "4"),
  var_a_ponderar = vacb_m_i_t # La variable se pasa sin comillas
)
#VACB C123
CV_vacb_c123 <- calcular_CVIICAP(
  df = metro_EC,
  clusters_sectores = c("1", "2", "3"),
  var_a_ponderar = vacb_m_i_t # La variable se pasa sin comillas
)
#VACB C12
CV_vacb_c12 <- calcular_CVIICAP(
  df = metro_EC,
  clusters_sectores = c("1", "2"),
  var_a_ponderar = vacb_m_i_t # La variable se pasa sin comillas
)
# 1. Base CV_vacb_c1234
CV_vacb_c1234 <- CV_vacb_c1234 %>%
  mutate(CV_vacb_c1234 = paste0(NOM_ZM, ": ", CVIICAP_m))
# 2. Base CV_vacb_c123
CV_vacb_c123 <- CV_vacb_c123 %>%
  mutate(CV_vacb_c123 = paste0(NOM_ZM, ": ", CVIICAP_m))
# 3. Base CV_vacb_c12
CV_vacb_c12 <- CV_vacb_c12 %>%
  mutate(CV_vacb_c12 = paste0(NOM_ZM, ": ", CVIICAP_m))

# Paso A: Unir la Base 1 y la Base 2 usando NOM_ZM
base_parcial_vacb <- CV_vacb_c1234 %>%
  full_join(CV_vacb_c123, by = "NOM_ZM")
# Paso B: Unir la Base Parcial con la Base 3 para obtener el resultado final
CV_vacb_clusters <- base_parcial_vacb %>%
  full_join(CV_vacb_c12, by = "NOM_ZM")

write.xlsx(CV_vacb_clusters,
             file = "tablas_texto/CV_vacb_clusters.xlsx",
             rowNames = TRUE)

#Variable ingresos_m_i_t y Todos Clusters----
CV_ingresos_c1234 <- calcular_CVIICAP(
  df = metro_EC,
  clusters_sectores = c("1", "2", "3", "4"),
  var_a_ponderar = ingresos_m_i_t
)
#Ingresos C123
CV_ingresos_c123 <- calcular_CVIICAP(
  df = metro_EC,
  clusters_sectores = c("1", "2", "3"),
  var_a_ponderar = ingresos_m_i_t
)
#Ingresos C12
CV_ingresos_c12 <- calcular_CVIICAP(
  df = metro_EC,
  clusters_sectores = c("1", "2"),
  var_a_ponderar = ingresos_m_i_t
)

# 1. Base CV_ingresos_c1234
CV_ingresos_c1234 <- CV_ingresos_c1234 %>%
  mutate(CV_ingresos_c1234 = paste0(NOM_ZM, ": ", CVIICAP_m))

# 2. Base CV_ingresos_c123
CV_ingresos_c123 <- CV_ingresos_c123 %>%
  mutate(CV_ingresos_c123 = paste0(NOM_ZM, ": ", CVIICAP_m))

# 3. Base CV_ingresos_c12
CV_ingresos_c12 <- CV_ingresos_c12 %>%
  mutate(CV_ingresos_c12 = paste0(NOM_ZM, ": ", CVIICAP_m))

# Paso A: Unir la Base 1 y la Base 2 usando NOM_ZM
base_parcial_ingresos <- CV_ingresos_c1234 %>%
  full_join(CV_ingresos_c123, by = "NOM_ZM")
# Paso B: Unir la Base Parcial con la Base 3 para obtener el resultado final
CV_ingresos_clusters <- base_parcial_ingresos %>%
  full_join(CV_ingresos_c12, by = "NOM_ZM")

write.xlsx(CV_ingresos_clusters,
             file = "tablas_texto/CV_ingresos_clusters.xlsx",
             rowNames = TRUE)

#COMPLETA CV----
# 1. Cree una lista con todos los data frames unificados previamente
lista_maestra_a_unir <- list(
  CV_ue_clusters,
  CV_pot_clusters,
  CV_vacb_clusters,
  CV_ingresos_clusters
)
# 2. Unir todos los data frames secuencialmente usando full_join y la llave "NOM_ZM"
base_maestra_clusters <- lista_maestra_a_unir %>%
  reduce(full_join, by = "NOM_ZM")

write.xlsx(base_maestra_clusters,
           file = "tablas_texto/base_maestra_clusters.xlsx",
           rowNames = TRUE)
colnames(base_maestra_clusters)

#Corregir nombres de variables
# 1. Obtener los nombres de columna actuales
nombres_actuales <- colnames(base_maestra_clusters)
# 2. Identificar las posiciones que contienen los nombres que queremos usar como base
# Estos son las posiciones impares (3, 5, 7, 9, etc.) a partir de la tercera columna (índice 3).
# n_pares contiene los nombres 'CV_ue_c1234', 'CV_ue_c123', etc.
nombres_base <- nombres_actuales[seq(3, length(nombres_actuales), by = 2)]
# 3. Construir los nuevos nombres:
# Nombres para las columnas numéricas (posición 2, 4, 6, etc.): CV_ue_c1234, CV_ue_c123, etc.
nuevos_nombres_num <- nombres_base
# Nombres para las columnas de texto (posición 3, 5, 7, etc.): tx_CV_ue_c1234, tx_CV_ue_c123, etc.
nuevos_nombres_tx <- paste0("tx_", nombres_base)
# 4. Asignar los nuevos nombres a las posiciones correctas:
# Asignar a las posiciones pares (2, 4, 6, etc.)
nombres_actuales[seq(2, length(nombres_actuales), by = 2)] <- nuevos_nombres_num
# Asignar a las posiciones impares (3, 5, 7, etc.)
nombres_actuales[seq(3, length(nombres_actuales), by = 2)] <- nuevos_nombres_tx
# 5. Aplicar los nombres corregidos a la base de datos
colnames(base_maestra_clusters) <- nombres_actuales

colnames(base_maestra_clusters)
#Mapa de Calor----

base_heatmap <- base_maestra_clusters %>%
  select(NOM_ZM, starts_with("CV_") ) %>%
  arrange(CV_ue_c1234)%>%
  mutate(
    # 1. Selecciona todas las columnas que inician con "CV_"
    # 2. Aplica la condición '< 100'. Esto crea una matriz lógica (TRUE/FALSE).
    # 3. rowSums() suma las filas de esta matriz. En R, TRUE se evalúa como 1 y FALSE como 0.
    Contracción = rowSums(select(., starts_with("CV_")) < 100, na.rm = TRUE),
    Crecimiento = rowSums(select(., starts_with("CV_")) > 100, na.rm = TRUE),
    Clasificación = rank(CV_ue_c1234, ties.method = "first")
  ) %>% select(Clasificación, NOM_ZM, Crecimiento, Contracción, everything() )

write.xlsx(base_heatmap,
           file = "tablas_texto/base_heatmap.xlsx",
           rowNames = TRUE)

#Elaborar Mapa de Calor para Texto----
str(base_heatmap)
library(tidyverse)
library(openxlsx)

exportar_cv_excel <- function(base_heatmap, archivo = "tablas_texto/base_heatmap_coloreada.xlsx") {

  # Seleccionar solo columnas CV
  columnas_cv <- base_heatmap %>%
    select(starts_with("CV_")) %>%
    as.matrix()

  # Normalizar cada columna (0-1)
  columnas_cv_norm <- apply(columnas_cv, 2, function(x) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  })

  # Dataframe exportado
  df_export <- base_heatmap %>%
    select(Clasificación, NOM_ZM, Crecimiento, Contracción, starts_with("CV_"))

  # Workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Datos")
  writeData(wb, "Datos", df_export)

  # Índices de columnas CV
  col_indices <- grep("^CV_", names(df_export))

  # Aplicar colores
  for (i in seq_along(col_indices)) {
    col <- col_indices[i]
    for (row in 1:nrow(columnas_cv_norm)) {
      valor_norm <- columnas_cv_norm[row, i]

      if (!is.na(valor_norm)) {

        if (valor_norm <= 0.5) {
          # Rojo pastel → blanco
          prop <- valor_norm * 2
          r <- 255
          g <- round(200 + 55 * prop)
          b <- round(200 + 55 * prop)

        } else {
          # Blanco → azul pastel
          prop <- (valor_norm - 0.5) * 2
          r <- round(255 - 90 * prop)
          g <- round(255 - 50 * prop)
          b <- 255
        }

        color_hex <- sprintf("#%02X%02X%02X", r, g, b)
        style <- createStyle(fgFill = color_hex)
        addStyle(wb, "Datos", style, rows = row + 1, cols = col)
      }
    }
  }

  # Guardar con sobrescritura
  saveWorkbook(wb, archivo, overwrite = TRUE)
  cat("Archivo guardado:", archivo, "\n")
}

# Generar Mapa de Calor
exportar_cv_excel(base_heatmap)


#Contar las veces que hubo contracción----
# 1. Identificar las columnas que inician con "CV_"
columnas_cv <- colnames(base_maestra_clusters)[startsWith(colnames(base_maestra_clusters), "CV_")]

# 2. Iterar sobre estas columnas y crear la nueva variable de clasificación
base_maestra_clusters <- base_maestra_clusters %>%
  # Usamos 'mutate' para crear nuevas columnas
  mutate(across(.cols = all_of(columnas_cv),
                # Para cada columna CV_XXX:
                # 3. Calcular el ranking (min_rank)
                #    Aplicamos a -variable para que el valor más ALTO obtenga el RANK 1
                .fns = list(clasificacion = ~ min_rank(.)),
                # 4. El nombre de la nueva columna será "clasificacion_CV_XXX"
                .names = "clasificacion_{.col}"))

CV_contracion <- base_maestra_clusters %>%
  filter(if_any(starts_with("CV_"), ~ . < 100))

# Agregar la variable 'contracciones' al dataframe 'CV_contracion'
CV_contracion <- CV_contracion %>%
  mutate(
    # 1. Selecciona todas las columnas que inician con "CV_"
    # 2. Aplica la condición '< 100'. Esto crea una matriz lógica (TRUE/FALSE).
    # 3. rowSums() suma las filas de esta matriz. En R, TRUE se evalúa como 1 y FALSE como 0.
    contracciones = rowSums(select(., starts_with("CV_")) < 100)
  )

library(dplyr)

# Definimos el orden deseado para las variables base y sus niveles de agregación
# El patrón es: clasificacion_CV_XXX, CV_XXX, tx_CV_XXX
orden_deseado <- c(
  # Bloque 1: Unidades Económicas (UE)
  "clasificacion_CV_ue_c1234", "CV_ue_c1234", "tx_CV_ue_c1234",
  "clasificacion_CV_ue_c123", "CV_ue_c123", "tx_CV_ue_c123",
  "clasificacion_CV_ue_c12", "CV_ue_c12", "tx_CV_ue_c12",

  # Bloque 2: Personal Ocupado Total (POT)
  "clasificacion_CV_pot_c1234", "CV_pot_c1234", "tx_CV_pot_c1234",
  "clasificacion_CV_pot_c123", "CV_pot_c123", "tx_CV_pot_c123",
  "clasificacion_CV_pot_c12", "CV_pot_c12", "tx_CV_pot_c12",

  # Bloque 3: Valor Agregado Censal Bruto (VACB)
  "clasificacion_CV_vacb_c1234", "CV_vacb_c1234", "tx_CV_vacb_c1234",
  "clasificacion_CV_vacb_c123", "CV_vacb_c123", "tx_CV_vacb_c123",
  "clasificacion_CV_vacb_c12", "CV_vacb_c12", "tx_CV_vacb_c12",

  # Bloque 4: Ingresos
  "clasificacion_CV_ingresos_c1234", "CV_ingresos_c1234", "tx_CV_ingresos_c1234",
  "clasificacion_CV_ingresos_c123", "CV_ingresos_c123", "tx_CV_ingresos_c123",
  "clasificacion_CV_ingresos_c12", "CV_ingresos_c12", "tx_CV_ingresos_c12"
)

# Aplicar el reordenamiento
CV_contracion <- CV_contracion %>%
  select(
    # 1. Variables principales (al inicio)
    NOM_ZM,
    contracciones,

    # 2. Las 36 variables de Clasificación, Valor y Descripción (en el orden lógico)
    all_of(orden_deseado)
      )

CV_contracion <- CV_contracion %>% arrange(-contracciones)

write.xlsx(CV_contracion,
           file = "tablas_texto/CV_contracion.xlsx",
           rowNames = TRUE)

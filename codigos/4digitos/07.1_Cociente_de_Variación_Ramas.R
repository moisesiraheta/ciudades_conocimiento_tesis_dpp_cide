# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: Evaluación de las Trayectorias Metropolitanas: Dimensión de Crecimiento y Función de CVIICAP Datos Ramas
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
metro_data<-read_csv("datos/4digitos/data_metropoli_sectores_intensidad_4digitos_Ramas.csv", locale = locale(encoding = "latin1"))
metro_data<- metro_data %>%
  rename(`Año Censal`=`AÃ±o Censal`)
metro_data<- metro_data %>%
  rename(`Actividad económica`=`Actividad econÃ³mica`)
metro_data$cve_sector<- as.character(metro_data$cve_sector)
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
  # 2. Crear la clave de la Rama (cve_sector) de 4 dígitos
  mutate(
    Concepto_6d = substr(Concepto, 1, 6),
    # Extraer el código de 4 dígitos del SCIAN (el 'as.character' porque es texto no número)
    cve_sector = as.character(substr(Concepto_6d, 1, 4))
  ) %>%
  # 3. Filtrar solo los registros que son Ramas de 4 dígitos (Ej. "3251 -")
  filter(grepl("^[0-9]{4} -$", Concepto_6d)) %>%
  # 4. Seleccionar las columnas finales requeridas
  select(cve_sector, IPI_2003)


#De acuerdo con las Notas Metodológicas del INEGI: i) El código y la denominación ´4300Comercio al por mayor´ se ha creado para concentrar todas las ramas contempladas en el SCIAN 2018 dedicadas a las actividades de distribución de bienes al por mayor (Con un IPI_2003 de 44.8). ii) El código y la denominación ´4600Comercio al por mayor´ se ha creado para concentrar todas las ramas contempladas en el SCIAN 2018 dedicadas a las actividades de distribución de bienes al por mayor. (Con un IPI_2003 de 43.3). Adicionalmente, la Rama  4922 - Servicios de mensajería y paquetería local carece de valor de Índice de Precios Implícitos, sin embargo, no afecta en el análisis pues no pertenece al grupo de Ramas-Industrias Manufactureras, por lo cual no se realiza mayor operación al respecto.
#Instituto Nacional de Estadística y Geografía [INEGI]. (s.f.). Cuentas de Bienes y Servicios (detallada). Año base 2018. Cuentas de producción, por actividad económica de origen/ Valor agregado bruto en valores básicos. Recuperado el 7 de agosto de 2025, de https://www.inegi.org.mx/app/tabulados/default.aspx?pr=1&vr=4&in=31&tp=20&wr=1&cno=1&idrt=3247&opc=p
# Definir los valores de imputación según la nota metodológica
IPI_MAYORISTA <- 44.8
IPI_MINORISTA <- 43.3

#Unir Base de Datos Metropolitanos de 2003 con el IPI y Deflactar
metro_EC_2003_deflactado <- metro_EC_2003 %>%
  # 1. Unir el IPI_2003 específico de la Rama
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

#Seleccionar Ramás-Industrias Manufactureras. Es decir, las Ramas que corresponden a los Grandes Sectores a 2 dígitos 31-33 Industrias manufactureras.----

metro_EC <- metro_EC %>%
  mutate(Dos_digitos = substr(cve_sector, 1, 2)) %>%
  filter(Dos_digitos %in% c("31", "32", "33")) %>%
  select(-Dos_digitos) # Borrar al final para no ensuciar la base
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

##### Cociente de Variación de Industrias Intensivas en el Conocimiento Agregadas Ponderadas----

#Fórmula General:
#Filtrar: Sólo Industrias Intensivas en el Conocimiento
#CVIICAP: (Suma (var_t1*k)/Suma (Var_t0*k))*100
colnames(metro_EC)
View(metro_EC)


#Función CVIICAP----
calcular_CVIICAP <- function(
    df,
    clusters_sectores,
    var_a_ponderar,
    t0_year = "2003",
    t1_year = "2018"
) {
  # Verificar si la variable a ponderar existe en el dataframe
  var_a_ponderar_sym <- rlang::enquo(var_a_ponderar)
  if (quo_name(var_a_ponderar_sym) %in% names(df) == FALSE) {
    stop(paste("La variable", quo_name(var_a_ponderar_sym), "no se encuentra en el dataframe."))
  }

  # --- 1. Ponderación y Filtrado ---
  # La variable se evalúa con {{var_a_ponderar}} para la evaluación no estándar (NSE)
  df_ponderado <- df %>%
    filter(top_cluster %in% clusters_sectores) %>%
    mutate(
      var_metro_ponderada = {{var_a_ponderar}} * Media_Indice_Cluster
    )

  # --- 2. Agregación a Nivel Metropolitano y Anual ---
  CV_metro_EC <- df_ponderado %>%
    group_by(`Año Censal`, NOM_ZM) %>%
    summarise(
      EC_AyP_m_t = sum(var_metro_ponderada, na.rm = TRUE),
      .groups = 'drop'
    )

  # --- 3. Separación de Años y Unión Horizontal (full_join) ---
  CV_metro_EC_t0 <- CV_metro_EC %>% filter(`Año Censal` == t0_year) %>% select(NOM_ZM, EC_AyP_m_t)
  CV_metro_EC_t1 <- CV_metro_EC %>% filter(`Año Censal` == t1_year) %>% select(NOM_ZM, EC_AyP_m_t)

  CV_final <- full_join(
    x = CV_metro_EC_t0,
    y = CV_metro_EC_t1,
    by = "NOM_ZM",
    suffix = c(paste0("_", t0_year), paste0("_", t1_year))
  )

  # --- 4. Cálculo del Cociente de Variación (CVIICAP) ---
  # CVIICAP = (T1 / T0) * 100
  T0_col <- paste0("EC_AyP_m_t_", t0_year)
  T1_col <- paste0("EC_AyP_m_t_", t1_year)

  CV_final <- CV_final %>%
    mutate(
      # Manejo algebraico, metodológico y de control de calidad por ceros, vacíos y negativos.
      CVIICAP_m = case_when(
        # --- Discernimiento de Negativos ---
        # Si cualquiera de los dos años presenta valores negativos agregados,
        # se asigna NA_real_ para evitar trayectorias o crecimientos espurios.
        !!sym(T0_col) < 0 | !!sym(T1_col) < 0 ~ NA_real_,

        # Caso A (0/0): Ausencia de total en ambos años por confidencialidad.
        # Uso de NA_real para no penalizar ni premiar por datos inexistentes.
        !!sym(T0_col) == 0 & !!sym(T1_col) == 0 ~ NA_real_,

        # Caso B (X/0): Emergencia desde el secreto estadístico (Matemáticamente: Crecimiento infinito).
        # Se "capea" en un índice de 1000 como proxy de crecimiento.
        !!sym(T0_col) == 0 & !!sym(T1_col) > 0 ~ 1000,

        # Caso C: Cálculo normal de la trayectoria (Valores estrictamente positivos)
        TRUE ~ round(((!!sym(T1_col) / !!sym(T0_col)) * 100), digits = 2)
      )
    ) %>%
    select(NOM_ZM, CVIICAP_m) %>%
    arrange(CVIICAP_m)

  return(CV_final)
}


#APLICACIÓN DE LA FUNCIÓN----
#Clúster a Considerar del 1 al 4. Puede ser un clúster o un grupo de clústeres.
#Las posibles variables a ponderar "var_a_ponderar" son: "ue_m_i_t", "pot_m_i_t", "vacb_m_i_t" y "ingresos_m_i_t".

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

View(CV_ue_clusters)

write.xlsx(CV_ue_clusters,
           file = "tablas_texto/4_digitos/Ramas_CV_ue_clusters.xlsx",
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
           file = "tablas_texto/4_digitos/Ramas_CV_pot_clusters.xlsx",
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
           file = "tablas_texto/4_digitos/Ramas_CV_vacb_clusters.xlsx",
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
           file = "tablas_texto/4_digitos/Ramas_CV_ingresos_clusters.xlsx",
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
           file = "tablas_texto/4_digitos/Ramas_base_maestra_clusters.xlsx",
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
    Crecimiento = rowSums(select(., starts_with("CV_")) > 100, na.rm = TRUE),
    Contracción = rowSums(select(., starts_with("CV_")) < 100, na.rm = TRUE)
  ) %>% select(NOM_ZM, Crecimiento, Contracción, everything()) %>%
arrange(desc(Contracción))

write.xlsx(base_heatmap,
           file = "tablas_texto/4_digitos/Ramas_base_heatmap.xlsx",
           rowNames = TRUE)

#-----
#Elaboración de Mapa de Calor
library(tidyverse)
library(openxlsx)

exportar_cv_excel <- function(base_heatmap, archivo = "tablas_texto/4_digitos/Rama_base_heatmap_coloreada.xlsx") {

  # Dataframe exportado
  df_export <- base_heatmap %>%
    select(NOM_ZM,  Contracción, Crecimiento, starts_with("CV_"))

  # Extraer la matriz de valores reales para los cálculos de color
  columnas_cv <- df_export %>%
    select(starts_with("CV_")) %>%
    as.matrix()

  # Workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Datos")
  writeData(wb, "Datos", df_export)

  # Índices de columnas CV en el dataframe exportado
  col_indices <- grep("^CV_", names(df_export))

  # Aplicar colores iterando sobre cada columna de manera independiente
  for (i in seq_along(col_indices)) {
    col <- col_indices[i]

    # --- CAMBIO CLAVE: Mínimo y Máximo específicos de ESTA columna ---
    min_val <- min(columnas_cv[, i], na.rm = TRUE)
    max_val <- max(columnas_cv[, i], na.rm = TRUE)

    # Controles por si los datos de la columna no cruzan el umbral de 100
    if (min_val >= 100) min_val <- 0
    if (max_val <= 100) max_val <- 200

    for (row in 1:nrow(columnas_cv)) {
      valor_real <- columnas_cv[row, i]

      if (!is.na(valor_real)) {

        if (valor_real <= 100) {
          # --- ESCALA BLANCO A ROJO PASTEL (Por columna) ---
          # Proporción: 0 en el 100 (Blanco), 1 en el mínimo de la columna (Rojo Pastel)
          prop <- (100 - valor_real) / (100 - min_val)

          r <- 255
          g <- round(255 - 55 * prop) # Rango Verde: 255 a 200
          b <- round(255 - 55 * prop) # Rango Azul: 255 a 200

        } else {
          # --- ESCALA BLANCO A AZUL PASTEL (Por columna) ---
          # Proporción: 0 en el 100 (Blanco), 1 en el máximo de la columna (Azul Pastel)
          prop <- (valor_real - 100) / (max_val - 100)

          r <- round(255 - 55 * prop) # Rango Rojo: 255 a 200
          g <- round(255 - 25 * prop) # Rango Verde: 255 a 230
          b <- 255
        }

        # Generar HEX y aplicar estilo
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


#----

#Contar las veces que hubo contracción
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
    # 2. Aplica la condición '< 100'. Esto crea una matriz lógica (TRUE/FALSE/NA).
    # 3. rowSums() suma las filas. Agregamos na.rm = TRUE para ignorar los NAs.
    contracciones = rowSums(select(., starts_with("CV_")) < 100, na.rm = TRUE)
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

    # Nota: Si hubiera variables adicionales, se podría usar 'everything()'
    # después de 'all_of(orden_deseado)' para incluirlas al final.
  )

CV_contracion <- CV_contracion %>% arrange(-contracciones)

write.xlsx(CV_contracion,
           file = "tablas_texto/4_digitos/Ramas_CV_contracion.xlsx",
           rowNames = TRUE)


#Crecimiento----
CV_crecimiento <- base_maestra_clusters %>%
  filter(if_any(starts_with("CV_"), ~ . > 100))

# Agregar la variable 'crecimiento' al dataframe 'CV_crecimiento'
CV_crecimiento <- CV_crecimiento %>%
  mutate(
    # 1. Selecciona todas las columnas que inician con "CV_"
    # 2. Aplica la condición '> 100'. Esto crea una matriz lógica (TRUE/FALSE).
    # 3. rowSums() suma las filas de esta matriz. En R, TRUE se evalúa como 1 y FALSE como 0.
    crecimiento = rowSums(select(., starts_with("CV_")) > 100, na.rm = TRUE)
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
CV_crecimiento <- CV_crecimiento %>%
  select(
    # 1. Variables principales (al inicio)
    NOM_ZM,
    crecimiento,

    # 2. Las 36 variables de Clasificación, Valor y Descripción (en el orden lógico)
    all_of(orden_deseado)

    # Nota: Si hubiera variables adicionales, se podría usar 'everything()'
    # después de 'all_of(orden_deseado)' para incluirlas al final.
  )

CV_crecimiento <- CV_crecimiento %>% arrange(-crecimiento)

write.xlsx(CV_crecimiento,
           file = "tablas_texto/4_digitos/Ramas_CV_crecimiento.xlsx",
           rowNames = TRUE)

#Base Maestra Con Crecimiento y Contracción----
crecimiento_robusto<-base_maestra_clusters%>%
mutate(
  # 1. Selecciona todas las columnas que inician con "CV_"
  # 2. Aplica la condición '< 100'. Esto crea una matriz lógica (TRUE/FALSE/NA).
  # 3. rowSums() suma las filas. Agregamos na.rm = TRUE para ignorar los NAs.
  contracciones = rowSums(select(., starts_with("CV_")) < 100, na.rm = TRUE),
  crecimiento = rowSums(select(., starts_with("CV_")) > 100, na.rm = TRUE)
)

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
crecimiento_robusto <- crecimiento_robusto %>%
  select(
    # 1. Variables principales (al inicio)
    NOM_ZM,
    contracciones, crecimiento,

    # 2. Las 36 variables de Clasificación, Valor y Descripción (en el orden lógico)
    all_of(orden_deseado)

      )

crecimiento_robusto <- crecimiento_robusto %>% arrange(contracciones)

colnames(crecimiento_robusto)


write.xlsx(crecimiento_robusto,
           file = "tablas_texto/4_digitos/Ramas_CV_crecimiento_robusto.xlsx",
           rowNames = TRUE)

crecimiento_robusto_limpio <- crecimiento_robusto %>%
  select(
    NOM_ZM,
    contracciones,
    crecimiento,
    starts_with("CV_")
  ) %>% filter(contracciones == 0)

write.xlsx(crecimiento_robusto_limpio,
           file = "tablas_texto/4_digitos/Ramas_CV_crecimiento_robusto_limpio.xlsx",
           rowNames = TRUE)

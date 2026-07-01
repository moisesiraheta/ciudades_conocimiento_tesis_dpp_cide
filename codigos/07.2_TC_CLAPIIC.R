# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: Evaluación de la TCCLPIIC para las Zonas Metropolitanas.
# ==============================================================================
#Limpiar R
rm(list=ls (all=T))
### Paquetes  ----
library(pacman)
p_load(haven,      # Paquete para importar archivos de Stata, SAS y SPSS
       readxl,     # Paquete para importar archivos de Excel
       tidyverse,openxlsx)  # Metapaquete que incluye readr, paquete para importar achivos de texto plano

### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen = 999) # Prevenir notación científica

#Cargar la Data----
metro_data<-read_csv("datos/data_metropoli_sectores_intensidad.csv", locale = locale(encoding = "latin1"))

metro_data<- metro_data %>%
  rename(`Año Censal`=`AÃ±o Censal`)
metro_data<- metro_data %>%
  rename(`Actividad económica`=`Actividad econÃ³mica`)
#Filtrar: Sólo años 2018 y 2003.
metro_EC <- metro_data %>%
  filter(`Año Censal` %in% c("2018", "2003"))
colnames(metro_EC)

#Tamaño de la Economía a nivel Nacional en t----
#Recupero valores de las variables del total_nacional en el año t.
ce_mun<-read_csv("datos/SAIC_Exporta_2025518_83148141.csv")
total_nacional_t<-ce_mun %>%
  filter(`Actividad económica` == "Total nacional") %>%
  filter(`Año Censal` %in% c("2018", "2003")) %>%
  select(-Entidad, -Municipio, -`Actividad económica`)
#Renombrar Variables
total_nacional_t<-total_nacional_t %>%
  rename( ue_nac_t = `UE Unidades económicas` ) %>%
  rename( pot_nac_t = `H001A Personal ocupado total`) %>%
  rename( vacb_nac_t = `A131A Valor agregado censal bruto (millones de pesos)`)%>%
  rename( ingresos_nac_t=`M000A Total de ingresos por suministro de bienes y servicios (millones de pesos)`) %>%
  rename( vacb_percap_nac_t= `A204A Valor agregado en promedio por persona ocupada (Pesos)`) %>%
  rename(ingresos_percap_nac_t= `A436A Ingresos por suministro de bienes y servicios por persona ocupada (Pesos)`)

#Evaluación de Negativos para la ECAyP----
# 1. Definir variables financieras a evaluar
vars_financieras <- c("vacb_m_i_t", "vacb_m_t", "ingresos_m_i_t", "ingresos_m_t")

# 2. Identificar registros con valores negativos en cualquier componente del CLPIIC
diagnostico_clpiic <- metro_EC %>%
  filter(top_cluster %in% c(1, 2, 3, 4)) %>%
  mutate(negativos_detectados = rowSums(across(all_of(vars_financieras), ~ . < 0), na.rm = TRUE)) %>%
  filter(negativos_detectados > 0) %>%
  select(`Año Censal`, NOM_ZM, all_of(vars_financieras))

# 3. Resumen por Metrópoli y Año
resumen_negativos_clpiic <- diagnostico_clpiic %>%
  group_by(NOM_ZM, `Año Censal`) %>%
  summarise(across(all_of(vars_financieras), ~ sum(. < 0)), .groups = 'drop')

print(resumen_negativos_clpiic)

# Paso 1: Localizar sectores con VACB negativo
sectores_negativos <- metro_EC %>%
  filter(top_cluster %in% c(1, 2, 3, 4), vacb_m_i_t < 0) %>%
  select(NOM_ZM, `Año Censal`, cve_sector, vacb_m_i_t)

print(sectores_negativos)

# Paso 2: Sumatoria ponderada por metrópoli y año
diagnostico_suma <- metro_EC %>%
  filter(top_cluster %in% c(1, 2, 3, 4)) %>%
  mutate(var_ponderada = vacb_m_i_t * Media_Indice_Cluster) %>%
  group_by(NOM_ZM, `Año Censal`) %>%
  summarise(ECAyP = sum(var_ponderada, na.rm = TRUE), .groups = 'drop')

# Ver metrópolis con ECAyP inviable (<= 0)
casos_invalidos <- diagnostico_suma %>% filter(ECAyP <= 0)
print(casos_invalidos)


#TASA DE CRECIMIENTO DEL CLPIIC----
##### Cociente de Localización Ponderado de Industrias Intensivas en el Conocimiento Agregadas (CLPIICA)----

#Fórmula General:
#CLIICA: (Peso de la Economía del Conocimiento (Agregada y Ponderada) en la METROPOLI)/(Peso de la Economía del Conocimiento (Agregada y Ponderada) a nivel NACIONAL)
###Peso de la Economía del Conocimiento (Agregada y Ponderada) en la metropoli
#(Suma (var_i_m_t*k))/total_metro_t
#Peso de la Economía del Conocimiento (Agregada y Ponderada) a nivel nacional
#(Suma (var_i_n_t*k))/total_nacional_t

library(dplyr)
library(rlang)

calcular_TCCLPIIC_simplificada <- function(
    df,
    df_nac_total,
    clusters,
    var_base_metro,
    t0 = "2003",
    t1 = "2018"
) {

  # 1. Definición del Mapeo de Variables
  variable_map <- list(
    "ue_m_i_t" = list(total_metro = sym("ue_m_t"), nac_ponderar = sym("ue_nac_subsector_t"), total_nac = sym("ue_nac_t")),
    "pot_m_i_t" = list(total_metro = sym("pot_m_t"), nac_ponderar = sym("pot_nac_subsector_t"), total_nac = sym("pot_nac_t")),
    "vacb_m_i_t" = list(total_metro = sym("vacb_m_t"), nac_ponderar = sym("vacb_nac_subsector_t"), total_nac = sym("vacb_nac_t")),
    "ingresos_m_i_t" = list(total_metro = sym("ingresos_m_t"), nac_ponderar = sym("ingresos_nac_subsector_t"), total_nac = sym("ingresos_nac_t")),
    "va_percap_m_i_t" = list(total_metro = sym("va_percap_m_t"), nac_ponderar = sym("vapercap_nac_subsector_t"), total_nac = sym("vacb_percap_nac_t")),
    "ingresos_pecap_m_t" = list(total_metro = sym("ingresos_pecap_m_t"), nac_ponderar = sym("ingresos_percap_nac_subsector_t"), total_nac = sym("ingresos_percap_nac_t"))
  )

  if (!var_base_metro %in% names(variable_map)) stop("Variable no definida en mapeo.")

  variables <- variable_map[[var_base_metro]]
  var_metro_ponderar <- sym(var_base_metro)

  # 3. Filtro Inicial
  df_filtrado <- df %>% filter(top_cluster %in% clusters)

  # --- PESO DE LA EC EN LA METROPOLI CON DISCERNIMIENTO ---
  EC_metro_AyP <- df_filtrado %>%
    mutate(var_metro_ponderada = !!var_metro_ponderar * Media_Indice_Cluster) %>%
    group_by(`Año Censal`, NOM_ZM) %>%
    summarise(EC_AyP_m_t = sum(var_metro_ponderada, na.rm = TRUE), .groups = 'drop')

  tot_metro_sin_duplicados <- df_filtrado %>%
    select(`Año Censal`, NOM_ZM, !!variables$total_metro) %>%
    distinct()

  EC_metro_AyP_conTot <- EC_metro_AyP %>%
    left_join(tot_metro_sin_duplicados, by = c("Año Censal", "NOM_ZM")) %>%
    mutate(Peso_EC_Metropoli = ifelse(EC_AyP_m_t <= 0 | !!variables$total_metro <= 0,
                                      NA,
                                      EC_AyP_m_t / !!variables$total_metro))

  # --- PESO DE LA EC A NIVEL NACIONAL ---
  tot_sector_duplicados <- df_filtrado %>%
    select(`Año Censal`, Media_Indice_Cluster, !!variables$nac_ponderar) %>%
    distinct()

  EC_Nacional_AyP <- tot_sector_duplicados %>%
    mutate(sector_ponderado = !!variables$nac_ponderar * Media_Indice_Cluster) %>%
    group_by(`Año Censal`) %>%
    summarise(EC_AyP_Nac_t = sum(sector_ponderado, na.rm = TRUE), .groups = 'drop')

  base_nacional <- EC_Nacional_AyP %>%
    left_join(df_nac_total %>% select(`Año Censal`, !!variables$total_nac), by = "Año Censal") %>%
    mutate(Peso_EC_Nacional_t = EC_AyP_Nac_t / !!variables$total_nac)

  # --- CLPIIC y TASA DE CRECIMIENTO ---
  bd_para_clpiic <- EC_metro_AyP_conTot %>%
    left_join(base_nacional, by = "Año Censal") %>%
    mutate(CLPIIC_m_t = Peso_EC_Metropoli / Peso_EC_Nacional_t)

  bd_clpiic_t0 <- bd_para_clpiic %>% filter(`Año Censal` == t0) %>% select(NOM_ZM, CLPIIC_m_t)
  bd_clpiic_t1 <- bd_para_clpiic %>% filter(`Año Censal` == t1) %>% select(NOM_ZM, CLPIIC_m_t)

  TC_CLIIC_final <- full_join(bd_clpiic_t0, bd_clpiic_t1, by = "NOM_ZM", suffix = c(paste0("_", t0), paste0("_", t1)))

  TC_CLIIC_final <- TC_CLIIC_final %>%
    mutate(
      tasa_crecimiento_cliic = ((!!sym(paste0("CLPIIC_m_t_", t1)) / !!sym(paste0("CLPIIC_m_t_", t0))) - 1) * 100
    ) %>%
    arrange(tasa_crecimiento_cliic)

  return(TC_CLIIC_final)
}


# Paso 1: Asegúrese de tener el dataframe de totales nacionales cargado y renombrado.
# total_nacional_t (Debe contener ue_nac_t, pot_nac_t, vacb_nac_t, etc.)
#Las posibles variables a ponderar a nivel metropolitano "var_a_ponderar" son: "ue_m_i_t", "pot_m_i_t", "vacb_m_i_t", "ingresos_m_i_t"
# Caso 1: Usando Unidades Económicas (UE)----
TC_ue_c1234 <- calcular_TCCLPIIC_simplificada(
  df = metro_EC,
  df_nac_total = total_nacional_t,
  clusters = c("1", "2", "3", "4"),
  var_base_metro = "ue_m_i_t" # Se pasa como string
)

#Caso 1.1: UE con Clusters 1, 2 y 3
TC_ue_c123 <- calcular_TCCLPIIC_simplificada(
  df = metro_EC,
  df_nac_total = total_nacional_t,
  clusters = c("1", "2", "3"),
  var_base_metro = "ue_m_i_t" # Se pasa como string
)
#Caso 1.2: UE con Clusters 1 y 2
TC_ue_c12 <- calcular_TCCLPIIC_simplificada(
  df = metro_EC,
  df_nac_total = total_nacional_t,
  clusters = c("1", "2"),
  var_base_metro = "ue_m_i_t" # Se pasa como string
)

# Caso 2: Usando Personal Ocupado Total (POT)----
TC_pot_c1234 <- calcular_TCCLPIIC_simplificada(
  df = metro_EC,
  df_nac_total = total_nacional_t,
  clusters = c("1", "2", "3", "4"),
  var_base_metro = "pot_m_i_t" # Se pasa como string
)
#POT C123
TC_pot_c123 <- calcular_TCCLPIIC_simplificada(
  df = metro_EC,
  df_nac_total = total_nacional_t,
  clusters = c("1", "2", "3"),
  var_base_metro = "pot_m_i_t" # Se pasa como string
)
#POT C12
TC_pot_c12 <- calcular_TCCLPIIC_simplificada(
  df = metro_EC,
  df_nac_total = total_nacional_t,
  clusters = c("1", "2"),
  var_base_metro = "pot_m_i_t" # Se pasa como string
)
# Caso 3: Usando Valor Agregado Censal Bruto (VACB)----
TC_vacb_c1234 <- calcular_TCCLPIIC_simplificada(
  df = metro_EC,
  df_nac_total = total_nacional_t,
  clusters = c("1", "2", "3", "4"),
  var_base_metro = "vacb_m_i_t" # Se pasa como string
)
#VACB C123
TC_vacb_c123 <- calcular_TCCLPIIC_simplificada(
  df = metro_EC,
  df_nac_total = total_nacional_t,
  clusters = c("1", "2", "3"),
  var_base_metro = "vacb_m_i_t" # Se pasa como string
)
#VACB C12
TC_vacb_c12 <- calcular_TCCLPIIC_simplificada(
  df = metro_EC,
  df_nac_total = total_nacional_t,
  clusters = c("1", "2"),
  var_base_metro = "vacb_m_i_t" # Se pasa como string
)

# Caso 4: Usando el Ingreso (ingreso)----
TC_ingreso_c1234 <- calcular_TCCLPIIC_simplificada(
  df = metro_EC,
  df_nac_total = total_nacional_t,
  clusters = c("1", "2", "3", "4"),
  var_base_metro = "ingresos_m_i_t" # Se pasa como string
)
#Ingreso C123
TC_ingreso_c123 <- calcular_TCCLPIIC_simplificada(
  df = metro_EC,
  df_nac_total = total_nacional_t,
  clusters = c("1", "2", "3"),
  var_base_metro = "ingresos_m_i_t" # Se pasa como string
)

#Ingreso C12
TC_ingreso_c12 <- calcular_TCCLPIIC_simplificada(
  df = metro_EC,
  df_nac_total = total_nacional_t,
  clusters = c("1", "2"),
  var_base_metro = "ingresos_m_i_t" # Se pasa como string
)

#Renombrar Variables----
library(dplyr)
library(stringr)

# Función para renombrar un dataframe
renombrar_vars_tc <- function(df, df_name) {

  # 1. Extraer el sufijo de identificación (ej: "ue_c1234") del nombre del df
  sufijo <- str_extract(df_name, "(ue|pot|vacb|ingreso)_c[0-9]+")

  if (is.na(sufijo)) {
    # Manejar caso de nombres inesperados
    warning(paste("No se pudo extraer el sufijo del dataframe:", df_name))
    return(df)
  }

  # 2. Definir los nuevos nombres de las variables usando el sufijo
  df_renombrado <- df %>%
    rename(
      # Columna 2003
      !!paste0("CL_", sufijo, "_2003") := CLPIIC_m_t_2003,
      # Columna 2018
      !!paste0("CL_", sufijo, "_2018") := CLPIIC_m_t_2018,
      # Tasa de Crecimiento
      !!paste0("TasaC_", sufijo) := tasa_crecimiento_cliic
    )

  return(df_renombrado)
}
library(purrr)

# 1. Crear una lista con todos los dataframes a renombrar
lista_dfs <- list(
  TC_ue_c1234 = TC_ue_c1234,
  TC_ue_c123 = TC_ue_c123,
  TC_ue_c12 = TC_ue_c12,
  TC_pot_c1234 = TC_pot_c1234,
  TC_pot_c123 = TC_pot_c123,
  TC_pot_c12 = TC_pot_c12,
  TC_vacb_c1234 = TC_vacb_c1234,
  TC_vacb_c123 = TC_vacb_c123,
  TC_vacb_c12 = TC_vacb_c12,
  TC_ingreso_c1234 = TC_ingreso_c1234,
  TC_ingreso_c123 = TC_ingreso_c123,
  TC_ingreso_c12 = TC_ingreso_c12
)

# 2. Aplicar la función a todos los elementos de la lista y actualizar los dataframes
lista_dfs_renombrados <- map2(
  .x = lista_dfs,
  .y = names(lista_dfs),
  .f = renombrar_vars_tc
)
# 3. Asignar los dataframes renombrados al entorno global
list2env(lista_dfs_renombrados, .GlobalEnv)

#Unir los DataFrame----
lista_dfs_completos <- list(
  TC_ue_c1234,
  TC_ue_c123,
  TC_ue_c12,
  TC_pot_c1234,
  TC_pot_c123,
  TC_pot_c12,
  TC_vacb_c1234,
  TC_vacb_c123,
  TC_vacb_c12,
  TC_ingreso_c1234,
  TC_ingreso_c123,
  TC_ingreso_c12
)
library(dplyr)
library(purrr)

# Se define el dataframe base y la lista de dataframes restantes
df_base <- TC_ue_c1234
dfs_a_unir <- lista_dfs_completos # Excluimos el primer elemento que es la base

# Aplicar left_join secuencialmente a todos los dataframes
df_consolidado <- dfs_a_unir %>%
  reduce(left_join, by = "NOM_ZM")

colnames(df_consolidado)
#Reducir puntos decimales
df_consolidado <- df_consolidado %>%
  mutate(
    # Aplica la función round a todas las columnas que no son NOM_ZM
    across(
      .cols = -NOM_ZM,  # Selecciona todas las columnas excepto NOM_ZM
      .fns = ~ round(., 2) # Aplica la función de redondeo a 2 decimales
    )
  )

write.xlsx(df_consolidado,
           file = "tablas_texto/TCCL_completo.xlsx",
           rowNames = TRUE)

#Crear una Data sólo con los clúster de industrias 1,2,3 y 4 y 1,2 y 3

#Unir los DataFrame----
lista_dfs_completos <- list(
  TC_ue_c1234,
  TC_ue_c123,
    TC_pot_c1234,
  TC_pot_c123,
   TC_vacb_c1234,
  TC_vacb_c123,
  TC_ingreso_c1234,
  TC_ingreso_c123
  )
library(dplyr)
library(purrr)

# Se define el dataframe base y la lista de dataframes restantes
df_base <- TC_ue_c1234
dfs_a_unir <- lista_dfs_completos # Excluimos el primer elemento que es la base

# Aplicar left_join secuencialmente a todos los dataframes
TC_completo_c123 <- dfs_a_unir %>%
  reduce(left_join, by = "NOM_ZM")

colnames(TC_completo_c123)
#Reducir puntos decimales
TC_completo_c123  <- TC_completo_c123  %>%
  mutate(
    # Aplica la función round a todas las columnas que no son NOM_ZM
    across(
      .cols = -NOM_ZM,  # Selecciona todas las columnas excepto NOM_ZM
      .fns = ~ round(., 2) # Aplica la función de redondeo a 2 decimales
    )
  )

# Agregar la variable Tendencia Positiva y Tendencia Negativa al dataframe 'CV_contracion'
TC_completo_c123 <- TC_completo_c123 %>%
  mutate(
    # 1. Selecciona todas las columnas que inician con "Tasa_"
    # 2. Aplica la condición '<= 0'. Esto crea una matriz lógica (TRUE/FALSE).
    # 3. rowSums() suma las filas de esta matriz. En R, TRUE se evalúa como 1 y FALSE como 0.
    Tendencia_Nega = rowSums(select(., starts_with("Tasa")) <= 0,na.rm = TRUE),
    Tendencia_Post= rowSums(select(., starts_with("Tasa")) > 0,na.rm = TRUE)
  ) %>% select(NOM_ZM,Tendencia_Nega, Tendencia_Post,  everything()) %>% arrange(desc(Tendencia_Nega))


write.xlsx(TC_completo_c123,
           file = "tablas_texto/TC_completo_c123.xlsx",
           rowNames = TRUE)

# Elaborar Mapa de Calor para Tasas de Crecimiento ----
library(tidyverse)
library(openxlsx)
#Solo TC para vista completa mapa de calor
solo_tc<-TC_completo_c123 %>%
  select(NOM_ZM, Tendencia_Nega, Tendencia_Post, starts_with("Tasa"))

exportar_tasas_excel <- function(solo_tc, archivo = "tablas_texto/solo_tc_coloreada.xlsx") {

  # 1. Seleccionar solo las columnas que inician con "Tasa" para el cálculo
  columnas_tasa <- solo_tc %>%
    select(starts_with("Tasa")) %>%
    as.matrix()

  # 2. Normalizar cada columna (0-1)
  columnas_tasa_norm <- apply(columnas_tasa, 2, function(x) {
    # Manejo de casos donde todos los valores son iguales o NA para evitar división por cero
    if(all(is.na(x)) || max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) return(rep(0.5, length(x)))
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  })

  # 3. Estructurar el dataframe que se exportará a Excel
  # Incluye las columnas de identificación y las de Tasas
  df_export <- solo_tc %>%
    select(NOM_ZM, Tendencia_Nega, Tendencia_Post, starts_with("Tasa"))

  # 4. Crear el Workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Tasas_Crecimiento")
  writeData(wb, "Tasas_Crecimiento", df_export)

  # 5. Identificar índices de las columnas que deben llevar color
  col_indices <- grep("^Tasa", names(df_export))

  # 6. Aplicar la lógica de colores (Rojo pastel -> Blanco -> Azul pastel)
  for (i in seq_along(col_indices)) {
    col <- col_indices[i]
    for (row in 1:nrow(columnas_tasa_norm)) {
      valor_norm <- columnas_tasa_norm[row, i]

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
        addStyle(wb, "Tasas_Crecimiento", style, rows = row + 1, cols = col)
      }
    }
  }

  # 7. Guardar el archivo
  # Crear el directorio si no existe
  if(!dir.exists("tablas_texto")) dir.create("tablas_texto")

  saveWorkbook(wb, archivo, overwrite = TRUE)
  cat("Archivo de tasas guardado:", archivo, "\n")
}

# Ejecutar la función con tu nueva base de datos
exportar_tasas_excel(solo_tc)


#----

#Mapa de Calor Incluyendo CL----

TC_con_CL<-TC_completo_c123 %>% filter(Tendencia_Nega >=5 | Tendencia_Post == 8) %>% arrange(desc(Tendencia_Nega))

# Función adaptada para incluir todas las variables pero colorear solo Tasas
exportar_mapa_completo <- function(datos, archivo = "tablas_texto/mapa_tasas_completo.xlsx") {

  # 1. Seleccionar y preparar matriz solo de las columnas "Tasa" para el cálculo de normalización
  df_tasas <- datos %>% select(starts_with("Tasa"))
  columnas_tasa_matriz <- as.matrix(df_tasas)

  # 2. Normalizar (0-1) para el gradiente de color
  columnas_tasa_norm <- apply(columnas_tasa_matriz, 2, function(x) {
    if(all(is.na(x)) || max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) return(rep(0.5, length(x)))
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  })

  # 3. El dataframe a exportar es el original completo
  df_export <- datos

  # 4. Crear el Workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Datos_y_Tasas")
  writeData(wb, "Datos_y_Tasas", df_export)

  # 5. Identificar índices de las columnas que inician con "Tasa" en el dataframe completo
  col_indices <- grep("^Tasa", names(df_export))

  # 6. Aplicar la lógica de colores SOLO a esas columnas
  for (i in seq_along(col_indices)) {
    col_idx_en_excel <- col_indices[i] # Índice real en el Excel

    for (row in 1:nrow(columnas_tasa_norm)) {
      valor_norm <- columnas_tasa_norm[row, i] # i corresponde a la columna en la matriz norm

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
        # Aplicamos el estilo usando el índice de columna real (col_idx_en_excel)
        addStyle(wb, "Datos_y_Tasas", style, rows = row + 1, cols = col_idx_en_excel)
      }
    }
  }

  # 7. Guardar el archivo
  if(!dir.exists("tablas_texto")) dir.create("tablas_texto")
  saveWorkbook(wb, archivo, overwrite = TRUE)
  cat("Mapa de calor generado exitosamente en:", archivo, "\n")
}

# Ejecutar con tu tabla actual
exportar_mapa_completo(TC_con_CL)


#----
#Seleccionar los casos de tendencia positiva y negativa
tendencia_tc<-TC_completo_c123 %>% filter(Tendencia_Nega >=5 | Tendencia_Post == 8) %>% arrange(Tendencia_Nega) %>%
  mutate(
    cl_2003 = case_when(
      CL_pot_c1234_2003 >= 1 ~ "CLPIIC>1", # Condición 1: Mayor o igual a 1
      CL_pot_c1234_2003 < 1 ~ "CLPIIC<1",   # Condición 2: Menor a 1
              ),
    cl_2018 = case_when(
      CL_pot_c1234_2018 >= 1 ~ "CLPIIC>1", # Condición 1: Mayor o igual a 1
      CL_pot_c1234_2018 < 1 ~ "CLPIIC<1",   # Condición 2: Menor a 1
  ),
  tendencia = case_when(
    Tendencia_Post == 8  ~ "Positiva", # Condición 1: Mayor o igual a 1
    Tendencia_Nega >=5 ~ "Negativa",
  )
    )%>%
  select(NOM_ZM, tendencia, cl_2003, cl_2018 )


write.xlsx(tendencia_tc,
           file = "tablas_texto/tendencia_tc.xlsx",
           rowNames = TRUE)

#Alternativa

#Seleccionar los casos de tendencia positiva y negativa
tendencia_tc_robusta<-TC_completo_c123 %>% filter(Tendencia_Nega == 0 | Tendencia_Post == 0) %>% arrange(Tendencia_Nega) %>%
  mutate(
    cl_2003 = case_when(
      CL_pot_c1234_2003 >= 1 ~ "CLPIIC>1", # Condición 1: Mayor o igual a 1
      CL_pot_c1234_2003 < 1 ~ "CLPIIC<1",   # Condición 2: Menor a 1
    ),
    cl_2018 = case_when(
      CL_pot_c1234_2018 >= 1 ~ "CLPIIC>1", # Condición 1: Mayor o igual a 1
      CL_pot_c1234_2018 < 1 ~ "CLPIIC<1",   # Condición 2: Menor a 1
    ),
    tendencia = case_when(
      Tendencia_Post == 8  ~ "Positiva", # Condición 1: Mayor o igual a 1
      Tendencia_Nega >=6 ~ "Negativa",
    )
  )%>%
  select(NOM_ZM, tendencia, cl_2003, cl_2018 )


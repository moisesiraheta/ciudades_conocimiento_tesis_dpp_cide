# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: Evaluación de la TCCLPIIC para las Zonas Metropolitanas con datos de Ramas.
# ==============================================================================

#Limpiar R
rm(list=ls (all=T))
### Paquetes  ----
library(pacman)
p_load(haven,      # Paquete para importar archivos de Stata, SAS y SPSS
       readxl,     # Paquete para importar archivos de Excel
       tidyvers,openxlsx)  # Metapaquete que incluye readr, paquete para importar achivos de texto plano
library(writexl)
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
#Renonbrar Variables
total_nacional_t<-total_nacional_t %>%
  rename( ue_nac_t = `UE Unidades económicas` ) %>%
  rename( pot_nac_t = `H001A Personal ocupado total`) %>%
  rename( vacb_nac_t = `A131A Valor agregado censal bruto (millones de pesos)`)%>%
  rename( ingresos_nac_t=`M000A Total de ingresos por suministro de bienes y servicios (millones de pesos)`) %>%
  rename( vacb_percap_nac_t= `A204A Valor agregado en promedio por persona ocupada (Pesos)`) %>%
  rename(ingresos_percap_nac_t= `A436A Ingresos por suministro de bienes y servicios por persona ocupada (Pesos)`)
colnames(total_nacional_t)


#Evaluación de Negativos para la ECAyP----
#Revisar Negativos en VACB
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
#Si hay negativos.



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

calcular_TCCLPIIC_simplificada <-function(
    df,
    df_nac_total,
    clusters,
    var_base_metro,
    t0 = "2003",
    t1 = "2018"
) {

  # --- 1. Mapeo de Variables ---
  variable_map <- list(
    "ue_m_i_t" = list(total_metro = sym("ue_m_t"), nac_ponderar = sym("ue_nac_Rama_t"), total_nac = sym("ue_nac_t")),
    "pot_m_i_t" = list(total_metro = sym("pot_m_t"), nac_ponderar = sym("pot_nac_Rama_t"), total_nac = sym("pot_nac_t")),
    "vacb_m_i_t" = list(total_metro = sym("vacb_m_t"), nac_ponderar = sym("vacb_nac_Rama_t"), total_nac = sym("vacb_nac_t")),
    "ingresos_m_i_t" = list(total_metro = sym("ingresos_m_t"), nac_ponderar = sym("ingresos_nac_Rama_t"), total_nac = sym("ingresos_nac_t"))
  )

  if (!var_base_metro %in% names(variable_map)) stop("Variable no definida.")

  variables <- variable_map[[var_base_metro]]
  var_metro_p <- sym(var_base_metro)

  # --- 2. Lógica de Cálculo con Filtro de Negativos ---
  df_filtrado <- df %>% filter(top_cluster %in% clusters)

  # A) Nivel Metropolitano
  EC_metro_AyP <- df_filtrado %>%
    mutate(
      # Mantenemos el replace_na original pero controlamos que los valores sean positivos
      v_limpio = replace_na(!!var_metro_p, 0),
      v_pond = v_limpio * Media_Indice_Cluster
    ) %>%
    group_by(`Año Censal`, NOM_ZM) %>%
    summarise(EC_AyP_m_t = sum(v_pond, na.rm = TRUE), .groups = 'drop')

  tot_metro <- df_filtrado %>%
    select(`Año Censal`, NOM_ZM, !!variables$total_metro) %>%
    distinct()

  EC_metro_AyP_conTot <- EC_metro_AyP %>%
    left_join(tot_metro, by = c("Año Censal", "NOM_ZM")) %>%
    mutate(
      # DISCERNIMIENTO DE NEGATIVOS (Criterio Función 1):
      # Si el acumulado de la EC es <= 0 o el total de la metrópoli es <= 0, asignamos NA
      Peso_EC_Metropoli = if_else(
        EC_AyP_m_t > 0 & !!variables$total_metro > 0,
        EC_AyP_m_t / !!variables$total_metro,
        NA_real_
      )
    )

  # B) Nivel Nacional
  base_nacional <- df_filtrado %>%
    select(`Año Censal`, `Actividad económica`, Media_Indice_Cluster, !!variables$nac_ponderar) %>%
    distinct() %>%
    mutate(v_p_n = replace_na(!!variables$nac_ponderar, 0) * Media_Indice_Cluster) %>%
    group_by(`Año Censal`) %>%
    summarise(EC_AyP_Nac_t = sum(v_p_n, na.rm = TRUE), .groups = 'drop') %>%
    left_join(df_nac_total %>% select(`Año Censal`, !!variables$total_nac), by = "Año Censal") %>%
    mutate(
      # DISCERNIMIENTO DE NEGATIVOS NACIONAL:
      # Si la EC nacional o el total país es <= 0, el peso es NA
      Peso_EC_Nacional_t = if_else(
        EC_AyP_Nac_t > 0 & !!variables$total_nac > 0,
        EC_AyP_Nac_t / !!variables$total_nac,
        NA_real_
      )
    )

  # --- 3. Pivotado ---
  bd_para_clpiic <- EC_metro_AyP_conTot %>%
    left_join(base_nacional, by = "Año Censal") %>%
    mutate(CLPIIC_m_t = Peso_EC_Metropoli / Peso_EC_Nacional_t)

  res_final <- bd_para_clpiic %>%
    select(`Año Censal`, NOM_ZM, CLPIIC_m_t) %>%
    filter(`Año Censal` %in% c(t0, t1)) %>%
    tidyr::pivot_wider(names_from = `Año Censal`, values_from = CLPIIC_m_t, names_prefix = "V_")

  col0 <- paste0("V_", t0)
  col1 <- paste0("V_", t1)

  # --- 4. Cálculo de Tasa y Ordenamiento (PURAMENTE NUMÉRICO Y SEGURO) ---
  res_final %>%
    mutate(
      tasa_crecimiento_cliic = if_else(
        # Al haber filtrado negativos antes, solo calculamos si ambos años tienen valores válidos (> 0)
        !is.na(!!sym(col0)) & !is.na(!!sym(col1)) & !!sym(col0) > 0 & !!sym(col1) > 0,
        ((!!sym(col1) / !!sym(col0)) - 1) * 100,
        NA_real_
      ),
      # Los ceros o NAs quedan explícitamente como NA_real_
      !!col0 := if_else(is.na(!!sym(col0)) | !!sym(col0) <= 0, NA_real_, !!sym(col0)),
      !!col1 := if_else(is.na(!!sym(col1)) | !!sym(col1) <= 0, NA_real_, !!sym(col1))
    ) %>%
    arrange(tasa_crecimiento_cliic) %>%
    rename(
      !!paste0("CLPIIC_m_t_", t0) := !!sym(col0),
      !!paste0("CLPIIC_m_t_", t1) := !!sym(col1)
    )
}



# Paso 1: Asegurarse de tener su dataframe de totales nacionales cargado y renombrado.
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
    # Manejar caso de nombres inesperados, aunque no aplica a tu lista
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
           file = "tablas_texto/4_digitos/TCCL_completo.xlsx",
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


# Agregar la variable Tendencia Negativa y Positiva
TC_completo_c123 <- TC_completo_c123 %>%
  mutate(
    # Convertimos a numérico temporalmente para la lógica de conteo
    Tendencia_Nega = rowSums(
      across(starts_with("Tasa"), ~ as.numeric(.) <= 0),
      na.rm = TRUE
    ),
    Tendencia_Post = rowSums(
      across(starts_with("Tasa"), ~ as.numeric(.) > 0),
      na.rm = TRUE
    )
  ) %>%
  select(NOM_ZM, Tendencia_Nega, Tendencia_Post, everything()) %>% arrange(desc(Tendencia_Nega) )
#Reducir puntos decimales
TC_completo_c123  <- TC_completo_c123  %>%
  mutate(
    # Aplica la función round a todas las columnas que no son NOM_ZM
    across(
      .cols = -NOM_ZM,  # Selecciona todas las columnas excepto NOM_ZM
      .fns = ~ round(., 4) # Aplica la función de redondeo a 2 decimales
    )
  )

write.xlsx(TC_completo_c123,
           file = "tablas_texto/4_digitos/Ramas_TC_completo_c123.xlsx",
           rowNames = TRUE)


library(openxlsx)
library(dplyr)
#Mapa de Calor Incluyendo CL----

TC_con_CL<-TC_completo_c123 %>% arrange(desc(Tendencia_Nega))

exportar_mapa_completo <- function(datos, archivo = "tablas_texto/4_digitos/Ramas_mapa_tasas_completo.xlsx") {

  df_export <- datos
  wb <- createWorkbook()
  addWorksheet(wb, "Datos_y_Tasas")
  writeData(wb, "Datos_y_Tasas", df_export)

  col_indices <- grep("^Tasa", names(df_export))

  for (col_idx in col_indices) {
    valores_col <- df_export[[col_idx]]

    # --- TRANSFORMACIÓN LOGARÍTMICA DE LOS EXTREMOS ---
    # Convertimos los límites usando log1p para achicar la distancia de los outliers
    max_log_pos <- max(log1p(valores_col[valores_col > 0]), na.rm = TRUE)
    max_log_neg <- max(log1p(abs(valores_col[valores_col < 0])), na.rm = TRUE)

    if (!is.finite(max_log_pos) || length(max_log_pos) == 0) max_log_pos <- 1
    if (!is.finite(max_log_neg) || length(max_log_neg) == 0) max_log_neg <- 1

    for (row in seq_along(valores_col)) {
      val <- valores_col[row]

      if (is.na(val) || val == 0) next

      if (val > 0) {
        # --- BLANCO A AZUL PASTEL (LOGARÍTMICO) ---
        # La proporción se calcula en base al logaritmo, suavizando el extremo
        prop <- log1p(val) / max_log_pos

        r <- round(255 - 100 * prop)
        g <- round(255 - 60 * prop)
        b <- 255

      } else {
        # --- BLANCO A ROJO PASTEL (LOGARÍTMICO) ---
        # Usamos el valor absoluto para calcular el logaritmo del negativo
        prop <- log1p(abs(val)) / max_log_neg

        r <- 255
        g <- round(255 - 110 * prop)
        b <- round(255 - 110 * prop)
      }

      # Forzar límites RGB reglamentarios
      r <- max(0, min(255, r))
      g <- max(0, min(255, g))
      b <- max(0, min(255, b))

      color_hex <- sprintf("#%02X%02X%02X", r, g, b)
      style <- createStyle(fgFill = color_hex)

      addStyle(wb, "Datos_y_Tasas", style, rows = row + 1, cols = col_idx)
    }
  }

  if(!dir.exists(dirname(archivo))) dir.create(dirname(archivo), recursive = TRUE)
  saveWorkbook(wb, archivo, overwrite = TRUE)
  cat("Mapa de calor pseudo-logarítmico generado con éxito en:", archivo, "\n")
}

# Ejecutar con tabla actual
exportar_mapa_completo(TC_con_CL)


# Seleccionar solo las columnas que contienen "Tasa"
TC_completo_c123_tasas <- TC_completo_c123 %>%
  select(NOM_ZM, Tendencia_Nega, Tendencia_Post, contains("Tasa"))

# Guardar en Excel
write_xlsx(TC_completo_c123_tasas,
           "tablas_texto/4_digitos/TC_completo_c123_tasas.xlsx")

##----
#Seleccionar los casos de tendencia positiva y negativa
tendencia_tc<-TC_completo_c123 %>% filter(Tendencia_Nega >=5 | Tendencia_Nega == 0) %>% arrange(Tendencia_Nega) %>%
  mutate(
    cl_2003 = case_when(
      CL_pot_c1234_2003 >= 1 ~ "CLPIIC>1*", # Condición 1: Mayor o igual a 1
      CL_pot_c1234_2003 < 1 ~ "CLPIIC<1",   # Condición 2: Menor a 1
    ),
    cl_2018 = case_when(
      CL_pot_c1234_2018 >= 1 ~ "CLPIIC>1*", # Condición 1: Mayor o igual a 1
      CL_pot_c1234_2018 < 1 ~ "CLPIIC<1",   # Condición 2: Menor a 1
    ),
    tendencia = case_when(
      Tendencia_Nega >=5  ~ "Negativa", # Condición 1: Mayor o igual a 1
      Tendencia_Nega == 0 ~ "Positiva",
    )
  )%>%
  select(NOM_ZM, tendencia, cl_2003, cl_2018 )


write.xlsx(tendencia_tc,
           file = "tablas_texto/4_digitos/tendencia_tc.xlsx",
           rowNames = TRUE)

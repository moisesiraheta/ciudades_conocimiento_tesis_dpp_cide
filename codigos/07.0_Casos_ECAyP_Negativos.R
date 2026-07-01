# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: CASO DE ESTUDIO: Coatzacoalcos Economía del Conocimiento Agregada y Ponderada
# ==============================================================================

# 1. Carga de Librerías
library(openxlsx)
library(tidyverse) # Carga dplyr, ggplot2, tidyr, etc.

# 2. Carga y Limpieza de Datos
metro_EC <- read.xlsx("tablas_texto/metro_EC_deflac.xlsx", rowNames = TRUE) %>%
  rename(
    `Año Censal` = Año.Censal,
    `Actividad económica` = Actividad.económica
  )

# Parámetros Globales
clusters_sectores <- c(1, 2, 3, 4)

# ------------------------------------------------------------------------------
# 3. Diagnóstico General de Valores Negativos
# ------------------------------------------------------------------------------

# Identificar variables de interés (VACB e Ingresos)
vars_diagnostico <- grep("vacb|ingreso", colnames(metro_EC), value = TRUE, ignore.case = TRUE)

# Identificar registros con valores negativos en los clusters del conocimiento
detalle_negativos <- metro_EC %>%
  filter(top_cluster %in% clusters_sectores) %>%
  mutate(n_negativos = rowSums(across(all_of(vars_diagnostico), ~ . < 0), na.rm = TRUE)) %>%
  filter(n_negativos > 0) %>%
  select(`Año Censal`, NOM_ZM, cve_sector, `Actividad económica`, all_of(vars_diagnostico), top_cluster)

# Metrópolis identificadas con riesgo de distorsión
metros_en_riesgo <- unique(detalle_negativos$NOM_ZM)
print(metros_en_riesgo)

# ------------------------------------------------------------------------------
# 4. Cálculo de la EC Agregada Ponderada y Verificación de Signos
# ------------------------------------------------------------------------------

verificacion_cviicap <- metro_EC %>%
  filter(NOM_ZM %in% metros_en_riesgo, top_cluster %in% clusters_sectores) %>%
  mutate(var_metro_ponderada = vacb_m_i_t * Media_Indice_Cluster) %>%
  group_by(`Año Censal`, NOM_ZM) %>%
  summarise(
    EC_AyP_m_t = sum(var_metro_ponderada, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = `Año Censal`,
    values_from = EC_AyP_m_t,
    names_prefix = "VACB_Sum_"
  )

# Imprimir tabla general de verificación
print("Verificación de Metrópolis en Riesgo (Totales Ponderados):")
print(verificacion_cviicap)

# ------------------------------------------------------------------------------
# 5. Análisis Profundo: Caso Coatzacoalcos
# ------------------------------------------------------------------------------

# 5.1. Comparativa Temporal de Totales
resultados_coatza <- verificacion_cviicap %>%
  filter(NOM_ZM == "Coatzacoalcos")

print("Resultados Agregados Coatzacoalcos (2003 vs 2018):")
print(resultados_coatza)

# 5.2. Desglose Sectorial (Identificación del factor de distorsión)
desglose_coatza <- metro_EC %>%
  filter(NOM_ZM == "Coatzacoalcos", top_cluster %in% clusters_sectores) %>%
  mutate(EC_Ponderada_Sector = vacb_m_i_t * Media_Indice_Cluster) %>%
  select(`Año Censal`, cve_sector, `Actividad económica`, vacb_m_i_t, Media_Indice_Cluster, EC_Ponderada_Sector) %>%
  arrange(`Año Censal`)

print("Sectores con mayor impacto en 2018:")
desglose_coatza %>% filter(`Año Censal` == 2018) %>% print()

# 5.3. Análisis de Proporción del Sector Negativo
analisis_proporcion <- desglose_coatza %>%
  filter(`Año Censal` == 2018) %>%
  mutate(tipo = ifelse(EC_Ponderada_Sector < 0, "Sectores_Negativos", "Sectores_Positivos")) %>%
  group_by(tipo) %>%
  summarise(
    Suma_Ponderada = sum(EC_Ponderada_Sector, na.rm = TRUE),
    N_Sectores = n(),
    .groups = 'drop'
  ) %>%
  mutate(Proporcion_Absoluta = abs(Suma_Ponderada) / sum(abs(Suma_Ponderada)) * 100)

print("Impacto relativo de los sectores negativos en la EC de Coatza (2018):")
print(analisis_proporcion)

# 5.4. Impacto Específico del Subsector 325 (Industria Química)
impacto_325 <- desglose_coatza %>%
  filter(`Año Censal` == 2018, cve_sector == 325)

print("Detalle técnico del Subsector Crítico (325):")
print(impacto_325)


# 1. Preparar los datos
datos_texto <- metro_EC %>%
  filter(NOM_ZM == "Coatzacoalcos", `Año Censal` == 2018, top_cluster %in% clusters_sectores) %>%
  mutate(EC_Ponderada = vacb_m_i_t * Media_Indice_Cluster)

# 2. Extraer y calcular las variables para el texto (usando valores PONDERADOS)
sect_neg <- datos_texto %>% filter(EC_Ponderada < 0)
sect_pos <- datos_texto %>% filter(EC_Ponderada >= 0)

# Formatear variables para el string
nombres_neg       <- paste(sect_neg$`Actividad económica`, collapse = ", ")
# Usamos el valor ponderado para que la suma sea lógica en el texto
valores_neg_pond  <- paste(format(round(sect_neg$EC_Ponderada, 2), big.mark=","), collapse = ", ")
suma_positiva     <- format(round(sum(sect_pos$EC_Ponderada, na.rm = TRUE), 2), big.mark=",")
valor_final       <- format(round(sum(datos_texto$EC_Ponderada, na.rm = TRUE), 2), big.mark=",")

# Cálculo de la proporción (Impacto absoluto sobre la dinámica ponderada)
total_abs         <- sum(abs(datos_texto$EC_Ponderada))
prop_negativa     <- round((sum(abs(sect_neg$EC_Ponderada)) / total_abs) * 100, 1)

# 3. Generar el texto usando stringr::str_interp
# Se especifica que los valores son "ponderados" para mayor claridad académica
# 3. Generar la versión integrada (Síntesis Técnica y Narrativa)
# 2. Generar la narrativa final ajustada
texto_final <- str_interp("
  En la zona metropolitana de Coatzacoalcos, el ${nombres_neg}
  registró una contracción en VACB de ${valores_neg_pond}.
  Los sectores positivos apenas sumaron ${suma_positiva}, lo que resultó en una
  ECAyP final de ${valor_final}. En términos estructurales, el ${nombres_neg}
  concentra el ${prop_negativa}% de la dinámica de la economía del conocimiento
  en la metrópoli, explicando la alta sensibilidad del índice ante variaciones de este sector.
")

# 3. Imprimir resultado limpio
cat(texto_final)


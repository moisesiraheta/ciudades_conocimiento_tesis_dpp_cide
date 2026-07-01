# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: Análisis de los resultados del CVIICAP por metrópoli. Contracciones registradas, en el top 10 y 15 de las peores por variable.
# ==============================================================================
#Limpiar R
rm(list=ls (all=T))
### Paquetes  ----
library(pacman)
p_load(dplyr,
       readxl,
       openxlsx)

# 1. Cargar la base de datos

base_heatmap <-read.xlsx("tablas_texto/base_heatmap.xlsx", rowNames = TRUE)


colnames(base_heatmap)

# Define las columnas que quieres rankear
metricas <- c("CV_ue_c1234", "CV_ue_c123", "CV_ue_c12",
              "CV_pot_c1234", "CV_pot_c123", "CV_pot_c12",
              "CV_vacb_c1234", "CV_vacb_c123", "CV_vacb_c12",
              "CV_ingresos_c1234", "CV_ingresos_c123", "CV_ingresos_c12")



# 3. Función para obtener el reporte de una ZM específica
validar_zm <- function(nombre_zm) {
  # Filtrar la fila
  fila_zm <- base_heatmap %>%
    filter(grepl(nombre_zm, NOM_ZM, ignore.case = TRUE))

  if (nrow(fila_zm) == 0) {
    return("Zona Metropolitana no encontrada.")
  }

  reporte_ranks <- data.frame(Metrica = metricas, Valor = NA, Ranking = NA)

  for (i in 1:length(metricas)) {
    m <- metricas[i]
    todos_los_ranks <- rank(base_heatmap[[m]], ties.method = "min")

    indice <- which(base_heatmap$NOM_ZM == fila_zm$NOM_ZM[1])
    reporte_ranks$Valor[i] <- fila_zm[[m]][1]
    reporte_ranks$Ranking[i] <- todos_los_ranks[indice]
  }

  en_top_10 <- sum(reporte_ranks$Ranking <= 10)
  en_top_15 <- sum(reporte_ranks$Ranking > 10 & reporte_ranks$Ranking <= 15)

  # 'contracciones'
  Contracciones <- fila_zm$contracciones[1]

  cat("--- Reporte para:", fila_zm$NOM_ZM[1], "---\n")
  cat("Contracciones registradas:", Contracciones, "\n")
  cat("En bloque inferior (Top 10):", en_top_10, "\n")
  cat("En bloque de las 15 peores:", en_top_15, "\n\n")
  print(reporte_ranks)
}
# 4. Análisis de Casos:
validar_zm("Tianguistenco")
validar_zm("Saltillo")
validar_zm("Zacatecas-Guadalupe")

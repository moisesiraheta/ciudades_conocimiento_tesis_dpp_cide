# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: Construcción del Índice de Intensidad del Conocimiento por Subsector Económico a 3 dígitos del SCIAN-México
# ==============================================================================

# Limpiar entorno de trabajo para asegurar que no existan variables de sesiones previas
rm(list = ls(all = TRUE))

#Instalar Paquetes
library(pacman)
p_load(
  haven,      # Importar archivos Stata/SAS/SPSS
  readxl,     # Importar archivos Excel
  tidyverse,  # Conjunto de paquetes para manipulación y análisis de datos
  dplyr,      # Manipulación de datos (parte de tidyverse)
  openxlsx,   # Exportar archivos Excel
  stats,      # Funciones estadísticas básicas y multivariadas
  psych,      # Estadística descriptiva y psicometría
  ggrepel,    # Etiquetas en gráficos sin traslape
  tibble,     # Data frames modernos
  NbClust     # Determinación del número óptimo de clusters
)

# Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen = 999) # Prevenir notación científica

#Cargar Base de datos
data_intensidad_K <- read.xlsx("datos/data_intensidad_K_imputed.xlsx")
#Análisis Exploratorio previo al ACP----
#Escalar Datos
colnames(data_intensidad_K)
#Asignar nombres a las observaciones
data_con_nombres <- data_intensidad_K %>%
  mutate(nombre_observacion = paste(cve_sector, nombre_sector, sep = "_")) %>%
  select(nombre_observacion, everything()) %>%
  tibble::column_to_rownames("nombre_observacion")
#Eliminar variables no numéricas
data_a_escalar_con_nombres <- data_con_nombres %>%
  select(-cve_sector, -nombre_sector)

?scale
#(Valor columna-media de columna)/ Desviación Estándar de la Columna (raíz de la Variaran)
#Resultado: la columna tiene una media cero y desviación estandar 1
data_escalada <- scale(data_a_escalar_con_nombres)
data_escalada <- as.data.frame(data_escalada)

#Matriz de Correlación----
#Este gráfico muestra las correlaciones entre todas las variables. Las correlaciones indican la fuerza y la dirección de la relación lineal entre pares de variables.
#Un heatmap utiliza colores para representar la magnitud de las correlaciones, lo que facilita la identificación de patrones.

library(dplyr)
library(ggplot2)
library(reshape2) # Para el pre procesamiento de los datos para ggplot2

# Calcular la matriz de correlación
cor_matrix <- cor(data_a_escalar_con_nombres) # Usamos data_a_escalar antes de estandarizar

# Preparar los datos para ggplot2
cor_matrix_melted <- melt(cor_matrix)

# Crear el heatmap
ggplot(data = cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Matriz de Correlación de Indicadores de la Intensidad del Conocimiento por Subsector" )

#Variables Menos Correlacionadas----

# Asegurarse de que la diagonal (correlación de una variable consigo misma) sea NA
diag(cor_matrix) <- NA

as.data.frame(cor_matrix)

# Calcular la media del valor absoluto de las correlaciones por variable
mean_abs_cor <- apply(abs(cor_matrix), 2, mean, na.rm = TRUE)

# Ordenar las variables por su media de correlación absoluta de menor a mayor
sorted_mean_abs_cor <- sort(mean_abs_cor)

# Imprimir los resultados
print("Media de las correlaciones absolutas por variable (ordenadas de menor a mayor):")
print(sorted_mean_abs_cor)

corr_vab<-as.data.frame(sorted_mean_abs_cor)

write.xlsx(corr_vab ,
           file = "tablas_texto/corr_vab.xlsx",
           rowNames = T)

#ACP----
# Aplicar el ACP
colnames(data_escalada)
pca_result <- prcomp(data_escalada, center = FALSE, scale. = FALSE)
# Mostrar un resumen del resultado
summary(pca_result)

resumen<-summary(pca_result)

library(openxlsx)
write.xlsx(resumen$importance,
           file = "tablas_texto/tabla_import_componentes.xlsx",
           rowNames = FALSE)

#Cargas de los Componentes----
# Extraer las cargas de los componentes principales
cargas_pca <- pca_result$rotation

# Convertir las cargas a un data frame
cargas_df <- as.data.frame(cargas_pca)

sum(cargas_df$PC1)

# Mostrar las cargas del primer componente principal (PC1)
print("Cargas del PC1:")
print(cargas_df$PC1)

# Mostrar las cargas de las variables en PC2 y PC3
print("Cargas en PC2:")
print(cargas_df$PC2)

print("\nCargas en PC3:")
print(cargas_df$PC3)

cargas_df_nombres<- cargas_df%>%
  mutate(Indicador = rownames(cargas_df)) %>%
  select(Indicador, everything())


write.xlsx(cargas_df_nombres,
           file = "tablas_texto/cargas_variable_compo.xlsx",
           rowNames = FALSE)

#Puntuaciones Subsector de la PC1----

# Obtener las puntuaciones de los componentes principales
puntuaciones_pca <- pca_result$x

# Convertir las puntuaciones a un data frame
puntuaciones_df <- as.data.frame(puntuaciones_pca)

# Asignar nombres de fila (subsectores)
puntuaciones_df$Subsector <- rownames(data_escalada)

# Seleccionar y mostrar solo las puntuaciones del PC1 y el nombre del subsector
indice_intensidad_PC1 <- puntuaciones_df[, c("Subsector", "PC1")]

colnames(indice_intensidad_PC1)

# Ordenar el data frame por la columna PC1 de mayor a menor
indice_intensidad_PC1_ordenado <- indice_intensidad_PC1 %>%
  arrange(desc(PC1))

# Calcular el índice normalizado
indice_intensidad_PC1_ordenado <- indice_intensidad_PC1_ordenado %>%
  mutate(Indice_Normalizado = (PC1 - min(PC1)) / (max(PC1) - min(PC1)))

# Agregar la columna "Posición"
indice_intensidad_PC1_ordenado <- indice_intensidad_PC1_ordenado %>%
  mutate(Posicion = row_number()) %>%
  select(Posicion, everything()) # Mover la columna Posición al principio

# Mostrar las primeras filas del data frame ordenado
head(indice_intensidad_PC1_ordenado)

colnames(indice_intensidad_PC1_ordenado)

write.xlsx(indice_intensidad_PC1_ordenado,
           file = "tablas_texto/indice_pc1.xlsx",
           rowNames = FALSE)


#Evaluación de Consistencia----
# Calcular Omega de McDonald
# Máxima Verosimilitud
omega_result_ml <- omega(data_escalada, fm = "ml")
print(omega_result_ml)

#Obtener las Cargar Factoriales por dimensiones
cargas_factoriales_Schmid_Leiman<-as.data.frame(omega_result_ml[["schmid"]][["sl"]])

colnames(cargas_factoriales_Schmid_Leiman)

cargas_factoriales_Schmid_Leiman <- cargas_factoriales_Schmid_Leiman %>%
  mutate(across(c("g", "F1*", "F2*", "F3*", "h2"), ~ if_else(. < 0.2, NA_real_, .)))

cargas_factoriales_Schmid_Leiman <- cargas_factoriales_Schmid_Leiman %>%
  mutate(across(everything(), ~ round(., 2)))
cargas_factoriales_Schmid_Leiman

write.xlsx(cargas_factoriales_Schmid_Leiman ,
           file = "tablas_texto/cargas_factoriales_Schmid_Leiman.xlsx",
           rowNames = T)



#Análisis de Sensibilidad 7 Componentes Principales----
# Extraer las puntuaciones de los componentes principales
puntuaciones_pca <- pca_result$x
# Convertir las puntuaciones a un data frame
puntuaciones_df <- as.data.frame(puntuaciones_pca)
# Extraer la proporción de varianza explicada por cada componente
pca_result$sdev
str(pca_result)

varianza_explicada <- pca_result$sdev^2 / sum(pca_result$sdev^2)
print(varianza_explicada)
# Calcular el índice ponderado usando los primeros 7 componentes
puntuaciones_df$Indice_Ponderado_7 <- 0  # Iniciar la columna del índice
for (i in 1:7) {
  puntuaciones_df$Indice_Ponderado_7 <- puntuaciones_df$Indice_Ponderado_7 + puntuaciones_df[,i] * varianza_explicada[i]
}

# Crear un data frame con el índice ponderado y los nombres de los subsectores
indice_intensidad_PC1_7<- data.frame(
  Subsector = rownames(puntuaciones_df),
  Indice_Ponderado_7 = puntuaciones_df$Indice_Ponderado_7
)



# Ordenar el data frame por el índice ponderado de mayor a menor
indice_intensidad_PC1_7_ordenado <- indice_intensidad_PC1_7 %>%
  arrange(desc(Indice_Ponderado_7))

colnames(indice_intensidad_PC1_7_ordenado)

# Calcular el índice normalizado
indice_intensidad_PC1_7_ordenado <- indice_intensidad_PC1_7_ordenado %>%
  mutate(Indice_Normalizado_PC1_7 = (Indice_Ponderado_7 - min(Indice_Ponderado_7)) / (max(Indice_Ponderado_7) - min(Indice_Ponderado_7)))

# Agregar la columna "Posición"
indice_intensidad_PC1_7_ordenado <- indice_intensidad_PC1_7_ordenado %>%
  mutate(Posicion = row_number()) %>%
  select(Posicion, everything()) # Mover la columna Posición al principio

colnames(indice_intensidad_PC1_7_ordenado)

# Mostrar las primeras filas del data frame ordenado
head(indice_intensidad_PC1_7_ordenado )

write.xlsx(indice_intensidad_PC1_7_ordenado,
           file = "tablas_texto/indice_pc1_7.xlsx",
           rowNames = FALSE)

# Mostrar las primeras filas del data frame ordenado
head(indice_intensidad_PC1_7_ordenado)

#Índice Multidimensional----

#Dimensión Capital Humano y Educación----
# 1. Seleccionar las variables de la dimensión "Capital Humano y Educación"
data_CH_Educ <- data_escalada %>%
  select(CE.1ProporcionPACD, CE.5porcent_educa_superior, ENOE.1intensidad_trab_conoc, ENOE.2porce_posgrado)

# 2. Realizar el Análisis de Componentes Principales (ACP)
pca_CH_Educ <- prcomp(data_CH_Educ, center = FALSE, scale. = FALSE) # Ya están escaladas

# 3. Extraer las puntuaciones del PC1
PC1_CH_Educ <- pca_CH_Educ$x[, 1]

# 4. Crear un data frame con las puntuaciones del PC1 y los nombres de los subsectores
indice_CH_Educ <- data.frame(
  Subsector = rownames(data_escalada),
  PC1_CH_Educ = PC1_CH_Educ
)

# Ordenar el data frame por la columna PC1 de mayor a menor
indice_CH_Educ <- indice_CH_Educ %>%
  arrange(desc(PC1_CH_Educ))

######Dimensión Investigación y Desarrollo----

# 1. Seleccionar las variables de la dimensión "Investigación y Desarrollo"
data_IyD <- data_escalada %>%
  select(CE.2IntensidadSP, CE.10patentes_por_ue, CE.11gasto_percap_iyd)

# 2. Realizar el Análisis de Componentes Principales (ACP)
pca_IyD <- prcomp(data_IyD, center = FALSE, scale. = FALSE) # Ya están escaladas

# 3. Extraer las puntuaciones del PC1
PC1_IyD <- pca_IyD$x[, 1]

# 4. Crear un data frame con las puntuaciones del PC1 y los nombres de los subsectores
indice_IyD <- data.frame(
  Subsector = rownames(data_escalada),
  PC1_IyD = PC1_IyD
)

# Ordenar el data frame por la columna PC1 de mayor a menor
indice_IyD <- indice_IyD %>%
  arrange(desc(PC1_IyD))

######Dimensión Innovación----

# 1. Seleccionar las variables de la dimensión "Innovación"
data_Innovacion <- data_escalada %>%
  select(CE.3Produc_inmaterial, CE.7propor_ue_innovan, CE.8propor_act_coord, CE.9propor_per_innova)

# 2. Realizar el Análisis de Componentes Principales (ACP)
pca_Innovacion <- prcomp(data_Innovacion, center = FALSE, scale. = FALSE) # Ya están escaladas

# 3. Extraer las puntuaciones del PC1
PC1_Innovacion <- pca_Innovacion$x[, 1]

# 4. Crear un data frame con las puntuaciones del PC1 y los nombres de los subsectores
indice_Innovacion <- data.frame(
  Subsector = rownames(data_escalada),
  PC1_Innovacion = PC1_Innovacion
)

# Ordenar el data frame por la columna PC1 de mayor a menor
indice_Innovacion <- indice_Innovacion %>%
  arrange(desc(PC1_Innovacion))

######Dimensión Infraestructura Tecnológica y TIC ----
# 1. Seleccionar las variables de la dimensión "Infraestructura Tecnológica y TIC"
data_Infraestructura_TIC <- data_escalada %>%
  select(CE.4Intensidad_TIC, CE.6.1porcen_si_compu, CE.6.2porcent_si_internet, CE.12gasto_percap_soft)

# 2. Realizar el Análisis de Componentes Principales (ACP)
pca_Infraestructura_TIC <- prcomp(data_Infraestructura_TIC, center = FALSE, scale. = FALSE) # Ya están escaladas

# 3. Extraer las puntuaciones del PC1
PC1_Infraestructura_TIC <- pca_Infraestructura_TIC$x[, 1]

# 4. Crear un data frame con las puntuaciones del PC1 y los nombres de los subsectores
indice_Infraestructura_TIC <- data.frame(
  Subsector = rownames(data_escalada),
  PC1_Infraestructura_TIC = PC1_Infraestructura_TIC
)

# Ordenar el data frame por la columna PC1 de mayor a menor
indice_Infraestructura_TIC <- indice_Infraestructura_TIC %>%
  arrange(desc(PC1_Infraestructura_TIC))


######Dimensión Capital Humano Especializado en TIC y STEM----

# 1. Seleccionar las variables de la dimensión "Capital Humano Especializado en TIC y STEM"
data_CH_TIC_STEM <- data_escalada %>%
  select(ENOE.3porce_stem, ENOE.4porce_tic)

# 2. Realizar el Análisis de Componentes Principales (ACP)
pca_CH_TIC_STEM <- prcomp(data_CH_TIC_STEM, center = FALSE, scale. = FALSE) # Ya están escaladas

# 3. Extraer las puntuaciones del PC1
PC1_CH_TIC_STEM <- pca_CH_TIC_STEM$x[, 1]

# 4. Crear un data frame con las puntuaciones del PC1 y los nombres de los subsectores
indice_CH_TIC_STEM <- data.frame(
  Subsector = rownames(data_escalada),
  PC1_CH_TIC_STEM = PC1_CH_TIC_STEM
)

# Ordenar el data frame por la columna PC1 de mayor a menor
indice_CH_TIC_STEM<- indice_CH_TIC_STEM %>%
  arrange(desc(PC1_CH_TIC_STEM))
######Ponderación y creación del índice multidimensional----
# 1. Extraer la proporción de varianza explicada por el PC1 en cada dimensión
summary(pca_CH_Educ)
summary(pca_IyD)
summary(pca_Innovacion)
summary(pca_Infraestructura_TIC)
summary(pca_CH_TIC_STEM)

varianza_explicada_CH_Educ <- summary(pca_CH_Educ)$importance[2, 1]
varianza_explicada_IyD <- summary(pca_IyD)$importance[2, 1]
varianza_explicada_Innovacion <- summary(pca_Innovacion)$importance[2, 1]
varianza_explicada_Infraestructura_TIC <- summary(pca_Infraestructura_TIC)$importance[2, 1]
varianza_explicada_CH_TIC_STEM <- summary(pca_CH_TIC_STEM)$importance[2, 1]

# 2. Combinar los data frames de los PC1 por Subsector
indice_multidimensional_df <- indice_CH_Educ %>%
  left_join(indice_IyD, by = "Subsector") %>%
  left_join(indice_Innovacion, by = "Subsector") %>%
  left_join(indice_Infraestructura_TIC, by = "Subsector") %>%
  left_join(indice_CH_TIC_STEM, by = "Subsector")

# 3. Calcular el índice multidimensional ponderado
indice_multidimensional_df$Indice_Multidimensional <-
  indice_multidimensional_df$PC1_CH_Educ * varianza_explicada_CH_Educ +
  indice_multidimensional_df$PC1_IyD * varianza_explicada_IyD +
  indice_multidimensional_df$PC1_Innovacion * varianza_explicada_Innovacion +
  indice_multidimensional_df$PC1_Infraestructura_TIC * varianza_explicada_Infraestructura_TIC +
  indice_multidimensional_df$PC1_CH_TIC_STEM * varianza_explicada_CH_TIC_STEM


# Ordenar el data frame por la columna indice multidimensional de mayor a menor
indice_multidimensional_df<- indice_multidimensional_df %>%
  arrange(desc(Indice_Multidimensional)) %>%
  select(Subsector,Indice_Multidimensional, everything())

colnames(indice_multidimensional_df)

# Calcular el índice normalizado
indice_multidimensional_df <- indice_multidimensional_df %>%
  mutate(Indice_Multidimensional_Normalizado = (Indice_Multidimensional - min(Indice_Multidimensional)) / (max(Indice_Multidimensional) - min(Indice_Multidimensional)))

# Agregar la columna "Posición"
indice_multidimensional_df <- indice_multidimensional_df %>%
  mutate(Posicion = row_number()) %>%
  select(Posicion, Subsector, Indice_Multidimensional, Indice_Multidimensional_Normalizado, everything()) # Mover la columna Posición al principio

colnames(indice_multidimensional_df)

write.xlsx(indice_multidimensional_df,
           file = "tablas_texto/indice_multidimensional.xlsx",
           rowNames = FALSE)

#Mostrar las primeras filas del resultado
head(indice_multidimensional_df)

varianza_explicada_CH_Educ
varianza_explicada_IyD
varianza_explicada_Innovacion
varianza_explicada_Infraestructura_TIC
varianza_explicada_CH_TIC_STEM


#Indice por Selección Aleatoria de Variables----

# 1. Establecer la semilla para la replicabilidad
set.seed(123) # Puedes usar cualquier número entero

# 2. Obtener los nombres de todas las variables
todas_las_variables <- colnames(data_escalada)

# 3. Seleccionar aleatoriamente 8 variables
variables_aleatorias <- sample(todas_las_variables, 8, replace = FALSE)

# 4. Crear un nuevo data frame con las variables seleccionadas
data_escalada_aleatoria <- data_escalada %>%
  select(all_of(variables_aleatorias))

variables_aleatorias

# 5. Realizar el Análisis de Componentes Principales (ACP)
pca_aleatorio <- prcomp(data_escalada_aleatoria, center = FALSE, scale. = FALSE) # Ya están escaladas

# 6. Extraer las puntuaciones del PC1
PC1_aleatorio <- pca_aleatorio$x[, 1]

# 7. Crear un data frame con las puntuaciones del PC1 y los nombres de los subsectores
indice_aleatorio <- data.frame(
  Subsector = rownames(data_escalada),
  PC1_aleatorio = PC1_aleatorio
)


# Ordenar el data frame por la columna indice multidimensional de mayor a menor
indice_aleatorio<- indice_aleatorio %>%
  arrange(desc(PC1_aleatorio))


# Calcular el índice normalizado
indice_aleatorio <- indice_aleatorio %>%
  mutate(Indice_Aleatorio_Normalizado = (PC1_aleatorio - min(PC1_aleatorio)) / (max(PC1_aleatorio) - min(PC1_aleatorio)))

# Agregar la columna "Posicion"
indice_aleatorio <- indice_aleatorio %>%
  mutate(Posicion = row_number()) %>%
  select(Posicion, everything()) # Mover la columna Posicion al principio

colnames(indice_aleatorio)

write.xlsx(indice_aleatorio,
           file = "tablas_texto/indice_aleatorio.xlsx",
           rowNames = FALSE)


#Evaluación Visual de posiciones----
#Unir las tablas manteniendo todos los Subsectores de la tabla base
tabla_posiciones <- indice_intensidad_PC1_ordenado %>%
  left_join(indice_intensidad_PC1_7_ordenado %>% select(Subsector, Posicion_PC1_7 = Posicion), by = "Subsector") %>%
  left_join(indice_multidimensional_df %>% select(Subsector, Posicion_Multidimensional = Posicion), by = "Subsector") %>%
  left_join(indice_aleatorio %>% select(Subsector, Posicion_Aleatorio = Posicion), by = "Subsector") %>%
    select(Subsector, Posicion, Posicion_PC1_7, Posicion_Multidimensional, Posicion_Aleatorio)

colnames(tabla_posiciones)
tabla_posiciones<-tabla_posiciones %>% rename( Posicion_PC1= Posicion)
colnames(tabla_posiciones)

write.xlsx(tabla_posiciones,
           file = "tablas_texto/tabla_posiciones.xlsx",
           rowNames = FALSE)

#Gráfica ordenada
# 1. Preparar los datos para el gráfico (transformar el data frame)
data_grafico <- tabla_posiciones %>%
  pivot_longer(
    cols = starts_with("Posicion"),# Selecciona todas las columnas cuyo nombre empieza con "Posicion"
    names_to = "Indice",# El nombre de cada columna pasa a una nueva columna llamada "Indice"
    values_to = "Posicion" # Los valores de esas columnas pasan a una columna llamada "Posicion"
  ) %>%
  left_join(tabla_posiciones %>% select(Subsector, Posicion_PC1), by = "Subsector") # Añadir Posición_PC1

# Calcular la desviación estándar de los rankings
variacion_rankings <- tabla_posiciones %>%
  rowwise() %>%
  mutate(Desviacion_Estandar_Rank = sd(c(Posicion_PC1, Posicion_PC1_7, Posicion_Multidimensional, Posicion_Aleatorio))) %>%
  select(Subsector, Desviacion_Estandar_Rank)

#Combinación de Desviación estandar y gráfica de puntos
# Unir data_grafico con variacion_rankings
data_grafico <- data_grafico %>%
  left_join(variacion_rankings, by = "Subsector")

# Crear el gráfico de puntos con tamaño para Desviacion_Estandar_Rank
ggplot(data_grafico, aes(x = Posicion, y = reorder(Subsector, desc(Posicion_PC1)), color = Indice, size = Desviacion_Estandar_Rank)) +
  geom_point() +
  labs(
    title = "Comparación de la Posición de los Subsectores en los Diferentes Índices (Variabilidad en Tamaño)",
    x = "Posición",
    y = "Subsector SCIAN-México",
    color = "Índice",
    size = "Desviación Estándar de Rankings"
  ) +
  scale_color_manual(
    name = "Índice",
    values = c(
      "Posicion_PC1" = "gold",
      "Posicion_PC1_7" = "lightpink",
      "Posicion_Multidimensional" = "grey",
      "Posicion_Aleatorio" = "lightskyblue"
    ),
    limits = c("Posicion_PC1", "Posicion_PC1_7", "Posicion_Multidimensional", "Posicion_Aleatorio")
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 4),
    plot.title = element_text(hjust = 0.5)
  )

# Unir tabla_posiciones y variacion_rankings por Subsector
tabla_posiciones_con_variacion <- tabla_posiciones %>%
  left_join(variacion_rankings, by = "Subsector")

write.xlsx(tabla_posiciones_con_variacion,
           file = "tablas_texto/tabla_posiciones_con_variacion.xlsx",
           rowNames = FALSE)

library(dplyr)

# Calcular la desviación estándar promedio de los rankings
desviacion_estandar_promedio <- variacion_rankings %>%
  summarise(Desviacion_Estandar_Promedio = mean(Desviacion_Estandar_Rank))

mean(variacion_rankings$Desviacion_Estandar_Rank)

# Imprimir el resultado
print(desviacion_estandar_promedio)

# Ordenar variacion_rankings por Desviacion_Estandar_Rank de mayor a menor
variacion_rankings_ordenado <- variacion_rankings %>%
  arrange(desc(Desviacion_Estandar_Rank))

write.xlsx(variacion_rankings_ordenado ,
           file = "tablas_texto/variacion_rankings_ordenado .xlsx",
           rowNames = FALSE)

#Cluster----
library(dplyr)
library(ggplot2)
library(cluster)  # Para la función silhouette
library(NbClust) # Para determinar el número óptimo de clústeres
library(ggrepel) # Para evitar el solapamiento de etiquetas en el gráfico
library("pacman")
p_load (tidyverse, cluster, factoextra)

# 1. Determinar el número óptimo de clústeres (k)

#Método de la Silueta
set.seed(123)
silhouette_avg <- c()
for (i in 2:10) {
  km_res <- kmeans(indice_intensidad_PC1_ordenado$PC1, centers = i, nstart = 100)
  sil <- silhouette(km_res$cluster, dist(data.frame(indice_intensidad_PC1_ordenado$PC1)))
  silhouette_avg[i] <- mean(sil[, 3])
}
plot(2:10, silhouette_avg[2:10], type = "b", xlab = "Número de Clústeres", ylab = "Ancho de silueta promedio")

# Elegir el número de clústeres que maximiza el ancho de silueta promedio
# Basado en los métodos anteriores, elegir el número óptimo de clústeres es 2 y 9
# 1. El número de clústeres elegido
k <- 2
# 2. Realizar el agrupamiento k-means
set.seed(123) # Para reproducibilidad
km.res <- kmeans(indice_intensidad_PC1_ordenado$PC1, centers = k, nstart = 100)

# 3. Analizar los resultados
# Asignaciones de clúster
indice_intensidad_PC1_ordenado$Cluster <- factor(km.res$cluster)
print("Asignaciones de Clúster:")
print(table(indice_intensidad_PC1_ordenado$Cluster))

# Centros de los clústeres
print("\nCentros de los Clústeres:")
print(km.res$centers)

# 4. Visualizar los resultados
ggplot(indice_intensidad_PC1_ordenado, aes(x = PC1, y = 0, color = Cluster, label = Subsector)) + # y = 0 para alinear los puntos
  geom_point() +
  geom_text_repel(size = 3, box.padding = 0.5, point.padding = 0.5) +
  labs(
    title = paste("Agrupamiento K-means (k =", k, ")"),
    x = "PC1 (Índice de Intensidad del Conocimiento)",
    y = "", # Sin etiqueta para el eje y
    color = "Clúster"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) # Ocultar marcas y texto del eje y

sectores_top_2cluster<-as.data.frame(indice_intensidad_PC1_ordenado%>% filter(Cluster == 1) %>% select(Posicion, Subsector, PC1, Indice_Normalizado))

sectores_bottom_2cluster<-as.data.frame(indice_intensidad_PC1_ordenado%>% filter(Cluster == 2) %>% select(Subsector, PC1, Indice_Normalizado))

#Guardar la tabla
write.xlsx(sectores_top_2cluster, "tablas_texto/sectores_top_2cluster.xlsx", sheetName = "Sheet1",
           colnames = TRUE, rownames = FALSE, append = FALSE)


#Nueve Clusters----
# Basado en los métodos anteriores, elegir el número óptimo de clústeres es 2 y 9
# 1. Reemplazar con el número de clústeres elegido
k <- 9
# 2. Realizar el agrupamiento k-means
set.seed(123) # Para reproducibilidad
km.res <- kmeans(indice_intensidad_PC1_ordenado$PC1, centers = k, nstart = 100)

# 3. Analizar los resultados

# Asignaciones de clúster
indice_intensidad_PC1_ordenado$Cluster <- factor(km.res$cluster)
print("Asignaciones de Clúster:")
print(table(indice_intensidad_PC1_ordenado$Cluster))

# Centros de los clústeres
print("\nCentros de los Clústeres:")
print(km.res$centers)

# 4. Visualizar los resultados
ggplot(indice_intensidad_PC1_ordenado, aes(x = PC1, y = 0, color = Cluster, label = Subsector)) + # y = 0 para alinear los puntos
  geom_point() +
  geom_text_repel(size = 3, box.padding = 0.5, point.padding = 0.5) +
  labs(
    title = paste("Agrupamiento K-means (k =", k, ")"),
    x = "PC1 (Índice de Intensidad del Conocimiento)",
    y = "", # Sin etiqueta para el eje y
    color = "Clúster"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) # Ocultar marcas y texto del eje y

sectores_top_10cluster<-as.data.frame(indice_intensidad_PC1_ordenado%>%  filter(Cluster %in% c("6", "7", "9", "2")) %>%    select(Posicion, Subsector, PC1, Indice_Normalizado, Cluster))



sectores_bottom_10cluster<-as.data.frame(indice_intensidad_PC1_ordenado%>% filter(Cluster %in% c("5", "1", "4")) %>% select(Subsector, PC1, Indice_Normalizado))

#Guardar la tabla
write.xlsx(sectores_top_10cluster, "tablas_texto/sectores_top_10cluster.xlsx", sheetName = "Sheet1",
           colnames = TRUE, rownames = FALSE, append = FALSE)


indice_intensidad_PC1_ordenado <- indice_intensidad_PC1_ordenado %>%
  mutate(
    top_cluster = case_when(
      Cluster == "6" ~ "1",
      Cluster == "7" ~ "2",
      Cluster == "9" ~ "3",
      Cluster == "2" ~ "4",
      Cluster == "3" ~ "5",
      Cluster == "8" ~ "6",
      Cluster == "4" ~ "7",
      Cluster == "1" ~ "8",
      Cluster == "5" ~ "9",
      TRUE ~ as.character(Cluster)  # Default case
    )
  )

#Asignar Valor de "Intensidad del Conocimiento" a cada grupo de industrias
indice_intensidad_PC1_ordenado <- indice_intensidad_PC1_ordenado %>%
  group_by(Cluster) %>%
  mutate(Media_Indice_Cluster = mean(Indice_Normalizado, na.rm = TRUE)) %>%
  ungroup()

#Guardar el archivo
write.xlsx(indice_intensidad_PC1_ordenado, "tablas_texto/sectores_intensivos_cluster.xlsx", sheetName = "Sheet1",
           colnames = TRUE, rownames = FALSE, append = FALSE)

# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: #Auditar los valores perdidos o ausentes en variables POT, VACB e INGRESOS con más de 3 UE en
#Data de Municipios-Metrópolis 4 dígitos
# ==============================================================================

#Limpiar R
rm(list=ls (all=T))

### Paquetes  ----
library(pacman)
p_load(haven,      # Paquete para importar archivos de Stata, SAS y SPSS
       readxl,     # Paquete para importar archivos de Excel
       tidyverse)  # Metapaquete que incluye readr, paquete para importar achivos de texto plano

### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen = 999) # Prevenir notación científica

### Importar base de datos ----
# Cargar Datas----

municipios_metro_con_Rama_anio <-read_csv("datos/4digitos/data_municipios_metro_con_Rama_anio.csv")


colnames(municipios_metro_con_Rama_anio)

municipio_rama<-municipios_metro_con_Rama_anio %>% filter(`Año Censal` %in% c("2003", "2018"))%>%
  select(`Año Censal`, NOM_ZM, NOM_MUN, CVE_MUN,`Actividad económica`, `UE Unidades económicas`, `H001A Personal ocupado total`, `A131A Valor agregado censal bruto (millones de pesos)`, `M000A Total de ingresos por suministro de bienes y servicios (millones de pesos)`) %>%
  rename(ue=`UE Unidades económicas`, pot=`H001A Personal ocupado total`, vacb= `A131A Valor agregado censal bruto (millones de pesos)`, ingresos= `M000A Total de ingresos por suministro de bienes y servicios (millones de pesos)`
      )

# Extraer los primeros 4 dígitos de la variable Rama y crear la nueva variable cve_sector
municipio_rama$cve_sector <- substr(municipio_rama$`Actividad económica`, 6, 9)

#Integrar la Intensidad del Conocimiento con el índice creado----
indice_pc1 <- read_excel( "tablas_texto/ramas_industrias_manuf_4digitos.xlsx")

# Extraer los primeros cuatro dígitos de la variable Rama y crear la nueva variable cve_sector
indice_pc1$cve_sector <- substr(indice_pc1$Subsector, 1, 4)

mun_rama_intensidad <- merge(municipio_rama, indice_pc1 ,
                                             by.x = c("cve_sector"),
                                             by.y = c("cve_sector"),
                                             all.x = TRUE)
mun_rama_intensidad<-mun_rama_intensidad %>%
  filter(top_cluster %in% c("1", "2", "3", "4")) %>%
  select(`Año Censal`, NOM_ZM, NOM_MUN, CVE_MUN, `Actividad económica`, cve_sector, ue, pot, vacb, ingresos, top_cluster)

colnames(mun_rama_intensidad)


library(dplyr)

resultado <- mun_rama_intensidad %>%
  filter(
    ue > 3,
    is.na(pot) | pot == 0 |
      is.na(vacb) | vacb == 0 |
      is.na(ingresos) | ingresos == 0
  ) %>%
  select(`Año Censal`,NOM_ZM,`Actividad económica`,  NOM_MUN, ue, pot, vacb, ingresos, top_cluster) %>% arrange(desc(ue))

#Evaluar que Ramas tienen más veces valores ausencia de valores.
conteo_ramas_sin_dato <- resultado  %>%
  group_by(`Actividad económica`, top_cluster) %>%
  summarise(
    frecuencia = n(),
    .groups = "drop"
  ) %>% select(`Actividad económica`, frecuencia, top_cluster) %>%
  arrange(desc(frecuencia))

library(writexl)
write_xlsx(conteo_ramas_sin_dato, "tablas_texto/4_digitos/conteo_ramas_sin_dato.xlsx")

#Tabla comparativa por año
conteo_ramas_sin_dato_2003 <- resultado %>% filter(`Año Censal` == "2003")  %>%
  group_by(`Actividad económica`, top_cluster) %>%
  summarise(
    frecuencia = n(),
    .groups = "drop"
  ) %>% select(`Actividad económica`, frecuencia, top_cluster) %>%
  arrange(desc(frecuencia))

conteo_ramas_sin_dato_2018 <- resultado %>% filter(`Año Censal` == "2018")  %>%
  group_by(`Actividad económica`, top_cluster) %>%
  summarise(
    frecuencia = n(),
    .groups = "drop"
  ) %>% select(`Actividad económica`, frecuencia, top_cluster) %>%
  arrange(desc(frecuencia))

library(dplyr)

tabla_comparativa <- conteo_ramas_sin_dato_2003 %>%
  rename(`Frecuencia 2003` = frecuencia) %>%
  full_join(
    conteo_ramas_sin_dato_2018 %>%
      rename(`Frecuencia 2018` = frecuencia),
    by = c("Actividad económica", "top_cluster")
  ) %>%
  mutate(
    Total_Municipios_Sin_Dato = rowSums(
      select(., `Frecuencia 2003`, `Frecuencia 2018`),
      na.rm = TRUE
    )
  ) %>%
  arrange(desc(Total_Municipios_Sin_Dato)) %>%
  select(`Actividad económica`, Total_Municipios_Sin_Dato, `Frecuencia 2003`, `Frecuencia 2018`, top_cluster)
#Guardar la Tabla Comparativa para el texto
write_xlsx(tabla_comparativa, "tablas_texto/4_digitos/tabla_comparativa_ramas_sin_dato.xlsx")


#Evaluar por ZM
conteo_metropolis_actividad <- resultado  %>%
  group_by(NOM_ZM, `Actividad económica`, top_cluster) %>%
  summarise(
    frecuencia_total = n(),
    .groups = "drop"
  ) %>% select(NOM_ZM, `Actividad económica`, frecuencia_total, top_cluster) %>%
  arrange(desc(frecuencia_total))



#Evaluar por ZM 2003
conteo_metropolis_actividad_2003 <- resultado %>%
  filter(`Año Censal`== "2003") %>%
  group_by(NOM_ZM, `Actividad económica`, top_cluster) %>%
  summarise(
    frecuencia_2003 = n(),
    .groups = "drop"
  ) %>% select(NOM_ZM, `Actividad económica`, frecuencia_2003, top_cluster) %>%
  arrange(desc(frecuencia_2003))

#Evaluar por ZM 2018
conteo_metropolis_actividad_2018 <- resultado %>%
  filter(`Año Censal`== "2018") %>%
  group_by(NOM_ZM, `Actividad económica`, top_cluster) %>%
  summarise(
    frecuencia_2018 = n(),
    .groups = "drop"
  ) %>% select(NOM_ZM, `Actividad económica`, frecuencia_2018, top_cluster) %>%
  arrange(desc(frecuencia_2018))

#Tabla Comparativa Metropolis
tabla_comparativa_metros <- conteo_metropolis_actividad_2003 %>%
  full_join(
    conteo_metropolis_actividad_2018,
    by = c("NOM_ZM","Actividad económica", "top_cluster")
  ) %>%
  mutate(
    Total_Municipios_Sin_Dato = rowSums(
      select(., frecuencia_2003, frecuencia_2018),
      na.rm = TRUE
    )
  ) %>%
  arrange(desc(Total_Municipios_Sin_Dato)) %>%
  select(NOM_ZM,`Actividad económica`, Total_Municipios_Sin_Dato, frecuencia_2003, frecuencia_2018, top_cluster)
#Guardar la Tabla Comparativa para el texto
write_xlsx(tabla_comparativa, "tablas_texto/4_digitos/tabla_comparativa_metros_ramas_sin_dato.xlsx")

resumen_zm <- tabla_comparativa_metros %>%
  group_by(NOM_ZM) %>%
  summarise(
    total_2003 = sum(frecuencia_2003, na.rm = TRUE),
    total_2018 = sum(frecuencia_2018, na.rm = TRUE),
    total_general = sum(Total_Municipios_Sin_Dato, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_general))


#Evaluar por Zonas Metropolitanas, sus municipios y las Ramas Industrias Manufactureras

resultado_todos <- mun_rama_intensidad %>%
  select(`Año Censal`,NOM_ZM,`Actividad económica`,  NOM_MUN, ue, pot, vacb, ingresos, top_cluster) %>% arrange(desc(ue))

#Crear Función que compara la ZM
comparar_zm <- function(zona_metropolitana, datos) {

  # Filtrar la ZM en 2003
  zm_2003 <- datos %>%
    filter(NOM_ZM == zona_metropolitana, `Año Censal` == "2003") %>%
    select(NOM_ZM, NOM_MUN, `Actividad económica`, ue, pot, top_cluster) %>%
    mutate(presente_2003 = TRUE)

  # Filtrar la ZM en 2018
  zm_2018 <- datos %>%
    filter(NOM_ZM == zona_metropolitana, `Año Censal` == "2018") %>%
    select(NOM_ZM, NOM_MUN, `Actividad económica`, ue, pot, top_cluster) %>%
    mutate(presente_2018 = TRUE)

  # Comparar municipios-actividades entre 2003 y 2018
  comparacion <- full_join(
    zm_2003,
    zm_2018,
    by = c("NOM_ZM", "NOM_MUN", "Actividad económica", "top_cluster"),
    suffix = c("_2003", "_2018")
  ) %>%
    mutate(
      presente_2003 = ifelse(is.na(presente_2003), FALSE, TRUE),
      presente_2018 = ifelse(is.na(presente_2018), FALSE, TRUE)
    ) %>%
    arrange(NOM_MUN, `Actividad económica`)

  return(comparacion)
}
table(resultado$NOM_ZM) %>% tibble()
#Comparación de Guadalajara
comparacion_guadalajara <- comparar_zm("Guadalajara", resultado)

#Comparación "Ciudad Victoria"
Ciudad_Victoria_UE3 <- comparar_zm("Ciudad Victoria", resultado)
Ciudad_Victoria_UE_todas <- comparar_zm("Ciudad Victoria", resultado_todos )

#Comparación Juárez
Juárez_UE3 <- comparar_zm("Ju\xe1rez", resultado)
Juárez_UE_todas <- comparar_zm("Ju\xe1rez", resultado_todos )

#Comparación Tampico
table(resultado$NOM_ZM)
Tampico_UE3 <- comparar_zm("Tampico", resultado)
Tampico_UE_todas <- comparar_zm("Tampico", resultado_todos )

#Comparación Poza Rica
table(resultado$NOM_ZM)
Poza_Rica_UE3 <- comparar_zm("Poza Rica", resultado)
Poza_Rica_UE_todas <- comparar_zm("Poza Rica", resultado_todos )

#Comparación Delicias
table(resultado$NOM_ZM)
Delicias_UE3 <- comparar_zm("Delicias", resultado)
Delicias_UE_todas <- comparar_zm("Delicias", resultado_todos )

#Comparación Nuevo Laredo
table(resultado$NOM_ZM)
Nuevo_Laredo_UE3 <- comparar_zm("Nuevo Laredo", resultado)
Nuevo_Laredo_UE_todas <- comparar_zm("Nuevo Laredo", resultado_todos )

#Comparación Matamoros
table(resultado$NOM_ZM)
Matamoros_UE3 <- comparar_zm("Matamoros", resultado)
Matamoros_UE_todas <- comparar_zm("Matamoros", resultado_todos )

#Comparación Chetumal
table(resultado$NOM_ZM)
Chetumal_UE3 <- comparar_zm("Chetumal", resultado)
Chetumal_UE_todas <- comparar_zm("Chetumal", resultado_todos )

#Comparación Monterrey
table(resultado$NOM_ZM)
Monterrey_UE3 <- comparar_zm("Monterrey", resultado)
Monterrey_UE_todas <- comparar_zm("Monterrey", resultado_todos )

#Comparación La Paz
table(resultado$NOM_ZM)
La_Paz_UE3 <- comparar_zm("La Paz", resultado)
La_Paz_UE_todas <- comparar_zm("La Paz", resultado_todos )

#Comparación Mèrida
table(resultado$NOM_ZM)
Merida_UE3 <- comparar_zm("M\xe9rida", resultado)
Merida_UE_todas <- comparar_zm("M\xe9rida", resultado_todos )

#Comparación Ensenada
table(resultado$NOM_ZM)
Ensenada_UE3 <- comparar_zm("Ensenada", resultado)
Ensenada_UE_todas <- comparar_zm("Ensenada", resultado_todos )

#Comparación Ocotlàn
table(resultado$NOM_ZM)
Ocotlan_UE3 <- comparar_zm("Ocotl\xe1n", resultado)
Ocotlan_UE_todas <- comparar_zm("Ocotl\xe1n", resultado_todos )

#Comparación Villahermosa
table(resultado$NOM_ZM)
Villahermosa_UE3 <- comparar_zm("Villahermosa", resultado)
Villahermosa_UE_todas <- comparar_zm("Villahermosa", resultado_todos )

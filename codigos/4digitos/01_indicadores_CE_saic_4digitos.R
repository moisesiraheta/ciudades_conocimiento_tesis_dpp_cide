# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: Procesamiento Censos Económicos a 4 dígitos del SCIAN-México 2018
# ==============================================================================

#Procesamiento Censos Económicos a 4 dígitos del SCIAN
#Limpiar R
rm(list=ls (all=T))
#Instalar Paquetes
library(pacman)
p_load(haven,      # Paquete para importar archivos de Stata, SAS y SPSS
       readxl,     # Paquete para importar archivos de Excel
       tidyverse,# Metapaquete que incluye readr, paquete para importar achivos de texto plano
       dplyr, #Paquete para limpieza de bases de datos
       openxlsx, #Pquete para guardar archivos en formato excel
       stats)  #Para análisis multivariado como ACP

# Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen = 999) # Prevenir notación científica

#Cargar datos del Censo Económico 2019 obtenidos del SAIC----

#Código replicable-----
#Para demostrar el correcto procesamiento de la data del SAIC, descargo la misma información directamente de Internet.
#Cargar la base de datos correspondiente al Censo Económico 2019 a nivel nacional desde la página del INEGI.

# URL del archivo ZIP del Censo Económico 2019
url_zip <- "https://www.inegi.org.mx/contenidos/programas/ce/2019/Datosabiertos/ce2019_nac_csv.zip"

# Crear un archivo temporal para guardar el ZIP descargado del CE2019
temp_zip <- tempfile(fileext = ".zip")
options(timeout = 400) #Por lo pesado del archivo, ampliar a 300 segundos el tiempo que R puede usar para descargar
#Descargar el archivo ZIP con .csv desde la URL
download.file(url_zip, destfile = temp_zip, mode = "wb")
options(timeout = 60) #restablecer el timeout a su valor original.
#Listar los archivos dentro del ZIP.csv
archivos_zip <- unzip(temp_zip, list = TRUE)
archivos_zip$Name #Muestra el nombre de los nombres de los archivos dentro del zip

#Crear un archivo temporal para el archivo "conjunto_de_datos/ce2019_nac.csv" (directamente con el nombre)
temp <- file.path(tempdir(), "conjunto_de_datos/ce2019_nac.csv")
#Extraer el archivo CSV en el directorio temporal (directamente con el nombre)
unzip(temp_zip, files = "conjunto_de_datos/ce2019_nac.csv", exdir = tempdir())
# Leer el archivo CSV usando read_csv()
ce2019 <- read_csv(temp)
#Revisar la estructura del archivo
str(ce2019)

#i)Seleccionar las variables de interés, ii) asignar el nombre a las variables y iii)filtrar las observaciones de sectores a 4 dgitos del SCIAN.
datareplica<-ce2019 %>% select(CODIGO, ID_ESTRATO, UE, K060A, H001A, H203A, Q400A,  K000A, A111A, A131A,  Q000A, K820A) %>%
  rename(cve_sector = CODIGO,
         serv_pro_cientyt = K060A,
         personal_total = H001A,
         personal_direc_y_admon = H203A,
         capital_comp_perif = Q400A,
         insumos = K000A,
         prod_bruta = A111A,
         vacb = A131A,
         capital_fijo = Q000A,
         serv_comunicacion = K820A
  ) %>%
  filter(
    nchar(cve_sector) == 4) %>%
  filter(is.na(ID_ESTRATO)) %>%
  select(
    -ID_ESTRATO)


#Cálculo de indicadores del CE-2018 SAIC----
censo_eco<-datareplica
# Calcular la variable CE.1ProporcionPACD
censo_eco <- censo_eco %>%
  mutate(CE.1ProporcionPACD = personal_direc_y_admon / personal_total)
head(censo_eco$CE.1ProporcionPACD)# Verificar la nueva variable

#Calcular la variable CE.2IntensidadSP
censo_eco<-censo_eco %>%
  mutate(CE.2IntensidadSP = serv_pro_cientyt/personal_total)
head(censo_eco$CE.2IntensidadSP) # Verificar la nueva variable

#Calcular la variable CE.3Produc_inmaterial
#Calcular la productividad labora y la eficiencia del capital
censo_eco<- censo_eco %>%
  mutate(produc_laboral = vacb/personal_total) #Productividad Laboral
censo_eco <- censo_eco%>%
  mutate(eficiencia_capital = vacb/capital_fijo) #Eficiencia del capital
#Segundo calcular la produc_inmaterial
censo_eco <- censo_eco%>%
  mutate(CE.3Produc_inmaterial = produc_laboral * eficiencia_capital )
head(censo_eco$CE.3Produc_inmaterial)

#Calcular la variable CE.4Intensidad_TIC
censo_eco <- censo_eco %>%
  mutate(CE.4Intensidad_TIC = (capital_comp_perif + serv_comunicacion)/personal_total)
head(censo_eco$CE.4Intensidad_TIC)


#sacar el nombre del sector
archivos_zip$Name #Muestra el nombre de los nombres de los archivos dentro del zip

#Crear un archivo temporal para el archivo "conjunto_de_datos/ce2019_nac.csv" (directamente con el nombre)
temp <- file.path(tempdir(), "catalogos/tc_codigo_actividad.csv")
#Extraer el archivo CSV en el directorio temporal (directamente con el nombre)
unzip(temp_zip, files = "catalogos/tc_codigo_actividad.csv", exdir = tempdir())
# Leer el archivo CSV usando read_csv()
catalogo_actividad <- read_csv(temp)

rama<-catalogo_actividad %>%
  filter(CLASIFICADOR_CODIGO == "Rama,")

rama<-rama %>%
  rename(cve_sector=CODIGO) %>%
  rename(nombre_sector=DESC_CODIGO) %>%
  select(cve_sector, nombre_sector)

censo_eco <- left_join(censo_eco, rama, by = "cve_sector")


#Seleccionar los indicadores creados con los Censos Económicos

ce_4d_indicadores<-censo_eco %>%
  select(cve_sector,
         nombre_sector,
         CE.1ProporcionPACD,
         CE.2IntensidadSP,
         CE.3Produc_inmaterial,
         CE.4Intensidad_TIC)

#Guardar la Lista de Sectores Económicos a 4 dígitos del SCIAN existentes en los Censos Económicos----
sectores_scian4d<-ce_4d_indicadores
write.xlsx(sectores_scian4d,
           file = "datos/4digitos/sectores_scian4d.xlsx",
           rowNames = FALSE)

#Guardar la base con los indicadores de los CE2018-SAIC----
#Eliminar los sectores cuya Unidad de Observación no es el Establecimiento

no_establecimiento<- c("212", "1125", "1141", "211", "2211", "2212", "23", "481", "482", "483", "484", "485", "486", "487", "491", "4921", "52", "2213")#Sectores que no son establecimientos.

#Eliminar los Sectores (2 digitos)
ce_4d_indicadores<-ce_4d_indicadores %>%
  mutate(dos_digitos =  str_sub(cve_sector, 1,2))

ce_4d_indicadores<- ce_4d_indicadores %>%
  filter(!dos_digitos %in% no_establecimiento)#Eliminar subsectores cuya unidad de observación no fue el establecimiento.

#Eliminar los SubSectores (3 digitos)

ce_4d_indicadores<-ce_4d_indicadores %>%
  mutate(tres_digitos =  str_sub(cve_sector, 1,3))

ce_4d_indicadores<- ce_4d_indicadores %>%
  filter(!tres_digitos %in% no_establecimiento)

#Eliminar las Ramas (4 digitos)
ce_4d_indicadores<-ce_4d_indicadores %>%
  mutate(cuatro_digitos =  str_sub(cve_sector, 1,4))

ce_4d_indicadores<- ce_4d_indicadores %>%
  filter(!cuatro_digitos %in% no_establecimiento)


write.xlsx(ce_4d_indicadores,
           file = "datos/4digitos/ce_4d_indicadores.xlsx",
           rowNames = FALSE)



#Nota Final----
#Esta base contienes:
#Observaciones de sectores económicos a 4 dígitos SCIAN en el año 2018 cuya unidad de observación es el establecimiento.
#Variables y métricas construidas asociadas al conocimiento.


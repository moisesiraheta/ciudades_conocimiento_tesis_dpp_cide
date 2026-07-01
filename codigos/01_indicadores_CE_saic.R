# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: Procesamiento Censos Económicos a 3 dígitos del SCIAN-México 2018
# ==============================================================================

# Limpiar entorno de trabajo para asegurar que no existan variables de sesiones previas
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

#1 Cargar datos del Censo Económico 2019 obtenidos del SAIC----
censos_eco<- read.csv("datos/SAIC_Exporta_202541_14622363.csv")
colnames(censos_eco)#Reviso el nombre de las variables
str(censos_eco)#Reviso la estructura de la base.

#2 Limpieza de la data----
#Renombrar variables
censos_eco_renombrado <- censos_eco %>%
  rename(
    anio_censal = Año.Censal,
    cobert_geo = Entidad ,
    nombre_sector = Actividad.económica,
    serv_pro_cientyt = K060A.Contratación.de.servicios.profesionales..científicos.y.técnicos..millones.de.pesos.,
    personal_total = H001A.Personal.ocupado.total,
    personal_direc_y_admon = H203A.Personal.administrativo..contable.y.de.dirección.total,
    capital_comp_perif = Q400A.Acervo.total.de.equipo.de.cómputo.y.periféricos..millones.de.pesos.,
    insumos = K000A.Total.de.gastos.por.consumo.de.bienes.y.servicios..millones.de.pesos.,
    prod_bruta = A111A.Producción.bruta.total..millones.de.pesos.,
    vacb= A131A.Valor.agregado.censal.bruto..millones.de.pesos.,
    capital_fijo = Q000A.Acervo.total.de.activos.fijos..millones.de.pesos.,
    serv_comunicacion = K820A.Gastos.por.servicios.de.comunicación...millones.de.pesos.
  )
#Limpieza observaciones----
#De la base, quedarme con las observaciones que en:
#nombre_sector, tiene la palabra Subsector
censos_eco_filtrado <- censos_eco_renombrado %>%
  filter(str_detect(nombre_sector, "Subsector"))#Detecta la palabra "Subsector" en la variable nombre_sector
#Eliminar la palabra "Subsector "
censos_eco_filtrado$nombre_sector <- gsub("Subsector ", "", censos_eco_filtrado$nombre_sector) #gsub() remplaza "Subsector ", por una cadena vacía "" de la variable nombre_sector

#Creación de la variable cve_sector
censos_eco_filtrado$cve_sector <- substr(censos_eco_filtrado$nombre_sector, 1, 3)#Sustrae los caracteres del 1 al 3 de la variable.

#Dejar limpio el nombre del sector
censos_eco_filtrado$nombre_sector <- substr(censos_eco_filtrado$nombre_sector, 5, nchar(censos_eco_filtrado$nombre_sector)) #Toma la cadena de texto desde la posición 5 hasta donde termine. nchar() devuelve la extensión de una cadena de texto.

#Seleccionar solo variables de interés----
censos_eco_filtrado<-censos_eco_filtrado %>%
  select(cve_sector, nombre_sector, prod_bruta, vacb, insumos, serv_pro_cientyt, serv_comunicacion, personal_total, personal_direc_y_admon, capital_fijo, capital_comp_perif )


# Revisar la existencia de valores perdidos, no registrados o ceros----
conteo_valores <- censos_eco_filtrado %>%
  summarise(
    perdidos_serv_pro_cientyt = sum(is.na(serv_pro_cientyt)),
    ceros_serv_pro_cientyt = sum(serv_pro_cientyt == 0, na.rm = TRUE),
    perdidos_personal_total = sum(is.na(personal_total)),
    ceros_personal_total = sum(personal_total == 0, na.rm = TRUE),
    perdidos_personal_direc_y_admon = sum(is.na(personal_direc_y_admon)),
    ceros_personal_direc_y_admon = sum(personal_direc_y_admon == 0, na.rm = TRUE),
    perdidos_capital_comp_perif = sum(is.na(capital_comp_perif)),
    ceros_capital_comp_perif = sum(capital_comp_perif == 0, na.rm = TRUE),
    perdidos_insumos = sum(is.na(insumos)),
    ceros_insumos = sum(insumos == 0, na.rm = TRUE),
    perdidos_prod_bruta = sum(is.na(prod_bruta)),
    ceros_prod_bruta = sum(prod_bruta == 0, na.rm = TRUE),
    perdidos_capital_fijo = sum(is.na(capital_fijo)),
    ceros_capital_fijo = sum(capital_fijo == 0, na.rm = TRUE),
    perdidos_serv_comunicacion = sum(is.na(serv_comunicacion)),
    ceros_serv_comunicacion = sum(serv_comunicacion == 0, na.rm = TRUE)
  )
print(conteo_valores)
#1 Subsector tiene cero, el Banco Central. Lo cuales no afecta, pues será eliminado después, al no ser un Subsector con Unidad de Observación de tipo "Establecimiento"
#No hay valores perdidos.



#2.1 Código replicable con data de enlace de Internet-----
#Para permitir replicabilidad y verificar el correcto procesamiento de la data del SAIC, descargo la misma información directamente de Internet.
#Cargar la base de datos correspondiente al Censo Económico 2019 a nivel nacional desde la página del INEGI.

# URL del archivo ZIP del Censo Económico 2019
url_zip <- "https://www.inegi.org.mx/contenidos/programas/ce/2019/Datosabiertos/ce2019_nac_csv.zip"

# Especifica un nombre para el archivo ZIP en directorio de trabajo
dest_file_name <- "datos/ce2019_nac_csv.zip" # Define un nombre de archivo
options(timeout = 400) #Por lo pesado del archivo, ampliar a 400 segundos el tiempo que R puede usar para descargar
# Descargar el archivo ZIP con .csv desde la URL
# Definir el directorio para extraer los archivos.
extraction_directory <- "datos"

download.file(url_zip, destfile = dest_file_name, mode = "wb", method = "libcurl")
options(timeout = 60) #restablecer el timeout a su valor original.

# Descomprimir el archivo ZIP.
# unzip() extrae los contenidos manteniendo la estructura de carpetas interna del ZIP.
unzip(dest_file_name, exdir = extraction_directory)
#Muestra el nombre de los nombres de los archivos dentro del zip
archivos_zip<-unzip(dest_file_name, list = TRUE)
archivos_zip$Name
# Ruta completa al archivo CSV
csv_to_read_path <- file.path(extraction_directory, "conjunto_de_datos", "ce2019_nac.csv")
# Leer el archivo CSV usando read_csv()
ce2019 <- read_csv(csv_to_read_path)
#Revisar la estructura del archivo
str(ce2019)

#i)Seleccionar las variables de interés, ii) asignar el nombre a las variables y iii)filtrar las observaciones de sectores a 3 dgitos del SCIAN.
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
    nchar(cve_sector) == 3) %>%
  filter(is.na(ID_ESTRATO)) %>%
  select(
    -ID_ESTRATO)

#Comparo la coincidencia de las variables del SAIC y del Enlace directo del INEGI.
censos_eco_filtrado$serv_pro_cientyt == datareplica$serv_pro_cientyt
censos_eco_filtrado$personal_total == datareplica$personal_total
censos_eco_filtrado$personal_direc_y_admon == datareplica$personal_direc_y_admon
censos_eco_filtrado$capital_comp_perif == datareplica$capital_comp_perif
censos_eco_filtrado$insumos == datareplica$insumos
censos_eco_filtrado$prod_bruta == datareplica$prod_bruta
censos_eco_filtrado$capital_fijo == datareplica$capital_fijo
censos_eco_filtrado$serv_comunicacion == datareplica$serv_comunicacion
censos_eco_filtrado$vacb == datareplica$vacb
#Todo coincide. Puedo continuar.

#3 Sectores Económicos a 2 dígitos para el texto
#Muestra el nombre de los archivos dentro del zip
archivos_zip<-unzip(dest_file_name, list = TRUE)
archivos_zip$Name
# Ruta completa al archivo CSV
csv_to_read_path <- file.path(extraction_directory, "catalogos", "tc_codigo_actividad.csv")
# Leer el archivo CSV usando read_csv()
codigo_actividad <- read_csv(csv_to_read_path)
#Revisar la estructura del archivo
sectores_2d<- codigo_actividad %>%
  filter(CLASIFICADOR_CODIGO == "Sector,") %>%
  select(-CLASIFICADOR_CODIGO)

library(openxlsx)
write.xlsx(sectores_2d,
           file = "tablas_texto/sectores_2d.xlsx",
           rowNames = FALSE)

#4 INDICADORE CE-2018----
# Cálculo de indicadores del CE-2018 SAIC
censo_eco<-censos_eco_filtrado

# Calcular el indicador CE.1ProporcionPACD----
censo_eco <- censo_eco %>%
  mutate(CE.1ProporcionPACD = personal_direc_y_admon / personal_total)
head(censo_eco$CE.1ProporcionPACD)# Verificar la nueva variable

#Calcular el indicador CE.2IntensidadSP----
censo_eco<-censo_eco %>%
  mutate(CE.2IntensidadSP = serv_pro_cientyt/personal_total)
head(censo_eco$CE.2IntensidadSP) # Verificar la nueva variable

#Calcular el indicador CE.3Produc_inmaterial----
#Calcular la productividad laboral y la eficiencia del capital
censo_eco<- censo_eco %>%
  mutate(produc_laboral = vacb/personal_total) #Productividad Laboral
censo_eco <- censo_eco%>%
  mutate(eficiencia_capital = vacb/capital_fijo) #Eficiencia del capital
#Segundo calcular la produc_inmaterial
censo_eco <- censo_eco%>%
  mutate(CE.3Produc_inmaterial = produc_laboral * eficiencia_capital )
head(censo_eco$CE.3Produc_inmaterial)

#Calcular el indicador CE.4Intensidad_TIC----
censo_eco <- censo_eco %>%
  mutate(CE.4Intensidad_TIC = (capital_comp_perif + serv_comunicacion)/personal_total)
head(censo_eco$CE.4Intensidad_TIC)

#Seleccionar los indicadores creados con los datos del SAIC de los Censos Económicos

ce_3d_indicadores<-censo_eco %>%
  select(cve_sector,
         nombre_sector,
         CE.1ProporcionPACD,
         CE.2IntensidadSP,
         CE.3Produc_inmaterial,
         CE.4Intensidad_TIC)

#Guardar la Lista de Sectores Económicos a 3 dígitos del SCIAN existentes en los Censos Económicos----
sectores_scian3d<-ce_3d_indicadores %>%
  select(cve_sector, nombre_sector)
write.xlsx(sectores_scian3d,
           file = "datos/sectores_scian3d.xlsx",
           rowNames = FALSE)

#Guardar la base con los indicadores de los CE2018-SAIC----
#Eliminar los sectores cuya Unidad de Observación no es el Establecimiento

#Eliminar a los que no son establecimiento
no_establecimiento<- c("112","114","211", "212", "221", "236", "237","238", "481", "482", "483", "484", "485", "486", "487", "491", "492","521", "522", "523","524")#Sectores que no son establecimientos. Se discute en el anexo metodológico "Sectores a 3 dígitos del SCIAN-México 2018".

ce_3d_indicadores<- ce_3d_indicadores %>%
  filter(!cve_sector %in% no_establecimiento)#Eliminar subsectores cuya unidad de observación no fue el establecimiento.

write.xlsx(ce_3d_indicadores,
           file = "datos/ce_3d_indicadores.xlsx",
           rowNames = FALSE)


#5 Tabulados para el texto----
#Algunos tabulados para el documento escrito. Voy a trabajar con la base censo_eco
colnames(censo_eco)
#Eliminar los subsectores que no son establecimiento
censo_eco<- censo_eco %>%
  filter(!cve_sector %in% no_establecimiento) #Trabajo con la data censo_eco porque, además de los indicadores, necesito los datos con los cuales construí esos indicadores para las tablas del texto. La data "ce_3d_indicadores" sólo contiene la información de los indicadores.


#Tabla CE.1ProporcionPACD----
tabla_CE.1ProporcionPACD<-censo_eco %>%
  select(cve_sector, nombre_sector, personal_direc_y_admon, personal_total, CE.1ProporcionPACD) %>% #Selecciono variables de interés para la tabla
  arrange( desc(CE.1ProporcionPACD))%>% #Ordeno a partir de esta variable
  mutate(Clasificación = row_number()) %>% #Creo una variable Clasificación, la cual contiene el numero del renglón
  select(Clasificación, everything()) %>% #Reordeno las variables.
  slice(1:5, (n()-4):n()) #Seleccione las primeras cinco observaciones y las últimas cinco observaciones del dataframe.

library(openxlsx)
write.xlsx(tabla_CE.1ProporcionPACD,
           file = "tablas_texto/tabla_CE.1ProporcionPACD.xlsx",
           rowNames = FALSE)

#Tabla  CE.2IntensidadSP----

tabla_CE.2IntensidadSP<-censo_eco %>%
  select(cve_sector, nombre_sector, serv_pro_cientyt, personal_total, CE.2IntensidadSP) %>%
  arrange( desc(CE.2IntensidadSP))%>%
  mutate(Clasificación = row_number()) %>%
  select(Clasificación, everything()) %>%
  slice(1:5, (n()-4):n())

library(openxlsx)
write.xlsx(tabla_CE.2IntensidadSP,
           file = "tablas_texto/tabla_CE.2IntensidadSP.xlsx",
           rowNames = FALSE)

#Tabla  CE.3 Produc_inmaterial----
#Productividad Laboral
tabla_CE.3.1produc_lab<-censo_eco %>%
  select(cve_sector, nombre_sector, vacb, personal_total, produc_laboral) %>%
  arrange( desc(produc_laboral))%>%
  mutate(Clasificación = row_number()) %>%
  select(Clasificación, everything()) %>%
  slice(1:5, (n()-4):n())

library(openxlsx)
write.xlsx(tabla_CE.3.1produc_lab,
           file = "tablas_texto/tabla_CE.3.1produc_lab.xlsx",
           rowNames = FALSE)

#Eficiencia del Capital
tabla_CE.3.2eficiencia_cap<-censo_eco %>%
  select(cve_sector, nombre_sector, vacb, capital_fijo, eficiencia_capital) %>%
  arrange( desc(eficiencia_capital))%>%
  mutate(Clasificación = row_number()) %>%
  select(Clasificación, everything()) %>%
  slice(1:5, (n()-4):n())

library(openxlsx)
write.xlsx(tabla_CE.3.2eficiencia_cap,
           file = "tablas_texto/tabla_CE.3.2eficiencia_cap.xlsx",
           rowNames = FALSE)


#Producción Inmaterial

tabla_CE.3Produc_inmaterial<-censo_eco %>%
  select(cve_sector, nombre_sector, produc_laboral, eficiencia_capital , CE.3Produc_inmaterial) %>%
  arrange( desc(CE.3Produc_inmaterial))%>%
  mutate(Clasificación = row_number()) %>%
  select(Clasificación, everything()) %>%
  slice(1:5, (n()-4):n())

library(openxlsx)
write.xlsx(tabla_CE.3Produc_inmaterial,
           file = "tablas_texto/tabla_CE.3Produc_inmaterial.xlsx",
           rowNames = FALSE)


#Tabla  CE.4Intensidad_TIC----
tabla_CE.4Intensidad_TIC<-censo_eco %>%
  select(cve_sector, nombre_sector,capital_comp_perif, serv_comunicacion, personal_total, CE.4Intensidad_TIC) %>%
  arrange( desc(CE.4Intensidad_TIC))%>%
  mutate(Clasificación = row_number()) %>%
  select(Clasificación, everything()) %>%
  slice(1:5, (n()-4):n())

library(openxlsx)
write.xlsx(tabla_CE.4Intensidad_TIC,
           file = "tablas_texto/tabla_CE.4Intensidad_TIC.xlsx",
           rowNames = FALSE)


#Nota Final----
#Esta base contienes:
#Observaciones de Subsectores económicos a 3 dígitos SCIAN en el año 2018.
#Variables y métricas construidas asociadas al conocimiento.


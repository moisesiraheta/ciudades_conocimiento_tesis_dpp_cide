# ==============================================================================
# Tesis: Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)
# Autor: Moises Israel Iraheta Avila
# Institución: Doctorado en Políticas Públicas, CIDE
# Script: Procesamiento ENOE-2018-III
# ==============================================================================
# Limpiar entorno de trabajo para asegurar que no existan variables de sesiones previas
rm(list = ls(all = TRUE))
# Instalar Paquetes
library(pacman) # Pacman incluye la función p_load() que carga los paquetes que uno requiere, y si no están instalados, los instala.
p_load(haven,         # Paquete para importar archivos de Stata, SAS y SPSS
       readxl,        # Paquete para importar archivos de Excel
       tidyverse,     # Metapaquete que incluye readr, paquete para importar archivos de texto plano, y tidyr, entre otros.
       dplyr,         # Paquete para manipulación de datos (filtrar, seleccionar, transformar, etc.)
       openxlsx,      # Paquete para guardar archivos en formato Excel (.xlsx)
       stats,         # Paquete base de R para análisis estadístico (análisis multivariado como ACP, regresiones, etc.)
       importinegi,   # Paquete para descargar bases de datos del INEGI como la ENOE
       foreign,       # Paquete para importar archivos de otros programas estadísticos (dbf, etc.)
       sjlabelled,    # Paquete para trabajar con datos etiquetados (importados de SPSS, Stata, etc.)
       stargazer)   # Paquete para crear tablas de salida de modelos estadísticos en formato LaTeX, HTML, texto, etc.


# Configuración del entorno
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Ajuste de locale para codificación de caracteres en español
options(scipen = 999) # Desactivar notación científica para facilitar lectura de cifras económicas

#Cargar la base de datos ENOE_2018_T3 (Enoe del año 2018 del 3er trimestre)----
# URL del archivo ZIP de la ENOE 2018 3er Trimestre .dta
url_zip <- "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/2018trim3_dta.zip"

# Crear un archivo temporal para guardar el ZIP descargado de la ENOE
temp_zip <- tempfile(fileext = ".zip")
options(timeout = 300) #Por lo pesado del archivo, ampliar a 300 segundos el tiempo que R puede usar para descargar
# Descargar el archivo ZIP cons .csv desde la URL
download.file(url_zip, destfile = temp_zip, mode = "wb")
options(timeout = 60) #restablecer el timeout a su valor original.
# Listar los archivos dentro del ZIP.csv
archivos_zip <- unzip(temp_zip, list = TRUE)
archivos_zip$Name #Muestra el nombre de los archivos dentro del zip

#Crear un archivo temporal para el archivo CUESTIONARIO 1: COE1T318.dta (directamente con el nombre)
temp <- file.path(tempdir(), "COE1T318.dta")
#Extraer el archivo CSV en el directorio temporal (directamente con el nombre)
unzip(temp_zip, files = "COE1T318.dta", exdir = tempdir())
#Leer el archivo CSV usando read_dta()
coe1_18_t3 <- read_dta(temp)

#Crear un archivo temporal para el archivo CUESTIONARIO 2: COE2T318.dta (directamente con el nombre)
temp <- file.path(tempdir(), "COE2T318.dta")
#Extraer el archivo .dta en el directorio temporal (directamente con el nombre)
unzip(temp_zip, files = "COE2T318.dta", exdir = tempdir())
#Leer el archivo .dta usando read_dta()
coe2_18_t3 <- read_dta(temp)

#Crear un archivo temporal para el archivo CUESTIONARIO SocioeEconómico: SDEMT318.dta (directamente con el nombre)
temp <- file.path(tempdir(), "SDEMT318.dta")
#Extraer el archivo dta en el directorio temporal (directamente con el nombre)
unzip(temp_zip, files = "SDEMT318.dta", exdir = tempdir())
# Leer el archivo dta usando read_dta()
sdem_18_t3 <- read_dta(temp)
#Las Tres Bases Están Cargadas:
#CUESTIONARIO 1: coe1_18_t3
#CUESTIONARIO 2: coe2_18_t3
#SOCIOECONÓMICO:sdem_18_t3

# Creación de una sola bd----
coe1<- coe1_18_t3
coe2<- coe2_18_t3
sdem<- sdem_18_t3

# Crear id_persona en coe1_18_t3 (llave)
coe1 <- coe1 %>%
  mutate(id_persona = paste(cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, sep = "_"))
# Crear id_persona en coe2_18_t3 (llave)
coe2<- coe2 %>%
  mutate(id_persona = paste(cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, sep = "_"))
# Crear id_persona en sdem_18_t3 (llave)
sdem <- sdem%>%
  mutate(id_persona = paste(cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, sep = "_"))

# Verificar la creación de la variable (llave) en cada base de datos
head(coe1$id_persona)
head(coe2$id_persona)
head(sdem$id_persona)

# Seleccionar las variables de interés del Cuestionario 1, incluyendo la llave
coe1_vars <- coe1 %>% select(p3, p4a, fac, id_persona)

# Seleccionar las variables de interés del Cuestionario 2, incluyendo la llave
coe2_vars <- coe2%>% select(p7a, p7c, fac, id_persona)

# Seleccionar las variables de interés del Cuestionario SocioEconómico, incluyendo la llave
sdem_vars <- sdem%>% select(cs_p12, cs_p13_1, cs_p13_2, cs_p14_c, cs_p15, cs_p16, fac, rama, clase1, clase2, rama_est1, rama_est2, id_persona)

# Combinar las bases de datos coe1_vars y coe2_vars usando la llave
coe_vars <- full_join(coe1_vars, coe2_vars, by = c("id_persona"))

# Combinar la base de datos coe_vars con sdem_vars usando la llave
enoe_completa <- left_join(coe_vars, sdem_vars, by = c("id_persona"))#left_join permite quedarme únicamente con las observaciones que tengan presencia en coe_vars, descartando las observaciones sdem_vars que no tengan match en coe_vars

#De esta manera, se crea una base de datos única con todas las variables de interés y la llave que identifica cada observación.

#Limpieza de la Base de datos----
#Renombrar variables

enoe_completa <- enoe_completa %>%
  rename(
    cve_sinco = p3,
    scian_hogares = p4a,
    grado_acad = cs_p13_1,
    anio_estudio = cs_p13_2,
    cve_carrera = cs_p14_c,
    est_prev_basica = cs_p15,
    estudios_terminado = cs_p16
  )

#Crear Etiqueta del sector SCIAN a 3 dígitos del ocupado----
enoe_completa$enoe_3digitos <- substr(enoe_completa$scian_hogares , 1, 3)

#Eliminar observaciones que no tienen sector económico
enoe_completa <- enoe_completa %>%
  filter(!is.na(enoe_3digitos)) #Elimina los n.a en la clave 3 dígitos

#Tabla Equivalencias SCIAN Hogares y SCIAN México----
#Después de la revisión manual en el documento word y la elaboración de la nota metodológica y la subsecuente creación de la tabla de equivalencias SCIAN hogares-SCIAN México, procedo a cargar la tabla de equivalencias.

equivalencia_scianmx_scianenoe<- read.csv("datos/equivalencias_scian_mx_scian_hogares.csv")

equivalencias_scian<- equivalencia_scianmx_scianenoe %>%
  select(cve_scian_ce_3digitos, cve_scian_hogares_enoe_4digitos)

#Agregar a la data enoe_completa que tiene todas mis observaciones y variables de interés, las equivalencias de su SCIAN_hogares con el SCIAN_mx_3 dígitos. De esta forma, cada observación de la ENOE, corresponderá a un Subsector SCIAN-México a 3 dígitos.


enoe_con_equivalencias_scian <- full_join(enoe_completa, equivalencia_scianmx_scianenoe,
                                          by = c("scian_hogares" = "cve_scian_hogares_enoe_4digitos"))

#Renombrar la variable de los sectores económicos SCIAN a 3 dígitos que será mi variable clave para agrupar las variables a calcular.

enoe_con_equivalencias_scian <- enoe_con_equivalencias_scian %>%
  rename(scian_mx_3d=cve_scian_ce_3digitos)

#Limpiar nuevos datos perdidos

#Observaciones que quedaron con valores perdidos en la variable SCIAN_mx, porque en la tabla de equivalencia, no se le asignó una equivalencia con respecto al SCIAN_hogares
filas_con_na <- enoe_con_equivalencias_scian[is.na(enoe_con_equivalencias_scian$scian_mx_3d), ]
#En total 985 observaciones
table(as.factor(filas_con_na$scian_hogares))#Como se observa, todas las observaciones corresponden a actividades insuficientemente especificadas en el levantamiento de la encuesta.

#Por otro lado, solo 4 observaciones pertenecen a sector 23, 9 al sector 43, 1 al sector 52 y 1 al sector 93. Siendo que las clasificaciones SCIAN-hogares 9700, 9800 y 9999 en conjunto fueron más de 950 de las observaciones. Lo cual resta representatividad a la muestra, pero no sesga algún sector en particular.

#Quitar a enoe_con_equivalencias los valores perdidos.
any(is.na(enoe_con_equivalencias_scian$scian_mx_3d))

enoe_con_equivalencias_scian <- enoe_con_equivalencias_scian %>%
  filter(!is.na(scian_mx_3d))#Me quedo con 169,531 observaciones.

#Selecciono variables de interés de la enoe
enoe_limpia<- enoe_con_equivalencias_scian %>%
  select(scian_mx_3d,
         nombre_subsector_scian_ce_3digitos,
         scian_hogares,
         nombre_subsector_scian_hogares_enoe_4digitos,
         fac,
         cve_sinco,
         grado_acad,
         anio_estudio,
         cve_carrera,
         estudios_terminado,
         id_persona)


write.csv(enoe_limpia,
          file = "datos/enoe_limpia.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

#Generación de Variables con la ENOE
### Preparación de R para trabajar  ----
#Limpiar R
rm(list=ls (all=T))

#Cargar Base de datos
enoe_limpia<-read.csv("datos/enoe_limpia.csv")

#Creación de Bases de Datos para sectores----
enoe_limpia$scian_mx_3d<- as.character(enoe_limpia$scian_mx_3d)#Convertir a caracter la variable clave SCIAN.

#Eliminar los sectores cuya Unidad de Observación no es el Establecimiento
no_establecimiento<- c("112","114","211", "212", "221", "236", "237","238", "481", "482", "483", "484", "485", "486", "487", "491", "492","521", "522", "523","524")#Sectores que no son establecimientos.

enoe_limpia<- enoe_limpia %>%
  filter(!scian_mx_3d %in% no_establecimiento)#Eliminar subsectores cuya unidad de observación no fue el establecimiento.

169531-142889

#Crear código con nombres de los SECTORES
sectores<-enoe_limpia %>%
  select(scian_mx_3d,nombre_subsector_scian_ce_3digitos)
sectores <- sectores %>%
  distinct(scian_mx_3d, .keep_all = TRUE)#Eliminar repetitivos en SCIAN_mexico

# TOTAL TRABAJADORES por Sector
total_trabajadores <- enoe_limpia %>%
  group_by(scian_mx_3d) %>%
  summarize(total_ocupados = sum(fac), .groups = "drop")


##OCUPACIONES DEL CONOCIMIENTO----
#Cargar la data de ocupaciones del conocimiento SINCO de acuerdo con el anexo metodológico
sinco_conocimiento<-read.csv("datos/sinco_conocimiento.csv")
str(sinco_conocimiento)#Reviso tipo de variables
sinco_conocimiento$cve_sinco<- as.character(sinco_conocimiento$cve_sinco)#Convierto a caracter sinco

str(enoe_limpia) #Reviso tipo de variables
enoe_limpia$cve_sinco <- as.character(enoe_limpia$cve_sinco ) #Convierto a caracter.

#Determinar el Número de Trabajadores del conocimiento por sector SCIAN.
trabajadores_conocimiento <- enoe_limpia %>%
  filter(cve_sinco %in% sinco_conocimiento$cve_sinco) %>%
  group_by(scian_mx_3d) %>%
  summarise(n_ocupaciones_k = sum(fac), .groups = "drop")
str(trabajadores_conocimiento)

#NUMERO POSGRADUADOS----
#Determinar el número de posgrados por sector económico.
str(enoe_limpia$grado_acad)

posgraduados <- enoe_limpia %>%
  filter(grado_acad %in% c( 8,9)) %>% #Las claves 8 y 9 corresponden a maestría y doctorado
  group_by(scian_mx_3d) %>%
  summarize(n_posgrado = sum(fac), .groups = "drop")
str(posgraduados)

#NUMERO STEM----
n_stem <- enoe_limpia %>%
  filter(str_detect(cve_carrera, "^[578][45]..$")) %>%#Solo considera observaciones que en esta variable inicien con 5,7 u 8 (licenciatura, maestría o doctorado), y su segundo dígito sea 4 o 5, es decir, formaciones STEM
  group_by(scian_mx_3d) %>%
  summarise(n_stem = sum(fac), .groups = "drop")
str(n_stem)

#OCUPACIONES_TIC----
#Cargar la data de ocupaciones del TIC SINCO
sinco_tic<-read.csv("datos/sinco_tic.csv")
str(sinco_tic)#Reviso tipo de variables
sinco_tic$cve_sinco_tic <- as.character(sinco_tic$cve_sinco_tic )#Convierto a caracter sinco

ocupaciones_tic <- enoe_limpia %>%
  filter(cve_sinco %in% sinco_tic$cve_sinco_tic) %>%#Considera solo las observaciones que en clave sinco, pertenecen a la lista de ocupaciones tic que he definido.
  group_by(scian_mx_3d) %>%
  summarise( n_ocupaciones_tic = sum(fac), .groups = "drop")
str(ocupaciones_tic)


#En Este momento tengo 5 Datas, todas ellas con una columna SCIAN_mx_3d
#1.- total_trabajadores
#2.- trabajadores_conocimiento
#3.- pograduados
#4.- n_stem
#5.- ocupaciones_tic

str(total_trabajadores)
str(trabajadores_conocimiento)
str(posgraduados)
str( n_stem)
str(ocupaciones_tic)

total_trabajadores$scian_mx_3d

#Crear ENOE_CONOCIMIENTO, que tiene indicadores por sectores a 3 digitos SCIAN de variables asociadas al conocimiento.----

enoe_conocimiento <- sectores %>%
  full_join(total_trabajadores, by = "scian_mx_3d") %>%
  full_join(trabajadores_conocimiento, by = "scian_mx_3d") %>%
  full_join(posgraduados, by = "scian_mx_3d") %>%
  full_join(n_stem, by = "scian_mx_3d") %>%
  full_join(ocupaciones_tic, by = "scian_mx_3d")


#Calculo de Indicadores----
#Intensidad del Conocimiento
enoe_conocimiento$ENOE.1intensidad_trab_conoc<- enoe_conocimiento$n_ocupaciones_k/enoe_conocimiento$total_ocupados

#Crear la Tabla para el texto
tablaENOE.1_intensidad_trabaja_k <- enoe_conocimiento %>%
  select(scian_mx_3d, nombre_subsector_scian_ce_3digitos, n_ocupaciones_k, total_ocupados, ENOE.1intensidad_trab_conoc) %>%
  arrange(desc(ENOE.1intensidad_trab_conoc)) %>%
  mutate(ranking = row_number()) %>%
  select(ranking, everything())%>%
  slice(1:5, (n() - 4):n())

enoe_conocimiento %>%
  select(ENOE.1intensidad_trab_conoc) %>%
  is.na() %>%
  table()

library(openxlsx)
write.xlsx(tablaENOE.1_intensidad_trabaja_k,
           file = "tablas_texto/tablaENOE.1_intensidad_trabaja_k.xlsx",
           rowNames = FALSE)

#Porcentaje Posgradusos
enoe_conocimiento <- enoe_conocimiento %>%
  mutate(ENOE.2porce_posgrado = n_posgrado/total_ocupados)

tablaENOE.2_propor_posgrado <- enoe_conocimiento %>%
  select(scian_mx_3d, nombre_subsector_scian_ce_3digitos, n_posgrado, total_ocupados, ENOE.2porce_posgrado) %>%
  arrange(desc(ENOE.2porce_posgrado)) %>%
  mutate(ranking = row_number()) %>%
  select(ranking, everything())%>%
  slice(1:5, (n() - 4):n())

enoe_conocimiento %>%
  select(ENOE.2porce_posgrado) %>%
  is.na() %>%
  table()

library(openxlsx)
write.xlsx(tablaENOE.2_propor_posgrado,
           file = "tablas_texto/tablaENOE.2_propor_posgrado.xlsx",
           rowNames = FALSE)

#Proporción STEM
enoe_conocimiento <- enoe_conocimiento %>%
  mutate(ENOE.3porce_stem = n_stem/total_ocupados)

tablaENOE.3_propor_stem <- enoe_conocimiento %>%
  select(scian_mx_3d, nombre_subsector_scian_ce_3digitos, n_stem, total_ocupados, ENOE.3porce_stem) %>%
  arrange(desc( ENOE.3porce_stem )) %>%
  mutate(ranking = row_number()) %>%
  select(ranking, everything())%>%
  slice(1:5, (n() - 4):n())

enoe_conocimiento %>%
  select(ENOE.3porce_stem) %>%
  is.na() %>%
  table()

library(openxlsx)
write.xlsx(tablaENOE.3_propor_stem,
           file = "tablas_texto/tablaENOE.3_propor_stem.xlsx",
           rowNames = FALSE)

#Proporción de Ocupación TIC
enoe_conocimiento <- enoe_conocimiento %>%
  mutate(ENOE.4porce_tic = n_ocupaciones_tic/total_ocupados)

tablaENOE.4_propor_tic <- enoe_conocimiento %>%
  select(scian_mx_3d, nombre_subsector_scian_ce_3digitos, n_ocupaciones_tic, total_ocupados, ENOE.4porce_tic) %>%
  arrange(desc( ENOE.4porce_tic)) %>%
  mutate(ranking = row_number()) %>%
  select(ranking, everything())%>%
  slice(1:5, (n() - 4):n())

enoe_conocimiento %>%
  select(ENOE.4porce_tic) %>%
  is.na() %>%
  table()

library(openxlsx)
write.xlsx(tablaENOE.4_propor_tic,
           file = "tablas_texto/tablaENOE.4_propor_tic.xlsx",
           rowNames = FALSE)


#Guardar la data para posterior unión con SCIAN----
enoe_conocimiento<-enoe_conocimiento %>%
  select(scian_mx_3d, nombre_subsector_scian_ce_3digitos, ENOE.1intensidad_trab_conoc, ENOE.2porce_posgrado, ENOE.3porce_stem, ENOE.4porce_tic)

write.csv(enoe_conocimiento ,
          file = "datos/enoe_conocimiento.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

write.xlsx (enoe_conocimiento ,
            file = "datos/enoe_conocimiento.xlsx",
            rowNames = FALSE)

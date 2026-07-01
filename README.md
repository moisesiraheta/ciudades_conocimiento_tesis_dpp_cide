# Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)

Este repositorio contiene los materiales de investigación generados como parte de mi tesis doctoral. Incluye las bases de datos originales, los códigos de procesamiento en **R**, las gráficas y los tabulados finales utilizados en el estudio.

## Estructura del Proyecto

El repositorio está organizado en tres directorios principales. Cada uno sigue la misma jerarquía, diferenciando entre nivel **Subsector (3 dígitos SCIAN)** y **Rama (4 dígitos SCIAN)**:

* `/datos`: Bases de datos originales utilizadas para el análisis.
* `/códigos`: Scripts de limpieza, procesamiento y análisis.
* `/tablas_texto`: Tabulados y visualizaciones (insumos para el documento final).

> **Nota sobre la clasificación:** Dentro de cada carpeta principal, existe un subdirectorio llamado `/4digitos` que replica la información a nivel Rama. En la carpeta `/códigos/4digitos`, el script "03" es omitido debido a la no disponibilidad de datos de la ENOE a este nivel de desagregación.

## Guía de Procesamiento (Carpeta `/códigos`)

Los scripts han sido numerados para reflejar el flujo de trabajo metodológico:

* **01 - 04**: Limpieza de bases de datos originales y construcción de indicadores base.
* **05**: Elaboración del **Índice de Intensidad del Conocimiento** y pruebas de robustez/consistencia.
* **06**: Procedimiento de agregación de datos municipales a nivel metropolitano.
* **07.*** y **08**: Análisis de trayectorias metropolitanas mediante la Tipología Multidimensional y generación de resultados finales.

## Citación

Si utilizas estos materiales en futuras investigaciones, por favor cita el trabajo de la siguiente manera:

> Iraheta-Avila, M. I. (2026). *Ciudades del Conocimiento: Metrópolis en Transición en un País Emergente (México, 2003-2018)*. Tesis Doctoral, Centro de Investigación y Docencia Económicas (CIDE).

## Bases de Datos

Consejo Nacional de Población (CONAPO). (2018, 26 de enero). *Delimitación de Zonas Metropolitanas* [Archivo .csv]. https://conapo.segob.gob.mx/work/models/CONAPO/Datos_Abiertos/Delimitacion_ZM/ZM_2015.csv

Instituto Nacional de Estadística y Geografía. (s/f). *Actividades de innovación realizadas de manera coordinada con universidad, centros de investigación, empresas...* [Archivo de Excel]. Censos Económicos 2019: Tabulados de Ciencia, Tecnología e Innovación. https://www.inegi.org.mx/contenidos/programas/ce/2019/tabulados/innonce19_02.xlsx

Instituto Nacional de Estadística y Geografía. (s/f). *Características del personal ocupado* [Archivo de Excel]. Censos Económicos 2019: Tabulados básicos. https://www.inegi.org.mx/contenidos/programas/ce/2019/tabulados/edadnce19_02.xlsx

Instituto Nacional de Estadística y Geografía. (2020, 7 de diciembre). *Censos Económicos 2019. Resultados definitivos. Datos abiertos a nivel nacional* [Archivo ZIP]. https://www.inegi.org.mx/contenidos/programas/ce/2019/Datosabiertos/ce2019_nac_csv.zip

Instituto Nacional de Estadística y Geografía. (s/f). *Censos Económicos 2019: Tabulados básicos* [Base de Datos]. https://www.inegi.org.mx/programas/ce/2019/#tabulados

Instituto Nacional de Estadística y Geografía. (s/f). *Comparativo de Censos Económicos 2004 y 2019: Datos nacionales y municipales* [Base de datos]. Sistema Automatizado de Información Censal (SAIC). https://www.inegi.org.mx/app/saic/default.html

Instituto Nacional de Estadística y Geografía. (s.f.). *Cuentas de Bienes y Servicios (detallada). Año base 2018. Cuentas de producción, por actividad económica de origen/ Valor agregado bruto en valores básicos*. https://www.inegi.org.mx/app/tabulados/default.aspx?pr=1&vr=4&in=31&tp=20&wr=1&cno=1&idrt=3247&opc=p

Instituto Nacional de Estadística y Geografía. (s/f). *Encuesta Nacional de Ocupación y Empleo (ENOE): Microdatos, 15 y más años, tercer trimestre de 2018* [Archivo ZIP].  https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/2018trim3_dta.zip

Instituto Nacional de Estadística y Geografía. (s/f). *Gasto en miles de pesos en actividades de Investigación y Desarrollo; Gasto en miles de pesos en producción o adquisición de software o bases de datos* [Archivo de Excel]. Censos Económicos 2019: Tabulados de Ciencia, Tecnología e Innovación. https://www.inegi.org.mx/contenidos/programas/ce/2019/tabulados/innonce19_07.xlsx

Instituto Nacional de Estadística y Geografía. (s/f). *Personas dedicadas a actividades de innovación...* [Archivo de Excel]. Censos Económicos 2019: Tabulados de Ciencia, Tecnología e Innovación. https://www.inegi.org.mx/contenidos/programas/ce/2019/tabulados/innonce19_04.xlsx

Instituto Nacional de Estadística y Geografía. (s/f). *Resultados del Censo Económico 2019* [Base de datos]. Sistema Automatizado de Información Censal (SAIC). https://www.inegi.org.mx/app/saic/default.html

Instituto Nacional de Estadística y Geografía. (s/f). *Tecnologías de la información y las telecomunicaciones* [Archivo de Excel]. Censos Económicos 2019: Tabulados básicos. https://www.inegi.org.mx/contenidos/programas/ce/2019/tabulados/ticsnce19_01.xlsx

Instituto Nacional de Estadística y Geografía. (s/f). *Unidades económicas con actividades de innovación* [Archivo de Excel]. Censos Económicos 2019: Tabulados de Ciencia, Tecnología e Innovación. https://www.inegi.org.mx/contenidos/programas/ce/2019/tabulados/innonce19_01.xlsx

Instituto Nacional de Estadística y Geografía. (s/f). *Unidades económicas que registraron o tramitaron patentes de marcas, productos o procesos...* [Archivo de Excel]. Censos Económicos 2019: Tabulados de Ciencia, Tecnología e Innovación. https://www.inegi.org.mx/contenidos/programas/ce/2019/tabulados/innonce19_06.xlsx




---

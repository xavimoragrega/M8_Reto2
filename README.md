# M8 Reto 2 - Proyecto de Ciencia de Datos reproducible

## Descripción

Proyecto desarrollado para el Reto 2 del Módulo 8.

El proyecto analiza la satisfacción con la vida de las personas trabajadoras en diferentes países europeos entre 2002 y 2023, utilizando datos de la European Social Survey (ESS).

El análisis explora la evolución temporal de la satisfacción con la vida y sus diferencias según la situación económica percibida del hogar, la frecuencia de contacto social y el país.

La satisfacción con la vida se mide mediante la variable `stflife`, en una escala de 0 a 10.

## Repositorio de GitHub

El código, los datos y el informe del proyecto están disponibles en el siguiente repositorio:

[Repositorio de GitHub](https://github.com/xavimoragrega/M8_Reto2)

## Objetivo

El objetivo principal es identificar y visualizar patrones de satisfacción con la vida entre las personas trabajadoras de diferentes países europeos y explorar cómo estos patrones se relacionan con dimensiones económicas, sociales y geográficas a lo largo del tiempo.

Los resultados muestran asociaciones observadas en los datos y no permiten establecer relaciones causales.

## Datos

Los datos proceden de la European Social Survey (ESS), utilizando las rondas 1 a 11 correspondientes al periodo 2002-2023.

El análisis se centra en personas con actividad laboral remunerada y en los 15 países presentes en las 11 rondas utilizadas para la comparación temporal.

La carpeta `Datos/` contiene:

- `original/`: datos originales utilizados como punto de partida.
- `depuracion/`: scripts utilizados para la depuración y preparación de los datos.
- `depurada/`: conjunto de datos preparado para el análisis, el informe y el dashboard.

## Estructura del proyecto

- `Datos/`: datos originales, scripts de depuración y datos depurados.
- `Dashboard/`: código y archivos necesarios para ejecutar el dashboard interactivo.
- `Informe/`: código fuente y archivos generados del informe técnico.
- `README.md`: descripción y documentación del proyecto.

## Reproducibilidad

El proyecto está organizado para poder reproducir las principales fases del análisis.

### 1. Importación y depuración

Ejecutar:

`Datos/depuracion/importar_datos.R`

Este script:

- importa los datos originales;
- recodifica los códigos especiales de valores perdidos;
- crea las variables utilizadas en el análisis;
- identifica las personas trabajadoras;
- identifica los países presentes en las 11 rondas;
- genera el archivo `datos_depurados.csv`.

### 2. Análisis

Ejecutar:

`Datos/depuracion/analisis.R`

Este script calcula los principales resultados utilizados en el proyecto:

- evolución temporal de la satisfacción;
- diferencias según situación económica;
- diferencias según frecuencia de contacto social;
- satisfacción media por país;
- cambio de la satisfacción entre 2002 y 2023 por país.

### 3. Informe

El informe se encuentra en:

`Informe/codigo/informe.Rmd`

El documento utiliza los datos depurados y genera el informe técnico con los resultados y visualizaciones del análisis.

### 4. Dashboard

El dashboard se encuentra en:

`Dashboard/codigo/app.R`

Para ejecutarlo desde RStudio se debe abrir `app.R` y utilizar la opción **Run App**.

## Visualizaciones

El proyecto incluye visualizaciones para:

- evolución temporal de la satisfacción con la vida;
- satisfacción según la situación económica percibida;
- satisfacción según la frecuencia de contacto social;
- comparación de la satisfacción entre países;
- cambio de la satisfacción respecto a 2002 mediante un mapa interactivo.

## Tecnologías utilizadas

- R
- RStudio
- R Markdown
- Shiny
- Git y GitHub

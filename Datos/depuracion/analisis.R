# Seleccionar las personas que trabajan
# y tienen datos válidos

datos_trabajadores <- datos_depurados[
  datos_depurados$mnactic == 1 &
    !is.na(datos_depurados$stflife) &
    !is.na(datos_depurados$anweight_complet),
]


# Seleccionar los países que están presentes
# en todas las rondas

datos_temporales <- datos_trabajadores[
  datos_trabajadores$pais %in% paises_temporales,
]



# EVOLUCIÓN DE LA SATISFACCIÓN

# Calcular la satisfacción media de cada año

resultados_temporales <- do.call(
  rbind,
  lapply(
    sort(unique(datos_temporales$any)),
    function(a) {
      
      datos_any <- datos_temporales[
        datos_temporales$any == a,
      ]
      
      data.frame(
        any = a,
        satisfaccion_media = weighted.mean(
          datos_any$stflife,
          datos_any$anweight_complet,
          na.rm = TRUE
        )
      )
    }
  )
)


# Ver los resultados

print(resultados_temporales)


# Ver cuántas personas hay cada año

print(table(datos_temporales$any))


# Ver el cambio entre 2002 y 2023

satisfaccion_2002 <- resultados_temporales[
  resultados_temporales$any == 2002,
  "satisfaccion_media"
]

satisfaccion_2023 <- resultados_temporales[
  resultados_temporales$any == 2023,
  "satisfaccion_media"
]

cambio_total_2002_2023 <- (
  satisfaccion_2023 -
    satisfaccion_2002
)


print(cambio_total_2002_2023)

# SITUACIÓN ECONÓMICA


# Quedarnos con las personas que tienen
# información sobre su situación económica

datos_economicos <- datos_temporales[
  !is.na(datos_temporales$hincfel),
]


# Multiplicar la satisfacción por el peso

datos_economicos$satisfaccion_ponderada <- (
  datos_economicos$stflife *
    datos_economicos$anweight_complet
)


# Calcular los resultados para cada año
# y cada situación económica

resultados_economicos_temporales <- aggregate(
  cbind(
    satisfaccion_ponderada,
    anweight_complet
  ) ~ any + hincfel,
  data = datos_economicos,
  FUN = sum
)


# Calcular la satisfacción media

resultados_economicos_temporales$satisfaccion_media <- (
  resultados_economicos_temporales$satisfaccion_ponderada /
    resultados_economicos_temporales$anweight_complet
)


# Poner nombres fáciles de entender a las categorías

resultados_economicos_temporales$situacion_economica <- factor(
  resultados_economicos_temporales$hincfel,
  levels = c(1, 2, 3, 4),
  labels = c(
    "Vive cómodamente",
    "Se las arregla",
    "Tiene dificultades",
    "Tiene muchas dificultades"
  )
)


# Ordenar los resultados por año y categoría

resultados_economicos_temporales <- resultados_economicos_temporales[
  order(
    resultados_economicos_temporales$any,
    resultados_economicos_temporales$hincfel
  ),
]


# Ver los resultados

print(resultados_economicos_temporales)


# Ver los resultados de 2023

resultados_economicos_2023 <- resultados_economicos_temporales[
  resultados_economicos_temporales$any == 2023,
]


print(resultados_economicos_2023)


# Ver la diferencia entre la categoría
# con mayor y menor satisfacción en 2023

diferencia_economica_2023 <- (
  max(
    resultados_economicos_2023$satisfaccion_media,
    na.rm = TRUE
  ) -
    min(
      resultados_economicos_2023$satisfaccion_media,
      na.rm = TRUE
    )
)


print(diferencia_economica_2023)


# CONTACTO SOCIAL


# Quedarnos con las personas que tienen
# información sobre contacto social

datos_sociales <- datos_temporales[
  !is.na(datos_temporales$sclmeet),
]


# Multiplicar la satisfacción por el peso

datos_sociales$satisfaccion_ponderada <- (
  datos_sociales$stflife *
    datos_sociales$anweight_complet
)


# Calcular los resultados para cada año
# y cada frecuencia de contacto

resultados_sociales_temporales <- aggregate(
  cbind(
    satisfaccion_ponderada,
    anweight_complet
  ) ~ any + sclmeet,
  data = datos_sociales,
  FUN = sum
)


# Calcular la satisfacción media

resultados_sociales_temporales$satisfaccion_media <- (
  resultados_sociales_temporales$satisfaccion_ponderada /
    resultados_sociales_temporales$anweight_complet
)


# Poner nombres fáciles de entender a las categorías

resultados_sociales_temporales$frecuencia_contacto <- factor(
  resultados_sociales_temporales$sclmeet,
  levels = c(1, 2, 3, 4, 5, 6, 7),
  labels = c(
    "Nunca",
    "Menos de una vez al mes",
    "Una vez al mes",
    "Varias veces al mes",
    "Una vez a la semana",
    "Varias veces a la semana",
    "Cada día"
  )
)


# Ordenar los resultados por año y categoría

resultados_sociales_temporales <- resultados_sociales_temporales[
  order(
    resultados_sociales_temporales$any,
    resultados_sociales_temporales$sclmeet
  ),
]


# Ver los resultados

print(resultados_sociales_temporales)


# Ver los resultados de 2023

resultados_sociales_2023 <- resultados_sociales_temporales[
  resultados_sociales_temporales$any == 2023,
]


print(resultados_sociales_2023)


# Ver la diferencia entre la categoría
# con mayor y menor satisfacción en 2023

diferencia_social_2023 <- (
  max(
    resultados_sociales_2023$satisfaccion_media,
    na.rm = TRUE
  ) -
    min(
      resultados_sociales_2023$satisfaccion_media,
      na.rm = TRUE
    )
)


print(diferencia_social_2023)


# COMPARAR LOS PAÍSES


# Multiplicar la satisfacción por el peso

datos_temporales$satisfaccion_ponderada <- (
  datos_temporales$stflife *
    datos_temporales$anweight_complet
)


# Calcular los resultados para cada país

resultados_paises <- aggregate(
  cbind(
    satisfaccion_ponderada,
    anweight_complet
  ) ~ pais,
  data = datos_temporales,
  FUN = sum
)


# Calcular la satisfacción media de cada país

resultados_paises$satisfaccion_media <- (
  resultados_paises$satisfaccion_ponderada /
    resultados_paises$anweight_complet
)


# Ordenar los países de menor a mayor satisfacción

resultados_paises <- resultados_paises[
  order(
    resultados_paises$satisfaccion_media
  ),
]


# Ver los resultados

print(resultados_paises)


# CAMBIO DE CADA PAÍS DESDE 2002


# Quedarnos con los datos de 2002

datos_2002 <- datos_temporales[
  datos_temporales$any == 2002,
]


# Multiplicar la satisfacción por el peso

datos_2002$satisfaccion_ponderada <- (
  datos_2002$stflife *
    datos_2002$anweight_complet
)


# Calcular la satisfacción media de cada país en 2002

resultados_2002_paises <- aggregate(
  cbind(
    satisfaccion_ponderada,
    anweight_complet
  ) ~ pais,
  data = datos_2002,
  FUN = sum
)


resultados_2002_paises$satisfaccion_2002 <- (
  resultados_2002_paises$satisfaccion_ponderada /
    resultados_2002_paises$anweight_complet
)


# Quedarnos con los datos de 2023

datos_2023 <- datos_temporales[
  datos_temporales$any == 2023,
]


# Multiplicar la satisfacción por el peso

datos_2023$satisfaccion_ponderada <- (
  datos_2023$stflife *
    datos_2023$anweight_complet
)


# Calcular la satisfacción media de cada país en 2023

resultados_2023_paises <- aggregate(
  cbind(
    satisfaccion_ponderada,
    anweight_complet
  ) ~ pais,
  data = datos_2023,
  FUN = sum
)


resultados_2023_paises$satisfaccion_2023 <- (
  resultados_2023_paises$satisfaccion_ponderada /
    resultados_2023_paises$anweight_complet
)


# Juntar los resultados de 2002 y 2023

resultados_cambio_paises <- merge(
  resultados_2002_paises[
    c(
      "pais",
      "satisfaccion_2002"
    )
  ],
  resultados_2023_paises[
    c(
      "pais",
      "satisfaccion_2023"
    )
  ],
  by = "pais"
)


# Calcular cuánto ha cambiado cada país

resultados_cambio_paises$cambio_2002_2023 <- (
  resultados_cambio_paises$satisfaccion_2023 -
    resultados_cambio_paises$satisfaccion_2002
)


# Ordenar los países según su cambio

resultados_cambio_paises <- resultados_cambio_paises[
  order(
    resultados_cambio_paises$cambio_2002_2023
  ),
]


# Ver los resultados

print(resultados_cambio_paises)



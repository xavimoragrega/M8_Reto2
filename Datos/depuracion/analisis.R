# Seleccionar las personas ocupadas con información válida
# sobre satisfacción con la vida y peso de análisis

datos_trabajadores <- datos_depurados[
  datos_depurados$mnactic == 1 &
    !is.na(datos_depurados$stflife) &
    !is.na(datos_depurados$anweight_complet),
]


# Evolución temporal de la satisfacción con la vida
# en los países presentes en todas las rondas

datos_temporales <- datos_trabajadores[
  datos_trabajadores$pais %in% paises_temporales,
]

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
        satisfaccion_ponderada = weighted.mean(
          datos_any$stflife,
          datos_any$anweight_complet
        )
      )
    }
  )
)

resultados_temporales


# Número de trabajadores analizados por año

table(datos_temporales$any)


# Gráfico de la evolución de la satisfacción con la vida
# de los trabajadores entre 2002 y 2023

plot(
  resultados_temporales$any,
  resultados_temporales$satisfaccion_ponderada,
  type = "o",
  xlab = "Año",
  ylab = "Satisfacción media con la vida",
  main = "Evolución de la satisfacción con la vida de los trabajadores",
  ylim = c(1, 10),
  xaxt = "n"
)

axis(
  side = 1,
  at = resultados_temporales$any,
  labels = resultados_temporales$any,
  las = 2,
  cex.axis = 0.8
)


# Relación entre situación económica
# y satisfacción con la vida

datos_economicos <- datos_temporales[
  !is.na(datos_temporales$hincfel),
]


# Crear la satisfacción ponderada para cada observación

datos_economicos$satisfaccion_ponderada <- (
  datos_economicos$stflife *
    datos_economicos$anweight_complet
)


# Calcular los resultados por situación económica

resultados_economicos <- aggregate(
  cbind(
    satisfaccion_ponderada,
    anweight_complet
  ) ~ hincfel,
  data = datos_economicos,
  FUN = sum
)


# Calcular la satisfacción media ponderada

resultados_economicos$satisfaccion_media <- (
  resultados_economicos$satisfaccion_ponderada /
    resultados_economicos$anweight_complet
)


# Crear etiquetas descriptivas para las categorías

resultados_economicos$situacion_economica <- factor(
  resultados_economicos$hincfel,
  levels = c(1, 2, 3, 4),
  labels = c(
    "Vivir cómodamente",
    "Afrontar gastos",
    "Con dificultades",
    "Con muchas dificultades"
  )
)

resultados_economicos


# Gráfico de la relación entre situación económica
# y satisfacción con la vida

par(mar = c(8, 4, 4, 2) + 0.1)

barplot(
  resultados_economicos$satisfaccion_media,
  names.arg = FALSE,
  ylim = c(0, 10),
  ylab = "Satisfacción media con la vida",
  xlab = "",
  main = "Satisfacción con la vida según situación económica"
)

text(
  x = c(0.7, 1.9, 3.1, 4.3),
  y = -0.3,
  labels = c(
    "Vivir cómodamente",
    "Afrontar gastos",
    "Con dificultades",
    "Con muchas dificultades"
  ),
  srt = 45,
  adj = 1,
  xpd = TRUE,
  cex = 0.8
)


# Relación entre frecuencia de contacto social
# y satisfacción con la vida

datos_sociales <- datos_temporales[
  !is.na(datos_temporales$sclmeet),
]


# Crear la satisfacción ponderada para cada observación

datos_sociales$satisfaccion_ponderada <- (
  datos_sociales$stflife *
    datos_sociales$anweight_complet
)


# Calcular los resultados por frecuencia de contacto social

resultados_sociales <- aggregate(
  cbind(
    satisfaccion_ponderada,
    anweight_complet
  ) ~ sclmeet,
  data = datos_sociales,
  FUN = sum
)


# Calcular la satisfacción media ponderada

resultados_sociales$satisfaccion_media <- (
  resultados_sociales$satisfaccion_ponderada /
    resultados_sociales$anweight_complet
)


# Crear etiquetas descriptivas para las categorías

resultados_sociales$frecuencia_contacto <- factor(
  resultados_sociales$sclmeet,
  levels = c(1, 2, 3, 4, 5, 6, 7),
  labels = c(
    "Nunca",
    "Menos de una vez al mes",
    "Una vez al mes",
    "Varias veces al mes",
    "Una vez a la semana",
    "Varias veces a la semana",
    "Todos los días"
  )
)

resultados_sociales


# Gráfico de la relación entre frecuencia de contacto social
# y satisfacción con la vida

par(mar = c(9, 4, 4, 2) + 0.1)

barplot(
  resultados_sociales$satisfaccion_media,
  names.arg = FALSE,
  ylim = c(0, 10),
  ylab = "Satisfacción media con la vida",
  xlab = "",
  main = "Satisfacción con la vida según frecuencia de contacto social"
)

text(
  x = c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9),
  y = -0.3,
  labels = c(
    "Nunca",
    "Menos de una vez al mes",
    "Una vez al mes",
    "Varias veces al mes",
    "Una vez a la semana",
    "Varias veces a la semana",
    "Todos los días"
  ),
  srt = 45,
  adj = 1,
  xpd = TRUE,
  cex = 0.75
)


# Comparación de la satisfacción con la vida entre países

datos_temporales$satisfaccion_ponderada <- (
  datos_temporales$stflife *
    datos_temporales$anweight_complet
)

resultados_paises <- aggregate(
  cbind(
    satisfaccion_ponderada,
    anweight_complet
  ) ~ pais,
  data = datos_temporales,
  FUN = sum
)


# Calcular la satisfacción media ponderada por país

resultados_paises$satisfaccion_media <- (
  resultados_paises$satisfaccion_ponderada /
    resultados_paises$anweight_complet
)


# Ordenar los países de menor a mayor satisfacción

resultados_paises <- resultados_paises[
  order(resultados_paises$satisfaccion_media),
]

resultados_paises


# Gráfico de la satisfacción con la vida por país

par(mar = c(5, 10, 4, 2) + 0.1)

barplot(
  resultados_paises$satisfaccion_media,
  names.arg = resultados_paises$pais,
  horiz = TRUE,
  las = 1,
  xlim = c(0, 10),
  xlab = "Satisfacción media con la vida",
  main = "Satisfacción con la vida de los trabajadores por país"
)

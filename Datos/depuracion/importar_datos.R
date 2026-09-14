# M8 Reto 2 - Proyecto de Ciencia de Datos reproducible
# Importar datos originales

datos <- read.csv("Datos/original/Datafile-subset2.csv")

# Crear una copia para realizar la depuración
datos_depurados <- datos


# Depuración de las variables
# Los códigos especiales de la ESS que representan valores perdidos
# se recodifican como NA.
# Los códigos son específicos de cada variable.

datos_depurados$stflife[datos_depurados$stflife %in% c(77, 88, 99)] <- NA

datos_depurados$hincfel[datos_depurados$hincfel %in% c(7, 8, 9)] <- NA

datos_depurados$sclmeet[datos_depurados$sclmeet %in% c(77, 88, 99)] <- NA

datos_depurados$mnactic[datos_depurados$mnactic %in% c(77, 88, 99)] <- NA

datos_depurados$dcsfwrka[datos_depurados$dcsfwrka %in% c(6, 7, 8, 9)] <- NA

datos_depurados$jbprtfp[datos_depurados$jbprtfp %in% c(66, 77, 88, 99)] <- NA

datos_depurados$trdawrk[datos_depurados$trdawrk %in% c(6, 7, 8, 9)] <- NA

datos_depurados$stfmjob[datos_depurados$stfmjob %in% c(66, 77, 88, 99)] <- NA


# Crear el peso de análisis completo
# Equivale a anweight en las rondas donde está disponible

datos_depurados$anweight_complet <-
  datos_depurados$pspwght * datos_depurados$pweight


# Crear nombres de países a partir de los códigos de la ESS

paises <- c(
  AL = "Albania",
  AT = "Austria",
  BE = "Bélgica",
  BG = "Bulgaria",
  CH = "Suiza",
  CY = "Chipre",
  CZ = "Chequia",
  DE = "Alemania",
  DK = "Dinamarca",
  EE = "Estonia",
  ES = "España",
  FI = "Finlandia",
  FR = "Francia",
  GB = "Reino Unido",
  GR = "Grecia",
  HR = "Croacia",
  HU = "Hungría",
  IE = "Irlanda",
  IL = "Israel",
  IS = "Islandia",
  IT = "Italia",
  LT = "Lituania",
  LU = "Luxemburgo",
  LV = "Letonia",
  ME = "Montenegro",
  MK = "Macedonia del Norte",
  NL = "Países Bajos",
  NO = "Noruega",
  PL = "Polonia",
  PT = "Portugal",
  RO = "Rumanía",
  RS = "Serbia",
  RU = "Rusia",
  SE = "Suecia",
  SI = "Eslovenia",
  SK = "Eslovaquia",
  TR = "Turquía",
  UA = "Ucrania",
  XK = "Kosovo"
)

datos_depurados$pais <- paises[datos_depurados$cntry]


# Identificar los países presentes en las 11 rondas

paises_temporales <- c(
  "Bélgica",
  "Suiza",
  "Alemania",
  "España",
  "Finlandia",
  "Francia",
  "Reino Unido",
  "Hungría",
  "Irlanda",
  "Países Bajos",
  "Noruega",
  "Polonia",
  "Portugal",
  "Suecia",
  "Eslovenia"
)

datos_depurados$pais_temporal <-
  datos_depurados$pais %in% paises_temporales

# Identificar les persones amb activitat laboral remunerada
datos_depurados$trabajador <- datos_depurados$mnactic == 1

# Crear la variable any a partir de la ronda del ESS
datos_depurados$any <- c(
  `1` = 2002,
  `2` = 2004,
  `3` = 2006,
  `4` = 2008,
  `5` = 2010,
  `6` = 2012,
  `7` = 2014,
  `8` = 2016,
  `9` = 2018,
  `10` = 2020,
  `11` = 2023
)[as.character(datos_depurados$essround)]


# Exportación de los datos depurados

write.csv(
  datos_depurados,
  "Datos/depurada/datos_depurados.csv",
  row.names = FALSE
)
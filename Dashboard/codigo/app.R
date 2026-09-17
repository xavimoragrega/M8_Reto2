# ============================================================
# DASHBOARD - SATISFACCIÓN CON LA VIDA DE LOS TRABAJADORES
# M8 - Reto 2
# ============================================================

# Cargar paquetes
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(sf)
library(rnaturalearth)

# ============================================================
# 1. CARGAR Y PREPARAR LOS DATOS
# ============================================================

# Cargar los datos depurados
datos <- read.csv("../../Datos/depurada/datos_depurados.csv")

# Seleccionar las personas que trabajan
# y tienen datos válidos
datos_trabajadores <- datos[
  datos$mnactic == 1 &
    !is.na(datos$stflife) &
    !is.na(datos$anweight_complet),
]

# Países que están presentes en las 11 rondas
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

# Seleccionar los datos de estos países
datos_temporales <- datos_trabajadores[
  datos_trabajadores$pais %in% paises_temporales,
]

# Datos que utilizaremos para el mapa
datos_mapa <- datos_trabajadores[
  datos_trabajadores$pais %in% paises_temporales,
]

# Cargar el mapa y seleccionar Europa
mapa_mundial <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

mapa_europa <- mapa_mundial[
  mapa_mundial$continent == "Europe",
]

# Función para calcular medias ponderadas
media_ponderada <- function(x, w) {
  weighted.mean(x, w, na.rm = TRUE)
}

# Años que se pueden seleccionar en el mapa
opciones_mapa <- c(
  "2002",
  "2004",
  "2006",
  "2008",
  "2010",
  "2012",
  "2014",
  "2016",
  "2018",
  "2020",
  "2023"
)

# ============================================================
# 2. INTERFAZ DEL DASHBOARD
# ============================================================

ui <- fluidPage(
  
  # Estilos del dashboard
  tags$head(
    tags$style(
      HTML(
        "
        /* Mantiene los filtros visibles al hacer scroll */
        @media (min-width: 768px) {
          .filtros-columna {
            position: sticky;
            top: 20px;
            align-self: flex-start;
          }
        }
        "
      )
    )
  ),
  
  # Título principal
  titlePanel(
    "Satisfacción con la vida de los trabajadores"
  ),
  
  fluidRow(
    
    # --------------------------------------------------------
    # FILTROS
    # --------------------------------------------------------
    
    column(
      width = 3,
      class = "filtros-columna",
      
      div(
        class = "well",
        
        h4("Filtros"),
        
        pickerInput(
          inputId = "paises",
          label = "Países",
          choices = paises_temporales,
          selected = paises_temporales,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE
          )
        ),
        
        br(),
        
        sliderTextInput(
          inputId = "periodo_mapa",
          label = "Año del mapa",
          choices = opciones_mapa,
          selected = "2023",
          grid = TRUE
        )
      )
    ),
    
    # --------------------------------------------------------
    # GRÁFICOS
    # --------------------------------------------------------
    
    column(
      width = 9,
      
      # ------------------------------------------------------
      # 1. MAPA
      # ------------------------------------------------------
      
      h3(
        "1. Cambio de la satisfacción con la vida respecto a 2002"
      ),
      
      plotOutput(
        "mapa_satisfaccion",
        height = "650px"
      ),
      
      br(),
      
      # ------------------------------------------------------
      # 2. EVOLUCIÓN TEMPORAL
      # ------------------------------------------------------
      
      h3(
        "2. Evolución de la satisfacción con la vida"
      ),
      
      plotOutput(
        "grafico_temporal",
        height = "500px"
      ),
      
      br(),
      
      # ------------------------------------------------------
      # 3. SITUACIÓN ECONÓMICA
      # ------------------------------------------------------
      
      h3(
        "3. Satisfacción según la situación económica"
      ),
      
      plotOutput(
        "grafico_economico",
        height = "550px"
      ),
      
      br(),
      
      # ------------------------------------------------------
      # 4. CONTACTO SOCIAL
      # ------------------------------------------------------
      
      h3(
        "4. Satisfacción según la frecuencia de contacto social"
      ),
      
      plotOutput(
        "grafico_social",
        height = "550px"
      ),
      
      br(),
      
      # ------------------------------------------------------
      # 5. COMPARACIÓN ENTRE PAÍSES
      # ------------------------------------------------------
      
      h3(
        "5. Comparación de la satisfacción entre países"
      ),
      
      plotOutput(
        "grafico_paises",
        height = "550px"
      )
    )
  )
)

# ============================================================
# 3. SERVIDOR
# ============================================================

server <- function(input, output, session) {
  
  # Datos que cambian según los países seleccionados
  datos_filtrados <- reactive({
    
    req(input$paises)
    
    datos_temporales[
      datos_temporales$pais %in% input$paises,
    ]
  })
  
  # ==========================================================
  # 1. GRÁFICO DE EVOLUCIÓN TEMPORAL
  # ==========================================================
  
  output$grafico_temporal <- renderPlot({
    
    datos_grafico <- datos_filtrados()
    
    # Calcular la media para cada año y país
    resultados <- do.call(
      rbind,
      lapply(
        split(
          datos_grafico,
          list(
            datos_grafico$any,
            datos_grafico$pais
          )
        ),
        function(df) {
          
          data.frame(
            any = unique(df$any),
            pais = unique(df$pais),
            satisfaccion_media = media_ponderada(
              df$stflife,
              df$anweight_complet
            )
          )
        }
      )
    )
    
    ggplot(
      resultados,
      aes(
        x = any,
        y = satisfaccion_media,
        group = pais
      )
    ) +
      geom_line(
        linewidth = 0.9
      ) +
      geom_point(
        size = 2
      ) +
      facet_wrap(
        ~ pais,
        ncol = 3
      ) +
      scale_x_continuous(
        breaks = sort(unique(resultados$any))
      ) +
      scale_y_continuous(
        limits = c(0, 10)
      ) +
      labs(
        x = "Año",
        y = "Satisfacción media",
        title = "¿Cómo ha evolucionado la satisfacción con la vida?",
        subtitle = "Evolución entre 2002 y 2023 en los países seleccionados"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        strip.text = element_text(
          face = "bold"
        )
      )
  })
  
  # ==========================================================
  # 2. HEATMAP DE SITUACIÓN ECONÓMICA
  # ==========================================================
  
  output$grafico_economico <- renderPlot({
    
    datos_grafico <- datos_filtrados()
    
    # Calcular la media para cada año y categoría económica
    resultados <- do.call(
      rbind,
      lapply(
        split(
          datos_grafico,
          list(
            datos_grafico$any,
            datos_grafico$hincfel
          )
        ),
        function(df) {
          
          if (nrow(df) == 0) {
            return(NULL)
          }
          
          data.frame(
            any = unique(df$any),
            hincfel = unique(df$hincfel),
            satisfaccion_media = media_ponderada(
              df$stflife,
              df$anweight_complet
            )
          )
        }
      )
    )
    
    # Poner los nombres de las categorías
    resultados$any <- factor(
      resultados$any,
      levels = sort(unique(datos_grafico$any))
    )
    
    resultados$hincfel <- factor(
      resultados$hincfel,
      levels = 1:4,
      labels = c(
        "Vive cómodamente",
        "Se las arregla",
        "Tiene dificultades",
        "Tiene muchas dificultades"
      )
    )
    
    ggplot(
      resultados,
      aes(
        x = any,
        y = hincfel,
        fill = satisfaccion_media
      )
    ) +
      geom_tile(
        colour = "white",
        linewidth = 0.4
      ) +
      geom_text(
        aes(
          label = sprintf(
            "%.1f",
            satisfaccion_media
          )
        ),
        colour = "white",
        size = 4,
        fontface = "bold"
      ) +
      scale_fill_gradientn(
        name = "Satisfacción\nmedia",
        colours = c(
          "#E8F1F7",
          "#BFD8E7",
          "#7FAAC3",
          "#4B7F9D",
          "#285C7A",
          "#123B5D"
        ),
        values = c(
          0,
          0.20,
          0.45,
          0.65,
          0.82,
          0.92
        ),
        limits = c(0, 10),
        na.value = "grey90"
      ) +
      scale_x_discrete(
        drop = FALSE
      ) +
      labs(
        x = "Año",
        y = "Situación económica percibida",
        title = "¿Qué diferencias se observan según la situación económica?",
        subtitle = "Satisfacción media según la situación económica percibida del hogar"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5
        ),
        axis.title = element_text(
          face = "bold"
        )
      )
  })
  
  # ==========================================================
  # 3. HEATMAP DE CONTACTO SOCIAL
  # ==========================================================
  
  output$grafico_social <- renderPlot({
    
    datos_grafico <- datos_filtrados()
    
    # Calcular la media para cada año y categoría social
    resultados <- do.call(
      rbind,
      lapply(
        split(
          datos_grafico,
          list(
            datos_grafico$any,
            datos_grafico$sclmeet
          )
        ),
        function(df) {
          
          if (nrow(df) == 0) {
            return(NULL)
          }
          
          data.frame(
            any = unique(df$any),
            sclmeet = unique(df$sclmeet),
            satisfaccion_media = media_ponderada(
              df$stflife,
              df$anweight_complet
            )
          )
        }
      )
    )
    
    # Poner los nombres de las categorías
    resultados$any <- factor(
      resultados$any,
      levels = sort(unique(datos_grafico$any))
    )
    
    resultados$sclmeet <- factor(
      resultados$sclmeet,
      levels = 1:7,
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
    
    ggplot(
      resultados,
      aes(
        x = any,
        y = sclmeet,
        fill = satisfaccion_media
      )
    ) +
      geom_tile(
        colour = "white",
        linewidth = 0.4
      ) +
      geom_text(
        aes(
          label = sprintf(
            "%.1f",
            satisfaccion_media
          )
        ),
        colour = "white",
        size = 4,
        fontface = "bold"
      ) +
      scale_fill_gradientn(
        name = "Satisfacción\nmedia",
        colours = c(
          "#E8F1F7",
          "#BFD8E7",
          "#7FAAC3",
          "#4B7F9D",
          "#285C7A",
          "#123B5D"
        ),
        values = c(
          0,
          0.20,
          0.45,
          0.65,
          0.82,
          0.92
        ),
        limits = c(0, 10),
        na.value = "grey90"
      ) +
      scale_x_discrete(
        drop = FALSE
      ) +
      labs(
        x = "Año",
        y = "Frecuencia de contacto social",
        title = "¿Qué diferencias se observan según el contacto social?",
        subtitle = "Satisfacción media según la frecuencia de contacto con otras personas"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5
        ),
        axis.title = element_text(
          face = "bold"
        )
      )
  })
  
  # ==========================================================
  # 4. COMPARACIÓN ENTRE PAÍSES
  # ==========================================================
  
  output$grafico_paises <- renderPlot({
    
    datos_grafico <- datos_filtrados()
    
    # Calcular la media para cada país
    resultados <- do.call(
      rbind,
      lapply(
        split(
          datos_grafico,
          datos_grafico$pais
        ),
        function(df) {
          
          data.frame(
            pais = unique(df$pais),
            satisfaccion_media = media_ponderada(
              df$stflife,
              df$anweight_complet
            )
          )
        }
      )
    )
    
    # Ordenar los países según su satisfacción
    resultados$pais <- reorder(
      resultados$pais,
      resultados$satisfaccion_media
    )
    
    ggplot(
      resultados,
      aes(
        x = satisfaccion_media,
        y = pais
      )
    ) +
      geom_col(
        width = 0.7,
        fill = "#123B5D"
      ) +
      geom_text(
        aes(
          label = sprintf(
            "%.1f",
            satisfaccion_media
          )
        ),
        hjust = -0.15,
        size = 4
      ) +
      scale_x_continuous(
        limits = c(0, 10.5)
      ) +
      labs(
        x = "Satisfacción media",
        y = NULL,
        title = "¿Cómo se diferencian los países en satisfacción?",
        subtitle = "Satisfacción media ponderada para los países seleccionados"
      ) +
      theme_minimal()
  })
  
  # ==========================================================
  # 5. MAPA
  # ==========================================================
  
  output$mapa_satisfaccion <- renderPlot({
    
    periodo <- as.numeric(input$periodo_mapa)
    
    # Función para calcular la media de cada país
    calcular_media_pais <- function(df, nombre_variable) {
      
      if (nrow(df) == 0) {
        
        resultado <- data.frame(
          cntry = character(0)
        )
        
        resultado[[nombre_variable]] <- numeric(0)
        
        return(resultado)
      }
      
      paises <- unique(df$cntry)
      
      resultados <- lapply(
        paises,
        function(pais_actual) {
          
          datos_pais <- df[
            df$cntry == pais_actual,
          ]
          
          data.frame(
            cntry = pais_actual,
            media = media_ponderada(
              datos_pais$stflife,
              datos_pais$anweight_complet
            )
          )
        }
      )
      
      resultado <- do.call(
        rbind,
        resultados
      )
      
      names(resultado)[2] <- nombre_variable
      
      resultado
    }
    
    # Calcular la media de 2002
    datos_2002 <- datos_mapa[
      as.numeric(as.character(datos_mapa$any)) == 2002,
    ]
    
    resultados_2002 <- calcular_media_pais(
      datos_2002,
      "satisfaccion_2002"
    )
    
    # Calcular la media del año seleccionado
    datos_periodo <- datos_mapa[
      as.numeric(as.character(datos_mapa$any)) == periodo,
    ]
    
    resultados_periodo <- calcular_media_pais(
      datos_periodo,
      "satisfaccion_periodo"
    )
    
    # Juntar los resultados
    resultados_mapa <- merge(
      resultados_2002,
      resultados_periodo,
      by = "cntry",
      all = TRUE
    )
    
    # Calcular el cambio respecto a 2002
    resultados_mapa$cambio <-
      resultados_mapa$satisfaccion_periodo -
      resultados_mapa$satisfaccion_2002
    
    # Juntar los datos con el mapa de Europa
    mapa_resultados <- merge(
      mapa_europa,
      resultados_mapa,
      by.x = "iso_a2_eh",
      by.y = "cntry",
      all.x = TRUE
    )
    
    # Seleccionar los países que tienen datos
    mapa_etiquetas <- mapa_resultados[
      !is.na(mapa_resultados$cambio),
    ]
    
    # Calcular un punto dentro de cada país
    # para colocar las etiquetas
    if (nrow(mapa_etiquetas) > 0) {
      
      mapa_etiquetas_proyectado <- st_transform(
        mapa_etiquetas,
        3035
      )
      
      puntos_etiqueta <- st_point_on_surface(
        st_geometry(
          mapa_etiquetas_proyectado
        )
      )
      
      # Volver a la proyección del mapa
      puntos_etiqueta <- st_transform(
        puntos_etiqueta,
        st_crs(mapa_europa)
      )
      
      # Obtener las coordenadas
      coordenadas_etiquetas <- st_coordinates(
        puntos_etiqueta
      )
      
      # Crear los datos para las etiquetas
      datos_etiquetas <- data.frame(
        X = coordenadas_etiquetas[, 1],
        Y = coordenadas_etiquetas[, 2],
        cambio = mapa_etiquetas$cambio
      )
      
      # Evitar mostrar -0.0
      datos_etiquetas$cambio_etiqueta <-
        datos_etiquetas$cambio
      
      datos_etiquetas$cambio_etiqueta[
        abs(datos_etiquetas$cambio_etiqueta) < 0.05
      ] <- 0
      
    } else {
      
      # Crear un dataframe vacío si no hay datos
      datos_etiquetas <- data.frame(
        X = numeric(0),
        Y = numeric(0),
        cambio = numeric(0),
        cambio_etiqueta = numeric(0)
      )
    }
    
    # Crear el mapa
    ggplot(
      mapa_resultados
    ) +
      geom_sf(
        aes(
          fill = cambio
        ),
        colour = "white",
        linewidth = 0.2
      ) +
      
      # Naranja = disminución
      # Gris claro = cambio cercano a 0
      # Azul = aumento
      scale_fill_gradient2(
        name = "Cambio respecto\na 2002",
        low = "#D55E00",
        mid = "#F2F2F2",
        high = "#0072B2",
        midpoint = 0,
        limits = c(-2, 2),
        na.value = "#BDBDBD"
      ) +
      
      # Mostrar el cambio dentro de cada país
      geom_text(
        data = datos_etiquetas,
        aes(
          x = X,
          y = Y,
          label = sprintf(
            "%+.1f",
            cambio_etiqueta
          )
        ),
        inherit.aes = FALSE,
        size = 3.5,
        fontface = "bold"
      ) +
      
      coord_sf(
        xlim = c(-12, 35),
        ylim = c(34, 72),
        expand = FALSE
      ) +
      
      labs(
        title = "¿Cómo ha cambiado la satisfacción desde 2002?",
        subtitle = paste(
          "Cambio acumulado respecto a 2002 · Año seleccionado:",
          periodo,
          "· Trabajadores de países presentes en las 11 rondas del ESS"
        )
      ) +
      
      theme_minimal() +
      
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right"
      )
  })
}

# ============================================================
# 4. EJECUTAR EL DASHBOARD
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
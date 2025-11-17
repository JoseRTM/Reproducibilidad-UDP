# Cargar el paquete shiny
library(shiny)
library(rio)
library(tidyverse)
library(DT)
library(shinydashboard)
data <- import("clase4_Shiny/casen_red.sav")
# Para hacer una app en shiny necesitamos dos componentes:
# El ui y el server.

# El ui (user interface) define lo que el usuario ve y con lo que interactúa.
# Es la capa visual de la aplicación:
# Contiene los elementos de entrada (input): cajas de texto, selectores, sliders, botones.
# Contiene los elementos de salida (output): gráficos, tablas, textos, indicadores.
# Define la organización de la pantalla: columnas, paneles, pestañas, layouts y estilos.

# El server contiene la lógica, es decir, el código que responde a las acciones del usuario.
# Es la capa reactiva:
# Lee los valores de input$....
# Procesa datos, realiza cálculos, filtros, simulaciones, modelos.
# Genera objetos de salida (gráficos, tablas, textos) mediante funciones como renderPlot(), renderTable(), renderText(), etc.
# Actualiza dinámicamente lo que aparece en el ui.

##################################
# EJEMPLO 1 - fluidPage() básico #
##################################
# fluidPage() crea una página web responsiva basada en el sistema de grillas de Bootstrap.

# características
# 1. Layout responsivo
# La página se adapta automáticamente al tamaño de la pantalla: PC, tablet, celular.
# Full-width, fluido → ocupa todo el ancho disponible.

ui <- fluidPage(
  # Todo lo que va dentro se organiza en una página "fluida"
  h2("Ejemplo 1: fluidPage() básico"), # h es el encabezado el numero define el tamaño (- número, + grande)
  p("Esta app usa solo fluidPage() con texto fijo."), # p es un párrafo
  
  # Mostramos información muy simple de la base 'data'
  h4("Vista rápida de la base 'data':"),
  verbatimTextOutput("resumen_data") # crea en el ui un espacio donde se mostrará texto tal como aparece en la consola de R.
)

server <- function(input, output, session) {
  # Mostramos un resumen de la base de datos
  output$resumen_data <- renderPrint({
    summary(data)   # usa el objeto 'data' ya cargado
  })
}

shinyApp(ui, server)

###############
# EJERCICIO 1 #
###############

# importe la base que contiene los datos de la PAES
# Nombrela de forma diferente a data

# Crear una app que incluya:
 # Un título en h1
 # un párrafo que describa que indique el puntaje mínimo y máximo (100 y 1000)
 # Una tabla resumen de los datos

##################################################
# EJEMPLO 2 - titlePanel()                       #
# Agregamos titlePanel() dentro de fluidPage()   #
##################################################
# Dentro de fluidPage() se colocan elementos como:
# titlePanel()
# Genera un encabezado visible.
# Normalmente es el primer elemento dentro de fluidPage().

ui <- fluidPage(
  titlePanel("Ejemplo 2: uso de titlePanel()"),
  
  p("Este título aparece como encabezado grande arriba."),
  
  h4("Resumen de la variable v18 (pago de arriendo):"),
  verbatimTextOutput("resumen_v18")
)

server <- function(input, output, session) {
  output$resumen_v18 <- renderPrint({
    summary(data$v18)
  })
}

shinyApp(ui, server)

###############
# EJERCICIO 2 #
###############

# Crear un shinyapp sobre la variable puntajeNEM con un título apropiado usando titlePanel
# Incluya el resultado del resumen de la variable

###############################
# EJEMPLO 3 - sidebarLayout() # 
###############################
# Divide la pantalla en dos zonas:
# sidebarPanel(): controles, filtros, inputs.
# mainPanel(): gráficos, tablas, resultados.
# Mantiene proporciones responsivas.
# Es uno de los layouts más usados en apps simples.

ui <- fluidPage(
  titlePanel("Ejemplo 3: sidebarLayout()"),
  
  # sidebarLayout()
  sidebarLayout(
    
    # sidebarPanel(): típicamente controles, filtros, inputs
    sidebarPanel(
      h4("Panel lateral (sidebar)"),
      p("Aquí podríamos poner filtros, por ejemplo:"),
      
      # Input simple (sin usar todavía en el server)
      selectInput(
        inputId = "sexo",
        label   = "Filtrar por sexo (solo demostración, no aplica filtro):",
        choices = sort(unique(data$sexo))
      )
    ),
    
    # mainPanel(): resultados, gráficos, tablas
    mainPanel(
      h4("Panel principal (mainPanel)"),
      p("Mostramos un resumen de la variable edad."),
      verbatimTextOutput("resumen_edad")
    )
  )
)

server <- function(input, output, session) {
  output$resumen_edad <- renderPrint({
    summary(data$edad)
  })
}

shinyApp(ui, server)

###############
# EJERCICIO 3 #
###############

# Incorpore un sidepanel y un mainpanel a la shinyapp del ejercicio anterior. 
# proponga en el sidepanel un filtro por decil de ingreso
# disponga una tabla resumen de la variable puntaje NEM

#####################################
# EJEMPLO 4 - fluidRow() y column() #
#####################################
# Son elementos del sistema de grillas de Bootstrap.
# fluidRow() define una fila horizontal.
# column(width = X, ...)
# Define una columna dentro de una fila, donde el ancho va de 1 a 12.
ui <- fluidPage(
  titlePanel("Ejemplo 4: fluidRow() y column()"),
  
  # Una fila con dos columnas: 4 + 8 = 12
  fluidRow(
    column(
      width = 4,
      h4("Columna izquierda (4/12)"),
      p("Mostramos tabla de frecuencias de 'sexo'."),
      tableOutput("tabla_sexo")
    ),
    column(
      width = 8,
      h4("Columna derecha (8/12)"),
      p("Mostramos distribución de 'edad' (histograma)."),
      plotOutput("hist_edad", height = "300px")
    )
  )
)

server <- function(input, output, session) {
  
  output$tabla_sexo <- renderTable({
    # Tabla de frecuencias simples
    as.data.frame(table(data$sexo), responseName = "Frecuencia")
  })
  
  output$hist_edad <- renderPlot({
    hist(
      data$edad,
      main = "Histograma de edad",
      xlab = "Edad",
      col  = "steelblue",
      border = "white"
    )
  })
}

shinyApp(ui, server)

###############
# EJERCICIO 4 #
###############

# Utilice el sistema de grillas para hacer dos paneles.
# Ambos paneles deben ser del mismo tamaño
# En el panel de la izquierda incluya un histograma de la variable puntaje NEM
# En el panel derecho incluya una tabla de frecuencia de la variable ingreso

############################################################
# EJEMPLO 5 - Inputs y Outputs (textInput, selectInput,
#              plotOutput, tableOutput)
############################################################
# Inputs = elementos que permiten al usuario ingresar información.
# Sus valores se leen desde input$nombre.
# textInput()
# Argumentos:
# inputId: nombre interno del input
# label: texto visible
# value: valor inicial
# selectInput(): menú desplegable con opciones.
# Argumentos:
# inputId
# label
# choices: opciones posibles
# selected: valor por defecto
# multiple: TRUE/FALSE

ui <- fluidPage(
  # Título principal
  titlePanel("Ejemplo 5: reactividad simple con sexo"),
  
  # Layout con barra lateral + panel principal
  sidebarLayout(
    
    # --------------------- PANEL LATERAL ---------------------
    sidebarPanel(
      h4("Filtros"),
      
      # selectInput(): menú desplegable para elegir sexo
      selectInput(
        inputId = "sexo",                     # nombre interno
        label   = "Selecciona un sexo:",      # texto visible
        choices = c("Todos", sort(unique(data$sexo))),
        selected = "Todos"
      ),
      
      p("El histograma y el resumen se actualizan según el sexo seleccionado.")
    ),
    
    # --------------------- PANEL PRINCIPAL --------------------
    mainPanel(
      h4("Resumen de edad según el filtro de sexo"),
      
      # verbatimTextOutput(): muestra texto estilo consola
      verbatimTextOutput("resumen_edad"),
      
      br(), # salto de espacio
      h4("Histograma de edad"),
      
      # plotOutput(): espacio para el gráfico
      plotOutput("hist_edad", height = "300px")
    )
  )
)

server <- function(input, output, session) {
  
  # Datos filtrados según el sexo seleccionado ----------------
  datos_filtrados <- reactive({ # reactive es el reactivo para manipulación
    df <- data
    
    # Si el usuario elige un sexo específico, filtramos
    if (input$sexo != "Todos") {
      df <- df[df$sexo == input$sexo, , drop = FALSE]
    }
    
    df
  })
  
  # Resumen de edad (verbatimTextOutput + renderPrint) --------
  output$resumen_edad <- renderPrint({
    df <- datos_filtrados()
      summary(df$edad)
    }
  )
  
  # Histograma de edad (plotOutput + renderPlot) --------------
  output$hist_edad <- renderPlot({
    df <- datos_filtrados()
    hist(
      df$edad,
      main = paste("Histograma de edad - Sexo:", input$sexo),
      xlab = "Edad",
      col  = "steelblue",
      border = "white"
    )
  })
}

shinyApp(ui, server)

###############
# EJERCICIO 5 #
###############

# Ejecute una shinyapp para visualizar un histograma de puntaje nem utilizando un
# filtro por decil de ingreso
# en el mainpanel ponga arriba el gráfico y abajo el summary

##########################################
# EJEMPLO D1 - Estructura mínima dashboard
##########################################

ui <- dashboardPage(
  
  # Encabezado (barra superior)
  dashboardHeader(
    title = "Dashboard CASEN",
    titleWidth = 300
  ),
  
  # Barra lateral
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home"))
    )
  ),
  
  # Cuerpo
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "inicio",
        h2("Bienvenido al dashboard CASEN"),
        p("Este es un ejemplo mínimo de shinydashboard.")
      )
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)

# puedes revisar la lista de iconos en este link https://fontawesome.com/search#icongrid

##################################################
# EJEMPLO D2 - Menú lateral con dos pestañas
##################################################

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Dashboard CASEN",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Exploración gráfica", tabName = "exploracion", icon = icon("chart-bar")),
      menuItem("Tabla de datos",      tabName = "tabla",       icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "exploracion",
        h2("Pestaña: Exploración gráfica"),
        p("Aquí iremos agregando filtros y gráficos.")
      ),
      tabItem(
        tabName = "tabla",
        h2("Pestaña: Tabla de datos"),
        p("Aquí mostraremos una tabla con la base CASEN.")
      )
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)

# NOTA: menuItem(tabName = "X") ↔ tabItem(tabName = "X").
# Si tabName no coincide, la pestaña no se muestra.

########################################################
# EJEMPLO D3 - box() con gráfico y tabla estáticos
########################################################

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Dashboard CASEN",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Exploración gráfica", tabName = "exploracion", icon = icon("chart-bar")),
      menuItem("Tabla de datos",      tabName = "tabla",       icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Pestaña exploración
      tabItem(
        tabName = "exploracion",
        
        fluidRow(
          box(
            title = "Histograma de pago de arriendo (v18)",
            status = "primary",
            solidHeader = TRUE, # Color sólido en el encabezado
            width = 8,
            plotOutput("hist_v18", height = "300px") # Nombre del output que tenemos que linkear en el server
          ),
          box(
            title = "Resumen numérico de v18",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            verbatimTextOutput("resumen_v18")
          )
        )
      ),
      
      # Pestaña tabla
      tabItem(
        tabName = "tabla",
        fluidRow(
          box(
            title = "Primeras filas de la base",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            tableOutput("head_data")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$hist_v18 <- renderPlot({
    hist(
      data$v18,
      main = "Histograma de v18",
      xlab = "Pago de arriendo (v18)",
      col  = "steelblue",
      border = "white"
    )
  })
  
  output$resumen_v18 <- renderPrint({
    summary(data$v18)
  })
  
  output$head_data <- renderTable({
    head(data[, c("nse", "sexo", "edad", "region", "v18")])
  })
}

shinyApp(ui, server)


########################################################
# EJEMPLO D4 - Filtros + gráfico y resumen por grupo
########################################################

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Dashboard: pago de arriendo (v18)",
    titleWidth = 350
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Exploración gráfica", tabName = "exploracion", icon = icon("chart-bar")),
      menuItem("Tabla de datos",      tabName = "tabla",       icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # --------- Pestaña 1: Exploración gráfica ----------
      tabItem(
        tabName = "exploracion",
        
        fluidRow(
          # Caja izquierda: filtros
          box(
            title = "Filtros y opciones",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            
            selectInput( 
              inputId = "var_grupo",
              label   = "Variable para agrupar el pago de arriendo:",
              choices = c(
                "Nivel socioeconómico" = "nse",
                "Sexo"                 = "sexo",
                "Región"               = "region"
              ),
              selected = "nse"
            ),
            
            sliderInput( # Agregamos un slider para controlar el filtro de edad
              inputId = "edad_rango",
              label   = "Rango de edad:",
              min     = min(data$edad, na.rm = TRUE),
              max     = max(data$edad, na.rm = TRUE),
              value   = c(
                min(data$edad, na.rm = TRUE),
                max(data$edad, na.rm = TRUE)
              )
            ),
            
            checkboxInput( # Agregamos un botón para activar y desactivar una función
              inputId = "log_v18",
              label   = "Usar escala logarítmica para v18",
              value   = FALSE
            )
          ),
          
          # Caja derecha: gráfico
          box(
            title = "Pago de arriendo (v18) por grupo",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            
            plotOutput("plot_arriendo", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Resumen numérico por grupo",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            tableOutput("tabla_resumen")
          )
        )
      ),
      
      # --------- Pestaña 2: Tabla simple (sin DT todavía) ----------
      tabItem(
        tabName = "tabla",
        fluidRow(
          box(
            title = "Datos filtrados (según rango de edad)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            tableOutput("tabla_datos")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Datos filtrados por edad
  datos_filtrados <- reactive({
    data %>%
      filter(
        !is.na(v18),
        !is.na(edad),
        edad >= input$edad_rango[1],
        edad <= input$edad_rango[2]
      )
  })
  
  # Resumen por grupo
  resumen_por_grupo <- reactive({
    df <- datos_filtrados()
    if (nrow(df) == 0) return(NULL)
    
    df %>%
      group_by(.data[[input$var_grupo]]) %>%
      summarise(
        n       = n(),
        media   = mean(v18, na.rm = TRUE),
        mediana = median(v18, na.rm = TRUE),
        sd      = sd(v18, na.rm = TRUE),
        min     = min(v18, na.rm = TRUE),
        max     = max(v18, na.rm = TRUE)
      ) %>%
      ungroup()
  })
  
  # Gráfico
  output$plot_arriendo <- renderPlot({
    df <- datos_filtrados()
    if (nrow(df) == 0) {
      plot.new()
      title("No hay datos para el filtro seleccionado")
      return(invisible(NULL))
    }
    
    p <- ggplot(
      df,
      aes(x = .data[[input$var_grupo]], y = v18)
    ) +
      geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red") +
      labs(
        x = input$var_grupo,
        y = "Pago de arriendo (v18)",
        title = "Distribución de v18 por grupo"
      ) +
      theme_minimal()
    
    if (input$log_v18) {
      p <- p + scale_y_log10()
    }
    
    p
  })
  
  # Tabla resumen
  output$tabla_resumen <- renderTable({
    resumen_por_grupo()
  })
  
  # Tabla de datos filtrados
  output$tabla_datos <- renderTable({
    datos_filtrados() %>%
      select(nse, sexo, edad, region, v18) %>%
      head(50)  # para que no sea enorme
  })
}

shinyApp(ui, server)


########################################################
# EJEMPLO D5 - Uso de DT
########################################################

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Dashboard: pago de arriendo (v18)",
    titleWidth = 350
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Exploración gráfica", tabName = "exploracion", icon = icon("chart-bar")),
      menuItem("Tabla de datos",      tabName = "tabla",       icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # --------- Pestaña 1: Exploración gráfica ----------
      tabItem(
        tabName = "exploracion",
        
        fluidRow(
          # Caja izquierda: filtros
          box(
            title = "Filtros y opciones",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            
            selectInput(
              inputId = "var_grupo",
              label   = "Variable para agrupar el pago de arriendo:",
              choices = c(
                "Nivel socioeconómico" = "nse",
                "Sexo"                 = "sexo",
                "Región"               = "region"
              ),
              selected = "nse"
            ),
            
            sliderInput(
              inputId = "edad_rango",
              label   = "Rango de edad:",
              min     = min(data$edad, na.rm = TRUE),
              max     = max(data$edad, na.rm = TRUE),
              value   = c(
                min(data$edad, na.rm = TRUE),
                max(data$edad, na.rm = TRUE)
              )
            ),
            
            checkboxInput(
              inputId = "log_v18",
              label   = "Usar escala logarítmica para v18",
              value   = FALSE
            )
          ),
          
          # Caja derecha: gráfico
          box(
            title = "Pago de arriendo (v18) por grupo",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            
            plotOutput("plot_arriendo", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Resumen numérico por grupo",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("tabla_resumen") # Sustituimos tableOutput() por DTOutput()
          )
        )
      ),
      
      # --------- Pestaña 2: Tabla simple (con DT) ----------
      tabItem(
        tabName = "tabla",
        fluidRow(
          box(
            title = "Datos filtrados (según rango de edad)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("tabla_datos")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Datos filtrados por edad
  datos_filtrados <- reactive({
    data %>%
      filter(
        !is.na(v18),
        !is.na(edad),
        edad >= input$edad_rango[1],
        edad <= input$edad_rango[2]
      ) %>% 
      mutate(nse = factor(nse),
             sexo = factor(sexo),
             region = factor(region))
  })
  
  # Resumen por grupo
  resumen_por_grupo <- reactive({
    df <- datos_filtrados()
    df %>%
      group_by(.data[[input$var_grupo]]) %>%
      summarise(
        n       = n(),
        media   = mean(v18, na.rm = TRUE),
        mediana = median(v18, na.rm = TRUE),
        sd      = sd(v18, na.rm = TRUE),
        min     = min(v18, na.rm = TRUE),
        max     = max(v18, na.rm = TRUE)
      ) %>%
      ungroup()
  })
  
  # Gráfico
  output$plot_arriendo <- renderPlot({
    df <- datos_filtrados()
    p <- ggplot(
      df,
      aes(x = .data[[input$var_grupo]], y = v18)
    ) +
      geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red") +
      labs(
        x = input$var_grupo,
        y = "Pago de arriendo (v18)",
        title = "Distribución de v18 por grupo"
      ) +
      theme_minimal()
    
    if (input$log_v18) {
      p <- p + scale_y_log10()
    }
    
    p
  })
  
  # Tabla resumen
  output$tabla_resumen <- renderDT({
    resumen_por_grupo()
  })
  
  # Tabla de datos filtrados
  output$tabla_datos <- renderDT({
    datos_filtrados() %>%
      select(nse, sexo, edad, region, v18) %>%
      head(50)  # para que no sea enorme
  })
}

shinyApp(ui, server)

###################
# EJERCICIO FINAL #
###################

# crear un shinydashboard con dos pestañas.
# la primera pestaña debe incluir un input que agrupe por decil de ingreso
# debe limpiar el 99, -88 y 0 de las variables  en el reactive
# En el main, muestre un boxplot de la variable puntaje NEM
# Abajo muestre una tabla con DT
# la segunda pestaña debe incluir una tabla de la base completa.


# GLOSARIO DE CÓDIGO
# 1. menuItem()
# Define un ítem dentro del menú lateral del dashboard.
# menuItem("Texto", tabName = "nombre_tab", icon = icon("home"))
# Argumentos principales
# text: texto que se muestra en el menú.
# tabName: nombre interno que debe coincidir con el tabItem() correspondiente.
# icon: ícono (Font Awesome) para el menú; se define con icon("nombre").

# 2. tabItems()
# Contenedor que agrupa todas las pestañas (tabItem) del dashboard.
# tabItems(
#   tabItem(tabName = "...", ...),
#   tabItem(tabName = "...", ...)
# )
# Argumentos
# Una lista de objetos tabItem().

# 3. tabItem()
#Define el contenido mostrado cuando un menuItem() está activo.
#Uso
#tabItem(
#  tabName = "simulacion",
#  h2("Contenido de la pestaña")
#)
#Argumentos principales
#tabName: debe coincidir con el del menuItem().
#Contenido interno (textos, gráficos, cajas, filas, etc.).

# 4. selectInput()
# Crea un selector de opciones (menu desplegable).
# Uso
# selectInput("id", "Etiqueta", choices = c("A", "B"), selected = "A")
# Argumentos
# inputId: nombre interno del input → se accederá vía input$id.
# label: texto visible para el usuario.
# choices: vector o lista de opciones disponibles.
# selected (opcional): valor seleccionado por defecto.
# multiple (opcional): permite seleccionar más de uno.

# 5. sliderInput()
#Crea un control deslizante (válido para números o fechas).

#Uso
#sliderInput("id", "Edad:", min = 18, max = 80, value = c(18, 80))
#Argumentos principales
#inputId: nombre interno.
#label: descripción del control.
#min / max: valores mínimo y máximo.
#value:
#  un número → slider simple;
#un vector de 2 valores → rango.
#step (opcional): tamaño de los saltos.
#animate (opcional): agrega animación.

# 6. checkboxInput()
#Crea una casilla para seleccionar TRUE/FALSE.

#Uso
#checkboxInput("id", "Usar escala log", value = FALSE)
#Argumentos
#inputId: nombre interno.
#label: texto visible.
#value: valor inicial (TRUE o FALSE).

# 7. plotOutput()
#Espacio reservado para mostrar un gráfico generado en el servidor.

#Uso
#plotOutput("id", height = "400px")
#Argumentos principales
#outputId: nombre del objeto de salida → debe coincidir con output$id en el server.
#width (opcional): ancho del espacio (ej. "100%", "400px").
#height (opcional): alto del área del gráfico.
#click, hover, brush (opcionales): permiten interacciones.


#8. fluidRow()
#Crea una fila dentro del sistema de grillas (12 columnas) de Bootstrap.
#Uso
#fluidRow(
#  box(width = 4, ...),
#  box(width = 8, ...)
#)

#Qué hace
#Alinea horizontalmente los elementos dentro de la fila.
#Cada elemento ocupa un ancho (width) entre 1 y 12.
#Si la suma supera 12, el exceso pasa a una nueva fila automáticamente.
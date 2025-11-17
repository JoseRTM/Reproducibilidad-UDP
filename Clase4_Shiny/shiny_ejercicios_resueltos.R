###############
# EJERCICIO 1 #
###############

# importe la base que contiene los datos de la PAES
# Nombrela de forma diferente a data
paes <- import("clase4_Shiny/mineduc_paes.rds")
# Crear una app que incluya:
# Un título en h1
# un párrafo que describa que indique el puntaje mínimo y máximo (100 y 1000)
# Una tabla resumen de los datos

ui <- fluidPage(
  h2(" Puntajes de la PAES"),
  p("Los puntajes de la prueba van desde el 100 hasta el 1000"),
  h4("Vista rápida de la base de datos"),
  verbatimTextOutput("resumen")
)
server <- function( input, output, session) {
  output$resumen <- renderPrint({
    summary(paes)
  })
}

shinyApp(ui, server)

###############
# EJERCICIO 2 #
###############

# Crear un shinyapp sobre la variable puntajeNEM con un título apropiado usando titlePanel
# Incluya el resultado del resumen de la variable

ui <- fluidPage(
  titlePanel("Resumen de puntaje NEM"),
  verbatimTextOutput("resumen")
)

server <- function(input, output, session) {
  output$resumen <- renderPrint({
    summary(paes$PTJE_NEM)
  })
}

shinyApp(ui, server)




###############
# EJERCICIO 3 #
###############

# Incorpore un sidepanel y un mainpanel a la shinyapp del ejercicio anterior. 
# proponga en el sidepanel un filtro por decil de ingreso
# disponga una tabla resumen de la variable puntaje NEM

ui <- fluidPage(
  titlePanel("Panel principal Ejercicio 3"),
  sidebarLayout(
    sidebarPanel(
      h4("Filtro por decil de ingreso"),
      selectInput(
        inputId = "decil",
        label = "Filtrar por decil de ingreso",
        choices = sort(unique(paes$INGRESO_PERCAPITA_GRUPO_FA))
      )
    ),
    mainPanel(
      h4("Resultados"),
      verbatimTextOutput("resumen")
    ) 
  )
)

server <- function(input, output, session) {
  output$resumen <- renderPrint({
    summary(paes$PTJE_NEM)
  }
  
  )
}

shinyApp(ui, server)

###############
# EJERCICIO 4 #
###############

# Utilice el sistema de grillas para hacer dos paneles.
# Ambos paneles deben ser del mismo tamaño
# En el panel de la izquierda incluya un histograma de la variable puntaje NEM
# En el panel derecho incluya una tabla de frecuencia de la variable ingreso

ui <- fluidPage(
  titlePanel("Paneles resumen PAES"),
  fluidRow(
    column(width = 6,
           h4("Histograma de puntaje NEM"),
           plotOutput("hist_nem", height = "300px")),
    column(width = 6,
           h4("Frecuencia de Decil de ingreso"),
           tableOutput("tabla_decil"))
  )
)

server <- function(input, output, session){
  
  output$tabla_decil <- renderTable({
    as.data.frame(table(paes$INGRESO_PERCAPITA_GRUPO_FA), responseName = "Frecuencia")
  })
  
  output$hist_nem <- renderPlot({
    hist(
      paes$PTJE_NEM,
      main = "Histograma de edad",
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

ui <- fluidPage(
  titlePanel("Ejercicio 5"),
  sidebarLayout(
    sidebarPanel(
      h4("Filtro por decil"),
      
      selectInput(
        inputId = "decil",
        label = "Selecciona el decil",
        choices = c("Todos", sort(unique(paes$INGRESO_PERCAPITA_GRUPO_FA))),
        selected = "Todos"
      )
    ),
    mainPanel(
      h4("Histograma del puntaje NEM por decil"),
      plotOutput("hist_nem", height = "300px"),
      br(),
      h4("Resumen del puntaje NEM"),
      verbatimTextOutput("tabla_nem")
    )
  )
)

server <- function(input, output, session) {
  df <- paes
  data_filtro <- reactive({
    if (input$decil != "Todos") {
      df <- df[df$INGRESO_PERCAPITA_GRUPO_FA == input$decil, , drop = F]
    }
    df
  })
  
  output$tabla_nem <- renderPrint({
    df <- data_filtro()
    summary(paes$PTJE_NEM)
  })
  
  output$hist_nem <- renderPlot({
    df <- data_filtro()
    hist(
      df$PTJE_NEM,
      main = paste("Histograma de edad - Sexo:", input$PTJE_NEM),
      xlab = "PTJE NEM",
      col  = "steelblue",
      border = "white"
    )
  }
  )
}


shinyApp(ui, server)

ui <- dashboardPage(
  
  dashboardHeader(title = "Ejercicio final",
                  titleWidth = 350),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Exploración gráfica", tabName = "grafico", icon = icon("chart-bar")),
      menuItem("Tabla de datos",      tabName = "tabla",   icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = "grafico",
        
        fluidRow(
          # Caja izquierda: filtros
          box(
            title = "Agrupación",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            
            selectInput(
              inputId = "decil",
              label   = "Agrupar por decil de ingreso",
              choices = c("Todo", sort(unique(paes$INGRESO_PERCAPITA_GRUPO_FA))),
              selected = "Todo"
            )
          ),
          
          
          box(
            title = "Boxplot puntaje NEM",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            
            plotOutput("boxplot_nem", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Datos filtrados (DT)",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            DTOutput("tabla_filtrada")
          )
        )
      ),
      
      
      tabItem(
        tabName = "tabla",
        fluidRow(
          box(
            title = "Datos completos (limpios)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            DTOutput("tabla_completa")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  datos_clean <- reactive({
    paes %>%
      mutate(
        INGRESO_PERCAPITA_GRUPO_FA = na_if(INGRESO_PERCAPITA_GRUPO_FA, 99),
        PTJE_NEM = ifelse(PTJE_NEM <= 0, NA, PTJE_NEM)
      ) %>%
      filter(!is.na(INGRESO_PERCAPITA_GRUPO_FA),
             !is.na(PTJE_NEM))
  })
  
  # 2) Datos filtrados por decil
  datos_filtrados <- reactive({
    df <- datos_clean()
    
    if (input$decil != "Todo") {
      df <- df %>%
        filter(as.character(INGRESO_PERCAPITA_GRUPO_FA) == input$decil)
    }
    
    df
  })
  
  # 3) Boxplot de PTJE_NEM por decil (usando datos filtrados)
  output$boxplot_nem <- renderPlot({
    df <- datos_filtrados()
    
    if (nrow(df) == 0) {
      plot.new()
      title("No hay datos para el filtro seleccionado")
      return(invisible(NULL))
    }
    
    ggplot(df, aes(
      x = factor(INGRESO_PERCAPITA_GRUPO_FA),
      y = PTJE_NEM
    )) +
      geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red") +
      labs(
        x = "Decil de ingreso per cápita",
        y = "Puntaje NEM",
        title = "Distribución del puntaje NEM por decil de ingreso"
      ) +
      theme_minimal()
  })
  
  # 4) Tabla filtrada (pestaña gráfico)
  output$tabla_filtrada <- renderDT({
    datos_filtrados()
  })
  
  # 5) Tabla completa limpia (pestaña tabla)
  output$tabla_completa <- renderDT({
    datos_clean()
  })
}

shinyApp(ui, server)
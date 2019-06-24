library(shiny)
library(shinydashboard)
library(htmltools)
library(vembedr)

shinyUI(
  dashboardPage(skin = "green",
    dashboardHeader(title = "Accidentalidad en Medellín", titleWidth = 700),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem(text = "Acerca de", tabName = "acerca", icon=icon("play")),
        menuItem("Predicciones", tabName = "predicciones", icon=icon("code")),
        menuItem("Agrupamiento", tabName = "agrupamiento", icon=icon("sitemap"))
        # https://fontawesome.com/icons?d=gallery
      )
    ),
    
    
    dashboardBody(
      # within tabitems(), define the pages for sidebar menu items
      tabItems(
        tabItem(tabName = "acerca",h1("Accidentalidad en Medellín", style = "text-align: center;"),br(),
                h4("Video introductorio a la aplicación. Se explica el funcionamiento de la aplicación y que métodos se utilizaron para lograr los resultados obtenidos."),br(),
                tags$div(align = "center", embed_url("https://www.youtube.com/watch?v=tpYdGgzCW-M"))),
        
        #tabItem(tabName = "predicciones", embed_url("https://www.youtube.com/watch?v=tpYdGgzCW-M")),
        
        tabItem(tabName = "agrupamiento", h2("Mapa de accidentes en Medellín", 
                style = "text-align: center;"), 
                p("El agrupamiento de los barrios se realizó utilizando el algoritmo k-means. Se realizó el agrupamiento respecto a las variables: hora, día, periodo, clase, gravedad, diseño, nombre del día y mes. "), 
                p("Se determinó utilizar 8 grupos puesto que el algoritmo arrojaba un valor de suma total de cuadrados (total_ss) del 95%. Dicho valor es una medida de la calidad de la clasificación que el algoritmo ha encontrado."),plotOutput("k_means"), br(), p("De la gráfica anterior se puede observar que predominan los grupos 5 y 8. Se puede concluir que en el centro de la ciudad se presentan patrones de accidentalidad similares, una causa de esto es que el centro de Medellín es una de las zonas más congestionadas de la ciudad. "), p("Por otro lado, se evidencia que el nororiente y noreste de la ciudad presentan también patrones de accidentalidad similares. "),h4("A continuación podrá visualizar y buscar los barrios y grupos"), 
                dataTableOutput("mydatatable"))
      )
    )
  )
)
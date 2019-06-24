library(shiny)
library(shinydashboard)
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
        tabItem(tabName = "acerca",h1("Accidentalidad en Medellín", style = "text-align: center;"),br(),h4("Video introductorio a la aplicación. Se explica el funcionamiento de la aplicación y que métodos se utilizaron para lograr los resultados obtenidos."),br(),tags$video(src='intro.mp4', type = "video/mp4",width="800px", height="500px", controls="controls", style = "display: block; margin-left: auto; margin-right: auto;")),
        #tabItem(tabName = "predicciones", tags$video(src='reactive.mp4', type="video/mp4", width="350px", height="350px", controls="controls")),
        tabItem(tabName = "agrupamiento", h2("Mapa de accidentes en Medellín", style = "text-align: center;"),plotOutput("k_means"), br(),h4("A continuación podrá visualizar y buscar los barrios y grupos"), dataTableOutput("mydatatable"))
      )
    )
  )
)
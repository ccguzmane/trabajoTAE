library(shiny)
library(shinydashboard)
library(htmltools)
library(vembedr)
library(dplyr)
library(caret)
library(shinycssloaders)
library(lubridate)
library(shinyWidgets)

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
                    
                    tabItem(
                      tabName = "predicciones",
                      h2("Predicción de accidentes en Medellín"),
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("grupo", "Selecciona el grupo", list("Comuna","Barrio"),"Comuna"),
                          conditionalPanel(
                            condition = "input.grupo == 'Comuna'",
                            selectizeInput("selectComuna", "Comuna", choices = NULL, selected=NULL)
                          ),
                          conditionalPanel(
                            condition = "input.grupo == 'Barrio'",
                            selectizeInput("selectBarrio","Barrio", choices = NULL, selected = NULL)
                          ),
                          radioButtons("tipo", "Selecciona el tipo", c("Mes","Semana","Día"),"Mes"),
                          dateInput("fInicio", "Fecha de inicio", value = "2018-01-01" ,min="2018-01-01", max="2018-12-31" ), 
                          dateInput("fFin", "Fecha de fin", value="2018-12-31", min="2018-01-01", max="2018-12-31" )
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          conditionalPanel(
                            condition = "(input.tipo == 'Mes') && (input.grupo == 'Comuna')",
                            plotOutput("plotCMes") %>% withSpinner(color="#0dc5c1"),
                            plotOutput("plotCMes2") %>% withSpinner(color="#0dc5c1")
                          ),
                          conditionalPanel(
                            condition = "(input.tipo == 'Semana') && (input.grupo == 'Comuna')",
                            plotOutput("plotCSemana") %>% withSpinner(color="#0dc5c1"),
                            plotOutput("plotCSemana2") %>% withSpinner(color="#0dc5c1")
                          ),
                          conditionalPanel(
                            condition = "(input.tipo == 'Día') && (input.grupo == 'Comuna')",
                            plotOutput("plotCDia") %>% withSpinner(color="#0dc5c1"),
                            plotOutput("plotCDia2") %>% withSpinner(color="#0dc5c1")
                          ),
                          conditionalPanel(
                            condition = "(input.tipo == 'Mes') && (input.grupo == 'Barrio')",
                            plotOutput("plotBMes") %>% withSpinner(color="#0dc5c1"),
                            plotOutput("plotBMes2") %>% withSpinner(color="#0dc5c1")
                          ),
                          conditionalPanel(
                            condition = "(input.tipo == 'Semana') && (input.grupo == 'Barrio')",
                            plotOutput("plotBSemana") %>% withSpinner(color="#0dc5c1"),
                            plotOutput("plotBSemana2") %>% withSpinner(color="#0dc5c1")
                          ),
                          conditionalPanel(
                            condition = "(input.tipo == 'Día') && (input.grupo == 'Barrio')",
                            plotOutput("plotBDia") %>% withSpinner(color="#0dc5c1"),
                            plotOutput("plotBDia2") %>% withSpinner(color="#0dc5c1")
                          )
                        )
                      )
                      
                    ),
                    
                    tabItem(tabName = "agrupamiento", h2("Mapa de accidentes en Medellín", 
                                                         style = "text-align: center;"), 
                            p("El agrupamiento de los barrios se realizó utilizando el algoritmo k-means. Se realizó el agrupamiento respecto a las variables: hora, día, periodo, clase, gravedad, diseño, nombre del día y mes. "), 
                            p("Se determinó utilizar 8 grupos puesto que el algoritmo arrojaba un valor de suma total de cuadrados (total_ss) del 95%. Dicho valor es una medida de la calidad de la clasificación que el algoritmo ha encontrado."),plotOutput("k_means") %>% withSpinner(color="#0dc5c1"), br(), p("De la gráfica anterior se puede observar que predominan los grupos 5 y 8. Se puede concluir que en el centro de la ciudad se presentan patrones de accidentalidad similares, una causa de esto es que el centro de Medellín es una de las zonas más congestionadas de la ciudad. "), p("Por otro lado, se evidencia que el nororiente y noreste de la ciudad presentan también patrones de accidentalidad similares. "),h4("A continuación podrá visualizar y buscar los barrios y grupos"), 
                            dataTableOutput("mydatatable") %>% withSpinner(color="#0dc5c1"))
                  )
                )
  )
)
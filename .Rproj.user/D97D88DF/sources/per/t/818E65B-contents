library(shiny)
library(shinydashboard)
library(rgdal)
library(sp)
library(dplyr)

shinyServer(function(input, output, session){
  
  
  output$k_means <- renderPlot({
    
    #Lectura de datos
    total <- read.csv("./data/data-2014-2018.csv", encoding = "UTF-8")
    datos <- total[-c(173651,149226,148650,16711,37110,69754,65873),-1]
    datos$clase <- as.numeric(datos$clase)
    datos$gravedad <- as.numeric(datos$gravedad)
    datos$mes <- as.numeric(datos$mes)
    datos$dia_nombre <- as.numeric(datos$dia_nombre)
    datos$diseno <- as.numeric(datos$diseno)
    datos$radicado <- as.integer(datos$radicado)
    datos$hora <- as.numeric(datos$hora)
    by_barrio <- group_by(datos,barrio)
    summary_by_barrio <- summarize(by_barrio, mean_hora = mean(hora,na.rm = TRUE), mean_dia = mean(dia,na.rm = TRUE),mean_periodo = mean(periodo,na.rm = TRUE),mean_clase = mean(clase,na.rm = TRUE),mean_gravedad = mean(gravedad,na.rm = TRUE),mean_diseno = mean(diseno,na.rm = TRUE),mean_dia_nombre = mean(dia_nombre,na.rm = TRUE),mean_mes = mean(mes,na.rm = TRUE))
    barrios <- data.frame(summary_by_barrio, row.names = summary_by_barrio$barrio)
    barrios <- barrios[,-1]
    
    #K-means
    set.seed(20)
    clusters = kmeans(barrios,8,nstart = 50)
    restult <- clusters$cluster
    write.csv(restult,file = "./data/kmeans-result.csv")
    
    # Luego de ordenar los resultados obtenidos por la función k-means se
    # gráfican en un mapa de Medellín en formato shape. 
    med <- readOGR("./data/barrioVereda/BarrioVereda_2014.shp",layer = "BarrioVereda_2014")
    datos <- read.csv("./data/resultados-ordenados.csv", header = T, sep = ";", dec = ",", row.names = 1)
    med@data <- datos
    spplot(med,"grupo")
  })
  
  # Se agrega la tabla basada en los resultados obtenidos en el k-means
  output$mydatatable <- renderDataTable({
    tbl <- read.csv("./data/kmeans-result.csv")
    colnames(tbl) <- c("BARRIO","GRUPO")
    return(tbl)
  })
}
)
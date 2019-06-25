library(shiny)
library(shinydashboard)
library(rgdal)
library(sp)
library(dplyr)
library(caret)
library(lubridate)

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
  
  
  ##################
  ###################### Comuna
  #MES
  comunaPred <- read.csv("./data/comunaMes.csv", colClasses=c("MES"="character"))
  #comunaPred <- comuna2018("MES",99)
  comunas <- reactive({
    comunaPred <- comunaPred[(comunaPred$COMUNA == input$selectComuna), ] 
    comunaPred <- na.omit(comunaPred[(comunaPred$MES >= format(input$fInicio, format="%m")),  ])
    comunaPred <- na.omit(comunaPred[(comunaPred$MES <= format(input$fFin, format="%m")),  ])
  })
  
  output$plotCMes <- renderPlot({
    ggplot(data=comunas(), aes(x=MES, y=AÑO_2018_PRED, group=1)) +
      geom_line()+
      geom_point()+
      ggtitle("Modelo 2018, MSE 11.3")+
      xlab("Mes")+
      ylab("Accidentes")
  })
  
  #Semana
  comunaPWeek <- read.csv("./data/comunaSemana.csv", colClasses=c("SEMANA"="character"))
  #comunaPWeek <- comuna2018("SEMANA",99)
  comunasWeek <- reactive({
    comunaPWeek <- comunaPWeek[(comunaPWeek$COMUNA == input$selectComuna), ] 
    comunaPWeek <- na.omit(comunaPWeek[(comunaPWeek$SEMANA >= format(input$fInicio, format="%W")),  ])
    comunaPWeek <- na.omit(comunaPWeek[(comunaPWeek$SEMANA <= format(input$fFin, format="%W")),  ])
  })
  
  output$plotCSemana <- renderPlot({
    ggplot(data=comunasWeek(), aes(x=SEMANA, y=AÑO_2018_PRED, group=1)) +
      geom_line()+
      geom_point()+
      ggtitle("Modelo 2018, MSE 11.3")+
      xlab("Semana")+
      ylab("Accidentes")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  #DIA
  comunaPDay <- read.csv("./data/comunaDia.csv", colClasses=c("FECHA"="Date"))
  #comunaPDay <- comuna2018("DIA",99)
  comunasPDay <- reactive({
    comunaPDay <- comunaPDay[(comunaPDay$COMUNA == input$selectComuna), ] 
    comunaPDay <- na.omit(comunaPDay[(comunaPDay$FECHA >= input$fInicio),  ])
    comunaPDay <- na.omit(comunaPDay[(comunaPDay$FECHA <= input$fFin),  ])
  })
  output$plotCDia <- renderPlot({
    ggplot(data=comunasPDay(), aes(x=FECHA, y=AÑO_2018_PRED, group=1)) +
      geom_line()+
      geom_point()+
      ggtitle("Modelo 2018, MSE 11.3")+
      xlab("Día")+
      ylab("Accidentes")
  })
  ################# Barrio
  #MES
  barrioPred <- read.csv("./data/barrioMes.csv", colClasses=c("MES"="character"))
  #barrioPred <- barrio2018("MES",99)
  barrios <- reactive({
    barrioPred <- barrioPred[(barrioPred$BARRIO == input$selectBarrio), ] 
    barrioPred <- na.omit(barrioPred[(barrioPred$MES >= format(input$fInicio, format="%m")),  ])
    barrioPred <- na.omit(barrioPred[(barrioPred$MES <= format(input$fFin, format="%m")),  ])
  })
  
  output$plotBMes <- renderPlot({
    ggplot(data=barrios(), aes(x=MES, y=AÑO_2018_PRED, group=1)) +
      geom_line()+
      geom_point()+
      ggtitle("Modelo 2018, MSE 11.3")+
      xlab("Mes")+
      ylab("Accidentes")
  })
  #Semana
  barrioPWeek <- read.csv("./data/barrioSemana.csv", colClasses=c("SEMANA"="character"))
  #barrioPWeek <- barrio2018("SEMANA",99)
  barriosWeek <- reactive({
    barrioPWeek <- barrioPWeek[(barrioPWeek$BARRIO == input$selectBarrio), ] 
    barrioPWeek <- na.omit(barrioPWeek[(barrioPWeek$SEMANA >= format(input$fInicio, format="%W")),  ])
    barrioPWeek <- na.omit(barrioPWeek[(barrioPWeek$SEMANA <= format(input$fFin, format="%W")),  ])
  })
  
  output$plotBSemana <- renderPlot({
    ggplot(data=barriosWeek(), aes(x=SEMANA, y=AÑO_2018_PRED, group=1)) +
      geom_line()+
      geom_point()+
      ggtitle("Modelo 2018, MSE 11.3")+
      xlab("Semana")+
      ylab("Accidentes")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  #DIA
  barrioPDay <- read.csv("./data/barrioDia.csv", colClasses=c("FECHA"="Date"))
  #barrioPDay <- barrio2018("DIA",99)
  barriosPDay <- reactive({
    barrioPDay <- barrioPDay[(barrioPDay$BARRIO == input$selectBarrio), ] 
    barrioPDay <- na.omit(barrioPDay[(barrioPDay$FECHA >= input$fInicio),  ])
    barrioPDay <- na.omit(barrioPDay[(barrioPDay$FECHA <= input$fFin),  ])
  })
  output$plotBDia <- renderPlot({
    ggplot(data=barriosPDay(), aes(x=FECHA, y=AÑO_2018_PRED, group=1)) +
      geom_line()+
      geom_point()+
      ggtitle("Modelo 2018, MSE 11.3")+
      xlab("Día")+
      ylab("Accidentes")
  })
  ################################################################################################
  #### 2017-2018
  ###################### Comuna
  #MES
  comunaPred2 <- read.csv("./data/comunaMes2.csv", colClasses=c("MES"="character"))
  #comunaPred2 <- comuna2018("MES",99)
  comunas2 <- reactive({
    comunaPred2 <- comunaPred2[(comunaPred2$COMUNA == input$selectComuna), ] 
    comunaPred2 <- na.omit(comunaPred2[(comunaPred2$MES >= format(input$fInicio, format="%m")),  ])
    comunaPred2 <- na.omit(comunaPred2[(comunaPred2$MES <= format(input$fFin, format="%m")),  ])
  })
  
  output$plotCMes2 <- renderPlot({
    ggplot(data=comunas2(), aes(x=MES, y=AÑO_2018_PRED, group=1)) +
      geom_line()+
      geom_point()+
      ggtitle("Modelo 2017 - 2018, MSE 12")+
      xlab("Mes")+
      ylab("Accidentes")
  })
  
  #Semana
  comunaPWeek2 <- read.csv("./data/comunaSemana2.csv", colClasses=c("SEMANA"="character"))
  #comunaPWeek2 <- comuna2018("SEMANA",99)
  comunasWeek2 <- reactive({
    comunaPWeek2 <- comunaPWeek2[(comunaPWeek2$COMUNA == input$selectComuna), ] 
    comunaPWeek2 <- na.omit(comunaPWeek2[(comunaPWeek2$SEMANA >= format(input$fInicio, format="%W")),  ])
    comunaPWeek2 <- na.omit(comunaPWeek2[(comunaPWeek2$SEMANA <= format(input$fFin, format="%W")),  ])
  })
  
  output$plotCSemana2 <- renderPlot({
    ggplot(data=comunasWeek2(), aes(x=SEMANA, y=AÑO_2018_PRED, group=1)) +
      geom_line()+
      geom_point()+
      ggtitle("Modelo 2017 - 2018, MSE 12")+
      xlab("Semana")+
      ylab("Accidentes")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  #DIA
  comunaPDay2 <- read.csv("./data/comunaDia2.csv", colClasses=c("FECHA"="Date"))
  #comunaPDay2 <- comuna2018("DIA",99)
  comunasPDay2 <- reactive({
    comunaPDay2 <- comunaPDay2[(comunaPDay2$COMUNA == input$selectComuna), ] 
    comunaPDay2 <- na.omit(comunaPDay2[(comunaPDay2$FECHA >= input$fInicio),  ])
    comunaPDay2 <- na.omit(comunaPDay2[(comunaPDay2$FECHA <= input$fFin),  ])
  })
  output$plotCDia2 <- renderPlot({
    ggplot(data=comunasPDay2(), aes(x=FECHA, y=AÑO_2018_PRED, group=1)) +
      geom_line()+
      geom_point()+
      ggtitle("Modelo 2017 - 2018, MSE 12")+
      xlab("Día")+
      ylab("Accidentes")
  })
  ################# Barrio
  #MES
  barrioPred2 <- read.csv("./data/barrioMes2.csv", colClasses=c("MES"="character"))
  #barrioPred2 <- barrio2018("MES",99)
  barrios2 <- reactive({
    barrioPred2 <- barrioPred2[(barrioPred2$BARRIO == input$selectBarrio), ] 
    barrioPred2 <- na.omit(barrioPred2[(barrioPred2$MES >= format(input$fInicio, format="%m")),  ])
    barrioPred2 <- na.omit(barrioPred2[(barrioPred2$MES <= format(input$fFin, format="%m")),  ])
  })
  
  output$plotBMes2 <- renderPlot({
    ggplot(data=barrios2(), aes(x=MES, y=AÑO_2018_PRED, group=1)) +
      geom_line()+
      geom_point()+
      ggtitle("Modelo 2017 - 2018, MSE 12")+
      xlab("Mes")+
      ylab("Accidentes")
  })
  #Semana
  barrioPWeek2 <- read.csv("./data/barrioSemana2.csv", colClasses=c("SEMANA"="character"))
  #barrioPWeek2 <- barrio2018("SEMANA",99)
  barriosWeek2 <- reactive({
    barrioPWeek2 <- barrioPWeek2[(barrioPWeek2$BARRIO == input$selectBarrio), ] 
    barrioPWeek2 <- na.omit(barrioPWeek2[(barrioPWeek2$SEMANA >= format(input$fInicio, format="%W")),  ])
    barrioPWeek2 <- na.omit(barrioPWeek2[(barrioPWeek2$SEMANA <= format(input$fFin, format="%W")),  ])
  })
  
  output$plotBSemana2 <- renderPlot({
    ggplot(data=barriosWeek2(), aes(x=SEMANA, y=AÑO_2018_PRED, group=1)) +
      geom_line()+
      geom_point()+
      ggtitle("Modelo 2017 - 2018, MSE 12")+
      xlab("Semana")+
      ylab("Accidentes")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  #DIA
  barrioPDay2 <- read.csv("./data/barrioDia2.csv", colClasses=c("FECHA"="Date"))
  #barrioPDay2 <- barrio2018("DIA",99)
  barriosPDay2 <- reactive({
    barrioPDay2 <- barrioPDay2[(barrioPDay2$BARRIO == input$selectBarrio), ] 
    barrioPDay2 <- na.omit(barrioPDay2[(barrioPDay2$FECHA >= input$fInicio),  ])
    barrioPDay2 <- na.omit(barrioPDay2[(barrioPDay2$FECHA <= input$fFin),  ])
  })
  output$plotBDia2 <- renderPlot({
    ggplot(data=barriosPDay2(), aes(x=FECHA, y=AÑO_2018_PRED, group=1)) +
      geom_line()+
      geom_point()+
      ggtitle("Modelo 2017 - 2018, MSE 12")+
      xlab("Día")+
      ylab("Accidentes")
  })
  
  updateSelectizeInput(session, 'selectBarrio',
                       choices = unique(cargarBarrio()$BARRIO),
                       server = TRUE
  )
  
  updateSelectizeInput(session, 'selectComuna',
                       choices = unique(cargarComuna()$COMUNA),
                       server = TRUE
  )
  
  
}
)

#####################################################################################################################
#Por barrio
#Cargar datos de barrio

cargarBarrio <- function(){
  
  t2014 <- read.csv("./data/Accidentalidad_georreferenciada_2014.csv")
  t2014 <- select(t2014,5, 15)
  t2014$FECHA <- substring(t2014$FECHA, 6,10)
  
  t2015 <- read.csv("./data/Accidentalidad_georreferenciada_2015.csv")
  t2015 <- select(t2015,5, 15)
  t2015$FECHA <- substring(t2015$FECHA, 6,10)
  
  t2016 <- read.csv("./data/Accidentalidad_georreferenciada_2016.csv")
  t2016 <- select(t2016,5, 15)
  t2016$FECHA <- substring(t2016$FECHA, 6,10)
  
  t2017 <- read.csv("./data/Accidentalidad_georreferenciada_2017.csv")
  t2017 <- select(t2017,5, 15)
  t2017$FECHA <- substring(t2017$FECHA, 6,10)
  
  t2018 <- read.csv("./data/Accidentalidad_georreferenciada_2018.csv")
  t2018 <- select(t2018,5, 15)
  t2018$FECHA <- substring(t2018$FECHA, 6,10)
  
  porBarrio2014 <- group_by(t2014, BARRIO, FECHA)
  porBarrio2014 <- summarize(porBarrio2014, AÑO_2014 = n())
  
  porBarrio2015 <- group_by(t2015, BARRIO, FECHA)
  porBarrio2015 <- summarize(porBarrio2015, AÑO_2015 = n())
  
  porBarrio2016 <- group_by(t2016, BARRIO, FECHA)
  porBarrio2016 <- summarize(porBarrio2016, AÑO_2016 = n())
  
  porBarrio2017 <- group_by(t2017, BARRIO, FECHA)
  porBarrio2017 <- summarize(porBarrio2017, AÑO_2017 = n())
  
  porBarrio2018 <- group_by(t2018, BARRIO, FECHA)
  porBarrio2018 <- summarize(porBarrio2018, AÑO_2018 = n())
  
  #Join de tablas
  PorBarrioT <- merge(porBarrio2014, porBarrio2015, by = c("BARRIO","FECHA"), all = TRUE)
  PorBarrioT <- merge(PorBarrioT, porBarrio2016, by = c("BARRIO","FECHA"), all = TRUE)
  PorBarrioT <- merge(PorBarrioT, porBarrio2017, by = c("BARRIO","FECHA"))
  PorBarrioT <- merge(PorBarrioT, porBarrio2018, by = c("BARRIO","FECHA"))
  PorBarrioT[is.na(PorBarrioT)] <- 0
  
  return(PorBarrioT)
}
#####################################################################################################################
#Por comuna
#Cargar datos de comuna

cargarComuna <- function(){
  
  t2014 <- read.csv("./data/Accidentalidad_georreferenciada_2014.csv")
  t2014 <- select(t2014,5, 16)
  t2014$FECHA <- substring(t2014$FECHA, 6,10)
  
  t2015 <- read.csv("./data/Accidentalidad_georreferenciada_2015.csv")
  t2015 <- select(t2015,5, 16)
  t2015$FECHA <- substring(t2015$FECHA, 6,10)
  
  t2016 <- read.csv("./data/Accidentalidad_georreferenciada_2016.csv")
  t2016 <- select(t2016,5, 16)
  t2016$FECHA <- substring(t2016$FECHA, 6,10)
  
  t2017 <- read.csv("./data/Accidentalidad_georreferenciada_2017.csv")
  t2017 <- select(t2017,5, 16)
  t2017$FECHA <- substring(t2017$FECHA, 6,10)
  
  t2018 <- read.csv("./data/Accidentalidad_georreferenciada_2018.csv")
  t2018 <- select(t2018,5, 16)
  t2018$FECHA <- substring(t2018$FECHA, 6,10)
  
  porComuna2014 <- group_by(t2014, COMUNA, FECHA)
  porComuna2014 <- summarize(porComuna2014, AÑO_2014 = n())
  
  porComuna2015 <- group_by(t2015, COMUNA, FECHA)
  porComuna2015 <- summarize(porComuna2015, AÑO_2015 = n())
  
  porComuna2016 <- group_by(t2016, COMUNA, FECHA)
  porComuna2016 <- summarize(porComuna2016, AÑO_2016 = n())
  
  porComuna2017 <- group_by(t2017, COMUNA, FECHA)
  porComuna2017 <- summarize(porComuna2017, AÑO_2017 = n())
  
  porComuna2018 <- group_by(t2018, COMUNA, FECHA)
  porComuna2018 <- summarize(porComuna2018, AÑO_2018 = n())
  
  #Join de tablas
  PorComunaT <- merge(porComuna2014, porComuna2015, by = c("COMUNA","FECHA"), all = TRUE)
  PorComunaT <- merge(PorComunaT, porComuna2016, by = c("COMUNA","FECHA"), all = TRUE)
  PorComunaT <- merge(PorComunaT, porComuna2017, by = c("COMUNA","FECHA"), all = TRUE)
  PorComunaT <- merge(PorComunaT, porComuna2018, by = c("COMUNA","FECHA"), all = TRUE)
  PorComunaT[is.na(PorComunaT)] <- 0
  
  return(PorComunaT)
}

#####################################################################################################################
##### ENCONTRAR K ÓPTIMO

mse_k2018<-function(k,data_tr,data_vl,formula_mod){
  adv_knn<-knnreg(formula_mod,data=datos_tr,k=k) 
  y_tr_pred<-predict(adv_knn,datos_tr) 
  mse_tr<-mean((datos_tr$AÑO_2018-y_tr_pred)^2) 
  y_vl_pred<-predict(adv_knn,datos_vl) 
  mse_vl<-mean((datos_vl$AÑO_2018-y_vl_pred)^2) 
  return(list(mse_tr=mse_tr,mse_vl=mse_vl))
}

set.seed(98956561)

k_opt <- function(datos, p_tr, modelo){
  N_datos<-dim(datos)[1]
  n_tr<-round(N_datos*p_tr)
  ix_tr<-sample(N_datos,n_tr,replace = FALSE)
  datos_tr<-datos[ix_tr,]
  datos_vl<-datos[-ix_tr,] 
  
  num_vec<-1:1000   
  tryCatch( { MSE <- lapply(num_vec,mse_k2018,data_tr=data_tr,data_vl = data_vl,formula_mod = modelo)}, error = function(e) {an.error.occured <<- TRUE})
  num_vec <- (1:length(MSE))
  mse_tr<-sapply(num_vec,function(x,y){`[[`(y,x)$mse_tr},y=MSE)
  mse_vl<-sapply(num_vec,function(x,y){`[[`(y,x)$mse_vl},y=MSE)
  new_k <- which.min(abs(mse_tr-mse_vl))
  
  return(new_k)
}

#####################################################################################################################
# Predecir 2018 en base a los demás años

comuna2018 <- function(tipo, new_k){
  DatosComuna <- cargarComuna()
  
  adv_knn<-knnreg(AÑO_2018~AÑO_2014+AÑO_2015+AÑO_2016+AÑO_2017,data=DatosComuna,k=new_k) 
  y_tr_pred<-predict(adv_knn,DatosComuna) 
  
  comunaPred <- DatosComuna 
  comunaPred["AÑO_2018_PRED"] <- y_tr_pred
  
  comunaPred["FECHA"] <- as.Date(strptime(comunaPred$FECHA,format="%m-%d"), format="%m-%d")
  
  if(tipo == "MES"){
    comunaPred[tipo] <- format(comunaPred$FECHA, format="%m")
    comunaPred <- group_by(comunaPred, MES, COMUNA)
    comunaPred <- summarize(comunaPred, AÑO_2018_PRED = sum(AÑO_2018_PRED))
  }else if(tipo == "SEMANA"){
    comunaPred["SEMANA"] <- format(comunaPred$FECHA, format="%W")
    comunaPred <- group_by(comunaPred, SEMANA, COMUNA)
    comunaPred <- summarize(comunaPred, AÑO_2018_PRED = sum(AÑO_2018_PRED))
  }else if(tipo == "DIA"){
    comunaPred <- select(comunaPred,1, 2, 8)
    year(comunaPred$FECHA) <- 2018
  }
  return(comunaPred)
}
#####################################################################################################################
# Predecir 2018 y 2017 en base a los demás años

comuna2017_2018 <- function(tipo, new_k){
  DatosComuna <- cargarComuna()
  
  adv_knn<-knnreg(AÑO_2017~AÑO_2014+AÑO_2015+AÑO_2016,data=DatosComuna,k=new_k) 
  y_tr_pred<-predict(adv_knn,DatosComuna) 
  
  comunaPred <- DatosComuna 
  comunaPred["AÑO_2017_PRED"] <- y_tr_pred
  
  adv_knn<-knnreg(AÑO_2018~AÑO_2014+AÑO_2015+AÑO_2016+AÑO_2017_PRED,data=comunaPred,k=new_k) 
  y_tr_pred<-predict(adv_knn,comunaPred) 
  
  comunaPred["AÑO_2018_PRED"] <- y_tr_pred
  
  comunaPred["FECHA"] <- as.Date(strptime(comunaPred$FECHA,format="%m-%d"), format="%m-%d")
  
  if(tipo == "MES"){
    comunaPred[tipo] <- format(comunaPred$FECHA, format="%m")
    comunaPred <- group_by(comunaPred, MES, COMUNA)
    comunaPred <- summarize(comunaPred, AÑO_2017_PRED = sum(AÑO_2017_PRED),AÑO_2018_PRED = sum(AÑO_2018_PRED))
  }else if(tipo == "SEMANA"){
    comunaPred["SEMANA"] <- format(comunaPred$FECHA, format="%W")
    comunaPred <- group_by(comunaPred, SEMANA, COMUNA)
    comunaPred <- summarize(comunaPred, AÑO_2017_PRED = sum(AÑO_2017_PRED),AÑO_2018_PRED = sum(AÑO_2018_PRED))
  }else if(tipo == "DIA"){
    comunaPred <- select(comunaPred,1, 2, 8, 9)
    year(comunaPred$FECHA) <- 2018
  }
  return(comunaPred)
}
#####################################################################################################################

barrio2018 <- function(tipo, new_k){
  DatosBarrio <- cargarBarrio()
  
  adv_knn<-knnreg(AÑO_2018~AÑO_2014+AÑO_2015+AÑO_2016+AÑO_2017,data=DatosBarrio,k=new_k) 
  y_tr_pred<-predict(adv_knn,DatosBarrio) 
  
  barrioPred <- DatosBarrio 
  barrioPred["AÑO_2018_PRED"] <- y_tr_pred
  
  barrioPred["FECHA"] <- as.Date(strptime(barrioPred$FECHA,format="%m-%d"), format="%m-%d")
  
  if(tipo == "MES"){
    barrioPred[tipo] <- format(barrioPred$FECHA, format="%m")
    barrioPred <- group_by(barrioPred, MES, BARRIO)
    barrioPred <- summarize(barrioPred, AÑO_2018_PRED = sum(AÑO_2018_PRED))
  }else if(tipo == "SEMANA"){
    barrioPred["SEMANA"] <- format(barrioPred$FECHA, format="%W")
    barrioPred <- group_by(barrioPred, SEMANA, BARRIO)
    barrioPred <- summarize(barrioPred, AÑO_2018_PRED = sum(AÑO_2018_PRED))
  }else if(tipo == "DIA"){
    barrioPred <- select(barrioPred,1, 2, 8)
    year(barrioPred$FECHA) <- 2018
  }
  return(barrioPred)
}

#####################################################################################################################

barrio2017_2018 <- function(tipo, new_k){
  DatosBarrio <- cargarBarrio()
  
  adv_knn<-knnreg(AÑO_2017~AÑO_2014+AÑO_2015+AÑO_2016,data=DatosBarrio,k=new_k) 
  y_tr_pred<-predict(adv_knn,DatosBarrio) 
  
  barrioPred <- DatosBarrio 
  barrioPred["AÑO_2017_PRED"] <- y_tr_pred
  
  adv_knn<-knnreg(AÑO_2018~AÑO_2014+AÑO_2015+AÑO_2016+AÑO_2017_PRED,data=barrioPred,k=new_k) 
  y_tr_pred<-predict(adv_knn,barrioPred) 
  
  barrioPred["AÑO_2018_PRED"] <- y_tr_pred
  
  barrioPred["FECHA"] <- as.Date(strptime(barrioPred$FECHA,format="%m-%d"), format="%m-%d")
  
  if(tipo == "MES"){
    barrioPred[tipo] <- format(barrioPred$FECHA, format="%m")
    barrioPred <- group_by(barrioPred, MES, BARRIO)
    barrioPred <- summarize(barrioPred, AÑO_2017_PRED = sum(AÑO_2017_PRED),AÑO_2018_PRED = sum(AÑO_2018_PRED))
  }else if(tipo == "SEMANA"){
    barrioPred["SEMANA"] <- format(barrioPred$FECHA, format="%W")
    barrioPred <- group_by(barrioPred, SEMANA, BARRIO)
    barrioPred <- summarize(barrioPred, AÑO_2017_PRED = sum(AÑO_2017_PRED),AÑO_2018_PRED = sum(AÑO_2018_PRED))
  }else if(tipo == "DIA"){
    barrioPred <- select(barrioPred,1, 2, 8, 9)
    year(barrioPred$FECHA) <- 2018
  }
  return(barrioPred)
}

mse <- function(datos, k, formu){
  
  N_datos<-dim(datos)[1]
  n_tr<-round(N_datos*0.7)
  ix_tr<-sample(N_datos,n_tr,replace = FALSE)
  datos_tr<-datos[ix_tr,]
  datos_vl<-datos[-ix_tr,] 
  
  ms <- mse_k2018(k,datos_tr,datos_vl, formu)
  return(ms)
}
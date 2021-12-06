library(shiny)
library(shinythemes)
library(shinyLP)
library(dashboardthemes)
library(shinydashboard)
library(DT)
library(readxl)
library(openxlsx)
library(randomForest)
library(MLmetrics)
library(car)
library(MASS)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(xgboost)
library(caret)
library(dplyr)
library(mlrMBO)
library(rgenoud)
library(caTools)
library(caret)
library(e1071)
library(rpart)

##funciones-------------------------------------------
errorJhon <- function(tablaConfusion){
  sum(tablaConfusion[lower.tri(tablaConfusion)])/sum(tablaConfusion)
}


tuning_rf_mtry <- function(df, y, ntree = 999){
  max_predictores <- 20
  n_predictores   <- rep(NA, max_predictores)
  oob_err_rate    <- rep(NA, max_predictores)
  for (i in 1:max_predictores) {
    f <- formula(paste(y,"~ ."))
    modelo_rf <- randomForest(formula = f, data = df, mtry = i, ntree = ntree)
    n_predictores[i] <- i
    oob_err_rate[i] <- tail(modelo_rf$err.rate[, 1], n = 1)
  }
  results <- data_frame(n_predictores, oob_err_rate)
  return(results)
}

tuning_rf_nodesize <- function(df, y, size = NULL, ntree = 999, predictoresj){
  if (is.null(size)){
    size <- seq(from = 1, to = nrow(df), by = 5)
  }
  oob_err_rate <- rep(NA, length(size))
  for (i in seq_along(size)) {
    f <- formula(paste(y,"~ ."))
    modelo_rf <- randomForest(formula = f, data = df, mtry = predictoresj, ntree = ntree,
                              nodesize = i)
    oob_err_rate[i] <- tail(modelo_rf$err.rate[, 1], n = 1)
  }
  results <- data_frame(size, oob_err_rate)
  return(results)
}

Datos <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm", sheet = "Resultados")

###crear diseño original
original <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(80, 225, 76 )"
  
  ### titulo
  ,logoBackColor = "rgb(80, 225, 76 )"
  
  ,headerButtonBackColor = "rgb(80, 225, 76 )"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(80, 225, 76 )"
  ,headerBoxShadowColor = "#009C21"
  ,headerBoxShadowSize = "3px 3px 3px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(80, 225, 76)"
    ,colorMiddle = "rgb(52, 222, 35 )"
    ,colorEnd = "rgb(0, 182, 17 )"
    ,colorStartPos = 0
    ,colorMiddlePos = 40
    ,colorEndPos = 80
  )
  ,sidebarPadding = 1
  
  ,sidebarMenuBackColor = "rgb(17, 128, 0)" ##color del sidebar
  ,sidebarMenuPadding = 5
  ,sidebarMenuBorderRadius = 40
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "rgb(55, 123, 144 )"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none none solid"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(32, 197, 223,100)"
    ,colorMiddle = "rgba(0, 164, 241 ,1)"
    ,colorEnd = "rgba(50, 117, 241 ,1 )"
    ,colorStartPos = 0
    ,colorMiddlePos = 45
    ,colorEndPos = 83
  )
  
  ,sidebarTabTextColorSelected = "rgb(0,0,0)" ##rexto del tab seleccionado
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors( #para los tabs?
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 80
  )
  
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 5
  ,sidebarTabRadiusHover = "40px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 15
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)" ##color de pestañas Tab
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)" ##color de fondo!
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

ui <- dashboardPage(
  ### ui titulo
  dashboardHeader(
    
    ## logo
    title = shinyDashboardLogo(
      theme = "blue_gradient",
      boldText = "Analítica de Futbol-Soccer",
      mainText = "JA",
      badgeText = "versión prealfa"
    )
  ),
  
  ##lateral
  sidebar <- dashboardSidebar(
    # we need to use special function instead of uiOutput
    sidebarMenu(
      menuItem("Introducción", tabName = "intro", icon = icon("dashboard")),
      menuItem("Datos", tabName = "datos", icon = icon("database")),
      menuItem("'  Regresión Logística", tabName = "logis", icon = icon("stripe-s")),
      menuItem("Random Forest", tabName = "RFO", icon = icon("tree")),
      menuItem("RFO Step", tabName = "RfoStep", icon = icon("pagelines")),
      menuItem("'  XGBoost", tabName = "XGB", icon = icon("bootstrap")),
      menuItem("Salida", tabName = "prediccion", icon = icon("check-circle"))
    )
  ),
  
  dashboardBody(original,
    
    tags$head(tags$style(HTML('.info-box {width: 300px;} .info-box-icon {height: 90px; line-height: 90px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
    
    tabItems(
      tabItem(tabName = "intro", 
              h2(span("Proyecto Analítica de predicción de partidos y goles en futbol nacional e internacional",
                      style="color:royalblue"),align="center")
      ),#tabItem
      
      tabItem(tabName = "datos", 
              h2(span("Recolección de Datos", style="color:royalblue"),align="center"),
              br(),
              h2(span("Tabla de datos", style="color:royalblue"),align="center"), DT::dataTableOutput("RawData")
      ),#tabItem
      
      tabItem(tabName = "logis",
              tabBox(title = "Regresión Logística",
                     id = "RegLog", width = 12,
                     tabPanel("Introducción", 
                              h1(span("Regresión Logistica",style="color:royalblue"),align="center"),
                              p("La regresión logistica es... En este caso, se plantea 1 modelo por cada variable de interés: 
                                GPE(y), Diferencia de goles(Dif) y Número de goles anotados.",style="color:black;text-align:center;background-color:lavender;border-radius: 9px")
                              ),#tabPanel
                     
                     tabPanel("GPE", 
                              h1(span("Regresión Logistica",style="color:royalblue"),align="center"),
                              p("La regresión logistica es... En este caso, se plantea 1 modelo por cada variable de interés: 
                                GPE(y), Diferencia de goles(Dif) y Número de goles anotados.",style="color:black;text-align:center;background-color:lavender;border-radius: 9px"),
                              fluidRow(
                                column(
                                  verbatimTextOutput("summaryLogGPE"), width = 6),
                                column(
                                  infoBoxOutput("MAPELogGPE"), width = 6)
                              ) #fluidrow
                              ),#tabPanel
                     
                     tabPanel("DIF", 
                              h1(span("Regresión Logistica",style="color:royalblue"),align="center"),
                              p("La regresión logistica es... En este caso, se plantea 1 modelo por cada variable de interés: 
                                GPE(y), Diferencia de goles(Dif) y Número de goles anotados.",style="color:black;text-align:center;background-color:lavender;border-radius: 9px"),
                              fluidRow(
                                column(
                                  verbatimTextOutput("summaryLogDIF"), width = 6),
                                column(
                                  infoBoxOutput("MAPELogDIF"), width = 6)
                              ) #fluidrow
                     ),#tabPanel
                     
                     tabPanel("Goles", 
                              h1(span("Regresión Logistica",style="color:royalblue"),align="center"),
                              p("La regresión logistica es... En este caso, se plantea 1 modelo por cada variable de interés: 
                                GPE(y), Diferencia de goles(Dif) y Número de goles anotados.",style="color:black;text-align:center;background-color:lavender;border-radius: 9px")
                     )#tabPanel
              )#tapBox
      ),#tabItem

      
      tabItem(tabName = "RFO",
              tabBox(
                title = "Random Forest",
                id = "RanFor", width = 12,
                tabPanel("Introducción", 
                         h1(span("Random Forest - Bosque Aleatorio",style="color:royalblue"),align="center"),
                         p("RFO es... En este caso, se plantea 1 modelo por cada variable de interés: 
                           GPE(y), Diferencia de goles(Dif) y Número de goles anotados.",style="color:black;text-align:center;background-color:lavender;border-radius: 9px")
                         ),#tabPanel
                
                tabPanel("GPE",
                         h1(span("Random Forest - Bosque Aleatorio",style="color:royalblue"),align="center"),
                         fluidRow(
                           column(
                             sliderInput("predictoresGPE", "Número de predictores", min=1, max=20, value=5, step = 1), width = 6),
                           column(
                             plotOutput("plotpredictoresGPE"), width = 6)
                         ), #fluidrow
                         fluidRow(
                           column(
                             sliderInput("SizeGPE", "Número de Nodos", min=1, max=25, value=1, step = 1), width = 6),
                           column(
                             plotOutput("plotsizesGPE"), width = 6)
                         ), #fluidrow
                         fluidRow(
                           column(
                             sliderInput("ArbolesGPE", "Número de árboles", min=50, max=4000, value=50, step = 50), width = 6),
                           column(
                             plotOutput("plotarbolesGPE"), width = 6)
                         ), #fluidrow
                         fluidRow(
                           column(
                             verbatimTextOutput("summaryRFOGPE"), width = 6),
                           column(
                             infoBoxOutput("MAPERFOGPE"), width = 6)
                         ) #fluidrow
                ),#tabPanel
                
                tabPanel("DIF", 
                         h1(span("Random Forest - Bosque Aleatorio",style="color:royalblue"),align="center"),
                         fluidRow(
                           column(
                             sliderInput("predictoresDIF", "Número de predictores", min=1, max=20, value=1, step = 1), width = 6),
                           column(
                             plotOutput("plotpredictoresDIF"), width = 6)
                         ), #fluidrow
                         fluidRow(
                           column(
                             sliderInput("SizeDIF", "Número de Nodos", min=1, max=25, value=10, step = 1), width = 6),
                           column(
                             plotOutput("plotsizesDIF"), width = 6)
                         ), #fluidrow
                         fluidRow(
                           column(
                             sliderInput("ArbolesDIF", "Número de árboles", min=50, max=4000, value=500, step = 50), width = 6),
                           column(
                             plotOutput("plotarbolesDIF"), width = 6)
                         ), #fluidrow
                         fluidRow(
                           column(
                             verbatimTextOutput("summaryRFODIF"), width = 6),
                           column(
                             infoBoxOutput("MAPERFODIF"), width = 6)
                         ) #fluidrow
                ),#tabPanel
                
                tabPanel("Goles", 
                         h1(span("Random Forest - Bosque Aleatorio",style="color:royalblue"),align="center"),
                         p("La regresión logistica es... En este caso, se plantea 1 modelo por cada variable de interés: 
                           GPE(y), Diferencia de goles(Dif) y Número de goles anotados.",style="color:black;text-align:center;background-color:lavender;border-radius: 9px")
                         )#tabPanel
                
                )#tapBox
              ),#tabItem
      
      tabItem(tabName = "RfoStep",
              tabBox(
                title = "Random Forest",
                id = "RanForStep", width = 12,
                tabPanel("Introducción", 
                         h1(span("Random Forest - Modelos Step",style="color:royalblue"),align="center"),
                         p("RFO es... En este caso, se plantea 1 modelo por cada variable de interés: 
                     GPE(y), Diferencia de goles(Dif) y Número de goles anotados.",style="color:black;text-align:center;background-color:lavender;border-radius: 9px")
                ),#tabPanel
                
                tabPanel("GPE",
                         h1(span("Random Forest - Modelos Step",style="color:royalblue"),align="center"),
                         fluidRow(
                           column(
                             sliderInput("CpStepGPE", "Número de predictores", min=0.01, max=0.5, value=0.16, step = 0.01), 
                             verbatimTextOutput("summaryRFOStepGPE"),
                             infoBoxOutput("MAPERFOStepGPE"), width = 6),
                           
                           column(
                             verbatimTextOutput("SummaryCpStepGPE"), width = 6)
                         ) #fluidrow
                            
                ),#tabPanel
                
                tabPanel("DIF",
                         h1(span("Random Forest - Modelos Step",style="color:royalblue"),align="center"),
                         fluidRow(
                           column(
                             sliderInput("CpStepDIF", "Número de predictores", min=0.01, max=0.5, value=0.16, step = 0.01), 
                             verbatimTextOutput("summaryRFOStepDIF"),
                             infoBoxOutput("MAPERFOStepDIF"), width = 6),
                           
                           column(
                             verbatimTextOutput("SummaryCpStepDIF"), width = 6)
                         ) #fluidrow
                         
                ),#tabPanel
                
                tabPanel("Goles", 
                         h1(span("Random Forest - Modelos Step",style="color:royalblue"),align="center"),
                         p("La regresión logistica es... En este caso, se plantea 1 modelo por cada variable de interés: 
                     GPE(y), Diferencia de goles(Dif) y Número de goles anotados.",style="color:black;text-align:center;background-color:lavender;border-radius: 9px")
                )#tabPanel

                )#tapBox
              ),#tabItem
      
      
      tabItem(tabName = "XGB",
              tabBox(title = "XGBoost",
                     id = "xgboost", width = 12,
                     tabPanel("Introducción", 
                              h1(span("XGBoost",style="color:royalblue"),align="center"),
                              p("Los modelos XGBoost son... En este caso, se plantea 1 modelo por cada variable de interés: 
                                GPE(y), Diferencia de goles(Dif) y Número de goles anotados.",style="color:black;text-align:center;background-color:lavender;border-radius: 9px")
                              ),#tabPanel
                     
                     tabPanel("GPE", 
                              h1(span("XGBoost",style="color:royalblue"),align="center"),
                              fluidRow(
                                column(
                                  verbatimTextOutput("summaryXGBGPE"), width = 6),
                                column(
                                  infoBoxOutput("MAPEXGBGPE"), 
                                  DT::dataTableOutput("probabilidadesXGBGPE"),
                                  width = 6) #column
                              ) #fluidrow
                              ),#tabPanel
                     
                     tabPanel("DIF", 
                              h1(span("XGBoost",style="color:royalblue"),align="center"),
                              fluidRow(
                                column(
                                  verbatimTextOutput("summaryXGBDIF"), width = 6),
                                column(
                                  infoBoxOutput("MAPEXGBDIF"), 
                                  DT::dataTableOutput("probabilidadesXGBDIF"),
                                  width = 6)
                              ) #fluidrow
                     ),#tabPanel
                     
                     tabPanel("Goles", 
                              h1(span("XGBoost",style="color:royalblue"),align="center"),
                              fluidRow(
                                column(
                                  verbatimTextOutput("summaryXGBGol"), width = 6),
                                column(
                                  infoBoxOutput("MAPEXGBGol"), 
                                  DT::dataTableOutput("probabilidadesXGBGOL"),
                                  width = 6) #column
                              ) #fluidrow
                     )#tabPanel
                     )#tapBox
      ),#tabItem
      
      
      tabItem(tabName = "prediccion", 
              h2(span("Proyecto Analítica de predicción de partidos y goles en futbol nacional e internacional",style="color:royalblue"),align="center"),
              DT::dataTableOutput("predichosTodos")
              )
      

    ) #tabItems
  ) #body
) #ui


server<-shinyServer(function(input, output, session){
  
  output$RawData <- DT::renderDataTable(
    DT::datatable({
      Datos
    },
    filter = "top", extensions = c('Buttons', 'Scroller'),
    options = list(initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'seagreen', 'color': '1c1b1b'});",
                     "}"),
                   scrollX = 500,
                   scrollY = 300,
                   deferRender = TRUE,
                   scroller = TRUE,
                   buttons = list('excel',
                                  list(extend = 'colvis', targets = 0, visible = FALSE)),
                   dom = 'lBfrtip',
                   fixedColumns = TRUE
    ),
    rownames = FALSE))
  
  datosGPE<-reactive({
    i=nrow(Datos[which(Datos[,1] != 0),])
    Datos=Datos[1:i,-c(2,3,7,8,9,31,45)]
    Datos$PaisLocal=as.factor(Datos$PaisLocal)
    Datos$EstiloLocal=as.factor(Datos$EstiloLocal)
    Datos$PaisVisitante=as.factor(Datos$PaisVisitante)
    Datos$EstiloVisitante=as.factor(Datos$EstiloVisitante)
    Datos$Competición=as.factor(Datos$Competición)
    return(Datos)
  })
  
  datosDIF<-reactive({
    i=nrow(Datos[which(Datos[,2] != 0),])
    Datos=Datos[1:i,-c(1,3,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...
    Datos$PaisLocal=as.factor(Datos$PaisLocal)
    Datos$EstiloLocal=as.factor(Datos$EstiloLocal)
    Datos$PaisVisitante=as.factor(Datos$PaisVisitante)
    Datos$EstiloVisitante=as.factor(Datos$EstiloVisitante)
    Datos$Competición=as.factor(Datos$Competición)
    return(Datos)
  })
  
  datosGOL<-reactive({
    Datos=Datos[-which(is.na(Datos$Goles)),-c(1,2,7,8,9,31,45)]
    #Datos=Datos[1:i,-c(1,2,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...
    Datos$PaisLocal=as.factor(Datos$PaisLocal)
    Datos$EstiloLocal=as.factor(Datos$EstiloLocal)
    Datos$PaisVisitante=as.factor(Datos$PaisVisitante)
    Datos$EstiloVisitante=as.factor(Datos$EstiloVisitante)
    Datos$Competición=as.factor(Datos$Competición)
    return(Datos)
  })
  
  ##datos a evaluar------------------------------------------
  evaluarGPE<-reactive({
    evaluar=Datos[which(is.na(Datos[,1])),-c(2,3,7,8,9,31,45)]
    evaluar$PaisLocal=as.factor(evaluar$PaisLocal)
    evaluar$EstiloLocal=as.factor(evaluar$EstiloLocal)
    evaluar$PaisVisitante=as.factor(evaluar$PaisVisitante)
    evaluar$EstiloVisitante=as.factor(evaluar$EstiloVisitante)
    evaluar$Competición=as.factor(evaluar$Competición)
    return(evaluar)
  })
  
  evaluarDIF<-reactive({
    evaluar=Datos[which(is.na(Datos[,2])),-c(1,3,7,8,9,31,45)]
    evaluar$PaisLocal=as.factor(evaluar$PaisLocal)
    evaluar$EstiloLocal=as.factor(evaluar$EstiloLocal)
    evaluar$PaisVisitante=as.factor(evaluar$PaisVisitante)
    evaluar$EstiloVisitante=as.factor(evaluar$EstiloVisitante)
    evaluar$Competición=as.factor(evaluar$Competición)
    return(evaluar)
  })
  
  evaluarGol<-reactive({
    evaluar=Datos[179:nrow(Datos),]
    evaluar=evaluar[which(is.na(evaluar[,3])),-c(1,2,7,8,9,31,45)]
    evaluar$PaisLocal=as.factor(evaluar$PaisLocal)
    evaluar$EstiloLocal=as.factor(evaluar$EstiloLocal)
    evaluar$PaisVisitante=as.factor(evaluar$PaisVisitante)
    evaluar$EstiloVisitante=as.factor(evaluar$EstiloVisitante)
    evaluar$Competición=as.factor(evaluar$Competición)
    return(evaluar)
  })
  
  
  ##Regresión Logistica GPE--------------------------------------------
  output$summaryLogGPE <- renderPrint({
    modLog <- glm(y ~ ., data = datosGPE(), family = "poisson")
    summary(modLog)})
  
  output$MAPELogGPE <- renderInfoBox({
    modLog <- glm(y ~ ., data = datosGPE(), family = "poisson")
    infoBox(
      "MAPE", round(MAPE(modLog$fitted.values, datosGPE()$y), digits = 2), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple"
    ) #infobox
  })
  
  ##Regresión Logistica DIF--------------------------------------------
  output$summaryLogDIF <- renderPrint({
    modLog <- glm(Diferencia ~ ., data = datosDIF(), family = "poisson")
    summary(modLog)})
  
  output$MAPELogDIF <- renderInfoBox({
    modLog <- glm(Diferencia ~ ., data = datosDIF(), family = "poisson")
    infoBox(
      "MAPE", round(MAPE(modLog$fitted.values, datosDIF()$Diferencia), digits = 2), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple"
    ) #infobox
  })
  
  ##RFO GPE---------------------------------------
  output$plotpredictoresGPE <- renderPlot({
    datosGPE = datosGPE()
    datosGPE$y <- as.factor(datosGPE$y)
    hiperparametro_mtry <-  tuning_rf_mtry(df = datosGPE, y = "y")
    ggplot(data = hiperparametro_mtry, aes(x = n_predictores, y = oob_err_rate)) +
      scale_x_continuous(breaks = hiperparametro_mtry$n_predictores) +
      geom_line() +
      geom_point() +
      geom_point(data = hiperparametro_mtry %>% arrange(oob_err_rate) %>% head(1),
                 color = "red") +
      labs(title = "Evolucion del out-of-bag-error vs mtry",
           x = "n? predictores empleados") +
      theme_bw()
  })
  
  output$plotsizesGPE <- renderPlot({
    datosGPE = datosGPE()
    datosGPE$y <- as.factor(datosGPE$y)
    predictores=input$predictoresGPE
    hiperparametro_nodesize <-  tuning_rf_nodesize(df = datosGPE, y = "y", size = c(1:25), predictoresj = predictores)
    ggplot(data = hiperparametro_nodesize, aes(x = size, y = oob_err_rate)) +
      scale_x_continuous(breaks = hiperparametro_nodesize$size) +
      geom_line() +
      geom_point() +
      geom_point(data = hiperparametro_nodesize %>% arrange(oob_err_rate) %>% head(1),
                 color = "red") +
      labs(title = "Evoluci?n del out-of-bag-error vs nodesize",
           x = "n? observaciones en nodos terminales") +
      theme_bw()
  })
  
  output$plotarbolesGPE <- renderPlot({
    datosGPE = datosGPE()
    datosGPE$y <- as.factor(datosGPE$y)
    predictoresj=input$predictoresGPE
    obs=input$SizeGPE
    modelo_randomforest <- randomForest(y ~ ., data = datosGPE, mtry = predictoresj, ntree = 3999,
                                        importance = TRUE, nodesize = obs)
    
    oob_err_rate <- data.frame(oob_err_rate = modelo_randomforest$err.rate[, 1],
                               arboles = seq_along(modelo_randomforest$err.rate[, 1]))
    ggplot(data = oob_err_rate, aes(x = arboles, y = oob_err_rate )) +
      geom_line() +
      labs(title = "Evoluci?n del out-of-bag-error vs n?mero ?rboles",
           x = "n? ?rboles") +
      theme_bw()
  })
  
  modeloRFOGPE <- reactive({
    datosGPE = datosGPE()
    datosGPE$y <- as.factor(datosGPE$y)
    predictoresj=input$predictoresGPE
    obs=input$SizeGPE
    arboles=input$ArbolesGPE
    modelo_randomforest <- randomForest(y ~ ., data = datosGPE, mtry = predictoresj, ntree = arboles,
                                        importance = TRUE, nodesize = obs,
                                        norm.votes = TRUE )
  })
  
  output$summaryRFOGPE <- renderPrint({
    modeloRFOGPE()$confusion
  })
  
  output$MAPERFOGPE <- renderInfoBox({
    datosGPE = datosGPE()
    datosGPE$y <- as.factor(datosGPE$y)
    infoBox(
      "MAPE", round(MAPE(as.numeric(modeloRFOGPE()$predicted), as.numeric(datosGPE$y)), digits = 2), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple"
    ) #infobox
  })
  
  
  
  ##RFO DIF---------------------------------------
  output$plotpredictoresDIF <- renderPlot({
    datosDIF = datosDIF()
    datosDIF$Diferencia <- as.factor(datosDIF$Diferencia)
    hiperparametro_mtry <-  tuning_rf_mtry(df = datosDIF, y = "Diferencia")
    ggplot(data = hiperparametro_mtry, aes(x = n_predictores, y = oob_err_rate)) +
      scale_x_continuous(breaks = hiperparametro_mtry$n_predictores) +
      geom_line() +
      geom_point() +
      geom_point(data = hiperparametro_mtry %>% arrange(oob_err_rate) %>% head(1),
                 color = "red") +
      labs(title = "Evolucion del out-of-bag-error vs mtry",
           x = "n? predictores empleados") +
      theme_bw()
  })
  
  output$plotsizesDIF <- renderPlot({
    datosDIF = datosDIF()
    datosDIF$Diferencia <- as.factor(datosDIF$Diferencia)
    predictores=input$predictoresGPE
    hiperparametro_nodesize <-  tuning_rf_nodesize(df = datosDIF, y = "Diferencia", size = c(1:25), predictoresj = predictores)
    ggplot(data = hiperparametro_nodesize, aes(x = size, y = oob_err_rate)) +
      scale_x_continuous(breaks = hiperparametro_nodesize$size) +
      geom_line() +
      geom_point() +
      geom_point(data = hiperparametro_nodesize %>% arrange(oob_err_rate) %>% head(1),
                 color = "red") +
      labs(title = "Evoluci?n del out-of-bag-error vs nodesize",
           x = "n? observaciones en nodos terminales") +
      theme_bw()
  })
  
  output$plotarbolesDIF <- renderPlot({
    datosDIF = datosDIF()
    datosDIF$Diferencia <- as.factor(datosDIF$Diferencia)
    predictoresj=input$predictoresGPE
    obs=input$SizeGPE
    modelo_randomforest <- randomForest(Diferencia ~ ., data = datosDIF, mtry = predictoresj, ntree = 3999,
                                        importance = TRUE, nodesize = obs)
    
    oob_err_rate <- data.frame(oob_err_rate = modelo_randomforest$err.rate[, 1],
                               arboles = seq_along(modelo_randomforest$err.rate[, 1]))
    ggplot(data = oob_err_rate, aes(x = arboles, y = oob_err_rate )) +
      geom_line() +
      labs(title = "Evoluci?n del out-of-bag-error vs n?mero ?rboles",
           x = "n? ?rboles") +
      theme_bw()
  })
  
  modeloRFODIF <- reactive({
    datosDIF = datosDIF()
    datosDIF$Diferencia <- as.factor(datosDIF$Diferencia)
    predictoresj=input$predictoresGPE
    obs=input$SizeGPE
    arboles=input$ArbolesGPE
    modelo_randomforest <- randomForest(Diferencia ~ ., data = datosDIF, mtry = predictoresj, ntree = arboles,
                                        importance = TRUE, nodesize = obs,
                                        norm.votes = TRUE )
  })
  
  output$summaryRFODIF <- renderPrint({
    modeloRFODIF()$confusion
  })
  
  output$MAPERFODIF <- renderInfoBox({
    datosDIF = datosDIF()
    datosDIF$Diferencia <- as.factor(datosDIF$Diferencia)
    infoBox(
      "MAPE", round(MAPE(as.numeric(modeloRFODIF()$predicted), as.numeric(datosDIF$Diferencia)), digits = 2), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple"
    ) #infobox
  })
  
  
  ##Step RFO----------------------------------
  output$SummaryCpStepGPE <- renderPrint({
    datosGPE = datosGPE()
    datosGPE$y <- as.factor(datosGPE$y)
    numFolds <- trainControl(method = "cv", number = 10)
    cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
    a=caret::train(y ~ ., data = datosGPE, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
    a$results
  })
  
  modeloRFOStepGPE <- reactive({
    datosGPE = datosGPE()
    datosGPE$y <- as.factor(datosGPE$y)
    numFolds <- trainControl(method = "cv", number = 10)
    cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
    stepRFORes = rpart(y ~ ., data = datosGPE, method = "class", cp = input$CpStepGPE)
  })
  
  output$summaryRFOStepGPE <- renderPrint({
    datosGPE = datosGPE()
    datosGPE$y <- as.factor(datosGPE$y)
    confusionMatrix(factor(as.numeric(modeloRFOStepGPE()$where),levels=levels(as.factor(datosGPE$y))), #predicho
                    factor(datosGPE$y,levels=levels(as.factor(datosGPE$y)))) #real
  })
  
  output$MAPERFOStepGPE <- renderInfoBox({
    datosGPE = datosGPE()
    datosGPE$y <- as.factor(datosGPE$y)
    infoBox(
      "MAPE", round(MAPE(as.numeric(modeloRFOStepGPE()$where), as.numeric(datosGPE$y)), digits = 2), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple"
    ) #infobox
  })
  
  output$SummaryCpStepDIF <- renderPrint({
    datosDIF = datosDIF()
    datosDIF$Diferencia <- as.factor(datosDIF$Diferencia)
    numFolds <- trainControl(method = "cv", number = 10)
    cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
    a=caret::train(Diferencia ~ ., data = datosDIF, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
    a$results
  })
  
  modeloRFOStepDIF <- reactive({
    datosDIF = datosDIF()
    datosDIF$Diferencia <- as.factor(datosDIF$Diferencia)
    numFolds <- trainControl(method = "cv", number = 10)
    cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
    stepRFORes = rpart(Diferencia ~ ., data = datosDIF, method = "class", cp = input$CpStepDIF)
  })
  
  output$summaryRFOStepDIF <- renderPrint({
    datosDIF = datosDIF()
    datosDIF$Diferencia <- as.factor(datosDIF$Diferencia)
    confusionMatrix(factor(as.numeric(modeloRFOStepDIF()$where),levels=levels(as.factor(datosDIF$Diferencia))), #predicho
                    factor(datosDIF$Diferencia,levels=levels(as.factor(datosDIF$Diferencia)))) #real
  })
  
  output$MAPERFOStepGPE <- renderInfoBox({
    datosDIF = datosDIF()
    datosDIF$Diferencia <- as.factor(datosDIF$Diferencia)
    infoBox(
      "MAPE", round(MAPE(as.numeric(modeloRFOStepDIF()$where), as.numeric(datosDIF$Diferencia)), digits = 2), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple"
    ) #infobox
  })
  
  
  #XGBoost GPE-------------------------------
  paramXGBGPE <- reactive({
    datosGPE = datosGPE()
    datosGPE$y <- as.factor(datosGPE$y)
    num_class = length(levels(as.factor(datosGPE$y)))
    
    tsk = makeClassifTask("Xgboost Opt", data = datosGPE, target = "y")
    tsk = createDummyFeatures(tsk)
    lrn = makeLearner("classif.xgboost", nthread = 4)
    lrn$par.vals = list(
      nrounds             = 299,
      objective           = "multi:softmax",
      verbose=0,
      eval_metric = "mlogloss"
    )
    
    res = makeResampleDesc("CV", iters = 3)
    par = makeParamSet(
      makeNumericParam("eta",                    lower = 0.01, upper = 0.15),
      makeNumericParam("gamma",                  lower = 3,     upper = 9),
      makeIntegerParam("max_depth",              lower= 5,      upper = 15),
      makeIntegerParam("min_child_weight",       lower= 3,    upper = 15),
      makeNumericParam("subsample",              lower = 0.45,  upper = 0.65),
      makeNumericParam("colsample_bytree",       lower = 0.4,  upper = 0.6)
    )

    mbo.ctrl = makeMBOControl()
    mbo.ctrl = setMBOControlInfill(mbo.ctrl, crit = crit.ei)
    mbo.ctrl = setMBOControlTermination(mbo.ctrl, max.evals = 20L)
    
    design.mat = generateRandomDesign(n = 10, par.set = par)
    ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)
    
    tune.pars = tuneParams(learner = lrn, task = tsk, resampling = res,
                           par.set = par, control = ctrl)
    return(tune.pars$mbo.result$x)
  })
  
  nroundGPE <- reactive({
    datosGPE = datosGPE()
    datosGPE$PaisLocal=as.numeric(as.factor(datosGPE$PaisLocal))
    datosGPE$EstiloLocal=as.numeric(as.factor(datosGPE$EstiloLocal))
    datosGPE$PaisVisitante=as.numeric(as.factor(datosGPE$PaisVisitante))
    datosGPE$EstiloVisitante=as.numeric(as.factor(datosGPE$EstiloVisitante))
    datosGPE$Competición=as.numeric(as.factor(datosGPE$Competición))
    
    num_class = length(levels(as.factor(datosGPE$y)))
    best.params=paramXGBGPE()
    
    best.params$booster <- "gbtree"
    best.params$objective <- "multi:softmax"
    best.params$eval_metric <- "mlogloss"
    
    datosGPE$y <-as.integer(as.factor(datosGPE$y))-1
    datosGPE <- 
      datosGPE %>%  
      select(-y) %>%  
      as.matrix() %>% 
      xgb.DMatrix(data = ., label = datosGPE$y)
    
    optimal.cv <- xgb.cv(params = best.params,
                         data = datosGPE,
                         nrounds = 999,
                         nthread = 4,
                         nfold = 3,
                         prediction = FALSE,
                         showsd = TRUE,
                         early_stopping_rounds = 50,
                         verbose = 0,
                         num_class=3) #3 clases en y
    
    repeticiones <- optimal.cv$best_ntreelimit
    return(repeticiones) #1017 de momento
  })
  
  XGBmodGPE<-reactive({
    datosGPE = datosGPE()
    datosGPE$PaisLocal=as.numeric(as.factor(datosGPE$PaisLocal))
    datosGPE$EstiloLocal=as.numeric(as.factor(datosGPE$EstiloLocal))
    datosGPE$PaisVisitante=as.numeric(as.factor(datosGPE$PaisVisitante))
    datosGPE$EstiloVisitante=as.numeric(as.factor(datosGPE$EstiloVisitante))
    datosGPE$Competición=as.numeric(as.factor(datosGPE$Competición))
    
    num_class = length(levels(as.factor(Datos$y)))
    best.params=paramXGBGPE()
    best.params$booster <- "gbtree"
    best.params$objective <- "multi:softprob"
    best.params$eval_metric <- "mlogloss"
    repeticiones=nroundGPE()
    datosGPE$y <-as.integer(as.factor(datosGPE$y))-1
    datosGPE <- 
      datosGPE %>%  
      select(-y) %>%  
      as.matrix() %>% 
      xgb.DMatrix(data = ., label = datosGPE$y)
    
    final.model <- xgboost(params = best.params, 
                           data = datosGPE,
                           nrounds = repeticiones,
                           verbose = 0,
                           num_class=3)
    final.model
  })
  
  output$summaryXGBGPE <- renderPrint({
    datosGPE = datosGPE()
    datosGPE$y <-as.integer(as.factor(datosGPE$y))-1
    datosGPE$PaisLocal=as.numeric(as.factor(datosGPE$PaisLocal))
    datosGPE$EstiloLocal=as.numeric(as.factor(datosGPE$EstiloLocal))
    datosGPE$PaisVisitante=as.numeric(as.factor(datosGPE$PaisVisitante))
    datosGPE$EstiloVisitante=as.numeric(as.factor(datosGPE$EstiloVisitante))
    datosGPE$Competición=as.numeric(as.factor(datosGPE$Competición))
    
    predichos=predict(XGBmodGPE(), as.matrix(datosGPE[,-1]))
    tabla=matrix(predichos, ncol = 3, byrow = TRUE)
    predichos=max.col(tabla)
    
    a=confusionMatrix(factor(predichos,levels=levels(as.factor(datosGPE$y+1))), #predicho
                           factor(datosGPE$y+1,levels=levels(as.factor(datosGPE$y+1)))) #real
    
    a
  })
  
  output$MAPEXGBGPE <- renderInfoBox({
    datosGPE = datosGPE()
    datosGPE$y <- as.integer(as.factor(datosGPE$y))-1
    datosGPE$PaisLocal=as.numeric(as.factor(datosGPE$PaisLocal))
    datosGPE$EstiloLocal=as.numeric(as.factor(datosGPE$EstiloLocal))
    datosGPE$PaisVisitante=as.numeric(as.factor(datosGPE$PaisVisitante))
    datosGPE$EstiloVisitante=as.numeric(as.factor(datosGPE$EstiloVisitante))
    datosGPE$Competición=as.numeric(as.factor(datosGPE$Competición))
    
    predichos=predict(XGBmodGPE(), as.matrix(datosGPE[,-1]))
    tabla=matrix(predichos, ncol = 3, byrow = TRUE)
    predichos=max.col(tabla)
    mape=MAPE(predichos, as.numeric(datosGPE$y)+1)
    
    infoBox(
      "MAPE", round(mape, digits = 2), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple"
    ) #infobox
  })
  
  output$probabilidadesXGBGPE <- DT::renderDataTable(
    DT::datatable({
      evaluarGPE=evaluarGPE()
      evaluarGPE$y <-as.integer(as.factor(evaluarGPE$y))-1
      evaluarGPE$PaisLocal=as.numeric(as.factor(evaluarGPE$PaisLocal))
      evaluarGPE$EstiloLocal=as.numeric(as.factor(evaluarGPE$EstiloLocal))
      evaluarGPE$PaisVisitante=as.numeric(as.factor(evaluarGPE$PaisVisitante))
      evaluarGPE$EstiloVisitante=as.numeric(as.factor(evaluarGPE$EstiloVisitante))
      evaluarGPE$Competición=as.numeric(as.factor(evaluarGPE$Competición))
      
      predichos=predict(XGBmodGPE(), as.matrix(evaluarGPE[,-1]))
      tabla=matrix(predichos, ncol = 3, byrow = TRUE)
      as.data.frame(cbind(Datos$Local[which(is.na(Datos[,1]))],Datos$Visitante[which(is.na(Datos[,1]))],tabla))
    },
    filter = "top", extensions = c('Buttons', 'Scroller'),
    options = list(initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': 'seagreen', 'color': '1c1b1b'});",
      "}"),
      scrollX = 500,
      scrollY = 300,
      deferRender = TRUE,
      scroller = TRUE,
      buttons = list('excel',
                     list(extend = 'colvis', targets = 0, visible = FALSE),
                     list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'), text = 'Download')
      ),
      dom = 'lBfrtip',
      fixedColumns = TRUE
    ),
    rownames = FALSE))
  
  #XGBoost DIF------------
  paramXGBDIF <- reactive({
    datosDIF = datosDIF()
    datosDIF$Diferencia <- as.factor(datosDIF$Diferencia)
    num_class = length(levels(as.factor(datosDIF$Diferencia)))
    
    tsk = makeClassifTask("Xgboost Opt", data = datosDIF, target = "Diferencia")
    tsk = createDummyFeatures(tsk)
    lrn = makeLearner("classif.xgboost", nthread = 4)
    lrn$par.vals = list(
      nrounds             = 299,
      objective           = "multi:softmax",
      verbose=0,
      eval_metric = "mlogloss"
    )
    
    res = makeResampleDesc("CV", iters = 3)
    par = makeParamSet(
      makeNumericParam("eta",                    lower = 0.01, upper = 0.2),
      makeNumericParam("gamma",                  lower = 1,     upper = 8),
      makeIntegerParam("max_depth",              lower= 6,      upper = 15),
      makeIntegerParam("min_child_weight",       lower= 1,    upper = 9),
      makeNumericParam("subsample",              lower = 0.20,  upper = 0.8),
      makeNumericParam("colsample_bytree",       lower = 0.20,  upper = 0.9)
    )
    
    mbo.ctrl = makeMBOControl()
    mbo.ctrl = setMBOControlInfill(mbo.ctrl, crit = crit.ei)
    mbo.ctrl = setMBOControlTermination(mbo.ctrl, max.evals = 20L)
    
    design.mat = generateRandomDesign(n = 10, par.set = par)
    ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)
    
    tune.pars = tuneParams(learner = lrn, task = tsk, resampling = res,
                           par.set = par, control = ctrl)
    return(tune.pars$mbo.result$x)
  })
  
  nroundDIF <- reactive({
    datosDIF = datosDIF()
    datosDIF$PaisLocal=as.numeric(as.factor(datosDIF$PaisLocal))
    datosDIF$EstiloLocal=as.numeric(as.factor(datosDIF$EstiloLocal))
    datosDIF$PaisVisitante=as.numeric(as.factor(datosDIF$PaisVisitante))
    datosDIF$EstiloVisitante=as.numeric(as.factor(datosDIF$EstiloVisitante))
    datosDIF$Competición=as.numeric(as.factor(datosDIF$Competición))
    
    num_classDIF = length(levels(as.factor(datosDIF$Diferencia)))
    best.params=paramXGBDIF()
    
    best.params$booster <- "gbtree"
    best.params$objective <- "multi:softmax"
    best.params$eval_metric <- "mlogloss"
    
    datosDIF$Diferencia <-as.integer(as.factor(datosDIF$Diferencia))-1
    datosDIF <- 
      datosDIF %>%  
      select(-Diferencia) %>%  
      as.matrix() %>% 
      xgb.DMatrix(data = ., label = datosDIF$Diferencia)
    
    optimal.cv <- xgb.cv(params = best.params,
                         data = datosDIF,
                         nrounds = 999,
                         nthread = 4,
                         nfold = 3,
                         prediction = FALSE,
                         showsd = TRUE,
                         early_stopping_rounds = 50,
                         verbose = 0,
                         num_class=num_classDIF) #7 clases en Diferencia
    
    repeticiones <- optimal.cv$best_ntreelimit
    return(repeticiones) 
  })
  
  XGBmodDIF<-reactive({
    datosDIF = datosDIF()
    datosDIF$PaisLocal=as.numeric(as.factor(datosDIF$PaisLocal))
    datosDIF$EstiloLocal=as.numeric(as.factor(datosDIF$EstiloLocal))
    datosDIF$PaisVisitante=as.numeric(as.factor(datosDIF$PaisVisitante))
    datosDIF$EstiloVisitante=as.numeric(as.factor(datosDIF$EstiloVisitante))
    datosDIF$Competición=as.numeric(as.factor(datosDIF$Competición))
    
    num_classDIF = length(levels(as.factor(datosDIF$Diferencia)))
    best.params=paramXGBDIF()
    best.params$booster <- "gbtree"
    best.params$objective <- "multi:softprob"
    best.params$eval_metric <- "mlogloss"
    repeticiones=nroundDIF()
    datosDIF$Diferencia <-as.integer(as.factor(datosDIF$Diferencia))-1
    datosDIF <- 
      datosDIF %>%  
      select(-Diferencia) %>%  
      as.matrix() %>% 
      xgb.DMatrix(data = ., label = datosDIF$Diferencia)
    
    final.model <- xgboost(params = best.params, 
                           data = datosDIF,
                           nrounds = repeticiones,
                           verbose = 0,
                           num_class=num_classDIF)
    final.model
  })
  
  output$summaryXGBDIF <- renderPrint({
    datosDIF = datosDIF()
    datosDIF$Diferencia <-as.integer(as.factor(datosDIF$Diferencia))
    datosDIF$PaisLocal=as.numeric(as.factor(datosDIF$PaisLocal))
    datosDIF$EstiloLocal=as.numeric(as.factor(datosDIF$EstiloLocal))
    datosDIF$PaisVisitante=as.numeric(as.factor(datosDIF$PaisVisitante))
    datosDIF$EstiloVisitante=as.numeric(as.factor(datosDIF$EstiloVisitante))
    datosDIF$Competición=as.numeric(as.factor(datosDIF$Competición))
    
    predichos=predict(XGBmodDIF(), as.matrix(datosDIF[,-1]))
    tabla=matrix(predichos, ncol = 7, byrow = TRUE)
    predichos=max.col(tabla)-4
    
    a=confusionMatrix(factor(predichos,levels=levels(as.factor(datosDIF$Diferencia))), #predicho
                      factor(datosDIF$Diferencia,levels=levels(as.factor(datosDIF$Diferencia)))) #real
    
    a
  })
  
  output$MAPEXGBDIF <- renderInfoBox({
    datosDIF = datosDIF()
    datosDIF$Diferencia <- as.integer(as.factor(datosDIF$Diferencia))
    datosDIF$PaisLocal=as.numeric(as.factor(datosDIF$PaisLocal))
    datosDIF$EstiloLocal=as.numeric(as.factor(datosDIF$EstiloLocal))
    datosDIF$PaisVisitante=as.numeric(as.factor(datosDIF$PaisVisitante))
    datosDIF$EstiloVisitante=as.numeric(as.factor(datosDIF$EstiloVisitante))
    datosDIF$Competición=as.numeric(as.factor(datosDIF$Competición))
    
    predichos=predict(XGBmodDIF(), as.matrix(datosDIF[,-1]))
    tabla=matrix(predichos, ncol = 7, byrow = TRUE)
    predichos=max.col(tabla)-4
    mape=MAPE(predichos, as.numeric(datosDIF$Diferencia))
    
    infoBox(
      "MAPE", round(mape, digits = 2), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple"
    ) #infobox
  })
  
  output$probabilidadesXGBDIF <- DT::renderDataTable(
    DT::datatable({
      evaluarDIF=evaluarDIF()
      evaluarDIF$Diferencia <-as.integer(as.factor(evaluarDIF$Diferencia))
      evaluarDIF$PaisLocal=as.numeric(as.factor(evaluarDIF$PaisLocal))
      evaluarDIF$EstiloLocal=as.numeric(as.factor(evaluarDIF$EstiloLocal))
      evaluarDIF$PaisVisitante=as.numeric(as.factor(evaluarDIF$PaisVisitante))
      evaluarDIF$EstiloVisitante=as.numeric(as.factor(evaluarDIF$EstiloVisitante))
      evaluarDIF$Competición=as.numeric(as.factor(evaluarDIF$Competición))
      
      predichos=predict(XGBmodDIF(), as.matrix(evaluarDIF[,-1]))
      tabla=matrix(predichos, ncol = 7, byrow = TRUE)
      as.data.frame(cbind(Datos$Local[which(is.na(Datos[,2]))],Datos$Visitante[which(is.na(Datos[,2]))],tabla))
    },
    filter = "top", extensions = c('Buttons', 'Scroller'),
    options = list(initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': 'seagreen', 'color': '1c1b1b'});",
      "}"),
      scrollX = 500,
      scrollY = 300,
      deferRender = TRUE,
      scroller = TRUE,
      buttons = list('excel',
                     list(extend = 'colvis', targets = 0, visible = FALSE)),
      dom = 'lBfrtip',
      fixedColumns = TRUE
    ),
    rownames = FALSE))
  
  
  #XGBoost Goles------------
  paramXGBGol <- reactive({
    datosGOL = datosGOL()
    datosGOL$Goles <- as.factor(datosGOL$Goles)
    num_class = length(levels(as.factor(datosGOL$Goles)))
    
    tsk = makeClassifTask("Xgboost Opt", data = datosGOL, target = "Goles")
    tsk = createDummyFeatures(tsk)
    lrn = makeLearner("classif.xgboost", nthread = 4)
    lrn$par.vals = list(
      nrounds             = 199,
      objective           = "multi:softmax",
      verbose=0,
      eval_metric = "mlogloss"
    )
    
    res = makeResampleDesc("CV", iters = 3)
    par = makeParamSet(
      makeNumericParam("eta",                    lower = 0.05, upper = 0.3),
      makeNumericParam("gamma",                  lower = 1,     upper = 8),
      makeIntegerParam("max_depth",              lower= 6,      upper = 15),
      makeIntegerParam("min_child_weight",       lower= 1,    upper = 9),
      makeNumericParam("subsample",              lower = 0.20,  upper = 0.8),
      makeNumericParam("colsample_bytree",       lower = 0.20,  upper = 0.9)
    )
    
    mbo.ctrl = makeMBOControl()
    mbo.ctrl = setMBOControlInfill(mbo.ctrl, crit = crit.ei)
    mbo.ctrl = setMBOControlTermination(mbo.ctrl, max.evals = 20L)
    
    design.mat = generateRandomDesign(n = 10, par.set = par)
    ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)
    
    tune.pars = tuneParams(learner = lrn, task = tsk, resampling = res,
                           par.set = par, control = ctrl)
    return(tune.pars$mbo.result$x)
  })
  
  nroundGol <- reactive({
    datosGOL = datosGOL()
    datosGOL$PaisLocal=as.numeric(as.factor(datosGOL$PaisLocal))
    datosGOL$EstiloLocal=as.numeric(as.factor(datosGOL$EstiloLocal))
    datosGOL$PaisVisitante=as.numeric(as.factor(datosGOL$PaisVisitante))
    datosGOL$EstiloVisitante=as.numeric(as.factor(datosGOL$EstiloVisitante))
    datosGOL$Competición=as.numeric(as.factor(datosGOL$Competición))
    
    num_classGol = length(levels(as.factor(datosGOL$Goles)))
    best.params=paramXGBGol()
    
    best.params$booster <- "gbtree"
    best.params$objective <- "multi:softmax"
    best.params$eval_metric <- "mlogloss"
    
    datosGOL$Goles <-as.integer(as.factor(datosGOL$Goles))-1
    datosGOL <- 
      datosGOL %>%  
      select(-Goles) %>%  
      as.matrix() %>% 
      xgb.DMatrix(data = ., label = datosGOL$Goles)
    
    optimal.cv <- xgb.cv(params = best.params,
                         data = datosGOL,
                         nrounds = 499,
                         nthread = 4,
                         nfold = 3,
                         prediction = FALSE,
                         showsd = TRUE,
                         early_stopping_rounds = 50,
                         verbose = 0,
                         num_class=num_classGol) 
    
    repeticiones <- optimal.cv$best_ntreelimit
    return(repeticiones) 
  })
  
  XGBmodGol<-reactive({
    datosGOL = datosGOL()
    datosGOL$PaisLocal=as.numeric(as.factor(datosGOL$PaisLocal))
    datosGOL$EstiloLocal=as.numeric(as.factor(datosGOL$EstiloLocal))
    datosGOL$PaisVisitante=as.numeric(as.factor(datosGOL$PaisVisitante))
    datosGOL$EstiloVisitante=as.numeric(as.factor(datosGOL$EstiloVisitante))
    datosGOL$Competición=as.numeric(as.factor(datosGOL$Competición))
    
    num_classGol = length(levels(as.factor(datosGOL$Goles)))
    best.params=paramXGBGol()
    best.params$booster <- "gbtree"
    best.params$objective <- "multi:softprob"
    best.params$eval_metric <- "mlogloss"
    repeticiones=nroundGol()
    datosGOL$Goles <-as.integer(as.factor(datosGOL$Goles))-1
    datosGOL <- 
      datosGOL %>%  
      select(-Goles) %>%  
      as.matrix() %>% 
      xgb.DMatrix(data = ., label = datosGOL$Goles)
    
    final.model <- xgboost(params = best.params, 
                           data = datosGOL,
                           nrounds = repeticiones,
                           verbose = 0,
                           num_class=num_classGol)
    final.model
  })
  
  output$summaryXGBGol <- renderPrint({
    datosGOL = datosGOL()
    datosGOL$Goles <-as.integer(as.factor(datosGOL$Goles))-1
    datosGOL$PaisLocal=as.numeric(as.factor(datosGOL$PaisLocal))
    datosGOL$EstiloLocal=as.numeric(as.factor(datosGOL$EstiloLocal))
    datosGOL$PaisVisitante=as.numeric(as.factor(datosGOL$PaisVisitante))
    datosGOL$EstiloVisitante=as.numeric(as.factor(datosGOL$EstiloVisitante))
    datosGOL$Competición=as.numeric(as.factor(datosGOL$Competición))
    
    predichos=predict(XGBmodGol(), as.matrix(datosGOL[,-1]))
    tabla=matrix(predichos, ncol = 10, byrow = TRUE)
    predichos=max.col(tabla)-1
    
    a=confusionMatrix(factor(predichos,levels=levels(as.factor(datosGOL$Goles))), #predicho
                      factor(datosGOL$Goles,levels=levels(as.factor(datosGOL$Goles)))) #real
    a
  })
  
  output$MAPEXGBGol <- renderInfoBox({
    datosGOL = datosGOL()
    datosGOL$Goles <- as.integer(as.factor(datosGOL$Goles))
    datosGOL$PaisLocal=as.numeric(as.factor(datosGOL$PaisLocal))
    datosGOL$EstiloLocal=as.numeric(as.factor(datosGOL$EstiloLocal))
    datosGOL$PaisVisitante=as.numeric(as.factor(datosGOL$PaisVisitante))
    datosGOL$EstiloVisitante=as.numeric(as.factor(datosGOL$EstiloVisitante))
    datosGOL$Competición=as.numeric(as.factor(datosGOL$Competición))
    
    predichos=predict(XGBmodGol(), as.matrix(datosGOL[,-1]))
    tabla=matrix(predichos, ncol = 10, byrow = TRUE)
    predichos=max.col(tabla)-1
    a=confusionMatrix(factor(predichos,levels=levels(as.factor(datosGOL$Goles))), #predicho
                      factor(datosGOL$Goles,levels=levels(as.factor(datosGOL$Goles)))) #real
    #mape=MAPE(predichos, as.numeric(datosGOL$Goles))
    errorGol=errorJhon(tablaConfusion = a$table)
    
    infoBox(
      "Error Inferior", round(errorGol, digits = 2), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple"
    ) #infobox

  })
  
  output$probabilidadesXGBGOL <- DT::renderDataTable(
    DT::datatable({
      evaluarGol=evaluarGol()
      evaluarGol$Goles <-as.integer(as.factor(evaluarGol$Goles))-1
      evaluarGol$PaisLocal=as.numeric(as.factor(evaluarGol$PaisLocal))
      evaluarGol$EstiloLocal=as.numeric(as.factor(evaluarGol$EstiloLocal))
      evaluarGol$PaisVisitante=as.numeric(as.factor(evaluarGol$PaisVisitante))
      evaluarGol$EstiloVisitante=as.numeric(as.factor(evaluarGol$EstiloVisitante))
      evaluarGol$Competición=as.numeric(as.factor(evaluarGol$Competición))
      
      predichos=predict(XGBmodGol(), as.matrix(evaluarGol[,-1]))
      tabla=matrix(predichos, ncol = 10, byrow = TRUE)
      as.data.frame(cbind(Datos$Local[which(is.na(Datos[,1]))],Datos$Visitante[which(is.na(Datos[,1]))],tabla))
    },
    filter = "top", extensions = c('Buttons', 'Scroller'),
    options = list(initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': 'seagreen', 'color': '1c1b1b'});",
      "}"),
      scrollX = 500,
      scrollY = 300,
      deferRender = TRUE,
      scroller = TRUE,
      buttons = list('excel',
                     list(extend = 'colvis', targets = 0, visible = FALSE),
                     list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'), text = 'Download')
      ),
      dom = 'lBfrtip',
      fixedColumns = TRUE
    ),
    rownames = FALSE))
  
  
  ##predicciones---------------

  tablapredichos <- reactive({
    evaluarGPE=evaluarGPE()
    evaluarGPE$y <-as.integer(as.factor(evaluarGPE$y))-1
    evaluarGPE$PaisLocal=as.numeric(as.factor(evaluarGPE$PaisLocal))
    evaluarGPE$EstiloLocal=as.numeric(as.factor(evaluarGPE$EstiloLocal))
    evaluarGPE$PaisVisitante=as.numeric(as.factor(evaluarGPE$PaisVisitante))
    evaluarGPE$EstiloVisitante=as.numeric(as.factor(evaluarGPE$EstiloVisitante))
    evaluarGPE$Competición=as.numeric(as.factor(evaluarGPE$Competición))
    xgbGPE=predict(XGBmodGPE(), as.matrix(evaluarGPE[,-1]))
    tabla1=matrix(xgbGPE, ncol = 3, byrow = TRUE)
    xgbGPE=max.col(tabla1)
    
    evaluarGol=evaluarGol()
    evaluarGol$Goles <-as.integer(as.factor(evaluarGol$Goles))-1
    evaluarGol$PaisLocal=as.numeric(as.factor(evaluarGol$PaisLocal))
    evaluarGol$EstiloLocal=as.numeric(as.factor(evaluarGol$EstiloLocal))
    evaluarGol$PaisVisitante=as.numeric(as.factor(evaluarGol$PaisVisitante))
    evaluarGol$EstiloVisitante=as.numeric(as.factor(evaluarGol$EstiloVisitante))
    evaluarGol$Competición=as.numeric(as.factor(evaluarGol$Competición))
    xgbGol=predict(XGBmodGol(), as.matrix(evaluarGol[,-1]))
    tabla2=matrix(xgbGol, ncol = 10, byrow = TRUE)
    xgbGol=max.col(tabla2)-1
    
    tabla=as.data.frame(cbind( #dataframe
      Datos$Local[which(is.na(Datos[,2]))],Datos$Visitante[which(is.na(Datos[,2]))], #local y visitante
      xgbGPE, #XGBGPE
      xgbGol #XGBGol
      ))
    tabla
  })
  
  output$predichosTodos <- DT::renderDataTable(
    DT::datatable({
      tabla=tablapredichos()
      as.data.frame(tabla)
    },
    filter = "top", extensions = c('Buttons', 'Scroller'),
    options = list(initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': 'seagreen', 'color': '1c1b1b'});",
      "}"),
      scrollX = 500,
      scrollY = 300,
      deferRender = TRUE,
      scroller = TRUE,
      buttons = list('excel',
                     list(extend = 'colvis', targets = 0, visible = FALSE),
                     list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'), text = 'Download')
      ),
      dom = 'lBfrtip',
      fixedColumns = TRUE
    ),
    rownames = FALSE))
  
})

shinyApp(ui, server)


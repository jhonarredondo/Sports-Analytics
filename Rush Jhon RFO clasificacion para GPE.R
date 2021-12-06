#RFO Rush Bet (por Ganador)
library(readxl)
Datos <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
i=1

while (Datos[i,1] != "") {
  i=i+1
}

Datos=Datos[1:i-1,-c(2,3,7,8,9,31,45)]
Datos$y <-as.factor(Datos$y)
Datos$PaisLocal=as.factor(Datos$PaisLocal)
Datos$EstiloLocal=as.factor(Datos$EstiloLocal)
Datos$PaisVisitante=as.factor(Datos$PaisVisitante)
Datos$EstiloVisitante=as.factor(Datos$EstiloVisitante)
Datos$Competición=as.factor(Datos$Competición)

tuning_rf_mtry <- function(df, y, ntree = 500){
  # Esta funci?n devuelve el out-of-bag clasification error de un modelo RandomForest
  # en funci?n del n?mero de predictores evaluados (mtry)
  
  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   ntree = n?mero de ?rboles creados en el modelo randomForest
  require(dplyr)
  require(randomForest)
  max_predictores <- ncol(df) - 1
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

hiperparametro_mtry <-  tuning_rf_mtry(df = Datos, y = "y")

library(ggplot2)
ggplot(data = hiperparametro_mtry, aes(x = n_predictores, y = oob_err_rate)) +
  scale_x_continuous(breaks = hiperparametro_mtry$n_predictores) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_mtry %>% arrange(oob_err_rate) %>% head(1),
             color = "red") +
  labs(title = "Evoluci?n del out-of-bag-error vs mtry",
       x = "n? predictores empleados") +
  theme_bw()

predictoresj=3
#5 cuando tome todas las variables

#Identificaci?n del valor ?ptimo del hiperpar?metro nodesize.-------------------------------
tuning_rf_nodesize <- function(df, y, size = NULL, ntree = 500){
  # Esta funci?n devuelve el out-of-bag clasification error de un modelo RandomForest
  # en funci?n del tama?o m?nimo de los nodos terminales (nodesize).
  
  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   sizes = tama?os evaluados
  #   ntree = n?mero de ?rboles creados en el modelo randomForest
  
  require(dplyr)
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
hiperparametro_nodesize <-  tuning_rf_nodesize(df = Datos, y = "y",
                                               size = c(1:20))

ggplot(data = hiperparametro_nodesize, aes(x = size, y = oob_err_rate)) +
  scale_x_continuous(breaks = hiperparametro_nodesize$size) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_nodesize %>% arrange(oob_err_rate) %>% head(1),
             color = "red") +
  labs(title = "Evoluci?n del out-of-bag-error vs nodesize",
       x = "n? observaciones en nodos terminales") +
  theme_bw()

obs=10
#fueron 18 cuando tome todos, deberiamos evaluar un size mayor

modelo_randomforest <- randomForest(y ~ ., data = Datos, mtry = predictoresj, ntree = 5000,
                                    importance = TRUE, nodesize = obs)

oob_err_rate <- data.frame(oob_err_rate = modelo_randomforest$err.rate[, 1],
                           arboles = seq_along(modelo_randomforest$err.rate[, 1]))
ggplot(data = oob_err_rate, aes(x = arboles, y = oob_err_rate )) +
  geom_line() +
  labs(title = "Evoluci?n del out-of-bag-error vs n?mero ?rboles",
       x = "n? ?rboles") +
  theme_bw()

arboles=3000

##Modelo final RFO clasification:---------------------------------------------
modelo_randomforest <- randomForest(y ~ ., data = Datos, mtry = predictoresj, ntree = arboles,
                                    importance = TRUE, nodesize = obs,
                                    norm.votes = TRUE )
modelo_randomforest ##50.98%
library(MLmetrics)
R2_Score(as.numeric(modelo_randomforest$predicted), as.numeric(Datos$y)) #negativo...
MAPE(as.numeric(modelo_randomforest$predicted), as.numeric(Datos$y)) #0.37
RMSE(as.numeric(modelo_randomforest$predicted), as.numeric(Datos$y)) #1.04
##LLegamos a un 53% con todas las variables...

##Identificar predictores m?s influyentes---------------------------
library(tidyverse)
library(ggpubr)
importancia_pred <- as.data.frame(importance(modelo_randomforest, scale = TRUE))
importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
p1 <- ggplot(data=importancia_pred, aes(x=reorder(variable, MeanDecreaseAccuracy),
                                        y = MeanDecreaseAccuracy,
                                        fill = MeanDecreaseAccuracy)) +
  labs(x = "variable", title = "Reducci?n de Accuracy") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importancia_pred, aes(x = reorder(variable, MeanDecreaseGini),
                                          y = MeanDecreaseGini,
                                          fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Reducci?n de pureza (Gini)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggarrange(p1, p2) 

evaluar <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")

evaluar$PaisLocal=as.factor(evaluar$PaisLocal)
evaluar$EstiloLocal=as.factor(evaluar$EstiloLocal)
evaluar$PaisVisitante=as.factor(evaluar$PaisVisitante)
evaluar$EstiloVisitante=as.factor(evaluar$EstiloVisitante)
evaluar$Competición=as.factor(evaluar$Competición)


predicciones <- predict(modelo_randomforest, newdata = evaluar, type = "class")
(predicciones=predicciones[i:nrow(evaluar)])
summary(predicciones)

verificar <- predict(modelo_randomforest, newdata = evaluar, type = "vote")
verificar[i:nrow(evaluar),]
##Apuestas seguras: Mayor a 0.9?


library(xlsx)
predicciones<-as.vector(predicciones)
setwd("C:/Users/Administrador/Desktop")
write.xlsx(predicciones, "Rush Jhon.xlsm", append = TRUE, sheetName = "RFO GPE")


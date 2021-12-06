##Modelos Step
###Modelo Logistico---------------------------------------------
library(car)
library(readxl)
Datos <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm", sheet = "Resultados")
Datos <- as.data.frame(Datos)
i=1

while (Datos[i,1] != 0) {
  i=i+1
}

Datos=Datos[1:i-1,-c(2,3,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...

modLog <- glm(y ~ ., data = Datos, family = "poisson")
summary(modLog)

steplog=step(modLog, direction = "both") ##o backward?
summary(steplog)
library(MLmetrics)
R2_Score(steplog$fitted.values, Datos$y) ##0.16
MAPE(steplog$fitted.values, Datos$y) ##0.44
RMSE(steplog$fitted.values, Datos$y) ##0.78

dev <- steplog$deviance
nullDev <- steplog$null.deviance
modelChi <- nullDev - dev
modelChi
chigl <- steplog$df.null - steplog$df.residual
chisq.prob <- 1 - pchisq(modelChi, chigl)
chisq.prob ##si es menor a 0.05 podemos rechazar la hipótesis nula de que el modelo es mejor prediciendo la variable resultado que si elegimos por azar
##es casi 0... es muy bueno entonces?

evaluar <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
evaluar<-evaluar[i:nrow(evaluar),]
Prediccion2 <- predict.glm(object = steplog, newdata = evaluar, interval = "response")
Prediccion2
Prediccion2 <- exp(Prediccion2)/(1+exp(Prediccion2)) 
Prediccion2
##Menor a 0.63 gana local...
##Mayor a 0.67 gana visitante...
j=1
Imprimir=rep(3, length(Prediccion2))

for (j in (1:length(Prediccion2))) {
  if (Prediccion2[j]<0.635){
    Imprimir[j]=1
  } else if (Prediccion2[j]<0.665){
    Imprimir[j]=2
  }
}
(Imprimir)
write.xlsx(Imprimir, "Rush Jhon.xlsm", append = TRUE, sheetName = "RLogistica Step Model")



##step model para RFO Resultados-----------------------------------
Datos <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
i=1

while (Datos[i,1] != "") {
  i=i+1
}

Datos=Datos[1:i-1,-c(2,3,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...
Datos$y <-as.factor(Datos$y)
Datos$PaisLocal=as.factor(Datos$PaisLocal)
Datos$EstiloLocal=as.factor(Datos$EstiloLocal)
Datos$PaisVisitante=as.factor(Datos$PaisVisitante)
Datos$EstiloVisitante=as.factor(Datos$EstiloVisitante)
Datos$Competición=as.factor(Datos$Competición)

library(randomForest)
modelo_randomforest <- randomForest(y ~ ., data = Datos)
modelo_randomforest ##53.61%
R2_Score(as.numeric(modelo_randomforest$predicted), as.numeric(Datos$y)) #negativo...
MAPE(as.numeric(modelo_randomforest$predicted), as.numeric(Datos$y)) #0.42
RMSE(as.numeric(modelo_randomforest$predicted), as.numeric(Datos$y)) #1.08

library(caTools)
library(caret)
library(e1071)
library(rpart)
numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
caret::train(y ~ ., data = Datos, method = "rpart", trControl = numFolds, tuneGrid = cpGrid) #0.16
stepRFORes = rpart(y ~ ., data = Datos, method = "class", cp = 0.16)
stepRFORes$variable.importance ##variables mas importantes....
confusionMatrix(factor(as.numeric(stepRFORes$where),levels=levels(as.factor(Datos$y))), #predicho
                factor(Datos$y,levels=levels(as.factor(Datos$y)))) #real
R2_Score(as.numeric(stepRFORes$where), as.numeric(Datos$y)) #negativo...
MAPE(as.numeric(stepRFORes$where), as.numeric(Datos$y)) #0.61
RMSE(as.numeric(stepRFORes$where), as.numeric(Datos$y)) #0.94

evaluar <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")

evaluar$PaisLocal=as.factor(evaluar$PaisLocal)
evaluar$EstiloLocal=as.factor(evaluar$EstiloLocal)
evaluar$PaisVisitante=as.factor(evaluar$PaisVisitante)
evaluar$EstiloVisitante=as.factor(evaluar$EstiloVisitante)
evaluar$Competición=as.factor(evaluar$Competición)

predicciones <- predict(stepRFORes, newdata = evaluar, type = "class")
(predicciones=predicciones[i:nrow(evaluar)])
summary(predicciones)

verificar <- predict(modelo_randomforest, newdata = evaluar, type = "vote")
verificar[i:nrow(evaluar),]

write.xlsx(predicciones, "Rush Jhon.xlsm", append = TRUE, sheetName = "RFO GPE Step")



###modelo step RFO diferencia--------------------------
Datos <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Diferencia")
i=1

while (Datos[i,1] != "") {
  i=i+1
}

Datos <- Datos[1:i-1,-c(1,3,7,8,9,31,45)] #
Datos$Diferencia=as.factor(Datos$Diferencia)
Datos$PaisLocal=as.factor(Datos$PaisLocal)
Datos$EstiloLocal=as.factor(Datos$EstiloLocal)
Datos$PaisVisitante=as.factor(Datos$PaisVisitante)
Datos$EstiloVisitante=as.factor(Datos$EstiloVisitante)
Datos$Competición=as.factor(Datos$Competición)


library(randomForest)
modelo_randomforest <- randomForest(Diferencia ~ ., data = Datos)
modelo_randomforest ##73.07%
R2_Score(as.numeric(modelo_randomforest$predicted), as.numeric(Datos$Diferencia)) #negativo...
MAPE(as.numeric(modelo_randomforest$predicted), as.numeric(Datos$Diferencia)) #0.47
RMSE(as.numeric(modelo_randomforest$predicted), as.numeric(Datos$Diferencia)) #1.8

library(caTools)
library(caret)
library(e1071)
numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(Diferencia ~ ., data = Datos, method = "rpart", trControl = numFolds, tuneGrid = cpGrid) #0.02
stepRFODif = rpart(Diferencia ~ ., data = Datos, method = "class", cp = 0.02)
stepRFODif$variable.importance ##variables mas importantes....
R2_Score(as.numeric(stepRFODif$where), as.numeric(Datos$Diferencia)) #negativo...
MAPE(as.numeric(stepRFODif$where), as.numeric(Datos$Diferencia)) #4.13
RMSE(as.numeric(stepRFODif$where), as.numeric(Datos$Diferencia)) #16.28

evaluar <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Diferencia")

evaluar$PaisLocal=as.factor(evaluar$PaisLocal)
evaluar$EstiloLocal=as.factor(evaluar$EstiloLocal)
evaluar$PaisVisitante=as.factor(evaluar$PaisVisitante)
evaluar$EstiloVisitante=as.factor(evaluar$EstiloVisitante)
evaluar$Competición=as.factor(evaluar$Competición)

predicciones <- predict(stepRFODif, newdata = evaluar, type = "class")
(predicciones=predicciones[i:nrow(evaluar)])
summary(predicciones)

verificar <- predict(stepRFODif, newdata = evaluar, type = "prob")
verificar[i:nrow(evaluar),]

write.xlsx(predicciones, "Rush Jhon.xlsm", append = TRUE, sheetName = "RFO Diferencia Step")

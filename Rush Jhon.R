###Modelo Logistico--------------------------------------------
library(car)
library(readxl)
Datos <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm", sheet = "Resultados")
Datos <- as.data.frame(Datos)
i=1

while (Datos[i,1] != 0) {
  i=i+1
}

Datos=Datos[1:i-1,-c(2,3,7,8,9,31,45)]

modLog <- glm(y ~ ., data = Datos, family = "poisson")
summary(modLog)
anova(modLog)
vif(modLog)
library(MLmetrics)
R2_Score(modLog$fitted.values, Datos$y) ##0.26
MAPE(modLog$fitted.values, Datos$y) ##0.40
RMSE(modLog$fitted.values, Datos$y) ##0.73

dev <- modLog$deviance
nullDev <- modLog$null.deviance
modelChi <- nullDev - dev
modelChi
chigl <- modLog$df.null - modLog$df.residual
chisq.prob <- 1 - pchisq(modelChi, chigl)
chisq.prob ##si es menor a 0.05 podemos rechazar la hipótesis nula de que el modelo es mejor prediciendo la variable resultado que si elegimos por azar
#0.94
##es casi 1... es mejor elegir al azar definitivamente!

###Ecuacion 2-----------------------------
modLog$coefficients
modLog$residuals
boxplot(modLog$residuals)
shapiro.test(modLog$residuals)
qqPlot(modLog$residuals)
hist(modLog$residuals)

evaluar <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
evaluar<-evaluar[i:nrow(evaluar),]
Prediccion2 <- predict.glm(object = modLog, newdata = evaluar, interval = "response")
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
library(xlsx)
setwd("C:/Users/Administrador/Desktop")
write.xlsx(Imprimir, "Rush Jhon.xlsm", append = TRUE, sheetName = "RLogistica")

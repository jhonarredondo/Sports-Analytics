##parametrizacion XGB GPE
library(readxl)
library(lmtest)

parametros=read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "XGBoost parametros")
parametros=parametros[,1:8]

mod1=lm(formula = mape...8~., data = parametros)
summary(mod1)
anova(mod1)
vif(mod1)

hist(mod1$residuals)
acf(mod1$residuals)
boxplot(mod1$residuals)
shapiro.test(mod1$residuals)
bptest(mod1)
##Super bien el modelo :O

mod1$coefficients


##usar valores "optimizados"-----------------------------
library(xgboost)
library(tidyverse)
Datos <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
i=1

while (Datos[i,1] != "") {
  i=i+1
}

Datos=Datos[1:i-1,-c(2,3,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...
num_class = length(levels(as.factor(Datos$y)))
Datos$y <-as.factor(Datos$y)
Datos$PaisLocal=as.numeric(as.factor(Datos$PaisLocal))
Datos$EstiloLocal=as.numeric(as.factor(Datos$EstiloLocal))
Datos$PaisVisitante=as.numeric(as.factor(Datos$PaisVisitante))
Datos$EstiloVisitante=as.numeric(as.factor(Datos$EstiloVisitante))
Datos$Competición=as.numeric(as.factor(Datos$Competición))

Datos$y <-as.integer(as.factor(Datos$y))-1
Datos <- 
  Datos %>%  
  select(-y) %>%  
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = Datos$y)

final.model <- xgboost(params = list(
  eta=0.68111251385244,
  gamma= 0,
  max_depth=20,
  min_child_weight=0,
  subsample=0.999881564444006,
  colsample_bytree=0.0001
), 
data = Datos,
objective="multi:softprob",
nrounds = 17,
verbose = 0,
num_class=num_class)
final.model


Datos <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
i=1
while (Datos[i,1] != "") {
  i=i+1
}

Datos=Datos[1:i-1,-c(2,3,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...

num_class = length(levels(as.factor(Datos$y)))
Datos$y <-as.factor(Datos$y)
Datos$PaisLocal=as.numeric(as.factor(Datos$PaisLocal))
Datos$EstiloLocal=as.numeric(as.factor(Datos$EstiloLocal))
Datos$PaisVisitante=as.numeric(as.factor(Datos$PaisVisitante))
Datos$EstiloVisitante=as.numeric(as.factor(Datos$EstiloVisitante))
Datos$Competición=as.numeric(as.factor(Datos$Competición))

predichos=predict(final.model, as.matrix(Datos[,-1]))
tabla=matrix(predichos, ncol = 3, byrow = TRUE)
predichos=max.col(tabla)
predichos

RMSE(predichos, as.numeric(Datos$y)) #0
MAPE(predichos, as.numeric(Datos$y)) #0 OMG!!
a=confusionMatrix(factor(predichos,levels=levels(as.factor(Datos$y))), #predicho
                  factor(Datos$y,levels=levels(as.factor(Datos$y)))) #real
a
sum(a$table[lower.tri(a$table)])/sum(a$table)
##LLegamos a una precisión 0.6247% con todas las variables...

#Importancia variables
xgb.importance(model = final.model) %>% xgb.plot.importance()
xgb.importance(model = final.model) #lista completa

#escribir pronosticos optimizados
evaluar <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
evaluar=evaluar[i:nrow(evaluar),-c(2,3,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...
evaluar$y <-as.factor(evaluar$y)
evaluar$PaisLocal=as.numeric(as.factor(evaluar$PaisLocal))
evaluar$EstiloLocal=as.numeric(as.factor(evaluar$EstiloLocal))
evaluar$PaisVisitante=as.numeric(as.factor(evaluar$PaisVisitante))
evaluar$EstiloVisitante=as.numeric(as.factor(evaluar$EstiloVisitante))
evaluar$Competición=as.numeric(as.factor(evaluar$Competición))

predicciones=predict(final.model, as.matrix(evaluar[,-1]))
tabla2=matrix(predicciones, ncol = 3, byrow = TRUE)
predicciones=max.col(tabla2)
predicciones ##como poner las prob de cada predicción?
library(xlsx)
predecirnuevos<-cbind(predicciones,tabla2)
setwd("C:/Users/Administrador/Desktop")
writexl::write_xlsx(as.data.frame(predecirnuevos), "predichos opt xgb GPE.xlsx")




##validación xgb gpe------------------------------
Datos <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
i=1
while (Datos[i,1] != "") {
  i=i+1
}

Datos=Datos[1:i-1,-c(2,3,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...

num_class = length(levels(as.factor(Datos$y)))
Datos$y <-as.factor(Datos$y)
Datos$PaisLocal=as.numeric(as.factor(Datos$PaisLocal))
Datos$EstiloLocal=as.numeric(as.factor(Datos$EstiloLocal))
Datos$PaisVisitante=as.numeric(as.factor(Datos$PaisVisitante))
Datos$EstiloVisitante=as.numeric(as.factor(Datos$EstiloVisitante))
Datos$Competición=as.numeric(as.factor(Datos$Competición))
Datos$y <-as.integer(as.factor(Datos$y))-1

sub=Datos[601:658,]
Datos=Datos[1:600,]


Datos <- 
  Datos %>%  
  select(-y) %>%  
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = Datos$y)

final.model <- xgboost(params = list(
  eta=0.68111251385244,
  gamma= 0,
  max_depth=20,
  min_child_weight=0,
  subsample=0.999881564444006,
  colsample_bytree=0.0001
), 
data = Datos,
nrounds = 17,
verbose = 0,
num_class=num_class)
final.model

validacion=sub[,1]+1

predichos=predict(final.model, as.matrix(sub[,-1]))+1
predichos

RMSE(predichos, as.numeric(validacion$y)) #1.19
MAPE(predichos, as.numeric(validacion$y)) #0.6
a=confusionMatrix(factor(predichos,levels=levels(as.factor(validacion$y))), #predicho
                  factor(validacion$y,levels=levels(as.factor(validacion$y)))) #real
a
sum(a$table[lower.tri(a$table)])/sum(a$table) #0.37

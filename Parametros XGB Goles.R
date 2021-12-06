##parametrizacion XGB Goles
library(readxl)
library(lmtest)

parametros=read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "XGB Goles")
parametros=parametros[,1:8]

mod1=lm(formula = Error...8~., data = parametros)
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
library(readxl)
Datos <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
i=1

while (Datos[i,1] != "") {
  i=i+1
}

Datos=Datos[180:i-1,-c(1,2,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...
num_class = length(levels(as.factor(Datos$Goles)))
Datos$Goles <-as.factor(Datos$Goles)
Datos$PaisLocal=as.numeric(as.factor(Datos$PaisLocal))
Datos$EstiloLocal=as.numeric(as.factor(Datos$EstiloLocal))
Datos$PaisVisitante=as.numeric(as.factor(Datos$PaisVisitante))
Datos$EstiloVisitante=as.numeric(as.factor(Datos$EstiloVisitante))
Datos$Competición=as.numeric(as.factor(Datos$Competición))

Datos$Goles <-as.integer(as.factor(Datos$Goles))-1
Datos <- 
  Datos %>%  
  select(-Goles) %>%  
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = Datos$Goles)

final.model <- xgboost(params = list(
  eta=0.05,
  gamma= 1.02637974373382,
  max_depth=18,
  min_child_weight=0,
  subsample=0.665596470976708,
  colsample_bytree=0.9999
), 
                       data = Datos,
objective="multi:softprob",
                       nrounds = 11,
                       verbose = 0,
                       num_class=num_class)
final.model


Datos <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
i=1
while (Datos[i,1] != "") {
  i=i+1
}

Datos=Datos[180:i-1,-c(1,2,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...

num_class = length(levels(as.factor(Datos$Goles)))
Datos$Goles <-as.factor(Datos$Goles)
Datos$PaisLocal=as.numeric(as.factor(Datos$PaisLocal))
Datos$EstiloLocal=as.numeric(as.factor(Datos$EstiloLocal))
Datos$PaisVisitante=as.numeric(as.factor(Datos$PaisVisitante))
Datos$EstiloVisitante=as.numeric(as.factor(Datos$EstiloVisitante))
Datos$Competición=as.numeric(as.factor(Datos$Competición))

predichos=predict(final.model, as.matrix(Datos[,-1]))
tabla=matrix(predichos, ncol = 10, byrow = TRUE)
predichos=max.col(tabla)-1
predichos

RMSE(predichos, as.numeric(Datos$Goles)) #1.31
MAPE(predichos, as.numeric(Datos$Goles)) #0.36
a=confusionMatrix(factor(predichos,levels=levels(as.factor(Datos$Goles))), #predicho
                  factor(Datos$Goles,levels=levels(as.factor(Datos$Goles)))) #real
a
sum(a$table[lower.tri(a$table)])/sum(a$table)
##LLegamos a una precisión 0.6247% con todas las variables...

#Importancia variables
xgb.importance(model = final.model) %>% xgb.plot.importance()

#escribir pronosticos optimizados
evaluar <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
evaluar=evaluar[i:nrow(evaluar),-c(1,2,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...
evaluar$Goles <-as.factor(evaluar$Goles)
evaluar$PaisLocal=as.numeric(as.factor(evaluar$PaisLocal))
evaluar$EstiloLocal=as.numeric(as.factor(evaluar$EstiloLocal))
evaluar$PaisVisitante=as.numeric(as.factor(evaluar$PaisVisitante))
evaluar$EstiloVisitante=as.numeric(as.factor(evaluar$EstiloVisitante))
evaluar$Competición=as.numeric(as.factor(evaluar$Competición))

predicciones=predict(final.model, as.matrix(evaluar[,-1]))
tabla2=matrix(predicciones, ncol = 10, byrow = TRUE)
predicciones=max.col(tabla2)-1
predicciones ##como poner las prob de cada predicción?
library(xlsx)
predecirnuevos<-cbind(predicciones, tabla2)
setwd("C:/Users/Administrador/Desktop")
writexl::write_xlsx(as.data.frame(predecirnuevos), "predichos opt xgb goles.xlsx")




##validación xgb goles------------------------------
Datos <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
i=1
while (Datos[i,1] != "") {
  i=i+1
}

Datos=Datos[180:i-1,-c(1,2,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...

num_class = length(levels(as.factor(Datos$Goles)))
Datos$Goles <-as.factor(Datos$Goles)
Datos$PaisLocal=as.numeric(as.factor(Datos$PaisLocal))
Datos$EstiloLocal=as.numeric(as.factor(Datos$EstiloLocal))
Datos$PaisVisitante=as.numeric(as.factor(Datos$PaisVisitante))
Datos$EstiloVisitante=as.numeric(as.factor(Datos$EstiloVisitante))
Datos$Competición=as.numeric(as.factor(Datos$Competición))
Datos$Goles <-as.integer(as.factor(Datos$Goles))-1

sub=Datos[501:543,]
Datos=Datos[1:500,]


Datos <- 
  Datos %>%  
  select(-Goles) %>%  
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = Datos$Goles)

final.model <- xgboost(params = list(
  eta=0.05,
  gamma= 1.02637974373382,
  max_depth=18,
  min_child_weight=0,
  subsample=0.665596470976708,
  colsample_bytree=0.9999
), 
data = Datos,
nrounds = 11,
verbose = 0,
num_class=num_class)
final.model

validacion=sub[,1]

predichos=predict(final.model, as.matrix(sub[,-1]))
predichos

RMSE(predichos, as.numeric(validacion$Goles)) #1.79
MAPE(predichos, as.numeric(validacion$Goles)) #0.46
a=confusionMatrix(factor(predichos,levels=levels(as.factor(validacion$Goles))), #predicho
                  factor(validacion$Goles,levels=levels(as.factor(validacion$Goles)))) #real
a
sum(a$table[lower.tri(a$table)])/sum(a$table) #0.37


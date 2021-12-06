library(readxl)
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

###Optimización GPE-----------------
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


tsk = makeClassifTask("Xgboost Opt", data = Datos, target = "y")
tsk = createDummyFeatures(tsk)
# NOTE: You may want to decrease the threads here depending on your machine
lrn = makeLearner("classif.xgboost", nthread = 4)

lrn$par.vals = list(
  nrounds             = 1199,
  objective           = "multi:softmax",
  print_every_n = 500
)

res = makeResampleDesc("CV", iters = 3)
par = makeParamSet(
  makeNumericParam("eta",                    lower = 0.01, upper = 0.14),
  makeNumericParam("gamma",                  lower = 3,     upper = 9),
  makeIntegerParam("max_depth",              lower= 3,      upper = 12),
  makeIntegerParam("min_child_weight",       lower= 3,    upper = 15),
  makeNumericParam("subsample",              lower = 0.45,  upper = 0.65), #diria que bien
  makeNumericParam("colsample_bytree",       lower = 0.4,  upper = 0.6) #diria que bien
)

# In this simple example we construct the control object with the defaults:
mbo.ctrl = makeMBOControl()
# For this numeric optimization we are going to use the Expected Improvement as infill criterion:
mbo.ctrl = setMBOControlInfill(mbo.ctrl, crit = crit.ei)
# We will allow for exactly 25 evaluations of the objective function:
mbo.ctrl = setMBOControlTermination(mbo.ctrl, max.evals = 20L)

# Make a design matrix 
design.mat = generateRandomDesign(n = 10, par.set = par)
# add the mbo control and design matrix to the mlrMBO tuning function
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)

# Everything else is pretty standard
tune.pars = tuneParams(learner = lrn, task = tsk, resampling = res,
                       par.set = par, control = ctrl)
tune.pars

best.params <- tune.pars$mbo.result$x
print(best.params)

best.params$booster <- "gbtree"
best.params$objective <- "multi:softmax"

num_class = length(levels(as.factor(Datos$y)))
Datos$y <-as.integer(as.factor(Datos$y))-1
Datos <- 
  Datos %>%  
  select(-y) %>%  
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = Datos$y)

#optimizar nrounds
optimal.cv <- xgb.cv(params = best.params,
                     data = Datos,
                     nrounds = 2999,
                     nthread = 4,
                     nfold = 3,
                     prediction = FALSE,
                     showsd = TRUE,
                     early_stopping_rounds = 1000,
                     verbose = 1,
                     print_every_n = 500,
                     num_class=3) #3 clases en y

repeticiones <- optimal.cv$best_ntreelimit
repeticiones #1017 de momento

#Modelo final
final.model <- xgboost(params = best.params, 
                       data = Datos,
                       nrounds = repeticiones,
                       verbose = 1,
                       print_every_n = 500,
                       num_class=3)

Datos <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
i=1
while (Datos[i,1] != "") {
  i=i+1
}
Datos=Datos[1:i-1,-c(2,3,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...
num_class = length(levels(as.factor(Datos$y)))
Datos$y <-as.integer(as.factor(Datos$y))-1
Datos$PaisLocal=as.numeric(as.factor(Datos$PaisLocal))
Datos$EstiloLocal=as.numeric(as.factor(Datos$EstiloLocal))
Datos$PaisVisitante=as.numeric(as.factor(Datos$PaisVisitante))
Datos$EstiloVisitante=as.numeric(as.factor(Datos$EstiloVisitante))
Datos$Competición=as.numeric(as.factor(Datos$Competición))

predichos=predict(final.model, as.matrix(Datos[,-1]))+1

MAPE(predichos, as.numeric(Datos$y)+1) #1.18
RMSE(predichos, as.numeric(Datos$y)+1) #1.18
confusionMatrix(factor(predichos,levels=levels(as.factor(Datos$y+1))), #predicho
                factor(Datos$y+1,levels=levels(as.factor(Datos$y+1)))) #real
##LLegamos a una precisión 0.6247% con todas las variables...

#Importancia variables
xgb.importance(model = final.model) %>% xgb.plot.importance()


#Predecir GPE----------------
evaluar <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
evaluar=evaluar[i:nrow(evaluar),-c(2,3,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...
evaluar$y <-as.integer(as.factor(evaluar$y))-1
evaluar$PaisLocal=as.numeric(as.factor(evaluar$PaisLocal))
evaluar$EstiloLocal=as.numeric(as.factor(evaluar$EstiloLocal))
evaluar$PaisVisitante=as.numeric(as.factor(evaluar$PaisVisitante))
evaluar$EstiloVisitante=as.numeric(as.factor(evaluar$EstiloVisitante))
evaluar$Competición=as.numeric(as.factor(evaluar$Competición))

predicciones=predict(final.model, as.matrix(evaluar[,-1]))+1
predicciones
library(xlsx)
predicciones<-as.vector(predicciones)
setwd("C:/Users/Administrador/Desktop")
write.xlsx(predicciones, "Rush Jhon.xlsm", append = TRUE, sheetName = "XGBoost GPE")







###Optimizar Dif---------------------------------------------
Datos <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
i=1

while (Datos[i,1] != "") {
  i=i+1
}

Datos=Datos[1:i-1,-c(1,3,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...
num_class = length(levels(as.factor(Datos$Diferencia)))
Datos$Diferencia <-as.factor(Datos$Diferencia)
Datos$PaisLocal=as.numeric(as.factor(Datos$PaisLocal))
Datos$EstiloLocal=as.numeric(as.factor(Datos$EstiloLocal))
Datos$PaisVisitante=as.numeric(as.factor(Datos$PaisVisitante))
Datos$EstiloVisitante=as.numeric(as.factor(Datos$EstiloVisitante))
Datos$Competición=as.numeric(as.factor(Datos$Competición))


tsk = makeClassifTask("Xgboost Opt", data = Datos, target = "Diferencia")
tsk = createDummyFeatures(tsk)
# NOTE: You may want to decrease the threads here depending on your machine
lrn = makeLearner("classif.xgboost", nthread = 4)

lrn$par.vals = list(
  nrounds             = 3000,
  objective           = "multi:softmax"
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

# In this simple example we construct the control object with the defaults:
mbo.ctrl = makeMBOControl()
# For this numeric optimization we are going to use the Expected Improvement as infill criterion:
mbo.ctrl = setMBOControlInfill(mbo.ctrl, crit = crit.ei)
# We will allow for exactly 25 evaluations of the objective function:
mbo.ctrl = setMBOControlTermination(mbo.ctrl, max.evals = 25L)

# Make a design matrix 
design.mat = generateRandomDesign(n = 10, par.set = par)
# add the mbo control and design matrix to the mlrMBO tuning function
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)

# Everything else is pretty standard
tune.pars = tuneParams(learner = lrn, task = tsk, resampling = res,
                       par.set = par, control = ctrl)

tune.pars

best.params <- tune.pars$mbo.result$x
print(best.params)

best.params$booster <- "gbtree"
best.params$objective <- "multi:softmax"

num_class = length(levels(as.factor(Datos$Diferencia)))
Datos$Diferencia <-as.numeric(as.factor(Datos$Diferencia))-1

Datos <- 
  Datos %>%  
  select(-Diferencia) %>%  
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = Datos$Diferencia)

#optimizar nrounds
optimal.cv <- xgb.cv(params = best.params,
                     data = Datos,
                     nrounds = 5000,
                     nthread = 4,
                     nfold = 3,
                     prediction = FALSE,
                     showsd = TRUE,
                     early_stopping_rounds = 1000,
                     verbose = 1,
                     print_every_n = 500,
                     num_class=num_class) #7 clases en Diferencia

repeticiones <- optimal.cv$best_ntreelimit
repeticiones #56 de momento

#Modelo final
final.model <- xgboost(params = best.params, 
                       data = Datos,
                       nrounds = repeticiones,
                       verbose = 1,
                       print_every_n = 500,
                       num_class=num_class)

Datos <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
i=1
while (Datos[i,1] != "") {
  i=i+1
}

Datos=Datos[1:i-1,-c(1,3,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...
num_class = length(levels(as.factor(Datos$Diferencia)))
Datos$Diferencia <-as.factor(Datos$Diferencia)
Datos$PaisLocal=as.numeric(as.factor(Datos$PaisLocal))
Datos$EstiloLocal=as.numeric(as.factor(Datos$EstiloLocal))
Datos$PaisVisitante=as.numeric(as.factor(Datos$PaisVisitante))
Datos$EstiloVisitante=as.numeric(as.factor(Datos$EstiloVisitante))
Datos$Competición=as.numeric(as.factor(Datos$Competición))

predichos=predict(final.model, as.matrix(Datos[,-1]))-3
predichos

RMSE(predichos, as.numeric(Datos$Diferencia)) #1.79
confusionMatrix(factor(predichos,levels=levels(as.factor(Datos$Diferencia))), #predicho
                factor(Datos$Diferencia,levels=levels(as.factor(Datos$Diferencia)))) #real
##LLegamos a una precisión 0.6247% con todas las variables...

#Importancia variables
xgb.importance(model = final.model) %>% xgb.plot.importance()
xgb.importance(model = final.model) #lista completa


#Predecir Dif----------------
evaluar <- read_excel("C:/Users/Administrador/Desktop/Rush Jhon.xlsm",sheet = "Resultados")
evaluar=evaluar[i:nrow(evaluar),-c(1,3,7,8,9,31,45)] #31 y 45 local y visitante. El ressto son variables aun incompletas de datos...
evaluar$Diferencia <-as.integer(as.factor(evaluar$Diferencia))-1
evaluar$PaisLocal=as.numeric(as.factor(evaluar$PaisLocal))
evaluar$EstiloLocal=as.numeric(as.factor(evaluar$EstiloLocal))
evaluar$PaisVisitante=as.numeric(as.factor(evaluar$PaisVisitante))
evaluar$EstiloVisitante=as.numeric(as.factor(evaluar$EstiloVisitante))
evaluar$Competición=as.numeric(as.factor(evaluar$Competición))

predicciones=predict(final.model, as.matrix(evaluar[,-1]))-3
predicciones
library(xlsx)
predicciones<-as.vector(predicciones)
setwd("C:/Users/Administrador/Desktop")
write.xlsx(predicciones, "Rush Jhon.xlsm", append = TRUE, sheetName = "XGBoost Dif")



#Optimización Goles------------
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

Datos$Goles <- as.factor(Datos$Goles)
num_class = length(levels(as.factor(Datos$Goles)))
  
tsk = makeClassifTask("Xgboost Opt", data = Datos, target = "Goles")
tsk = createDummyFeatures(tsk)
lrn = makeLearner("classif.xgboost", nthread = 4)
lrn$par.vals = list(
    nrounds             = 119,
    objective           = "multi:softmax",
    verbose=0,
    eval_metric = "mlogloss"
)
  
res = makeResampleDesc("CV", iters = 3)
par = makeParamSet(
    makeNumericParam("eta",                    lower = 0.03, upper = 0.3),
    makeNumericParam("gamma",                  lower = 0.5,     upper = 5),
    makeIntegerParam("max_depth",              lower= 5,      upper = 20),
    makeIntegerParam("min_child_weight",       lower= 1,    upper = 10),
    makeNumericParam("subsample",              lower = 0.20,  upper = 0.8),
    makeNumericParam("colsample_bytree",       lower = 0.30,  upper = 0.99)
  )
  
mbo.ctrl = makeMBOControl()
mbo.ctrl = setMBOControlInfill(mbo.ctrl, crit = crit.ei)
mbo.ctrl = setMBOControlTermination(mbo.ctrl, max.evals = 20L)
  
design.mat = generateRandomDesign(n = 10, par.set = par)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)
  
tune.pars = tuneParams(learner = lrn, task = tsk, resampling = res,
                         par.set = par, control = ctrl)
(tune.pars$mbo.result$x)


best.params=tune.pars$mbo.result$x
best.params$booster <- "gbtree"
best.params$objective <- "multi:softmax"
best.params$eval_metric <- "mlogloss"
  
Datos$Goles <-as.integer(as.factor(Datos$Goles))-1
Datos <- 
  Datos %>%  
  select(-Goles) %>%  
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = Datos$Goles)
  
optimal.cv <- xgb.cv(params = best.params,
                       data = Datos,
                       nrounds = 299,
                       nthread = 4,
                       nfold = 3,
                       prediction = FALSE,
                       showsd = TRUE,
                       early_stopping_rounds = 50,
                       verbose = 0,
                       num_class=num_class) 
  
repeticiones <- optimal.cv$best_ntreelimit
repeticiones #8?

final.model <- xgboost(params = best.params, 
                         data = Datos,
                         nrounds = repeticiones,
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
predichos

RMSE(predichos, as.numeric(Datos$Goles)) #1.79
MAPE(predichos, as.numeric(Datos$Goles)) #0.46
a=confusionMatrix(factor(predichos,levels=levels(as.factor(Datos$Goles))), #predicho
                factor(Datos$Goles,levels=levels(as.factor(Datos$Goles)))) #real
a
sum(a$table[lower.tri(a$table)])/sum(a$table)
##LLegamos a una precisión 0.6247% con todas las variables...

#Importancia variables
xgb.importance(model = final.model) %>% xgb.plot.importance()



datos <- read.csv("Dane_Paramodelos_regresion.csv", header = T)

library(tidyverse)
library(caret)

names(datos)
datos <- datos %>% select(-c(1:4,15))

set.seed(1)
set_rows <- sample(nrow(datos),nrow(datos)*0.85, replace=FALSE, prob=NULL )
data_train <- datos[set_rows,]
data_test <- datos[-(set_rows),]

# Fit our regression model
modelo_prueba <- lm(Ingresos_total ~ ., data=datos) 
summary(modelo_prueba) 
anova(modelo_prueba)


#-----------------------

predicciones <- predict(modelo_prueba, data_test)
observado <- data_test$Ingresos_total

error_cuadrado_individual <- (observado-predicciones)^2 
dataframe <- data.frame(observado, predicciones, error_cuadrado_individual)


RMSE <- sqrt(sum(error_cuadrado_individual)/length(predicciones))
RMSE

coeficientes <- data.frame(modelo_prueba$coefficients)
view(coeficientes)




#------------- Now using Caret

library(randomForest)
library(elasticnet)
library(neuralnet)
library(KRLS)
library(kernlab)
library(earth)
library(mboost)
library(rpart)

datos <- read.csv("Dane_Paramodelos_regresion.csv", header = T)
data_valle <- datos %>% filter(Departamento==76) %>% select(-c(1:4,15))


set.seed(1)
set_rows <- sample(nrow(data_valle),nrow(data_valle)*0.85, replace=FALSE, prob=NULL )
data_train <- data_valle[set_rows,]
data_test <- data_valle[-(set_rows),]



trainControl <- trainControl(method="repeatedcv", number=5, repeats=2, search = "grid")
seed <- 1

# Linear Regression
set.seed(seed)
fit.lm<- train(Ingresos_total~., data=data_train, method="lm", metric="RMSE" ,trControl=trainControl)
# GLMNET
set.seed(seed)
fit.glmnet<- train(Ingresos_total~., data=data_train, method="glmnet", metric="RMSE" ,trControl=trainControl)
# SVM Radial
set.seed(seed)
fit.svmRadial<- train(Ingresos_total~., data=data_train, method="svmRadial", metric="RMSE" ,trControl=trainControl)
# kNN
set.seed(seed)
fit.knn <- train(Ingresos_total~., data=data_train, method="knn",metric="RMSE",trControl=trainControl)
# CART
set.seed(seed)
fit.CART <- train(Ingresos_total~., data=data_train, method="rpart2", metric="RMSE", trControl=trainControl)
# Bagged CART
set.seed(seed)
fit.treebag <- train(Ingresos_total~., data=data_train, method="treebag", metric="RMSE", trControl=trainControl)
# Random Forest
set.seed(seed)
fit.rf <- train(Ingresos_total~., data=data_train, method="rf",metric="RMSE", trControl=trainControl)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(Ingresos_total~., data=data_train, method="gbm", metric="RMSE", trControl=trainControl, verbose=FALSE)
# Radial Basis Function Kernel Regularized Least Squares
set.seed(seed)
fit.krlsRadial <- train(Ingresos_total~., data=data_train, method="krlsRadial", metric="RMSE", trControl=trainControl)
# Bagged MARS(Multivariate adaptive regression spline)
set.seed(seed)
fit.bagEarth <- train(Ingresos_total~., data=data_train, method="bagEarth", metric="RMSE", trControl=trainControl)
# eXtreme Gradient Boosting
set.seed(seed)
fit.Dart <- train(Ingresos_total~., data=data_train, method="xgbDART", metric="RMSE", trControl=trainControl)


# COMPARISON
results <- resamples(list(lm=fit.lm, glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn, CART=fit.CART, bagging=fit.treebag,
                          rf=fit.rf, gbm=fit.gbm, regularized=fit.krlsRadial))

#, , MARS=fit.bagEarth, DART=fit.Dart

#Table comparison
summary(results)


# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)




#### Comparación de predicciones

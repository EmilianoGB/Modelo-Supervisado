#Librerias
library(rio)
library(dplyr)
library(tidyverse)
library(lubridate)
library(smbinning)
library(DT)
library(ggplot2)
library(fastDummies)
library(gbm)
library(DT)
library(gridExtra)
library(DataExplorer)
library(pander)
library(caret)
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)


#Importacion de datos
#setwd("C:/Users/emili/Desktop/ORT Analitica/Modelos Supervisados/Obligatorio")
#datos<- read.csv('CreditScoring.csv', stringsAsFactors = TRUE, encoding = "Latin-1")

#Directorios - Javier
base.dir <- "C:/Users/emili/Desktop/ORT Analitica/Modelos Supervisados/Obligatorio"

# data.dir <- file.path(base.dir, "datos")
# report.dir <- file.path(base.dir, "reportes")
# graf.dir <- file.path(base.dir, "graficos")
# models.dir <- file.path(base.dir, "modelos")
#  
setwd(base.dir)

#Importacion de datos

datos<- import('CreditScoring.csv', stringsAsFactors = TRUE, encoding = "Latin-1")

#Verificacion de los datos incluidos
dim(datos)


# Funcion Tbl. Función que genera una tabla con proporciones para utilizar en el análisis.
tblFun <- function(x){
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
  res
}

# Exploración Inicial
# Estructura de los datos y estadísticas descriptivas.
head(datos)
str(datos)
summary(datos[,-1])

# Amálisis Univariado de Variable indpeneidente "Default".
tblFun(datos$Default)
ggplot(datos, aes(Default))+geom_bar(aes(fill=Default))+
  scale_fill_manual(values=c("gree n4", "red"))+ggtitle("Proporcion Default") 

# Análsis vibariado de las variables del dataset contra la variable "Default".
datos %>% ggplot(aes(Edad))+geom_bar(aes(fill=Default), position = "fill")+
  scale_fill_manual(values=c("gree n4", "red"))

datos %>% ggplot(aes(Sexo))+geom_bar(aes(fill=Default), position = "fill")+
  scale_fill_manual(values=c("gree n4", "red"))

datos %>% ggplot(aes(NED))+geom_bar(aes(fill=Default), position = "fill")+
  scale_fill_manual(values=c("gree n4", "red"))

datos %>% ggplot(aes(NumBancos))+geom_bar(aes(fill=Default), position = "fill")+
  scale_fill_manual(values=c("gree n4", "red"))

datos %>% ggplot(aes(NumFinanc))+geom_bar(aes(fill=Default), position = "fill")+
  scale_fill_manual(values=c("gree n4", "red"))

datos %>% ggplot(aes(NED))+geom_bar(aes(fill=Default), position = "fill")+
  scale_fill_manual(values=c("gree n4", "red"))

datos %>% ggplot(aes(C6M))+geom_bar(aes(fill=Default), position = "fill")+
  scale_fill_manual(values=c("gree n4", "red"))

plot_boxplot(datos[,-c(1,5,6)], by='Default', nrow = 3L, ncol = 2L)

# Creación de Variables
#BINARIZACIÓN DEFAULT
BinAux <- dummy_cols(.data = datos, select_columns = "Default")
datos <- mutate(datos, Default = BinAux$Default_M)

# C6M
datos <- datos %>% mutate(Rangos_C6M = ifelse(C6M == '1A'|C6M == '1C','AL_DIA',
                                              ifelse(C6M == '2A'|C6M == '2B'|C6M == '3','ATR_MODERADO',
                                                     ifelse(C6M == '4'|C6M == '5','ATR_ALTO','N/A'))))

datos$Rangos_C6M <- factor(datos$Rangos_C6M, order = TRUE, 
                           levels = c("AL_DIA", "ATR_MODERADO", "ATR_ALTO"))
# EDAD
datos <- datos %>% mutate(Rangos_EDAD = ifelse(Edad < 25,'MENOR_25',
                                               ifelse(Edad >= 25 & Edad < 35,'ENTRE_25_34',
                                                      ifelse(Edad >= 35 & Edad < 45,'ENTRE_35_44',
                                                             ifelse(Edad >= 45 & Edad < 55,'ENTRE_45_54',
                                                                    ifelse(Edad >= 55,'MAYOR_55','N/A'))))))
datos$Rangos_EDAD <- factor(datos$Rangos_EDAD, order = TRUE, 
                            levels = c("MENOR_25", "ENTRE_25_34", "ENTRE_35_44", "ENTRE_45_54", "MAYOR_55"))
# NUMBANCOS
datos <- datos %>% mutate(SOLO_BANCOS = ifelse(NumBancos > 0 & NumFinanc == 0,1,0))
# NUMBFINANC
datos <- datos %>% mutate(SOLO_FINANC = ifelse(NumFinanc > 0 & NumBancos == 0,1,0))
# CONT / INGRESO
datos <- datos %>% mutate(CONTsobreINGRESO = Cont/Ingreso)
# COtización Dic 2019 - 1 USD - $38
# VIGEN_PESOS
datos <- datos %>% mutate(VIGEN_PESOS = Vigen*38)
# CONT / VIGEN_PESOS
datos <- datos %>% mutate(CONTsobreVIGEN_PESOS = Cont/VIGEN_PESOS)

datos$PI <- as.factor(datos$PI)
datos$Rangos_C6M <- as.factor(datos$Rangos_C6M)
datos$Rangos_EDAD <- as.factor(datos$Rangos_EDAD)
datos$SOLO_BANCOS <- as.factor(datos$SOLO_BANCOS)
datos$SOLO_FINANC <- as.factor(datos$SOLO_FINANC)

# Importancia de las variables

correlaciones <- sort(sapply(datos[,-c(1:4,8,10,14:18)], cor, y=datos$Default), decreasing = TRUE)

sort_abs <- function(x, na.last = TRUE, decreasing = TRUE) {
  x[order(abs(x), na.last = na.last, decreasing = decreasing)] 
}
correlacionorden <- sort_abs(correlaciones)

# Modelo Boosting - Influencia Relativa
set.seed(612)
boost <- gbm(Default ~ ., data = datos[,-c(1,2,8)] , distribution = "bernoulli", n.trees = 100, 
             shrinkage = 0.1, interaction.depth = 1, bag.fraction = 0.5, train.fraction = 0.5,
             n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE)

# Estimacion de Modelos

# Partición de la muestra en train y test
n <- nrow(datos)
train.x <- datos[1:(n*0.8),-c(1,2,8,14)]
train.y <- datos$Default[1:(n*0.8)]
test.x <- datos[((n*0.8)+1):n,-c(1,2,8,14)]
test.y <- datos$Default[((n*0.8)+1):n]
df.train <- data.frame ("y" = train.y, train.x)
df.train$y<- as.factor(df.train$y)

#Árboles de Decision
modelo_dt <- rpart(y ~ . ,data = df.train, method = "class", minbucket = 10 , maxdepth = 6)
y_hat_dt <- ifelse (predict(modelo_dt , as.data.frame(test.x))[,2]>0.5,1,0)
tabla_dt <- table(y_hat_dt, test.y)
er_dt <- 1- sum(diag(tabla_dt))/sum(tabla_dt)

#Random Forest

mod_rf <- randomForest( y  ~ . , data = df.train, mtry = 6, ntree = 400, importance = TRUE)
y_hat_rf <- predict(mod_rf, test.x)
tabla_rf <- table(y_hat_rf, test.y)
er_rf <- 1- sum(diag(tabla_rf))/sum(tabla_rf)
importanceOrdenRF <- sort_abs(mod_rf$importance[,4])

#Boosting
grid <- expand.grid(n.trees = 400, interaction.depth= 1, 
                    shrinkage=0.1, n.minobsinnode=10)
ctrl <- trainControl(method = "cv",number = 5)

unwantedoutput <- capture.output(mod_GBM <- train(y~.,data = df.train, distribution="bernoulli",
                                                  method = "gbm", trControl = ctrl, tuneGrid = grid))
conf<- confusionMatrix(mod_GBM)

#Random Forest óptimo

mod_rf_op <- randomForest( y  ~ . , data = df.train, mtry = 7, ntree = 400, importance = TRUE)
y_hat_rf_op <- predict(mod_rf_op, test.x)
tabla_rf_op <- table(y_hat_rf_op, test.y)
er_rf_op <- 1- sum(diag(tabla_rf_op))/sum(tabla_rf_op)

#Boosting Optimo
gridopt <- expand.grid(n.trees = 400, interaction.depth= 3, 
                       shrinkage=0.1, n.minobsinnode=10)
ctrlopt <- trainControl(method = "cv",number = 5)

unwantedoutputopt <- capture.output(mod_GBMopt <- train(y~.,data = df.train, distribution="bernoulli",
                                                        method = "gbm", trControl = ctrlopt, tuneGrid = gridopt))

y_hat_boost_op <- predict(mod_GBMopt, test.x)
tabla_boost_op <- table(y_hat_boost_op, test.y)
er_boost_op <- 1- sum(diag(tabla_boost_op))/sum(tabla_boost_op)
###########################################################
# Competición: 
# Etapa: Primer modelo
# Característica: RandomForest - RANGER
# Resultado: 80.33
# Fecha: 11-06-2019
###########################################################

setwd("C:\\Users/n230104/Desktop/personal/master/BD_empresa/") # Work
setwd("~/Desktop/Santi/master_BD/empresa/BD_empresa/") # Home
setwd("~/Escritorio/master_BD/empresa/entrega/BD_empresa/") # Laptop
setwd("/cloud/project/entrega/") # Cloud (fill it)

# 0. Libraries
library(data.table)
library(dplyr)
library(ranger)
library(ggplot2)

# 1. Load data ----
datZero<- readRDS("./dat/datZero")
datY<- as.data.frame(fread("./dat/rawTrain_y.csv"))
submiss<- as.data.frame(fread("./dat/SubmissionFormat.csv")) 
datTrain<- inner_join(datZero, datY)
datTest<- datZero[datZero$id %in% submiss$id,]
datTrain$status_group<- as.factor(datTrain$status_group)

# 2. Model ----
frml<- as.formula(paste("status_group ~", paste(names(datZero), collapse="+")))

set.seed(1234)
my_mod<- ranger(frml,
                data = datTrain,
                importance = "impurity")

# 2.1 Model Metrics ---- 
# Importancia de las variables
aux<- as.data.frame(my_mod$variable.importance)
aux$id<- rownames(aux)
rownames(aux)<- seq(1, nrow(aux))
aux$importance<- aux[,1]
aux[,1]<- NULL
ggplot(data=aux, aes(x=id, y=importance))+
    geom_bar(stat="identity", color="black", fill="lightblue2", show.legend = FALSE, alpha=0.5) +
    scale_x_discrete(limits=aux$id[order(aux$importance)]) +
    coord_flip() + ggtitle("Importancia de las variables")
# Matriz de confusión
aux<- as.data.frame(my_mod$confusion.matrix)
ggplot(data=aux, aes(x=true, y=Freq, fill=predicted))+
    geom_bar(stat="identity", color="black", show.legend = TRUE, alpha=0.5) +
    scale_x_discrete(limits=unique(aux$predicted[order(aux$Freq)])) +
    coord_flip() + ggtitle("Matriz de confusión")
# Error en la predicción
my_mod$prediction.error

# Save the model
saveRDS(my_mod, "./mod/V02_ranger")

# 3. Prediction ----
mod<- readRDS("./mod/V02_ranger")
predicted<- predict(mod, datTest)
pred<- data.frame(id=datTest$id,
                  status_group = predicted$predictions)
# Write submission
fwrite(pred, "./sub/V02_ranger_base.csv")

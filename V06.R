###########################################################
# Competición: 
# Etapa: Primer modelo
# Característica: RandomForest - RANGER
# Resultado: 0.8222 
# Fecha: 12-06-2019
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
library(ggmosaic)
library(gridExtra)
# library(klaR)
library(caret)
source("funciones.R")

# 1. Load data ----
datTotal<- readRDS("./dat/datTotal_3")
datY<- as.data.frame(fread("./dat/rawTrain_y.csv"))
datY_1<- data.frame(id = datY$id,
                   y = ifelse(datY$status_group == "non functional", "non functional", "other"))
datY_2<- data.frame(id = datY_1$id[which(datY_1$y == "other")],
                   y = ifelse(datY$status_group[which(datY_1$y == "other")] == "functional",
                              "functional", "functional needs repair"))
submiss<- as.data.frame(fread("./dat/SubmissionFormat.csv"))
datTrain_1<- inner_join(datTotal, datY_1)
datTrain_2<- inner_join(datTotal, datY_2)
datTrain_1$y<- as.factor(datTrain_1$y)
datTrain_2$y<- as.factor(datTrain_2$y)
datTest<- datTotal[datTotal$id %in% submiss$id,]

# 2. Models ----
# control<-trainControl(method = "LGOCV", p=0.8, number=5, savePredictions = "all",
#                       summaryFunction=twoClassSummary, classProbs=TRUE, returnResamp="all",
#                       verboseIter=FALSE)
# datTrain_1$y<- make.names(datTrain_1$y)
# mod_1<-train(y~., data = datTrain_1,
#              method="nb",
#              metric="ROC",
#              trControl = control
# )
# mod_info<- as.data.frame(mod_1$resample)
# mod_info<- drop_na(mod_info)
# saveRDS(model, paste("./mod/", "bayes", sep=""))

frml<- as.formula(paste("y ~", paste(names(datTotal), collapse="+")))
set.seed(1234)
# 2.1 First model ----
my_mod_1<- ranger(frml,
                  data = datTrain_1,
                  importance = "impurity")

# Importancia de las variables
aux<- as.data.frame(my_mod_1$variable.importance)
aux$id<- rownames(aux)
rownames(aux)<- seq(1, nrow(aux))
aux$importance<- aux[,1]
aux[,1]<- NULL
impo<- ggplot(data=aux, aes(x=id, y=importance))+
    geom_bar(stat="identity", color="black", fill="lightblue2", show.legend = FALSE, alpha=0.5) +
    scale_x_discrete(limits=aux$id[order(aux$importance)]) +
    coord_flip() +  ggtitle("Importancia de las variables")
# Matriz de confusión
aux<- as.data.frame(my_mod_1$confusion.matrix)
conf<- ggplot(data=aux, aes(x=true, y=Freq, fill=predicted))+
    geom_bar(stat="identity", color="black", show.legend = TRUE, alpha=0.5) +
    scale_x_discrete(limits=unique(aux$predicted[order(aux$Freq)])) +
    theme(legend.position="top") + ggtitle("Matriz de confusión")
grid.arrange(impo, conf, ncol=2, widths=c(1.3, 1))

# Error en la predicción
my_mod_1$prediction.error
# Save the model
saveRDS(my_mod_1, "./mod/V06_ranger_1")

# 2.2 Second model
my_mod_2<- ranger(frml,
                  data = datTrain_2,
                  importance = "impurity")

# Importancia de las variables
aux<- as.data.frame(my_mod_2$variable.importance)
aux$id<- rownames(aux)
rownames(aux)<- seq(1, nrow(aux))
aux$importance<- aux[,1]
aux[,1]<- NULL
impo<- ggplot(data=aux, aes(x=id, y=importance))+
    geom_bar(stat="identity", color="black", fill="lightblue2", show.legend = FALSE, alpha=0.5) +
    scale_x_discrete(limits=aux$id[order(aux$importance)]) +
    coord_flip() +  ggtitle("Importancia de las variables")
# Matriz de confusión
aux<- as.data.frame(my_mod_2$confusion.matrix)
conf<- ggplot(data=aux, aes(x=true, y=Freq, fill=predicted))+
    geom_bar(stat="identity", color="black", show.legend = TRUE, alpha=0.5) +
    scale_x_discrete(limits=unique(aux$predicted[order(aux$Freq)])) +
    theme(legend.position="top") + ggtitle("Matriz de confusión")
grid.arrange(impo, conf, ncol=2, widths=c(1.3, 1))

# Error en la predicción
my_mod_2$prediction.error
# Save the model
saveRDS(my_mod_2, "./mod/V06_ranger_2")

# 3. Prediction ----
mod<- readRDS("./mod/V06_ranger_1")
predicted<- predict(mod, datTest)
pred_1<- data.frame(id=datTest$id,
                    status_group = predicted$predictions)
datTest_2<- datTest[which(pred_1$status_group == "other"),]
mod<- readRDS("./mod/V06_ranger_2")
predicted<- predict(mod, datTest_2)
pred_2<- data.frame(id=datTest_2$id,
                    status_group = predicted$predictions)
aux<- rbind(pred_1[pred_1$status_group == "non functional",], pred_2)
pred<- inner_join(submiss, aux, by="id")[,c(1,3)]
pred$status_group<- pred$status_group.y
pred$status_group.y<- NULL

# Checkeo tontorrón
length(which(datTrain_1$y == "non functional"))/nrow(datTrain_1)
length(which(pred$status_group == "non functional"))/nrow(pred)

# Write submission
fwrite(pred, "./sub/V06_ranger_double.csv")

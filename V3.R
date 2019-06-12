#############################
# version inicial
# fecha: 10/05/2019
# rbjetivo: version base
# resultado: 0.7135
#############################

# 0. Librerias ----
setwd("~/Escritorio/master_BD/empresa/")
library(data.table)
library(tidyverse) # dplyr, ggplot, lubridate, stringr, purr vienen todos aqui
library(caret)
library(glmnet)
library(xgboost)
library(ranger) # random forest pero rápido
library(DataExplorer) # para hacer un primer informe de los datos DA PROBLEMAS
library(SmartEDA)
library(dplyr)
library(ranger)

# # 1. Importacion ----
# datLabel<- as.data.frame(fread("train_set_labels.csv"))
# datTrain<- as.data.frame(fread("train_set.csv"))
# 
# ## 1.1 Junto Labels y Train ----
# head(datLabel[,1]) 
# head(datTrain[,1])
# # tienen el mismo orden pero aún así hago el merge
# datEnd<- merge(datTrain, datLabel, by.x="id", by.y="id", all=T)
# 
# ## 1.2 Checkpoint
# saveRDS(datEnd, "datEnd.rds")
datEnd<- as.data.frame(readRDS("datEnd.rds"))
head(datEnd)

# 2. Exploracion ----
# create_report(datEnd)
# SmartEDA::ExpData(datEnd
as.data.frame(mapply(class, datEnd))
as.data.frame(table(datEnd$basin))
as.data.frame(table(datEnd$subvillage))
## 2.1 Seleccion de variables
# Número de niveles de las variables (luego escogeremos las que tengan una cantidad razonable)
# date_recorded 356
# funder 1898 
# installer 2146 
# wpt_name 37400 
# basin 9 
# subvillage 19288 
# region 21 
# lga 125 
# ward 2092 
# recorded_by 1 
# scheme_management 13 
# scheme_name 2697 
# extraction_type 18 
# extraction_type_group 13 
# extraction_type_class 7 
# management 12 
# management_group 5 
# payment 7 
# payment_type 7 
# water_quality 8 
# quality_group 6 
# quantity 5 
# quantity_group 5 
# source 10 
# source_type 7 
# source_class 3 
# waterpoint_type 7 
# waterpoint_type_group 6 
# status_group 3
# Cogemos las numéricas solo
library(doMC) # para paralelizar
var_tp<- as.data.frame(mapply(class, datEnd))
var_tp$tipo<- var_tp$`mapply(class, datEnd)`
var_tp$`mapply(class, datEnd)`<- NULL
fwrite(var_tp, "datos_desc.csv")
var_num<- row.names(var_tp)[which(var_tp[,1] == "numeric" | var_tp[,1] == "integer")]
var_add<- c("extraction_type", "management", "water_quality",
            "source", "waterpoint_type")
var_catlte15<- c("basin", "scheme_management",
                 "extraction_type_group", "extraction_type_class",
                 "management", "management_group", "payment", "payment_type",
                 "water_quality", "quality_group", "quantity", "quantity_group",
                 "source", "source_type", "source_class", "waterpoint_type",
                 "waterpoint_type_group")
var_nas<- c()
for(col in names(datEnd)){
    aux<- any(is.na(datEnd[[col]]))
    if(aux){
        var_nas<- c(var_nas, col)    
    }
}
#Rbodis
var_mod<- c(var_num, var_catlte15)
var_mod<- var_mod[!(var_mod %in% var_nas)]
# 3. Modelo ----
frml<- as.formula(paste("status_group ~", paste(var_mod, collapse="+")))
trainData<- datEnd[,c("status_group", var_mod)]
trainClass<- datEnd$status_group
trainData$status_group<- ifelse(trainData$status_group=="functional",2,
                                ifelse(trainData$status_group=="non functional",0,1))
trainData$status_group<- as.factor(trainData$status_group)
registerDoMC(4)
set.seed(123)
control<-trainControl(method = "LGOCV",
                      p=0.8,number=1,savePredictions = "all") 
nnetgrid <- expand.grid(size=c(5), decay=c(0.04))
rednnet<- train(frml,
                data=trainData,
                method="xgboost", linout = TRUE, maxit=150,
                trControl=control, tuneGrid=nnetgrid)
test<- as.data.frame(fread("test_set.csv"))
pred_test<- as.vector(predict(rednnet, test))
pred_test<- ifelse(pred_test==2,"functional",
                   ifelse(pred_test==0,"non functional","functional needs repair"))
my_sub<- data.frame(
    id = test$id,
    status_group = pred_test
)
fwrite(my_sub, "sub_V3_base.csv", sep=",")

SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10),
                     degree=c(2,3),scale=c(0.1,0.5,1,2,5))
control<-trainControl(method = "LGOCV",
                      p=0.8,number=1,savePredictions = "all")
svm<- train(frml,
            data=trainData,
            method="svmPoly",trControl=control,
            tuneGrid=SVMgrid, verbose=TRUE)

install.packages("dataPreparation")
install.packages("vtreat")


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
library(dplyr)
library(glmnet)
library(xgboost)
library(ranger) # random forest pero rápido
library(DataExplorer) # para hacer un primer informe de los datos DA PROBLEMAS
library(SmartEDA)

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
datEnd_sample <- datEnd[sample(1:nrow(datEnd), 0.15*nrow(datEnd),
                               replace=FALSE),]
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
aux_f<- function(x){length(unique(datEnd[!is.na(datEnd[,x]),x]))}
var_tp$dist_count<- as.data.frame(mapply(aux_f,names(datEnd)))[,1]
fwrite(var_tp, "datos_desc.csv")

# Group variables ----
var_inp<- names(datEnd)[names(datEnd) != "status_group"]
var_num<- row.names(var_tp)[which(var_tp[,1] == "numeric" | var_tp[,1] == "integer")]
var_fac<- row.names(var_tp)[!(row.names(var_tp) %in% var_num)]
var_catlte25<- rownames(var_tp)[which(var_tp[,2] <= 25 & !(rownames(var_tp) %in% var_num))]
var_catgt25<- rownames(var_tp)[which(var_tp[,2] > 25 & !(rownames(var_tp) %in% var_num))]
var_nas<- c()
for(col in names(datEnd)){
    aux<- any(is.na(datEnd[[col]]))
    if(aux){
        var_nas<- c(var_nas, col)    
    }
}
var_shit<- c("recorded_by")

var_use<- var_inp[!(var_inp %in% var_shit)]
#Rbodis
# Data preparation ----
# library("vtreat")
# treatmentsZ = designTreatmentsZ(datEnd, c(var_use,"status_group"),
#                                 rareCount=0)
# trainTreated<- prepare(treatmentsZ, datEnd)
# testTreated<- prepare(treatmentsZ, test)
# set.seed(1234)

# Aggregation ----
test<- as.data.frame(fread("test_set.csv"))
var_y<- c("status_group")

datInput<- na.omit(datEnd[, var_use])
datJoin<- rbind(datInput, test[,var_use])
# NAs pctge
datJoin<- na.omit(datJoin)
datJoin$miss_pctge<- rowMeans(is.na(datJoin))

# factors for all categoric
for(col in var_use[(var_use %in% var_fac)]){
    datJoin[[col]]<- as.factor(datJoin[[col]])    
}
# Scale numeric variables
datJoin[,var_num[(var_num!="id")]]<- scale(datJoin[,var_num[(var_num!="id")]])

# Sust. over 15 categories with factors
for (var in var_catgt25){
    aux<- datJoin %>%
        select(!!sym(var)) %>%
        group_by(!!sym(var)) %>%
        summarize(tmp = n()) %>%
        right_join(datJoin)
    
    datJoin[[paste(var,"_freq",sep="")]]<- aux$tmp
}
datJoin$year_recorded<- as.factor(substring(datJoin$date_recorded, 1,4))
datJoin$month_recorded<- as.factor(substring(datJoin$date_recorded, 6,7))

# Fit var_y (3 categories)
y<- datEnd[(datEnd$id %in% datInput$id),var_y]
y_better<- ifelse(y=="functional",1,
                  ifelse(y=="non functional", 0, 2))

# Selected vars
var_add<- names(datJoin)[!(names(datJoin) %in% names(datEnd))]
var_sel<- names(datJoin)[names(datJoin) %in% c(var_num, var_catlte25, var_add)]
fwrite(datJoin, "datJoin.csv")

# Train - Test split
datTrain<- datJoin[seq(1, nrow(datInput)), var_sel]
datTrain$y<- as.factor(make.names(y_better))
datTest<- datJoin[seq(nrow(datInput)+1,nrow(datJoin)),var_sel]

# 3. Modelo ----
frml<- as.formula(paste("y~", paste(var_sel, collapse="+")))
registerDoMC(4)
set.seed(123)
control<-trainControl(method = "LGOCV",
                      p=0.8, number=1, savePredictions = "all",
                      classProbs=TRUE)

# 3.4 XGBoost ----
library(xgboost)

sparse_matrix <- sparse.model.matrix(frml, data = datTrain)
xgbm <- xgboost(data = data.matrix(datTrain[,-ncol(datTrain)]), 
                label = y,
                eta = 0.1,
                max_depth = 15, 
                nround=25,
                subsample = 0.5,
                colsample_bytree = 0.5,
                seed = 1,
                eval_metric = "merror",
                objective = "multi:softprob",
                num_class = 12,
                nthread = 3
)
mod<- xgbm

# 3.6 Ranger ----
rgr<- ranger(frml, datTrain,
             num.trees = 80, importance="impurity",
             write.forest = T, splitrule = "extratrees",
             num.random.splits = 10)
mod<- rgr

# 4. Submission ----
pred_train<- mod$pred$pred
pred_train<- as.vector(predict(mod, datTrain)$predictions)
length(which(pred_train==datTrain$y))/nrow(datTrain)

datTest_cut<- na.omit(datTest)
pred_test<- as.vector(predict(mod, datTest_cut)$predictions)
pred_test<- ifelse(pred_test=="X1","functional",
                   ifelse(pred_test=="X0","non functional","functional needs repair"))
my_sub<- data.frame(
    id = datTest_cut$id,
    status_group = pred_test
)
old_sub<- as.data.frame(fread("sub_V8_base.csv"))
sub<- old_sub
aux<- right_join(my_sub, old_sub, by=c("id"))
sub$status_group<- aux$status_group.x
sub$status_group[is.na(aux$status_group.x)]<- aux$status_group.y[is.na(aux$status_group.x)]
length(which(sub$status_group==old_sub$status_group))/nrow(test)

head(my_sub)
fwrite(sub, "sub_V9_base.csv", sep=",")
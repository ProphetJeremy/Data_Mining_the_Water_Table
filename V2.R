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
library(glmnet)
library(ranger) # random forest pero rápido
library(DataExplorer) # para hacer un primer informe de los datos DA PROBLEMAS

# 
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
## 2.1 Estudio de variables con dplyr ----
# verbos:
#     select
#     filter
#     group_by
#     summarize
#     arrange
#     mutate

# Use variables as strings
mycol<- "status_group"
datTrain %>%
    select(!!sym(mycol)) %>%
    group_by(!!sym(mycol)) %>%
        summarize(f_pump = n()) %>%
    ggplot(aes(!!sym(mycol), f_pump)) + 
        geom_col(fill='darkolivegreen')

# Como se haría lo mismo con data.table
datDT<- as.data.table(datEnd)
datDT[,.(f_pump= .N), by=c("status_group")]

# Cuantos niveles tienen las variables categóricas
length(unique(datEnd$funder)) # funder = 1898
for(i in seq(1, ncol(datEnd))){
    tmp_class<- class(datEnd[,i])
    if(tmp_class == "character"){
        tmp_lg<- length(unique(datEnd[,i]))
        cat(names(datEnd)[i], tmp_lg, "\n")
    }
}
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

# 3. Modelo ----
# Cogemos las numéricas solo
var_tp<- as.data.frame(mapply(class, datEnd))
var_num<- dput(row.names(var_tp)[which(var_tp$tipo == "numeric" | var_tp$tipo == "integer")])

# Modelo de árboles con ranger
library(ranger)
library(caret)
datEnd$status_group<- as.factor(datEnd$status_group)
frml<- as.formula(paste("status_group ~", paste(var_num, collapse="+")))
my_mod<- ranger(frml,
                data = datEnd,
                importance = "impurity")
pred_mod<- predict(my_mod, datEnd)
importance(my_mod)
confusionMatrix(pred_mod$predictions, datEnd$status_group)
saveRDS(my_mod, "modV2.rds")
my_mod<- readRDS("modV2.rds")

test<- as.data.frame(fread("test_set.csv"))
pred_test<- as.vector(predict(my_mod, test)$predictions)
my_sub<- data.frame(
    id = test$id,
    status_group = pred_test
)
fwrite(my_sub, "sub_V2_base.csv", sep=",")

###########################################################
# Competición: 
# Etapa: Tratamiento de datos
# Característica: Primer análisis
# Resultado: 0.7802
# Fecha: 10-06-2019
###########################################################

setwd("C:\\Users/n230104/Desktop/personal/master/BD_empresa/") # Work
setwd("~/Desktop/Santi/master_BD/empresa/BD_empresa/") # Home
setwd("~/Escritorio/master_BD/empresa/entrega/BD_empresa/") # Laptop
setwd("/cloud/project/entrega/") # Cloud (fill it)

# 0. Libraries
library(data.table)
library(h2o)
library(dplyr)

# 1. Load data ----
datZero<- readRDS("./dat/datZero")
datY<- as.data.frame(fread("./dat/rawTrain_y.csv"))
submiss<- as.data.frame(fread("./dat/SubmissionFormat.csv")) 
datTrain<- inner_join(datZero, datY)
datTest<- datZero[datZero$id %in% submiss$id,]
str(datTrain)
datTrain$status_group<- as.factor(datTrain$status_group)

# 2. Modelo ----
h2o.init(nthreads=-1)
train<- as.h2o(datTrain, destination_frame = "datTrain.hex")
y<- "status_group"
x<- names(datZero)

aml<- h2o.automl(x=x, y=y,
                 training_frame = train,
                 max_runtime_secs = 10,
                 stopping_metric = "missclassification",
                 nfolds = 5,
                 exclude_algos = c('DeepLearning', 'StackedEnsemble'),
                 seed = 1)

lb<- aml@leaderboard
print(lb, n=nrow(lb))
# Save the model
saveRDS(aml, "./mod/h2o_base")

# 3. Prediction ----
mod<- readRDS("./mod/h2o_base")
test<- as.h2o(datTest, destination_frame = "datTest.hex")
predicted<- as.data.frame(h2o.predict(mod@leader, test))
pred<- data.frame(id=datTest$id,
                  status_group = predicted$predict)
# Write submission
fwrite(pred, "./sub/V01_h2o_base.csv")

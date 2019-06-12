###########################################################
# Competición: 
# Etapa: Tratamiento de datos
# Característica: Primer análisis
# Resultado: -
# Posición: 
# Percentil: 
# Fecha: 10-06-2019
###########################################################

setwd("C:\\Users/n230104/Desktop/personal/master/BD_empresa/") # Work
setwd("~/Desktop/Santi/master_BD/empresa/BD_empresa/") # Home (fill it)
setwd("/cloud/project/entrega/") # Cloud (fill it)

# 0. Libraries
library(data.table)
library(psych)
library(dplyr)

# 1. Load data ----
datTrain<- as.data.frame(fread("./dat/rawTrain.csv"))
datTest<- as.data.frame(fread("./dat/rawTest.csv"))
datTotal<- rbind(datTrain, datTest)
# Data dimension
dim(datTrain)
dim(datTest)
# Objective variable
datY<- as.data.frame(fread("./dat/rawTrain_y.csv"))

# 1.1 Data analysis ----
# Variable class
varDescription<- as.data.frame(sapply(datTotal, class))
colnames(varDescription)<- "clase"
varDescription$name<- row.names(varDescription)
row.names(varDescription)<- seq(1, nrow(varDescription))
varDescription$n_NANs<- sapply(datTotal, function(x) sum(is.na(x)))
varDescription<- varDescription[,c(2,1,3)]

# 1.1.1 Categoric variables ----
datTotal_cha<- datTotal[, varDescription$clase == 'character']
datTotal_cha<- as.data.frame(sapply(datTotal_cha, as.factor))
varFac<- names(datTotal_cha)
aux<- as.data.frame(t(datTotal_cha %>%
                          summarise_all(funs(n_distinct(.)))))
aux$name<- row.names(aux)
varDescription$n_CATs<- left_join(varDescription, aux)$V1

# 1.1.2 Numeric variables ----
datTotal_num<- datTotal[, varDescription$clase %in% c('numeric', 'integer')]
datTotal_num<- as.data.frame(sapply(datTotal_num, as.numeric))
varNum<- names(datTotal_num)
aux<- as.data.frame(psych::describe(datTotal_num))
aux$vars<- NULL
aux$n<- NULL
aux$name<- row.names(aux)
varDescription<- left_join(varDescription, aux)

# 1.1.3 Logical variables
datTotal_boo<- datTotal[, varDescription$clase %in% 'logical']
varBoo<- names(datTotal_boo)
# Stupid but useful columns
varDescription$is_Fac<- varDescription$name %in% varFac
varDescription$is_Num<- varDescription$name %in% varNum
varDescription$is_Boo<- varDescription$name %in% varBoo
varResume<- varDescription[order(varDescription$n_NANs, varDescription$clase),
                           c(1,2,3,4,5,7,10,11,6)]
treat<- c("amount_tsh", "date_recorded", "funder",
          "installer", "wpt_name", "subvillage",
          "region_code", "district_code",
          "lga", "ward", "public_meeting", "recorded_by", 
          "scheme_name", "permit", "construction_year",
          "payment", "payment_type")
varResume$Tratamiento<- if_else(varResume$name %in% treat, "SI", "NO")
saveRDS(varResume, "./dat/varResume")
aux<- varResume[,c(2:ncol(varResume))]
rownames(aux)<- varResume$name

# 1.2 Save tipified data ----
datTotal<- cbind(datTotal_cha, datTotal_num, datTotal_boo)
datTotal$recorded_by<- NULL
saveRDS(datTotal, "./dat/datTotal")
saveRDS(cbind(datY, datTotal[c(1:nrow(datTrain)),]), "./dat/datTrain")
saveRDS(datTotal[c((nrow(datTrain)+1):nrow(datTotal)),], "./dat/datTest")

# 1.3 Limpieza alto nivel ---
datZero<- datTotal[,varResume$name[varResume$Tratamiento=="NO"]]
saveRDS(datZero, "./dat/datZero")


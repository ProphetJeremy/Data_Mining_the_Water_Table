###########################################################
# Competición: 
# Etapa: Primer modelo
# Característica: RandomForest - RANGER
# Resultado: 0.8189
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
library(gridExtra)
source("funciones.R")

# 1. Load data ----
datTotal<- readRDS("./dat/datTotal")
datY<- as.data.frame(fread("./dat/rawTrain_y.csv"))
submiss<- as.data.frame(fread("./dat/SubmissionFormat.csv")) 
datTrain<- inner_join(datTotal, datY)
datTest<- datTotal[datTotal$id %in% submiss$id,]
datTrain$status_group<- as.factor(datTrain$status_group)

# 1.1 Solve our questions ----
# Have payment and pyment_tupe the same info?
aux<- as.data.frame(datTotal %>%
                  group_by(payment_type) %>% 
                  summarise(count = n_distinct(payment))
)
aux
datTotal$payment<- NULL

# How is construction_year distributed?
length(unique(datTotal$construction_year)) # SOlo 55
length(datTotal$construction_year[datTotal$construction_year == 0]) # 1/3 son 0
myDistPlot(datTrain, "construction_year", "status_group", "construction_year - Dist")
datTotal$const_year_bin<- as.factor(ifelse(datTotal$construction_year ==0, 0,
                                           ifelse(datTotal$construction_year<1800, 1, 2)))

# How is num_private distributed?
length(unique(datTotal$num_private)) # SOlo 68
length(datTotal$num_private[datTotal$num_private == 0]) # 98,7% son 0
datTotal$num_private_bin<- as.factor(ifelse(datTotal$num_private == 0, 0, 1))

# What is wpt_name behaviour?
length(datTotal$wpt_name[datTotal$wpt_name == "none"]) # 5'98% son none
# Ni idea de qué hacer

# How is amount_tsh distributed?
length(unique(datTotal$amount_tsh)) # Solo 102
length(datTotal$amount_tsh[datTotal$amount_tsh == 0]) # 70,1% son 0
datTotal$amount_tsh_bin<- as.factor(ifelse(datTotal$amount_tsh == 0, 0, 1))

# Extract date info from date_recorded
datTotal$year_recorded <- as.factor(format(as.Date(datTotal$date_recorded), "%Y"))
datTotal$month_recorded <- as.factor(format(as.Date(datTotal$date_recorded), "%m"))
datTotal$day_recorded <- as.numeric(format(as.Date(datTotal$date_recorded), "%d"))

# 1.2 Update varResume ----
# Variable class
varDescription<- as.data.frame(sapply(datTotal, class))
colnames(varDescription)<- "clase"
varDescription$name<- row.names(varDescription)
varDescription$clase<- as.character(varDescription$clase)
row.names(varDescription)<- seq(1, nrow(varDescription))
varDescription$n_NANs<- sapply(datTotal, function(x) sum(is.na(x)))
varDescription<- varDescription[,c(2,1,3)]

# 1.2.1 Categoric variables ----
datTotal_cha<- datTotal[, varDescription$clase == "factor"]
datTotal_cha<- as.data.frame(sapply(datTotal_cha, as.factor))
varFac<- names(datTotal_cha)
aux<- as.data.frame(t(datTotal_cha %>%
                          summarise_all(funs(n_distinct(.)))))
aux$name<- row.names(aux)
varDescription$n_CATs<- left_join(varDescription, aux)$V1

# 1.2.2 Numeric variables ----
datTotal_num<- datTotal[, varDescription$clase %in% c('numeric', 'integer')]
datTotal_num<- as.data.frame(sapply(datTotal_num, as.numeric))
varNum<- names(datTotal_num)
aux<- as.data.frame(psych::describe(datTotal_num))
aux$vars<- NULL
aux$n<- NULL
aux$name<- row.names(aux)
varDescription<- left_join(varDescription, aux)

# 1.2.3 Logical variables
datTotal_boo<- datTotal[, varDescription$clase %in% 'logical']
varBoo<- names(datTotal_boo)

varResume<- varDescription[order(varDescription$n_NANs, varDescription$clase),
                           c(1,2,3,4,5,7,10,11,6)]
treat<- c("funder", "wpt_name",
          "installer", "subvillage",
          "region_code", "district_code",
          "lga", "ward", "public_meeting",
          "scheme_name", "permit")
varResume$Tratamiento<- if_else(varResume$name %in% treat, "SI", "NO")
saveRDS(varResume, "./dat/varResume_1")

# 1.3 New datasets ----
datNow<- datTotal[,varResume$name[varResume$Tratamiento=="NO"]]
datOne_sin<- datNow[,!(names(datNow) %in% c("construction_year", "date_recorded", "num_private",
                                            "amount_tsh"))]
datOne_con<- datNow[,!(names(datNow) %in% c("const_year_bin", "num_private_bin", "amount_tsh_bin",
                                            "year_recorded", "month_recorded", "day_recorded"))]

# 2. Models ----
datOne<- datOne_con
frml<- as.formula(paste("status_group ~", paste(names(datOne), collapse="+")))
datTrain<- inner_join(datOne, datY)
datTrain$status_group<- as.factor(datTrain$status_group)
datTest<- datOne[datOne$id %in% submiss$id,]
set.seed(1234)
my_mod_con<- ranger(frml,
                    data = datTrain,
                    importance = "impurity")

datOne<- datOne_sin
frml<- as.formula(paste("status_group ~", paste(names(datOne), collapse="+")))
datTrain<- inner_join(datOne, datY)
datTrain$status_group<- as.factor(datTrain$status_group)
datTest<- datOne[datOne$id %in% submiss$id,]
set.seed(1234)
my_mod_sin<- ranger(frml,
                    data = datTrain,
                    importance = "impurity")

# 2.1 Model Comparison ---- 
# Importancia de las variables
aux<- as.data.frame(my_mod_con$variable.importance)
aux$id<- rownames(aux)
rownames(aux)<- seq(1, nrow(aux))
aux$importance<- aux[,1]
aux[,1]<- NULL
p_con<- ggplot(data=aux, aes(x=id, y=importance))+
    geom_bar(stat="identity", color="black", fill="lightblue2", show.legend = FALSE, alpha=0.5) +
    scale_x_discrete(limits=aux$id[order(aux$importance)]) +
    coord_flip() + ggtitle("Importancia con originales")
aux<- as.data.frame(my_mod_sin$variable.importance)
aux$id<- rownames(aux)
rownames(aux)<- seq(1, nrow(aux))
aux$importance<- aux[,1]
aux[,1]<- NULL
p_sin<- ggplot(data=aux, aes(x=id, y=importance))+
    geom_bar(stat="identity", color="black", fill="lightblue2", show.legend = FALSE, alpha=0.5) +
    scale_x_discrete(limits=aux$id[order(aux$importance)]) +
    coord_flip() + ggtitle("Importancia con binarias")

grid.arrange(p_sin, p_con, ncol=2)

# 2.2 Final Data ----
datOne<- datNow[,!(names(datNow) %in% c("const_year_bin", "num_private", "amount_tsh",
                                        "date_recorded"))]
saveRDS(datOne, "./dat/datOne")
datTotal<- datTotal[,!(names(datTotal) %in% c("const_year_bin", "num_private", "amount_tsh",
                                              "date_recorded"))]
saveRDS(datTotal, "./dat/datTotal_1")

# 2. Final Model ----
frml<- as.formula(paste("status_group ~", paste(names(datOne), collapse="+")))
datTrain<- inner_join(datOne, datY)
datTrain$status_group<- as.factor(datTrain$status_group)
datTest<- datOne[datOne$id %in% submiss$id,]
set.seed(1234)
my_mod<- ranger(frml,
                data = datTrain,
                importance = "impurity")

# Matriz de confusión
aux<- as.data.frame(my_mod$confusion.matrix)
ggplot(data=aux, aes(x=true, y=Freq, fill=predicted))+
    geom_bar(stat="identity", color="black", show.legend = TRUE, alpha=0.5) +
    scale_x_discrete(limits=unique(aux$predicted[order(aux$Freq)])) +
    coord_flip() + ggtitle("Matriz de confusión")
# Error en la predicción
my_mod$prediction.error

# Save the model
saveRDS(my_mod, "./mod/V03_ranger")

# 3. Prediction ----
mod<- readRDS("./mod/V03_ranger")
predicted<- predict(mod, datTest)
pred<- data.frame(id=datTest$id,
                  status_group = predicted$predictions)
# Write submission
fwrite(pred, "./sub/V03_ranger_one.csv")

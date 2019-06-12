###########################################################
# Competición: 
# Etapa: Primer modelo
# Característica: RandomForest - RANGER
# Resultado: 0.8211
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
library(ggmosaic)
library(gridExtra)
source("funciones.R")

# 1. Load data ----
datTotal<- readRDS("./dat/datTotal_2")
datY<- as.data.frame(fread("./dat/rawTrain_y.csv"))
submiss<- as.data.frame(fread("./dat/SubmissionFormat.csv")) 
datTrain<- inner_join(datTotal, datY)
datTrain$status_group<- as.factor(datTrain$status_group)
datTest<- datTotal[datTotal$id %in% submiss$id,]

# 2. Replace factors with too much levels for it's absolute frequency ----
varResume<- readRDS("./dat/varResume_2")
treat_cols<- varResume$name[varResume$Tratamiento=="SI"]
for(col in treat_cols){
    cat_count<- as.data.frame(datTotal %>%
                                  group_by(!!sym(col)) %>% 
                                  summarise(count = n_distinct(id))
    )
    datTotal[[col]]<- as.numeric(cat_count$count[match(datTotal[[col]],cat_count[[col]])])    
}

# 2.1 Update varResume ----
# Variable class
varDescription<- as.data.frame(sapply(datTotal, class))
colnames(varDescription)<- "clase"
varDescription$name<- row.names(varDescription)
varDescription$clase<- as.character(varDescription$clase)
row.names(varDescription)<- seq(1, nrow(varDescription))
varDescription$n_NANs<- sapply(datTotal, function(x) sum(is.na(x)))
varDescription<- varDescription[,c(2,1,3)]

# 2.1.1 Categoric variables ----
datTotal_cha<- datTotal[, varDescription$clase == "factor"]
datTotal_cha<- as.data.frame(sapply(datTotal_cha, as.factor))
varFac<- names(datTotal_cha)
aux<- as.data.frame(t(datTotal_cha %>%
                          summarise_all(funs(n_distinct(.)))))
aux$name<- row.names(aux)
varDescription$n_CATs<- left_join(varDescription, aux)$V1

# 2.1.2 Numeric variables ----
datTotal_num<- datTotal[, varDescription$clase %in% c('numeric', 'integer')]
datTotal_num<- as.data.frame(sapply(datTotal_num, as.numeric))
varNum<- names(datTotal_num)
aux<- as.data.frame(psych::describe(datTotal_num))
aux$vars<- NULL
aux$n<- NULL
aux$name<- row.names(aux)
varDescription<- left_join(varDescription, aux)

# 2.1.3 Variables Resume ----
varResume<- varDescription[order(varDescription$n_CATs, decreasing=T),c(1,2,3,4,5,7,10,11,6)]
treat<- c()
varResume$Tratamiento<- if_else(varResume$name %in% treat, "SI", "NO")
saveRDS(varResume, "./dat/varResume_3")
saveRDS(datTotal, "./dat/datTotal_3")

# 3. Models ----
frml<- as.formula(paste("status_group ~", paste(names(datTotal), collapse="+")))
datTrain<- inner_join(datTotal, datY)
datTrain$status_group<- as.factor(datTrain$status_group)
datTest<- datTotal[datTotal$id %in% submiss$id,]
set.seed(1234)
my_mod<- ranger(frml,
                data = datTrain,
                importance = "impurity")
# Importancia de las variables
aux<- as.data.frame(my_mod$variable.importance)
aux$id<- rownames(aux)
rownames(aux)<- seq(1, nrow(aux))
aux$importance<- aux[,1]
aux[,1]<- NULL
impo<- ggplot(data=aux, aes(x=id, y=importance))+
    geom_bar(stat="identity", color="black", fill="lightblue2", show.legend = FALSE, alpha=0.5) +
    scale_x_discrete(limits=aux$id[order(aux$importance)]) +
    coord_flip() + ggtitle("Importancia de las variables")

# Matriz de confusión
aux<- as.data.frame(my_mod$confusion.matrix)
conf<- ggplot(data=aux, aes(x=true, y=Freq, fill=predicted))+
    geom_bar(stat="identity", color="black", show.legend = FALSE, alpha=0.5) +
    scale_x_discrete(limits=unique(aux$predicted[order(aux$Freq)])) +
    ggtitle("Matriz de confusión")

grid.arrange(impo, conf, ncol=2, widths=c(1.3, 1))

# Error en la predicción
my_mod$prediction.error

# Save the model
saveRDS(my_mod, "./mod/V05_ranger")

# 3. Prediction ----
mod<- readRDS("./mod/V05_ranger")
predicted<- predict(mod, datTest)
pred<- data.frame(id=datTest$id,
                  status_group = predicted$predictions)
# Write submission
fwrite(pred, "./sub/V05_ranger_three.csv")

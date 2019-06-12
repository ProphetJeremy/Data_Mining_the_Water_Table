###########################################################
# Competición: 
# Etapa: Primer modelo
# Característica: RandomForest - RANGER
# Resultado: 0.8174 
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
datTotal<- readRDS("./dat/datTotal_1")
datY<- as.data.frame(fread("./dat/rawTrain_y.csv"))
submiss<- as.data.frame(fread("./dat/SubmissionFormat.csv")) 
datTrain<- inner_join(datTotal, datY)
datTrain$status_group<- as.factor(datTrain$status_group)
datTest<- datTotal[datTotal$id %in% submiss$id,]

# 2.1 Logic vars distribution ----
aux<- datTrain[,c("public_meeting", "permit", "status_group")]
ggplot(data=aux)+
    geom_mosaic(aes(x=product(public_meeting, permit, status_group), fill=permit),
                na.rm=TRUE, offset = 0.04, divider=ddecker()) +
    theme_update(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                 axis.text.x = element_text(angle = 30, hjust = 1)) +
    ggtitle("Mosaico public_meeting - permit") + 
    xlab("permit") +
    ylab("public_meeting")

print(aux %>%
          group_by(status_group, permit, public_meeting) %>%
          summarise(N = n()) %>% 
          mutate(freq = N / sum(N)),
      n=27)

print(aux %>%
          group_by(status_group, public_meeting, permit) %>%
          summarise(N = n()) %>% 
          mutate(freq = N / sum(N)),
      n=27)
print(aux %>%
          group_by(public_meeting, permit) %>%
          summarise(N = n()) %>% 
          mutate(freq = N / sum(N)))

# 2.2 Input missing values ----
p1<- myBoxPlot(datTrain, "permit", "longitude", "status_group", "Longitud - permit", FALSE)
p2<- myBoxPlot(datTrain, "permit", "latitude", "status_group", "Latitud - permit", FALSE)
p3<- myBoxPlot(datTrain, "permit", "gps_height", "status_group", "Altitud - permit", TRUE)
grid.arrange(p1, p2, p3, ncol=3, widths=c(1,1,1.75))
datTotal<- fillMissCat(datTotal, "permit", "latitude", 15)

p1<- myBoxPlot(datTrain, "public_meeting", "longitude", "status_group", "Longitud - public_meeting",
               FALSE)
p2<- myBoxPlot(datTrain, "public_meeting", "latitude", "status_group", "Latitud - public_meeting",
               FALSE)
p3<- myBoxPlot(datTrain, "public_meeting", "gps_height", "status_group", "Altitud - public_meeting",
               TRUE)
grid.arrange(p1, p2, p3, ncol=3, widths=c(1,1,1.75))
datTotal<- fillMissCat(datTotal, "public_meeting", "latitude", 15)
# Las hacemos factor
datTotal$public_meeting<- as.factor(datTotal$public_meeting)
datTotal$permit<- as.factor(datTotal$permit)
summary(datTotal)

# What to do with numeric variables
# region_code
print(datTotal %>%
          group_by(region) %>% 
          summarise(count = n_distinct(region_code)) %>%
          filter(count > 1),
      n=21)

print(datTotal %>%
          group_by(region_code) %>% 
          summarise(count = n_distinct(region)) %>% 
          filter(count > 1),
      n=27)
# district_code
print(datTotal %>%
          group_by(subvillage) %>% 
          summarise(count = n_distinct(district_code)) %>% 
          filter(count > 1),
      n=27)

print(datTotal %>%
          group_by(district_code) %>% 
          summarise(count = n_distinct(subvillage)) %>% 
          filter(count > 1),
      n=27)

# 2.3 Add new variables ----
# Códigos por región - factor
code_by_region<- as.data.frame(datTotal %>%
                                   group_by(region) %>% 
                                   summarise(count = n_distinct(region_code))
)
datTotal$regcode_by_region<- as.factor(
    code_by_region$count[match(datTotal$region,code_by_region$region)]
)
# Regiones por código - factor
region_by_code<- as.data.frame(datTotal %>%
                                   group_by(region_code) %>% 
                                   summarise(count = n_distinct(region))
)
datTotal$region_by_regcode<- as.factor(
    ifelse(
        region_by_code$count[match(datTotal$region_code, region_by_code$region_code)] < 2,
        0,1
    )
)
# Distritos por pueblo - factor
code_by_subv<- as.data.frame(datTotal %>%
                                 group_by(subvillage) %>% 
                                 summarise(count = n_distinct(district_code))
)
datTotal$distcode_by_subv<- as.factor(
    code_by_subv$count[match(datTotal$subvillage,code_by_subv$subvillage)]
)
# Pueblos por distrito - numérica
subv_by_code<- as.data.frame(datTotal %>%
                                 group_by(district_code) %>% 
                                 summarise(count = n_distinct(subvillage))
)
datTotal$subv_by_distcode<- as.numeric(
    subv_by_code$count[match(datTotal$district_code,subv_by_code$district_code)]
)

# 2.4 Update varResume ----
# Variable class
varDescription<- as.data.frame(sapply(datTotal, class))
colnames(varDescription)<- "clase"
varDescription$name<- row.names(varDescription)
varDescription$clase<- as.character(varDescription$clase)
row.names(varDescription)<- seq(1, nrow(varDescription))
varDescription$n_NANs<- sapply(datTotal, function(x) sum(is.na(x)))
varDescription<- varDescription[,c(2,1,3)]

# 2.4.1 Categoric variables ----
datTotal_cha<- datTotal[, varDescription$clase == "factor"]
datTotal_cha<- as.data.frame(sapply(datTotal_cha, as.factor))
varFac<- names(datTotal_cha)
aux<- as.data.frame(t(datTotal_cha %>%
                          summarise_all(funs(n_distinct(.)))))
aux$name<- row.names(aux)
varDescription$n_CATs<- left_join(varDescription, aux)$V1

# 2.4.2 Numeric variables ----
datTotal_num<- datTotal[, varDescription$clase %in% c('numeric', 'integer')]
datTotal_num<- as.data.frame(sapply(datTotal_num, as.numeric))
varNum<- names(datTotal_num)
aux<- as.data.frame(psych::describe(datTotal_num))
aux$vars<- NULL
aux$n<- NULL
aux$name<- row.names(aux)
varDescription<- left_join(varDescription, aux)

# 2.4.3 Logical variables
datTotal_boo<- datTotal[, varDescription$clase %in% 'logical']
varBoo<- names(datTotal_boo)

varResume<- varDescription[order(varDescription$n_CATs, decreasing=T),c(1,2,3,4,5,7,10,11,6)]
treat<- c("funder", "wpt_name",
          "installer", "subvillage",
          "lga", "ward","scheme_name")
varResume$Tratamiento<- if_else(varResume$name %in% treat, "SI", "NO")
saveRDS(varResume, "./dat/varResume_2")
saveRDS(datTotal, "./dat/datTotal_2")

# 1.3 New datasets ----
datTwo<- datTotal[,varResume$name[varResume$Tratamiento=="NO"]]

# 3. Models ----
frml<- as.formula(paste("status_group ~", paste(names(datTwo), collapse="+")))
datTrain<- inner_join(datTwo, datY)
datTrain$status_group<- as.factor(datTrain$status_group)
datTest<- datTwo[datTwo$id %in% submiss$id,]
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
saveRDS(my_mod, "./mod/V04_ranger")

# 3. Prediction ----
mod<- readRDS("./mod/V04_ranger")
predicted<- predict(mod, datTest)
pred<- data.frame(id=datTest$id,
                  status_group = predicted$predictions)
# Write submission
fwrite(pred, "./sub/V04_ranger_one.csv")

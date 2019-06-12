###########################################################
# Competición Titanic Kaggle
# Algoritmo tipo: ENSAMBLADO
# Resultado-mean: 0.78497
# Resultado-mean_pond: 0.79425
# Resultado-best:
# Resultado-K-means:
# Posición: No mejora
# Percentil: No mejora
# Fecha: 15 - 05 - 2019
###########################################################

setwd("~/Desktop/Santi/master_BD/machine_learning/entrega/")
setwd("/cloud/project/entrega/")
setwd("C:\\Users/n230104/Desktop/personal/master/entrega/entrega/")
source("funcionesML.R")

# 0. Cargo paquetes ----
library(dplyr)
library(reshape2)
library(ggplot2)
library(data.table)
library(stringr)
library(tidyr)
library(gridExtra)
library(questionr)
library(caret)
library(cluster)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(NbClust)

# 1. Carga de datos ----
datTrain<- readRDS("train_clean")
datTest<- readRDS("test_clean")
datTotal<- rbind(datTrain[,c(2:ncol(datTrain))], datTest)

datTrain_sca<- readRDS("train_clean_sca")
datTest_sca<- readRDS("test_clean_sca")
datTotal_sca<- rbind(datTrain_sca[,c(2:ncol(datTrain_sca))], datTest_sca)

## 1.1. Carga de predicciones ----
probTrain<- readRDS("./tab/probTrain.csv")
probTest<- readRDS("./tab/probTest.csv")
probTotal<- rbind(probTrain[,c(1,3:ncol(probTrain))], probTest)

binTrain<- readRDS("./tab/binTrain.csv")
binTest<- readRDS("./tab/binbTest.csv")
binTotal<- rbind(binTrain[,c(1,3:ncol(binTrain))], binTest)

# Submission
submission<- data.frame(PassengerId = as.numeric(row.names(datTest_sca)))

## 1.2. Ponderación
ranking<- readRDS("./tab/ranking")
total<- readRDS("./tab/cv_all")
metricMean<- aggregate(metric~modelo, data=total, mean)
# Fix ponderation values between 0.3 - 0.7
metricMean$pond<- ((metricMean$metric - min(metricMean$metric))*0.4/(max(metricMean$metric)-min(metricMean$metric)))+0.3
metricMean$pond<- metricMean$pond/sum(metricMean$pond)
#% Sort values as they are in names(ProbTotal)
posi<- c()
for(mod in metricMean$modelo){
    posi<- c(posi, which(names(probTotal[,c(2:ncol(probTotal))]) == mod))
}
metricMean$posi<- posi
metricMean[order(metricMean$posi),]

# 2. Average predictions ----
# Aggregate the mean and pondered mean probability for each sample
probTotal$mean<- rowMeans(probTotal[,c(2:ncol(probTotal))])
probTotal$mean_pond<- rowSums(metricMean$pond * probTotal[,c(2:(ncol(probTotal)-1))])
# Reescaling
probTotal$mean_pond<- (probTotal$mean_pond - min(probTotal$mean_pond))/(max(probTotal$mean_pond) - min(probTotal$mean_pond))

binTotal_man<- binTotal
for(col in names(binTotal[,c(2:ncol(binTotal))])){
    binTotal_man[[col]]<- as.numeric(ifelse(binTotal_man[[col]]==0,0,1))
}
binTotal$mean<- as.factor(ifelse(rowMeans(binTotal_man[,c(2:ncol(binTotal_man))]) < 0.5,0,1))
binTotal$mean_pond<- as.factor(ifelse(rowSums(metricMean$pond * binTotal_man[,c(2:ncol(binTotal_man))]) < 0.5,0,1))

## 2.1 Doubts ----
# Calculate how many samples will switch if we use a different way to evaluate it
doubts<- c()
for(i in seq(1,nrow(binTotal))){
    a<- all(as.numeric(binTotal[i,c(2:ncol(binTotal))]) == as.numeric(binTotal[i,2]))
    doubts<- c(doubts, ifelse(a, 0, 1))
}
sum(doubts[1:nrow(probTrain)])/nrow(probTrain)
sum(doubts[(nrow(probTrain)+1):nrow(probTotal)])/nrow(probTest)

## 2.2 Mean submissions ----
Sub_mean<- submission
Sub_mean$Survived<- binTotal$mean[(nrow(probTrain)+1):nrow(probTotal)]
fwrite(Sub_mean, "./sub/mean_sub.csv", row.names = FALSE)
Sub_mean_pond<- submission
Sub_mean_pond$Survived<- binTotal$mean_pond[(nrow(probTrain)+1):nrow(probTotal)]
fwrite(Sub_mean_pond, "./sub/mean-pond_sub.csv", row.names = FALSE)

# 3. Heatmap ----
hm_df<- data.frame(PassengerID = row.names(datTrain))
for (col in names(binTotal[,2:ncol(binTotal)])){
    binTotal_sel<- as.numeric(binTotal[c(1:nrow(datTrain)), c(col)])
    hm_df[[col]]<- as.factor(ifelse(binTotal_sel == as.numeric(datTrain$Survived), "HIT", "FAIL"))
}
head(hm)
hm<- melt(hm_df, measure.vars = names(hm_df[,c(2:ncol(hm_df))]), factorsAsStrings = FALSE)
ggplot(data = hm, aes(x = PassengerID, y = variable, fill= value)) +
    geom_tile() +
    ggtitle("Models over Train set") +
    scale_fill_manual(values=c("#ce3939", "#86cc7a"), breaks=levels(hm$value)) + 
    theme_update(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                 axis.text.x = element_blank(),
                 axis.title.y = element_blank(),
                 axis.ticks.x = element_blank())

## 3.1 Weird Cases ----
weird_ids<- as.numeric(hm_df[which(hm_df$bagg_full == "FAIL"),]$PassengerID)
weird_pass<- datTrain[which(rownames(datTrain) %in% weird_ids),]
saveRDS(weird_pass, "./tab/weirdPass")

# 4. K-Means clustering ----
# Remove factors
num_cols<- names(which(sapply(datTotal_sca, is.numeric) == TRUE))
fac_cols<- names(datTotal_sca)[!(names(datTotal_sca) %in% num_cols)]

# Join all the measurable data
fullSet<- cbind(datTotal_sca[,num_cols], probTotal[, c(2:ncol(probTotal))])
summary(datTotal_sca)

# Distancias
distances <- dist(fullSet, method = "euclidean")
fviz_dist(distances, show_labels = FALSE)
res.hc <- hclust(distances, method="ward.D2") 
fviz_dend(res.hc, k = 10, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#cc71d6", "#8c0000", 
                       "#9b5726", "#a30488", "#016384", "#a168e2"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          main = "Clúster Jerárquico"
)
grp <- cutree(res.hc, k = 10)
table(grp)
fviz_cluster(list(data = fullSet, cluster = grp),
             # palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             geom = c("point"),
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = TRUE, ggtheme = theme_minimal(),
             )

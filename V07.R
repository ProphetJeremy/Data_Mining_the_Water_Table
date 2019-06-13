###########################################################
# Competición: 
# Etapa: Primer modelo
# Característica: RandomForest - RANGER
# Resultado: 0.8180
# Fecha: 13-06-2019
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
library(vtreat)
source("funciones.R")

# 1. Load data ----
datTotal<- readRDS("./dat/datTotal_3")
datY<- as.data.frame(fread("./dat/rawTrain_y.csv"))
submiss<- as.data.frame(fread("./dat/SubmissionFormat.csv"))
datTrain<- inner_join(datTotal, datY)
datTest<- datTotal[datTotal$id %in% submiss$id,]

# 1.1 Data treatment using vtreat ----
x_names <- c("funder", "installer", "wpt_name", "basin", "subvillage", "region", 
             "lga", "ward", "scheme_management", "scheme_name", "extraction_type", 
             "extraction_type_group", "extraction_type_class", "management", 
             "management_group", "payment_type", "water_quality", "quality_group", 
             "quantity", "quantity_group", "source", "source_type", "source_class", 
             "waterpoint_type", "waterpoint_type_group", "id", "gps_height", 
             "longitude", "latitude", "region_code", "district_code", "population", 
             "construction_year", "public_meeting", "permit", "num_private_bin", 
             "amount_tsh_bin", "year_recorded", "month_recorded", "day_recorded", 
             "regcode_by_region", "region_by_regcode", "distcode_by_subv", 
             "subv_by_distcode")
y_name <- "status_group"

# build the multi-class cross frame and treatments
cfe_m <- mkCrossFrameMExperiment(datTrain, x_names, y_name)
# Look at the new data, we have 256 variables
str(cfe_m$cross_frame)
# Treat the complete dataset
datTotal<- prepare(cfe_m$treat_m, datTotal)
names(datTotal)
saveRDS(datTotal, "./dat/datTotal_4")

# 2. Models ----
# Define the possible objective class
posibilities<- c("functional", "non functional", "functional needs repair")
predictionsDF<- data.frame(row.names = seq(1:nrow(datTest)))
for(obj in posibilities){
    others = posibilities[which(posibilities != obj)]
    # Generate each case data
    datY_1<- data.frame(id = datY$id,
                        y = ifelse(datY$status_group == obj, obj, "other"))
    datY_2<- data.frame(id = datY_1$id[which(datY_1$y == "other")],
                        y = ifelse(datY$status_group[which(datY_1$y == "other")] == others[1],
                                   others[1], others[2]))
    datTrain_1<- inner_join(datTotal, datY_1)
    datTrain_2<- inner_join(datTotal, datY_2)
    datTrain_1$y<- as.factor(datTrain_1$y)
    datTrain_2$y<- as.factor(datTrain_2$y)
    datTest<- datTotal[datTotal$id %in% submiss$id,]
    frml<- as.formula(paste("y ~", paste(names(datTotal), collapse="+")))
    set.seed(1234)
    # 2.1 First model ----
    my_mod_1<- ranger(frml,
                      data = datTrain_1,
                      importance = "impurity")
    print(paste(obj, " - 1. Error predicciones: ", my_mod_1$prediction.error, sep = ""))
    saveRDS(my_mod_1, paste("./mod/V07_ranger_1_",obj, sep = ""))
    # 2.2 Second model ----
    my_mod_2<- ranger(frml,
                      data = datTrain_2,
                      importance = "impurity")
    print(paste(obj, " - 2. Error predicciones: ", my_mod_2$prediction.error, sep = ""))
    saveRDS(my_mod_2, paste("./mod/V07_ranger_2_",obj, sep = ""))
    
    # 3. Prediction ----
    # Step 1
    predicted<- predict(my_mod_1, datTest)
    pred_1<- data.frame(id=datTest$id,
                        status_group = predicted$predictions)
    # Step 2
    datTest_2<- datTest[which(pred_1$status_group == "other"),]
    predicted<- predict(my_mod_2, datTest_2)
    pred_2<- data.frame(id=datTest_2$id,
                        status_group = predicted$predictions)
    aux<- rbind(pred_1[pred_1$status_group == obj,], pred_2)
    # Final pred
    pred<- inner_join(submiss, aux, by="id")[,c(1,3)]
    pred$status_group<- pred$status_group.y
    pred$status_group.y<- NULL
    # Checkeo tontorrón
    print(paste("Train proportion: ",
                length(which(datTrain_1$y == obj))/nrow(datTrain_1), sep="")
    )
    print(paste("Test proportion: ",
                length(which(pred$status_group == obj))/nrow(pred), sep="")
    )
    # 3.1 Join Results ----
    predictionsDF[[paste(obj,"_id",sep="")]] <- pred$id
    predictionsDF[[obj]] <- pred$status_group
} 

# Checking they have all the same ids ----
all(predictionsDF$functional_id == predictionsDF$`non functional_id`) # TRUE
all(predictionsDF$functional_id == predictionsDF$`functional needs repair_id`) # TRUE

# 4. Ensemble ----
# Split individual dfs 
pred<- predictionsDF[,c(1,2)]
pred_functional<- pred %>%
    rename(status_group = functional) %>%
    rename(id = functional_id)
pred<- predictionsDF[,c(3,4)]
pred_non_functional<- pred %>%
    rename(status_group = `non functional`) %>%
    rename(id = `non functional_id`)
pred<- predictionsDF[,c(5,6)]
pred_functional_needs_repair<- pred %>%
    rename(status_group = `functional needs repair`) %>%
    rename(id = `functional needs repair_id`)

# Save individuals
fwrite(pred_functional, "./sub/V07_ranger_functional.csv")
fwrite(pred_non_functional, "./sub/V07_ranger_non_functional.csv")
fwrite(pred_functional_needs_repair, "./sub/V07_ranger_functional_needs_repair.csv")

# 4.1 Use the most upvoted class ----
pred_to_melt<- data.frame(id = pred_functional$id,
                          status_group_f = pred_functional$status_group,
                          status_group_nf = pred_non_functional$status_group,
                          status_group_fnr = pred_functional_needs_repair$status_group)
aux<- melt(pred_to_melt, id.vars="id")
aux$variable<- NULL
length(unique(aux$id))
upvoted<- as.data.frame(aux %>%
                            add_count(id, value) %>% 
                            filter(n > 1) %>%
                            distinct(id, value)
)
onevoted<- as.vector(aux %>%
                         add_count(id, value) %>% 
                         filter(n == 1) %>%
                         distinct(id) %>% 
                         select(id)
)
noclue<- anti_join(onevoted, upvoted)
# For the 15 rows that models differs we trust non functional which seems to be the best 
aux<- data.frame(id = noclue,
                 value = pred_non_functional$status_group[pred_non_functional$id %in% noclue$id])
voted<- rbind(upvoted, aux)
predFin<- data.frame(id = pred_functional$id)
predFin$status_group<- voted$value[match(predFin$id, voted$id)]
# Check
predFin$id[!(predFin$status_group %in% posibilities)]

# Write submission
fwrite(predFin, "./sub/V07_ensemble.csv")

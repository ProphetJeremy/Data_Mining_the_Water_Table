var_y<- c("status_group")
var_num<- c("id", "amount_tsh", "gps_height", "longitude", "latitude", 
            "num_private", "region_code", "district_code", "population", 
            "construction_year")
var_catlte15<- c("basin", "public_meeting", "recorded_by", "scheme_management", 
                 "permit", "extraction_type_group", "extraction_type_class", "management", 
                 "management_group", "payment", "payment_type", "water_quality", 
                 "quality_group", "quantity", "quantity_group", "source", "source_type", 
                 "source_class", "waterpoint_type", "waterpoint_type_group", "status_group")
var_catgt15<-c("date_recorded", "funder", "installer", "wpt_name", "subvillage", 
               "region", "lga", "ward", "scheme_name", "extraction_type")
var_nas<- c("public_meeting", "permit")
var_shit<- c("recorded_by")

# Load data ----
test<- as.data.frame(fread("test_set.csv"))
datEnd<- as.data.frame(readRDS("datEnd.rds"))
datJoin<- as.data.frame(fread("datJoin.csv"))

# Selected vars
var_add<- names(datJoin)[!(names(datJoin) %in% names(datEnd))]
var_sel<- names(datJoin)[names(datJoin) %in% c(var_num, var_catlte15, var_nas, var_add)]

# Fit var_y (3 categories)
y<- datEnd[,var_y]
y_better<- ifelse(y=="functional",1,
                  ifelse(y=="non functional", 0, 2))

# Train - Test split
datTrain<- datJoin[seq(1, nrow(datEnd)), var_sel]
datTrain$y<- as.factor(make.names(y_better))
datTest<- datJoin[seq(nrow(datEnd)+1,nrow(datJoin)),var_sel]

# Ranger Model
frml<- as.formula(paste("y~", paste(var_sel, collapse="+")))
set.seed(123)
control<-trainControl(method = "LGOCV",
                      p=0.8, number=1, savePredictions = "all",
                      classProbs=TRUE)
rgr<- ranger(frml, datTrain,
             num.trees = 100, importance="impurity",
             write.forest = T, splitrule = "extratrees",
             num.random.splits = 10)
mod<- rgr

# Predictions
pred_train<- as.vector(predict(mod, datTrain)$predictions)
length(which(pred_train==datTrain$y))/nrow(datTrain)

pred_test<- as.vector(predict(mod, datTest)$predictions)
pred_test<- ifelse(pred_test=="X1","functional",
                   ifelse(pred_test=="X0","non functional","functional needs repair"))
my_sub<- data.frame(
    id = test$id,
    status_group = pred_test
)
head(my_sub)
fwrite(my_sub, "sub_V8_base.csv", sep=",")
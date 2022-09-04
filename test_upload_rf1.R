train_hr <- read.csv("C:\\Users\\MUKESH\\Downloads\\train_LZdllcl.csv", stringsAsFactors = T)
train_hr$label <- 'train'

test_hr <- read.csv("C:\\Users\\MUKESH\\Downloads\\test_2umaH9m.csv", stringsAsFactors = T)
test_hr$label <- 'test'

names(train_hr)

test_hr$is_promoted <- 1

final_data <- rbind(train_hr,test_hr)
str(final_data)
summary(final_data)

ratind_na <- subset(final_data,is.na(previous_year_rating))
summary(ratind_na)

final_data$education <- as.character(final_data$education)
final_data$education <- ifelse(final_data$education=="","secondary",final_data$education)
final_data$education <- as.factor(final_data$education)


final_data$previous_year_rating <- ifelse(is.na(final_data$previous_year_rating),0,final_data$previous_year_rating)

summary(final_data)

final_data$label <- as.factor(final_data$label)

data_num <- final_data[sapply(final_data,is.numeric)]
data_fac <- final_data[sapply(final_data,is.factor)]

library(caret) 
nzv_data <- nearZeroVar(data_num)

for(i in 1:8){
  final_data[,i]=(data_num[,i]-min(data_num[,i]))/(max(data_num[,i])-min(data_num[,i]))
}

final_data_new <- cbind(data_num,data_fac)

train <- subset(final_data_new, label=='train')
test <- subset(final_data_new, label=='test')

library(woeBinning)
woe <- woe.binning(train,'is_promoted',train)
woe
woe_1 <- woe[,c(-2)]
woe_1

library(caTools)
sample <- sample.split(train$is_promoted,SplitRatio = 0.8)
train_model <- subset(train,sample==T)
test_model <- subset(train,sample==F)

summary(train_model$is_promoted)
summary(test_model$is_promoted)

names(train_model)


library(randomForest)

rf_model <- randomForest(is_promoted~.,data = train_model[,c(-1,-11,-15)],mtry = 3, ntree = 200,importance = T)

test_model$pred_rf <- predict(rf_model,test_model)

test_model$prediction_rf <- ifelse(test_model$pred_rf > 0.40,1,0)
library(MLmetrics)
F1_Score(test_model$is_promoted,test_model$prediction_rf,positive = 1)

test$pred <- predict(rf_model,test)

test$is_promoted <- ifelse(test$pred>0.40,1,0)
names(test)
test_upload <- test[,c(1,9)]
test_upload$employee_id <- test_hr$employee_id
write.csv(test_upload,"test_upload_rf",row.names = F)
getwd()





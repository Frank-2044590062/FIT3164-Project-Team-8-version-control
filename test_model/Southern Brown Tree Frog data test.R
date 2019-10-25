# this file implement modelling of common beard-heat.
# data1 file 
# 2/19/2019
# author: Ken Lin
# version: 2


rm(list = ls())
library('openxlsx')
data= read.csv('Southern Brown Tree Frog cluster.csv', header = TRUE)
#modelling and testing
data$X=NULL
data$log=NULL
data$lan=NULL
names(data)[28]<-"RELIABILITY_TXT"
train.row = sample(1:nrow(data), 0.7*nrow(data))  
train=data[train.row,]
test= data[-train.row,]

library(tree)
library(randomForest)
library(e1071)
library(rpart)
library(adabag)
# attach train data set for model
attach(train)
#set the column as factor 
train$RELIABILITY_TXT = factor(train$RELIABILITY_TXT)
# model for random fprest
rf_model = randomForest(RELIABILITY_TXT~., data= train,  ntree =500,type='classification')
detach(train)
attach(test)
detach(test)
# show the importance of each column
rf_model$importance
# using the model for test set
rf_test= predict(rf_model,test)
# combine the test result and the actual result
df= as.data.frame(cbind(rf_test, test$RELIABILITY_TXT))
# build the comfusion matrix and output the result
rf_table = table(predicted = rf_test, actual = test$RELIABILITY_TXT)
print(rf_table)

# naiv bayes

naiv.model=naiveBayes(RELIABILITY_TXT~., data = train)
#  make the prediction for the test set
naiv.predict = predict(naiv.model, test)
# combine the predict result with actaul value
df1= as.data.frame(cbind(naiv.predict, test$RELIABILITY_TXT))
# build the table to show the output
tn=table(predicted = naiv.predict, actual = test$RELIABILITY_TXT)
print(tn)
#bagging
bag <- bagging(RELIABILITY_TXT ~. , data = train, mfinal=5)
#  make the prediction for the test set
pred.bag <- predict.bagging(bag, test)
print(pred.bag$confusion)
#boosting
Boost <- boosting(RELIABILITY_TXT ~. , data = train, mfinal=10)
#  make the prediction for the test set
pred.boost <- predict.boosting(Boost, newdata=test)
print(pred.boost$confusion)
# delet the columns with less importance
train$Point=NULL
train$Line_Distance=NULL
train$bua=NULL
train$vegtpye3_4.tif = NULL
train$X75m_dem_streams_burned_sept2012.tif=NULL
train$Anisotrophic_Heating_Ruggedbess.tif=NULL
train$ecoregion1750.tif=NULL
train$ibra_hex.tif=NULL
train$Radiometrics_2014_k.tif=NULL
train$Radiometrics_2014_th.tif=NULL

test$Point=NULL
test$Line_Distance=NULL
test$bua=NULL
test$vegtpye3_4.tif = NULL
test$X75m_dem_streams_burned_sept2012.tif=NULL
test$Anisotrophic_Heating_Ruggedbess.tif=NULL
test$ecoregion1750.tif=NULL
test$ibra_hex.tif=NULL
test$Radiometrics_2014_k.tif=NULL
test$Radiometrics_2014_th.tif=NULL
#rf
# attach train data set for model
attach(train)
#set the column as factor 
train$RELIABILITY_TXT = factor(train$RELIABILITY_TXT)
# model for random fprest
rf_model = randomForest(RELIABILITY_TXT~., data= train,  ntree =500,type='classification')
detach(train)
attach(test)
detach(test)
# show the importance of each column
rf_model$importance
# using the model for test set
rf_test= predict(rf_model,test)
# combine the test result and the actual result
df= as.data.frame(cbind(rf_test, test$RELIABILITY_TXT))
# build the comfusion matrix and output the result
rf_table = table(predicted = rf_test, actual = test$RELIABILITY_TXT)
print(rf_table)

# naiv bayes

naiv.model=naiveBayes(RELIABILITY_TXT~., data = train)
#  make the prediction for the test set
naiv.predict = predict(naiv.model, test)
# combine the predict result with actaul value
df1= as.data.frame(cbind(naiv.predict, test$RELIABILITY_TXT))
# build the table to show the output
tn=table(predicted = naiv.predict, actual = test$RELIABILITY_TXT)
print(tn)
#bagging
bag <- bagging(RELIABILITY_TXT ~. , data = train, mfinal=5)
#  make the prediction for the test set
pred.bag <- predict.bagging(bag, test)
print(pred.bag$confusion)
#boosting
Boost <- boosting(RELIABILITY_TXT ~. , data = train, mfinal=10)
#  make the prediction for the test set
pred.boost <- predict.boosting(Boost, newdata=test)
print(pred.boost$confusion)
#80%
train.row = sample(1:nrow(data), 0.8*nrow(data))  
train=data[train.row,]
test= data[-train.row,]
# attach train data set for model
attach(train)
#set the column as factor 
train$RELIABILITY_TXT = factor(train$RELIABILITY_TXT)
# model for random fprest
rf_model = randomForest(RELIABILITY_TXT~., data= train,  ntree =500,type='classification')
detach(train)
attach(test)
detach(test)
# show the importance of each column
rf_model$importance
# using the model for test set
rf_test= predict(rf_model,test)
# combine the test result and the actual result
df= as.data.frame(cbind(rf_test, test$RELIABILITY_TXT))
# build the comfusion matrix and output the result
rf_table = table(predicted = rf_test, actual = test$RELIABILITY_TXT)
print(rf_table)

# naiv bayes

naiv.model=naiveBayes(RELIABILITY_TXT~., data = train)
#  make the prediction for the test set
naiv.predict = predict(naiv.model, test)
# combine the predict result with actaul value
df1= as.data.frame(cbind(naiv.predict, test$RELIABILITY_TXT))
# build the table to show the output
tn=table(predicted = naiv.predict, actual = test$RELIABILITY_TXT)
print(tn)
#bagging
bag <- bagging(RELIABILITY_TXT ~. , data = train, mfinal=5)
#  make the prediction for the test set
pred.bag <- predict.bagging(bag, test)
print(pred.bag$confusion)
#boosting
Boost <- boosting(RELIABILITY_TXT ~. , data = train, mfinal=10)
#  make the prediction for the test set
pred.boost <- predict.boosting(Boost, newdata=test)
print(pred.boost$confusion)
# delet the columns with less importance
train$Point=NULL
train$RECORD_TYPE=NULL
train$bua =NULL
train$ridge=NULL
train$ecoregion1750.tif=NULL
train$ibra_hex.tif=NULL
train$Radiometrics_2014_k.tif=NULL
train$Radiometrics_2014_th.tif=NULL

test$Point=NULL
test$RECORD_TYPE=NULL
test$bua = NULL
test$ridge=NULL
test$ecoregion1750.tif=NULL
test$ibra_hex.tif=NULL
test$Radiometrics_2014_k.tif=NULL
test$Radiometrics_2014_th.tif=NULL


# attach train data set for model
attach(train)
#set the column as factor 
train$RELIABILITY_TXT = factor(train$RELIABILITY_TXT)
# model for random fprest
rf_model = randomForest(RELIABILITY_TXT~., data= train,  ntree =500,type='classification')
detach(train)
attach(test)
detach(test)
# show the importance of each column
rf_model$importance
# using the model for test set
rf_test= predict(rf_model,test)
# combine the test result and the actual result
df= as.data.frame(cbind(rf_test, test$RELIABILITY_TXT))
# build the comfusion matrix and output the result
rf_table = table(predicted = rf_test, actual = test$RELIABILITY_TXT)
print(rf_table)

# naiv bayes

naiv.model=naiveBayes(RELIABILITY_TXT~., data = train)
#  make the prediction for the test set
naiv.predict = predict(naiv.model, test)
# combine the predict result with actaul value
df1= as.data.frame(cbind(naiv.predict, test$RELIABILITY_TXT))
# build the table to show the output
tn=table(predicted = naiv.predict, actual = test$RELIABILITY_TXT)
print(tn)
#bagging
bag <- bagging(RELIABILITY_TXT ~. , data = train, mfinal=5)
#  make the prediction for the test set
pred.bag <- predict.bagging(bag, test)
print(pred.bag$confusion)
#boosting
Boost <- boosting(RELIABILITY_TXT ~. , data = train, mfinal=10)
#  make the prediction for the test set
pred.boost <- predict.boosting(Boost, newdata=test)
print(pred.boost$confusion)

train.row = sample(1:nrow(data), 0.9*nrow(data))  
train=data[train.row,]
test= data[-train.row,]
detach(train)
# attach train data set for model
attach(train)
#set the column as factor 
train$RELIABILITY_TXT = factor(train$RELIABILITY_TXT)
# model for random fprest
rf_model = randomForest(RELIABILITY_TXT~., data= train,  ntree =500,type='classification')
detach(train)
attach(test)
detach(test)
# show the importance of each column
rf_model$importance
# using the model for test set
rf_test= predict(rf_model,test)
# combine the test result and the actual result
df= as.data.frame(cbind(rf_test, test$RELIABILITY_TXT))
# build the comfusion matrix and output the result
rf_table = table(predicted = rf_test, actual = test$RELIABILITY_TXT)
print(rf_table)

# naiv bayes

naiv.model=naiveBayes(RELIABILITY_TXT~., data = train)
#  make the prediction for the test set
naiv.predict = predict(naiv.model, test)
# combine the predict result with actaul value
df1= as.data.frame(cbind(naiv.predict, test$RELIABILITY_TXT))
# build the table to show the output
tn=table(predicted = naiv.predict, actual = test$RELIABILITY_TXT)
print(tn)
#bagging
bag <- bagging(RELIABILITY_TXT ~. , data = train, mfinal=5)
#  make the prediction for the test set
pred.bag <- predict.bagging(bag, test)
print(pred.bag$confusion)
#boosting
Boost <- boosting(RELIABILITY_TXT ~. , data = train, mfinal=10)
#  make the prediction for the test set
pred.boost <- predict.boosting(Boost, newdata=test)
print(pred.boost$confusion)
# delet the columns with less importance
train$RECORD_TYPE=NULL
train$Point=NULL
train$bua=NULL
train$ibra_hex.tif=NULL
train$ecoregion1750.tif = NULL
train$ridge = NULL
train$Radiometrics_2014_k.tif=NULL
train$Radiometrics_2014_th.tif=NULL

test$RECORD_TYPE=NULL
test$Point=NULL
test$bua=NULL
test$ibra_hex.tif=NULL
test$ecoregion1750.tif = NULL
test$ridge = NULL
test$Radiometrics_2014_k.tif=NULL
test$Radiometrics_2014_th.tif=NULL
detach(train)
# attach train data set for model
attach(train)
#set the column as factor 
train$RELIABILITY_TXT = factor(train$RELIABILITY_TXT)
# model for random fprest
rf_model = randomForest(RELIABILITY_TXT~., data= train,  ntree =500,type='classification')
detach(train)
attach(test)
detach(test)
# show the importance of each column
rf_model$importance
# using the model for test set
rf_test= predict(rf_model,test)
# combine the test result and the actual result
df= as.data.frame(cbind(rf_test, test$RELIABILITY_TXT))
# build the comfusion matrix and output the result
rf_table = table(predicted = rf_test, actual = test$RELIABILITY_TXT)
print(rf_table)

# naiv bayes

naiv.model=naiveBayes(RELIABILITY_TXT~., data = train)
#  make the prediction for the test set
naiv.predict = predict(naiv.model, test)
# combine the predict result with actaul value
df1= as.data.frame(cbind(naiv.predict, test$RELIABILITY_TXT))
# build the table to show the output
tn=table(predicted = naiv.predict, actual = test$RELIABILITY_TXT)
print(tn)
#bagging
bag <- bagging(RELIABILITY_TXT ~. , data = train, mfinal=5)
#  make the prediction for the test set
pred.bag <- predict.bagging(bag, test)
print(pred.bag$confusion)
#boosting
Boost <- boosting(RELIABILITY_TXT ~. , data = train, mfinal=10)
#  make the prediction for the test set
pred.boost <- predict.boosting(Boost, newdata=test)
print(pred.boost$confusion)
# this file implement modelling of common beard-heat.
# data1 file 
# 2/19/2019
# author: Ken Lin
# version: 2


rm(list = ls())
#setwd("C:/Users/new/Desktop/3164_project_R/common Beard-heath")
library('openxlsx')
species_data= read.csv('common beard-heath cluster data.csv', header = TRUE)
species_data$X=NULL
species_data$lan=NULL
species_data$log=NULL
# generate pesudo absence data
data=read.xlsx('complete_data.xlsx')
data[,c(1,2,3,5,6,7,9,11,15,16,17,20,21,22,23,24,25,26,27)]=NULL
# delete some unimportant columns to make the model
data[,c(6,8)]=NULL
data$LATITUDEDD_NUM=NULL
data$LONGITUDEDD_NUM=NULL
data$bay=NULL
data$island=NULL
data$island_marine=NULL
data$lga=NULL
data$locality=NULL
data$mainland=NULL
data$sea=NULL
data$wb_lake=NULL
data$wb_lake_salt=NULL
data$wetland_swamp=NULL
data$bua  = NULL
data$named_natural_region=NULL
data$postcode=NULL
data = na.omit(data)
set.seed(28339797)
data = data[data$COMMON_NME!='Common Beard-heath' & data$RECORD_TYPE=='Captured and released',]
absence_data.row = sample(1:nrow(data), 0.9*nrow(data))  
absence_data= data[-absence_data.row,]
absence_data$COMMON_NME=NULL
absence_data$RECORD_TYPE=NULL
absence_data[,1]='low reliability'

names(species_data)[11]=names(absence_data)[11]
data = rbind(species_data,absence_data)
#write.csv(data,'common beard-heath modelling data.csv')

#######################
#######################
#modelling and testing
# set 70% to the train set and 30% to test set
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
train$Line = NULL
train$forest=NULL
train$public_land=NULL
train$ridge=NULL
train$ibra_hex.tif=NULL


test$Point=NULL
test$Line = NULL
test$forest=NULL
test$public_land=NULL
test$ridge=NULL
test$ibra_hex.tif=NULL
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
#set the training set to 80%
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

train$Line=NULL
train$forest=NULL
train$public_land=NULL
train$ridge=NULL
train$ecoregion2014.tif = NULL

test$Line=NULL
test$forest=NULL
test$public_land=NULL
test$ridge=NULL
test$ecoregion2014.tif = NULL
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
#set the trainset to 90%
train.row = sample(1:nrow(data), 0.9*nrow(data))  
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

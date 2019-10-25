# this file implement modelling of common beard-heat.
# data1 file 
# 2/19/2019
# author: Ken Lin
# version: 2


# initially clean the working platform to avoid confliction
# initially setting working environment
# initially import data
rm(list = ls())
#setwd("C:/Users/new/Desktop/3164_project_R/Agile Antechinus")
library('openxlsx')
species_data= read.csv('Agile Antechinus cluster data.csv', header = TRUE)
species_data$X=NULL
species_data$LATITUDEDD_NUM=NULL
species_data$LONGITUDEDD_NUM=NULL
species_data[species_data$RELIABILITY_TXT =='High reliability',1] ='high reliability'
# generate pesudo absence data
data=read.xlsx('complete_data.xlsx')
data[,c(1,2,3,5,6,7,9,11,15,16,17,20,21,22,23,24,25,26,27)]=NULL





data$bay=NULL

data$bua= NULL
data$island=NULL
data$island_marine=NULL

data$lga=NULL
data$locality=NULL
data$mainland=NULL

data$named_natural_region=NULL

data$postcode=NULL

data$sea=NULL

data$wb_lake=NULL
data$wb_lake_salt=NULL
data$wetland_swamp=NULL
data$LATITUDEDD_NUM=NULL
data$LONGITUDEDD_NUM=NULL
data$LAT_LONG_ACCURACYDD_INT=NULL
data$PRIMARY_CDE=NULL
data = na.omit(data)
set.seed(28339797)
data = data[data$COMMON_NME!='Agile Antechinus',]
data$COMMON_NME=NULL

absence_data.row = sample(1:nrow(data), 0.8*nrow(data))  
absence_data= data[-absence_data.row,]
absence_data[,1]='low reliability'

names(species_data)[11]=names(absence_data)[11]

names(species_data)[12]=names(absence_data)[12]
data = rbind(species_data,absence_data)
levels(data$RELIABILITY_TXT)[1]='High reliability'
data$RELIABILITY_TXT=as.factor(data$RELIABILITY_TXT)
#write.csv(data,'Agile_Antechinus.csv')


# aim: This file carry an test of the angile antechinus data
# Input: A csv file that contains all the instance about the agile antechinus with data processing.
# Output: this file will return all the test reault using comfusion matrix and also we can see the accracy of each model
# param: all the local parameters will been discussed later.

#######################
#######################
#modelling and testing
train.row = sample(1:nrow(data), 0.7*nrow(data))  
train=data[train.row,]
test= data[-train.row,]
#import library for the model
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

# delet the columns with less importance
train$Point_Distance=NULL
train$vegtpye3_4.tif = NULL
train$ridge = NULL
train$land_cov_use3.tif = NULL

test$Point_Distance=NULL
test$vegtpye3_4.tif = NULL
test$ridge = NULL
test$land_cov_use3.tif = NULL
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

# delet the columns with less importance
train$Point_Distance=NULL
train$vegtpye3_4.tif = NULL
train$land_cov_use3.tif = NULL
train$Point = NULL
train$ridge = NULL

test$Point_Distance=NULL
test$vegtpye3_4.tif = NULL
test$land_cov_use3.tif = NULL
test$Point = NULL
test$ridge = NULL


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

#set the training set to 90%
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



# delete the columns with less importance
train$Point_Distance=NULL
train$vegtpye3_4.tif = NULL
train$ridge = NULL

test$Point_Distance=NULL
test$vegtpye3_4.tif = NULL
test$ridge = NULL

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

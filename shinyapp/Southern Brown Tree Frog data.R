# this file implement data modelling
# data1 file  
# 8/10/2019 
# author: YinanKang, Ken Lin 
# approved by: YinanKang, Ken Lin 
# maintenance by:YinanKang, Ken Lin 
# version: 5 

# aim: This file carry an data modelling proccess of the Southern Brown Tree Frog data to a restricted format 
# Input: A csv file that contains all the instance about the Southern Brown Tree Frog. 
# Output: this file will return importance of the predictors, the reliability of the new observation and the distribution map 
# param: all the local parameters will been discussed later.  
# initially clean the working platform to avoid confliction 
# initially import data 


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

library(randomForest)

# attach train data set for model
attach(train)
#set the column as factor 
train$RELIABILITY_TXT = factor(train$RELIABILITY_TXT)
# model for random fprest
rf_model = randomForest(RELIABILITY_TXT~., data= train,  ntree =500,type='classification')
detach(train)
attach(test)
detach(test)

#read the input file
test1 = read.csv('test4.csv',header = TRUE)
test1 = as.data.frame(test1)
# store the latitude and longtitude 
lat = as.data.frame(test1$LATITUDEDD_NUM)
log = as.data.frame(test1$LONGITUDEDD_NUM)
# delete some columns to fit the model
test1$LONGITUDEDD_NUM=NULL
test1$LATITUDEDD_NUM=NULL
test1$X=NULL
# match the format of the new observtion with the train set
names(test1)[12]=names(train)[12]
test1 = rbind(train[1,],test1)
levels(test1$RELIABILITY_TXT) = levels(train$RELIABILITY_TXT)
#make the prediction for the new observation
rf_test= as.data.frame(predict(rf_model,test1,na.action=na.roughfix))
rf_test = rf_test[-length(rf_test),]
# combine the lantitude and longtitude with the reliability
lat = cbind(lat,log)
lat = cbind(lat,rf_test)
# change the name of the column
names(lat)[1] = 'Lantituded'
names(lat)[2] = 'Longitude'
names(lat)[3] = 'Reliability'
rf_test_3 = lat
# output the importance
important = as.data.frame(rf_model$importance)
rank = rank(important)
sort=  sort(rf_model$importance,decreasing  = TRUE)


# rank the importance and rename the columns
importance = as.data.frame(names(test1)[unlist(rank)])
importance = cbind(importance,sort)
names(importance)[1] = 'Importance Rank'
names(importance)[2] = 'Importance value'

importance = importance[importance$`Importance Rank` != 'Reliability',]
# select top 5 predictors
importance_3 = importance[c(1:5),]

# this file implement data modelling
# data1 file  
# 8/10/2019 
# author: YinanKang, Ken Lin 
# approved by: YinanKang, Ken Lin 
# maintenance by:YinanKang, Ken Lin 
# version: 5 

# aim: This file carry an data modelling proccess of the common beard heat data to a restricted format 
# Input: A csv file that contains all the instance about the common beard heat. 
# Output: this file will return importance of the predictors, the reliability of the new observation and the distribution map 
# param: all the local parameters will been discussed later.  
# initially clean the working platform to avoid confliction 
# initially import data 


rm(list = ls())

library('openxlsx')
species_data= read.csv('common beard-heath cluster data.csv', header = TRUE)
species_data$X=NULL
species_data$lan=NULL
species_data$log=NULL
# generate pesudo absence data
data=read.xlsx('complete_data.xlsx')
data[,c(1,2,3,5,6,7,9,11,15,16,17,20,21,22,23,24,25,26,27)]=NULL

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
train.row = sample(1:nrow(data), 0.7*nrow(data))  
train=data[train.row,]
test= data[-train.row,]
# attach detach the data set  
library(tree)
library(randomForest)
attach(train)
# build the model
rf_model = randomForest(RELIABILITY_TXT~., data= train,  ntree =500,type='classification')
detach(train)
attach(test)
detach(test)



#read the input file
test1 = read.csv('test3.csv',header = TRUE)
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
rf_test_2 = lat
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
importance_2 = importance[c(1:5),]

# this file implement data processing including data imputation.
# data1 file 
# 8/10/2019
# author: Ken Lin
# approved by: Ken Lin
# maintenance by: Ken Lin
# version: 5

# aim: This file carry an data processing proccess of the Common Beard-heath data to a restricted format
# Input: A csv file that contains all the instance about the agile antechinus.
# Output: this file will return an csv file that including all the instance of agile antechinus in a processed format
# param: all the local parameters will been discussed later. 

# initially clean the working platform to avoid confliction
# initially setting working environment
# initially import data
rm(list = ls())
setwd("C:/Users/new/Desktop/3164_project_R/common Beard-heath")
library('openxlsx')
data= read.xlsx('Common Beard-heath.xlsx', colNames = TRUE)

## data clean
## in this step I mainly force on clean the attribute that almost have same values for example, the beard-heat will
## never appear at sea, but their is still a sea which have this species, so I assume that, we can set that instance as
## low reliability and remove this column
data$COMMON_NME=NULL
data[,5:7]=NULL
log = data$LONGITUDEDD_NUM
lan= data$LATITUDEDD_NUM
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
#overview of data
summary(data)

# data computation. in these process, the relability is filled by a cluster.
# what we do is using a cluster find two groups of data, and then, based on the characteristic of data, we mannually give a
# high/low reiliablity to our data
library(stats)
library(klaR)
# cluster computation
data[data$bua=='Y',1]='low reliability'
data$bua  = NULL
data[data$named_natural_region=='Y',1]='low reliability'
data$named_natural_region=NULL
data[data$postcode=='N',1]='low reliability'
data$postcode=NULL
# attach data with the columns name as the namespace therefore I can access data by simply type the name of the columns
attach(data)


# covert all the lable to digitals, for example, convert Yes to 1, No to 0 
# store data table 
cluster_data= data[,2:24]

# convert Y/N of Samplng Method DESC column to 0-4 to present data
cluster_data[cluster_data$SAMPLING_METHOD_DESC =='Quadrat',1]=0
cluster_data[cluster_data$SAMPLING_METHOD_DESC =='Species List for Defined Area',1]=1
cluster_data[cluster_data$SAMPLING_METHOD_DESC =='Incidental',1]=2
cluster_data[cluster_data$SAMPLING_METHOD_DESC =='Monitoring',1]=3
cluster_data[cluster_data$SAMPLING_METHOD_DESC =='Specimen',1]=4

# convert the column of Point
cluster_data[cluster_data$Point=='place',2]=0
cluster_data[cluster_data$Point=='airport',2]=1
cluster_data[cluster_data$Point=='mountain',2]=2
cluster_data[cluster_data$Point=='rail_station',2]=3

# convert the column of Line
cluster_data[cluster_data$Line=='road',4]=0
cluster_data[cluster_data$Line=='watercourse_channel',4]=1
cluster_data[cluster_data$Line=='watercourse_stream',4]=2
cluster_data[cluster_data$Line=='state_border',4]=3
cluster_data[cluster_data$Line=='watercourse_river',4]=4
cluster_data[cluster_data$Line=='coast',4]=5
cluster_data[cluster_data$Line=='railway',4]=6

# convert column of forest
cluster_data[cluster_data$forest=='Y',6]= 1
cluster_data[cluster_data$forest=='N',6]= 0

cluster_data[cluster_data$public_land=='N',7]= 0
cluster_data[cluster_data$public_land=='Y',7]= 1

cluster_data[cluster_data$ridge=='N',8]= 0
cluster_data[cluster_data$forest=='Y',8]= 1

cluster_data[cluster_data$vicgov_region=='Y',9]= 1
cluster_data[cluster_data$vicgov_region=='N',9]= 0


a=kmodes(cluster_data, 2)
cluster_label= as.data.frame(a$cluster)


cluster_table=  cbind(cluster_label,data)
cluster_table[cluster_table$`a$cluster`==1,2]='high reliability'
cluster_table[cluster_table$`a$cluster`==2,2]='low reliability'
cluster_table$`a$cluster`=NULL
cluster_table = cbind(cluster_table,log)
cluster_table=cbind(cluster_table,lan)
write.csv(cluster_table, 'common beard-heath cluster data.csv')
detach(data)


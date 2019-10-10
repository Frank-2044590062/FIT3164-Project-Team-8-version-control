# this file implement data processing including data imputation.
# data1 file 
# 8/10/2019
# author: Ken Lin
# approved by: Ken Lin
# maintenance by: Ken Lin
# version: 4

# aim: This file carry an data processing proccess of the angile antechinus data to a restricted format
# Input: A csv file that contains all the instance about the agile antechinus.
# Output: this file will return an csv file that including all the instance of agile antechinus in a processed format
# param: all the local parameters will been discussed later. 

# initially clean the working platform to avoid confliction
# initially setting working environment
# initially import data
rm(list = ls())
setwd("C:/Users/new/Desktop/3164_project_R/Agile Antechinus")
library('openxlsx')
data= read.xlsx('Agile Antechinus.xlsx', colNames = TRUE)


# data clean

## in this step I mainly force on clean the attribute that almost have same values for example, the beard-heat will
## never appear at sea, but their is still a sea which have this species, so I assume that, we can set that instance as
## low reliability and remove this column
## Also, data cleaning and preliminary data computation
data$COMMON_NME=NULL
data[,c(5,7)]=NULL
# remove NA 
data = data[-(which(is.na(data$RECORD_TYPE))),]
data$bay=NULL
# set low reliability instances
data[data$bua=='Y',1]= 'low reliability'
data$bua= NULL
data$island=NULL
data$island_marine=NULL
data[data$lga=='N',1] =  'low reliability'
data$lga=NULL
data$locality=NULL
data$mainland=NULL
data[data$named_natural_region=='Y',1]= 'low reliability'
data$named_natural_region=NULL
data[data$postcode=='N',1] = 'low reliability'
data$postcode=NULL
data$sea=NULL
data[data$wb_lake=='Y',1]= 'low reliability'
data[data$wb_lake_salt=='Y',1]= 'low reliability'
data$wb_lake=NULL
data$wb_lake_salt=NULL
data$wetland_swamp=NULL
#overview of data to check the information of data
summary(data)

# data computation. in these process, the relability is filled by a cluster.
# what we do is using a cluster find two groups of data, and then, based on the characteristic of data, we mannually give a
# high/low reiliablity to our data
library(stats)

# data computation
# select na which going to be filled by cluter model.
na_data = data[which(is.na(data$RELIABILITY_TXT)),]
complete_data = data[-which(is.na(data$RELIABILITY_TXT)),]
# store the position of data for the future virtualization but do not adopt in model.
na_log = as.data.frame(na_data$LONGITUDEDD_NUM)
na_lan= as.data.frame(na_data$LATITUDEDD_NUM)
na_data$LATITUDEDD_NUM=NULL
na_data$LONGITUDEDD_NUM=NULL

# covert all the lable to digitals, for example, convert Yes to 1, No to 0 
# store data table
cluster_data= na_data[,2:24]  

# convert data in Point table to 0-3 helps model do an fast execution.
cluster_data[cluster_data$Point=='place',3]=0
cluster_data[cluster_data$Point=='airport',3]=1
cluster_data[cluster_data$Point=='mountain',3]=2
cluster_data[cluster_data$Point=='rail_station',3]=3

# convert column Line
cluster_data[cluster_data$Line=='road',4]=0
cluster_data[cluster_data$Line=='watercourse_channel',5]=1
cluster_data[cluster_data$Line=='watercourse_stream',5]=2
cluster_data[cluster_data$Line=='state_border',5]=3
cluster_data[cluster_data$Line=='watercourse_river',5]=4
cluster_data[cluster_data$Line=='coast',5]=5
cluster_data[cluster_data$Line=='railway',5]=6
cluster_data[cluster_data$Line=='state_border',5]=7
cluster_data[cluster_data$Line=='rail_',5]=7

# convert forest column
cluster_data[cluster_data$forest=='Y',7]= 1
cluster_data[cluster_data$forest=='N',7]= 0

# convert column of Public Land
cluster_data[cluster_data$public_land=='N',8]= 0
cluster_data[cluster_data$public_land=='Y',8]= 1
# convert column of ridge
cluster_data[cluster_data$ridge=='N',9]= 0
cluster_data[cluster_data$ridge=='Y',9]= 1
# convert column of vicgov_region
cluster_data[cluster_data$vicgov_region=='Y',10]= 1
cluster_data[cluster_data$vicgov_region=='N',10]= 0

##################
# after clean data, I takes an action on data computation for missing data
# k modes function is been adopted as there are some characteristic attribute
# then combinle with the observation, we can tell which group been classified is proved to be high reliability or low reliability
library(klaR)
a=kmodes(cluster_data, 2)
cluster_label= as.data.frame(a$cluster)
cluster_table=  cbind(cluster_label,na_data)
cluster_table[cluster_table$`a$cluster`==1,2]='high reliability'
cluster_table[cluster_table$`a$cluster`==2,2]='low reliability'
cluster_table$`a$cluster`=NULL
cluster_table= cbind(cluster_table,na_lan)
cluster_table=cbind(cluster_table,na_log)
# save position information
names(cluster_table)[28]= 'LATITUDEDD_NUM'
names(cluster_table)[29]= 'LONGITUDEDD_NUM'

LATITUDEDD_NUM =complete_data$LATITUDEDD_NUM
LONGITUDEDD_NUM = complete_data$LONGITUDEDD_NUM

complete_data$LATITUDEDD_NUM =NULL
complete_data$LONGITUDEDD_NUM=NULL
complete_data= cbind(complete_data,LATITUDEDD_NUM)
complete_data= cbind(complete_data,LONGITUDEDD_NUM)
# conbine data together
total_data= rbind(complete_data,cluster_table)
# save data to csv file
write.csv(total_data, 'Agile Antechinus cluster data.csv')



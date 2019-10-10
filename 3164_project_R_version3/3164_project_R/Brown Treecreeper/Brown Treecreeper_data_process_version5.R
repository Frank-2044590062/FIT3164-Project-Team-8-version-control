# this file implement data processing including data imputation.
# data1 file 
# 8/10/2019
# author: Ken Lin
# approved by: Ken Lin
# maintenance by: Ken Lin
# version: 5

# aim: This file carry an data processing proccess of the Brown Treecreeper data to a restricted format
# Input: A csv file that contains all the instance about the agile antechinus.
# Output: this file will return an csv file that including all the instance of agile antechinus in a processed format
# param: all the local parameters will been discussed later. 

# initially clean the working platform to avoid confliction
# initially setting working environment
# initially import data
rm(list = ls())
setwd("C:/Users/new/Desktop/3164_project_R/Brown Treecreeper")
library('openxlsx')
data= read.xlsx('complete_data.xlsx', colNames = TRUE)

# data clean

## in this step I mainly force on clean the attribute that almost have same values for example, the beard-heat will
## never appear at sea, but their is still a sea which have this species, so I assume that, we can set that instance as
## low reliability and remove this column
## Also, data cleaning and preliminary data computation
data=data[data$COMMON_NME=='Brown Treecreeper',]
# reference to preliminary data clean file
data[,c(1,2,3,5,9,11,14,15,16,17,19,20:27)]=NULL
data$RELIABILITY_label= 0
data=na.omit(data)
# setting intial realiability by combining of RELIABILITY column, Reliability Txt
# and Rating INT columns to define the obvious instances.
data[data$RELIABILITY=='Acceptable' & data$RATING_INT==0 & data$RELIABILITY_TXT == 'High reliability',46] = 'High reliability'
data[data$RELIABILITY!='Acceptable' | data$RATING_INT!=0 | data$RELIABILITY_TXT != 'High reliability',46] = 'low reliability'
data[,c(1:4)]=NULL


## data clean
## in this step I mainly force on clean the attribute that almost have same values for example, the Brown Treecreeper will
## never appear at sea, but their is still a sea which have this species, so I assume that, we can set that instance as
## low reliability and remove this column
data$bay=NULL
data[data$bua=='Y',41]='low reliability'
data$bua  = NULL
data$island=NULL
data$island_marine=NULL
data$lga=NULL
data$locality=NULL
data$mainland=NULL
data$sea=NULL
data[data$wb_lak=='Y', 34]='low reliability'
data$wb_lake=NULL
data$wb_lake_salt=NULL
data[data$wetland_swamp =='Y',32] = 'low reliablity'
data$wetland_swamp=NULL
data[data$named_natural_region=='Y',31]='low reliablity'
data$named_natural_region=NULL
data[data$postcode =='N',30]='low reliability'
data$postcode=NULL
data$ridge=NULL
data$vicgov_region=NULL



#overview of data
summary(data)

# data computation. in these process, the relability is filled by a cluster.
# what we do is using a cluster find two groups of data, and then, based on the characteristic of data, we mannually give a
# high/low reiliablity to our data

# store position for future adoption
log = data$LONGITUDEDD_NUM
lan= data$LATITUDEDD_NUM
data$LATITUDEDD_NUM=NULL
data$LONGITUDEDD_NUM=NULL
library(stats)
library(klaR)

# cluster computation

# attach data with the columns name as the namespace therefore I can access data by simply type the name of the columns
attach(data)

# covert all the lable to digitals, for example, convert Yes to 1, No to 0 
# store data table
cluster_data= data[,1:24]

# the kmodes model is been adopted in this clustering.
# kmodes model takes the data frame that contain brown treecreeper as the input and then classify into two groups including labels.
a=kmodes(cluster_data, 2)

# store the cluster result as a data frame for future conbine with
cluster_label= as.data.frame(a$cluster)

# conbine data with classification
cluster_table=  cbind(data,cluster_label)
cluster_table[cluster_table$`a$cluster`==1,26]='high reliability'
cluster_table[cluster_table$`a$cluster`==2,26]='low reliability'
cluster_table$RELIABILITY_label=NULL
names(cluster_table)[25]='Reliability'
cluster_table = cbind(cluster_table,log)
cluster_table=cbind(cluster_table,lan)
# save data to csv
write.csv(cluster_table, 'Brown Treecreeper data cluster.csv')
detach(data)


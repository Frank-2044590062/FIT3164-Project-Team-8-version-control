# this file implement data processing including data imputation.
# data1 file 
# 8/10/2019
# author: Ken Lin
# approved by: Ken Lin
# maintenance by: Ken Lin
# version: 5

# aim: This file carry an data processing proccess of the southern Brown Tree Frog data to a restricted format
# Input: A csv file that contains all the instance about the agile antechinus.
# Output: this file will return an csv file that including all the instance of agile antechinus in a processed format
# param: all the local parameters will been discussed later. 

# initially clean the working platform to avoid confliction
# initially setting working environment
# initially import data


rm(list = ls())
setwd("C:/Users/new/Desktop/3164_project_R/white-browed treecreeper")
library('openxlsx')
data= read.xlsx('White-browed Treecreeper.xlsx', colNames = TRUE)


## store position

## data clean
## in this step I mainly force on clean the attribute that almost have same values for example, the beard-heat will
## never appear at sea, but their is still a sea which have this species, so I assume that, we can set that instance as
## low reliability and remove this column
data$COMMON_NME=NULL
data[,5:7]=NULL
data$bay=NULL
data$bua=NULL
data$island=NULL
data$island_marine=NULL
data$lga=NULL
data$locality=NULL
data$mainland=NULL
data$sea=NULL
data$postode=NULL
data$ridge=NULL
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
# preliminary computation
log = data$LONGITUDEDD_NUM
lan= data$LATITUDEDD_NUM
data$LATITUDEDD_NUM=NULL
data$LONGITUDEDD_NUM=NULL
data$RELIABILITY_TXT=NULL

attach(data)
# covert all the lable to digitals, for example, convert Yes to 1, No to 0 
# as the 
cluster_data= data
a=kmodes(cluster_data, 2)
cluster_label= as.data.frame(a$cluster)


cluster_table=  cbind(cluster_label,data)
cluster_table[cluster_table$`a$cluster`==1,1]='high reliability'
cluster_table[cluster_table$`a$cluster`==2,1]='low reliability'
names(cluster_table)[1]='Reliability'
cluster_table = cbind(cluster_table,log)
cluster_table=cbind(cluster_table,lan)
write.csv(cluster_table, 'White-browed Treecreeper cluster data.csv')
detach(data)


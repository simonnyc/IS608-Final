################# This program has four major sections: ############################################################## 
# Author: Mohamed Elmoudni                                                                                           #
# IS 608 - Knowledge and Visual Analytics Final Project Spring 2016                                                  #
#                                                                                                                    #
#                                                                                                                    #
# 1- program to prepare the data for the worst scored restaurants.                                                   #
#                                                                                                                    #
# 2- Trend Analysis for SUBWAY sandwich shop from 2013 to 2016.. Please note this section was submitted in hw6 only. #
#                                                                                                                    #
# 3- Trend analysis: for the coffee shops data over the years.                                                       #
#    The five restaurants are (Starbucks, Gregory's, Dunkin donuts, the Coffee Bean Tea and Leaf, and #Piccolo Café. #
#                                                                                                                    #
# 4 -Mapping data preparation for the 50 best and worst restaurants in 2016                                          #
# 5 -data preparation for data selection by neighborhood and cuisine type.                                           #
#                                                                                                                    #
#                                                                                                                    #
######################################################################################################################


# 1- program to prepare the data for the worst scored restaurants. 
library(dplyr)
library(stringr)

## github project data
## https://github.com/simonnyc/IS608-NYC-data/

Manhattan_1900<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_1900.csv",head=TRUE,sep=",")
Manhattan_2010<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2010.csv",head=TRUE,sep=",")
Manhattan_2011<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2011.csv",head=TRUE,sep=",")
Manhattan_2012<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2012.csv",head=TRUE,sep=",")
Manhattan_2013<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2013.csv",head=TRUE,sep=",")
Manhattan_2014<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2014.csv",head=TRUE,sep=",")
Manhattan_2015<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2015.csv",head=TRUE,sep=",")
Manhattan_2016<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2016.csv",head=TRUE,sep=",")
###

library(dplyr)
library(stringr)
Manhattan_2016<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2016.csv",head=TRUE,sep=",")
m16<- Manhattan_2016
#dim(m16)
# group data
by_res<- group_by(m16, INSPECTION.DATE, DBA, SCORE, GRADE, CRITICAL.FLAG)

# renaming DBA(doing business as usual) to restaurant
by_res<- rename(by_res, RESTAURANT = DBA)

# Count voilations
res_count<- summarise(by_res, count = n())

# get only critical voilation
res_critical_count<- filter(res_count, CRITICAL.FLAG=='Critical')

# get worst scored restaurants with violation count higher than 5 critical violations.
res_critical_count<- arrange(filter(res_critical_count, SCORE>27, count>5), count)
# order the data 
res_critical_count<- res_critical_count[order(desc(res_critical_count$SCORE)),] 

#select relevant summary data for D3 Vis
D3_rest<- select(res_critical_count, RESTAURANT, SCORE, count)
# D3 data
write.csv(D3_rest, file = "C:/CUNY/Courses/IS608/Assignments/Assignment6/extra/data/D3_assign06.csv",row.names = FALSE)

#github link
#https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/D3_assign06_sub.csv

# select relevant summary data for Coogle Vis 
Gchart2_assign06 <- select(res_critical_count, GRADE,SCORE,count,RESTAURANT)
Gchart2_assign06 <- data.frame('GRADE'= res_critical_count$GRADE, 'SCORE'= res_critical_count$SCORE, 'count' =res_critical_count$count, 
                               'RETAURANT' = res_critical_count$RESTAURANT)
write.csv(D3_rest, file = "C:/CUNY/Courses/IS608/Assignments/Assignment6/extra/data/Gchart2_assign06.csv",row.names = FALSE)

#github link
#https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/Gchart2_assign06.csv

########################################################################################################################
# 2- Trend Analysis for SUBWAY sandwich shop from 2013 to 2016.. Please note this section submitted in hw6 only.       #
#                                                                                                                      #
# Trend Analysis                                                                                                       #
# In this example, we are selecting one the worst scored restaurant and see how it performed in the past years...      #
# We selected SUBWAY sandwich shop as it scored worst in 2016 and has data back to 2013..                              #
#                                                                                                                      #
#########################################################################################################################

# get the data for the SUBWAY sandwich shop 
m16_sub<- filter(Manhattan_2016, DBA=='SUBWAY', GRADE!='A', INSPECTION.TYPE!='Cycle Inspection / Initial Inspection', CRITICAL.FLAG=='Critical')
m15_sub<- filter(Manhattan_2015, DBA=='SUBWAY', GRADE!='A', INSPECTION.TYPE!='Cycle Inspection / Initial Inspection', CRITICAL.FLAG=='Critical')
m14_sub<- filter(Manhattan_2014, DBA=='SUBWAY', GRADE!='A', INSPECTION.TYPE!='Cycle Inspection / Initial Inspection', CRITICAL.FLAG=='Critical')
m13_sub<- filter(Manhattan_2013, DBA=='SUBWAY', GRADE!='A', INSPECTION.TYPE!='Cycle Inspection / Initial Inspection', CRITICAL.FLAG=='Critical')
m12_sub<- filter(Manhattan_2012, DBA=='SUBWAY', GRADE!='A', INSPECTION.TYPE!='Cycle Inspection / Initial Inspection', CRITICAL.FLAG=='Critical')
m11_sub<- filter(Manhattan_2011, DBA=='SUBWAY', GRADE!='A', INSPECTION.TYPE!='Cycle Inspection / Initial Inspection', CRITICAL.FLAG=='Critical')
m10_sub<- filter(Manhattan_2010, DBA=='SUBWAY', GRADE!='A', INSPECTION.TYPE!='Cycle Inspection / Initial Inspection', CRITICAL.FLAG=='Critical')
###
# Clean the data 
# clean data 2016
m16_sub <- m16_sub[complete.cases(m16_sub),]
m16_sub$CUISINE.DESCRIPTION<- gsub("[^0-9A-Za-z///' ]", "",m16_sub$CUISINE.DESCRIPTION)
m16_sub$DBA<- gsub("[^0-9A-Za-z///' ]", "",m16_sub$DBA)
# Finding and removing Outliers
library(outliers)
outlier_tf = outlier(m16_sub$SCORE,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
data_new = m16_sub[-find_outlier,]
#summary(m16_sub$SCORE)
#finding Duplicate rows 
m16_sub<- unique(m16_sub)

# clean data 2015
m15_sub <- m15_sub[complete.cases(m15_sub),]
m15_sub$CUISINE.DESCRIPTION<- gsub("[^0-9A-Za-z///' ]", "",m15_sub$CUISINE.DESCRIPTION)
m15_sub$DBA<- gsub("[^0-9A-Za-z///' ]", "",m15_sub$DBA)
# Finding and removing Outliers
library(outliers)
outlier_tf = outlier(m15_sub$SCORE,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
data_new = m15_sub[-find_outlier,]
#summary(m15_sub$SCORE)
#finding Duplicate rows 
m15_sub<- unique(m15_sub)


# clean data 2014
m14_sub <- m14_sub[complete.cases(m14_sub),]
m14_sub$CUISINE.DESCRIPTION<- gsub("[^0-9A-Za-z///' ]", "",m14_sub$CUISINE.DESCRIPTION)
m14_sub$DBA<- gsub("[^0-9A-Za-z///' ]", "",m14_sub$DBA)
# Finding and removing Outliers
library(outliers)
outlier_tf = outlier(m14_sub$SCORE,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
data_new = m14_sub[-find_outlier,]
#summary(m14_sub$SCORE)
#finding Duplicate rows 
m14_sub<- unique(m14_sub)


# clean data 2013
m13_sub <- m13_sub[complete.cases(m13_sub),]
m13_sub$CUISINE.DESCRIPTION<- gsub("[^0-9A-Za-z///' ]", "",m13_sub$CUISINE.DESCRIPTION)
m13_sub$DBA<- gsub("[^0-9A-Za-z///' ]", "",m13_sub$DBA)
# Finding and removing Outliers
library(outliers)
outlier_tf = outlier(m13_sub$SCORE,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
data_new = m13_sub[-find_outlier,]
#summary(m13_sub$SCORE)
#finding Duplicate rows 
m13_sub<- unique(m13_sub)



###########
# group the data by date
m16_sub_bydate<- group_by(m16_sub, DBA, GRADE, SCORE, INSPECTION.DATE)
m15_sub_bydate<- group_by(m15_sub, DBA, GRADE, SCORE, INSPECTION.DATE)
m14_sub_bydate<- group_by(m14_sub, DBA, GRADE, SCORE, INSPECTION.DATE)
m13_sub_bydate<- group_by(m13_sub, DBA, GRADE, SCORE, INSPECTION.DATE)

# renaming DBA(doing business as usual) to restaurant
m16_sub_bydate<- rename(m16_sub_bydate, RESTAURANT = DBA)
m15_sub_bydate<- rename(m15_sub_bydate, RESTAURANT = DBA)
m14_sub_bydate<- rename(m14_sub_bydate, RESTAURANT = DBA)
m13_sub_bydate<- rename(m13_sub_bydate, RESTAURANT = DBA)

# summarise data 
m16_sub_bydate<- summarise(m16_sub_bydate, count = n())
m15_sub_bydate<- summarise(m15_sub_bydate, count = n())
m14_sub_bydate<- summarise(m14_sub_bydate, count = n())
m13_sub_bydate<- summarise(m13_sub_bydate, count = n())

# combine dfata from years 2016,2015,v,2014,2013.  years 2012-2010 had no data for SUBWAY 
sub_all_years<- rbind(m15_sub_bydate, m16_sub_bydate, m14_sub_bydate, m13_sub_bydate)

# preparing data for D3 Vis
D3_assignment06 <- data.frame('YEAR'=sub_all_years$INSPECTION.DATE,
                              'RESTAURANT'= sub_all_years$RESTAURANT, 
                              'GRADE' = sub_all_years$GRADE,
                              'SCORE'= sub_all_years$SCORE,
                              'count' = sub_all_years$count
)

write.csv(D3_assignment06, file = "C:/CUNY/Courses/IS608/Assignments/Assignment6/extra/data/D3_assign06_sub.csv",row.names = FALSE)

#githun link 
#https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/D3_assign06_sub.csv

######################################################################################################################
# 3- Trend analysis: for the coffee shops data over the years.                                                       #
#    The five restaurants are (Starbucks, Gregory's, Dunkin donuts, the Coffee Bean Tea and Leaf, and #Piccolo Café. #
#                                                                                                                    #
#This program prepares the coffee shops data over the years.                                                         #
#The five restaurants are (Starbucks, Gregory's, Dunkin donuts, the Coffee Bean Tea and Leaf, and #Piccolo Café.     #
#                                                                                                                    #
# For Google Chart Coffee shops 2012-2016                                                                            #  
######################################################################################################################

library(dplyr)
library(stringr)

## github project data
## https://github.com/simonnyc/IS608-NYC-data/

Manhattan_1900<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_1900.csv",head=TRUE,sep=",")
Manhattan_2010<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2010.csv",head=TRUE,sep=",")
Manhattan_2011<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2011.csv",head=TRUE,sep=",")
Manhattan_2012<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2012.csv",head=TRUE,sep=",")
Manhattan_2013<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2013.csv",head=TRUE,sep=",")
Manhattan_2014<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2014.csv",head=TRUE,sep=",")
Manhattan_2015<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2015.csv",head=TRUE,sep=",")
Manhattan_2016<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2016.csv",head=TRUE,sep=",")

########  2016  ########
Manhattan_2016<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2016.csv",head=TRUE,sep=",")
m16<- Manhattan_2016


# clean data
m16 <- m16[complete.cases(m16),]
m16$CUISINE.DESCRIPTION<- gsub("[^0-9A-Za-z///' ]", "",m16$CUISINE.DESCRIPTION)
m16$DBA<- gsub("[^0-9A-Za-z///' ]", "",m16$DBA)

# Finding and removing Outliers
library(outliers)
outlier_tf = outlier(m16$SCORE,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
data_new = m16[-find_outlier,]
#summary(m16$SCORE)


#finding Duplicate rows 
m16<- unique(m16)

# summary data

# dis-regarding rows from initial inspections as grades and scores are not counted
m16<- filter(m16, INSPECTION.TYPE!='Cycle Inspection / Initial Inspection')

################
m16_critical<- filter(m16, CRITICAL.FLAG=='Critical')
dim(m16_critical)

#filter(m16_critical, DBA %in% "^STARBUCKS*")
STARBUCKS <- m16_critical[ grep("STARBUCKS", m16_critical$DBA) , ]
dim(STARBUCKS)
STARBUCKS_mean16<- round(mean(STARBUCKS$SCORE),2)
###############
DUNKIN.DONUTS <- m16_critical[ grep("DUNKIN*", m16_critical$DBA) , ]
dim(DUNKIN.DONUTS)
DUNKIN.DONUTS_mean16<- round(mean(DUNKIN.DONUTS$SCORE),2)
###############
PICCOLO.CAFE <- m16_critical[ grep("PICCOLO*", m16_critical$DBA) , ]
dim(PICCOLO.CAFE)
PICCOLO.CAFE_mean16<- round(mean(PICCOLO.CAFE$SCORE),2)
##################
THECOFFEE.BEAN.TEA.LEAF <- m16_critical[ grep("THECOFFEEBEANTEA*", str_replace_all(m16_critical$DBA, fixed(" "), "") ) , ]
dim(THECOFFEE.BEAN.TEA.LEAF)
THECOFFEE.BEAN.TEA.LEAF_mean16<- round(mean(THECOFFEE.BEAN.TEA.LEAF$SCORE),2)
###################
GREGORYS.COFFEE<- m16_critical[ grep("GREGORY'S COFFEE*", m16_critical$DBA) , ]
dim(GREGORYS.COFFEE)
GREGORYS.COFFEE_mean16<- round(mean(GREGORYS.COFFEE$SCORE),2)
###################

#####  2015  #####
# Assignment 06 data preparations
Manhattan_2015<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2015.csv",head=TRUE,sep=",")
m15<- Manhattan_2015


# clean data
m15 <- m15[complete.cases(m15),]
m15$CUISINE.DESCRIPTION<- gsub("[^0-9A-Za-z///' ]", "",m15$CUISINE.DESCRIPTION)
m15$DBA<- gsub("[^0-9A-Za-z///' ]", "",m15$DBA)

# Finding and removing Outliers
library(outliers)
outlier_tf = outlier(m15$SCORE,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
data_new = m15[-find_outlier,]
#summary(m15$SCORE)


#finding Duplicate rows 
m15<- unique(m15)

# summary data

# dis-regarding rows from initial inspections as grades and scores are not counted
m15<- filter(m15, INSPECTION.TYPE!='Cycle Inspection / Initial Inspection')

################
m15_critical<- filter(m15, CRITICAL.FLAG=='Critical')

dim(m15_critical)

#filter(m15_critical, DBA %in% "^STARBUCKS*")
STARBUCKS <- m15_critical[ grep("STARBUCKS", m15_critical$DBA) , ]
dim(STARBUCKS)
STARBUCKS_mean15<- round(mean(STARBUCKS$SCORE),2)
###############
DUNKIN.DONUTS <- m15_critical[ grep("DUNKIN*", m15_critical$DBA) , ]
dim(DUNKIN.DONUTS)
DUNKIN.DONUTS_mean15<- round(mean(DUNKIN.DONUTS$SCORE),2)
###############
PICCOLO.CAFE <- m15_critical[ grep("PICCOLO*", m15_critical$DBA) , ]
dim(PICCOLO.CAFE)
PICCOLO.CAFE_mean15<- round(mean(PICCOLO.CAFE$SCORE),2)
##################
THECOFFEE.BEAN.TEA.LEAF <- m15_critical[ grep("THECOFFEEBEANTEA*", str_replace_all(m15_critical$DBA, fixed(" "), "") ) , ]
dim(THECOFFEE.BEAN.TEA.LEAF)
THECOFFEE.BEAN.TEA.LEAF_mean15<- round(mean(THECOFFEE.BEAN.TEA.LEAF$SCORE),2)
###################
GREGORYS.COFFEE<- m15_critical[ grep("GREGORY'S COFFEE*", m15_critical$DBA) , ]
dim(GREGORYS.COFFEE)
GREGORYS.COFFEE_mean15<- round(mean(GREGORYS.COFFEE$SCORE),2)
###################

##### 2014 ##########
 
Manhattan_2014<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2014.csv",head=TRUE,sep=",")
m14<- Manhattan_2014


# clean data
m14 <- m14[complete.cases(m14),]
m14$CUISINE.DESCRIPTION<- gsub("[^0-9A-Za-z///' ]", "",m14$CUISINE.DESCRIPTION)
m14$DBA<- gsub("[^0-9A-Za-z///' ]", "",m14$DBA)

# Finding and removing Outliers
library(outliers)
outlier_tf = outlier(m14$SCORE,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
data_new = m14[-find_outlier,]
#summary(m14$SCORE)


#finding Duplicate rows 
m14<- unique(m14)

# summary data

# dis-regarding rows from initial inspections as grades and scores are not counted
m14<- filter(m14, INSPECTION.TYPE!='Cycle Inspection / Initial Inspection')

################
m14_critical<- filter(m14, CRITICAL.FLAG=='Critical')

dim(m14_critical)

#filter(m14_critical, DBA %in% "^STARBUCKS*")
STARBUCKS <- m14_critical[ grep("STARBUCKS", m14_critical$DBA) , ]
dim(STARBUCKS)
STARBUCKS_mean14<- round(mean(STARBUCKS$SCORE),2)
###############
DUNKIN.DONUTS <- m14_critical[ grep("DUNKIN*", m14_critical$DBA) , ]
dim(DUNKIN.DONUTS)
DUNKIN.DONUTS_mean14<- round(mean(DUNKIN.DONUTS$SCORE),2)
###############
PICCOLO.CAFE <- m14_critical[ grep("PICCOLO*", m14_critical$DBA) , ]
dim(PICCOLO.CAFE)
PICCOLO.CAFE_mean14<- round(mean(PICCOLO.CAFE$SCORE),2)
##################
THECOFFEE.BEAN.TEA.LEAF <- m14_critical[ grep("THECOFFEEBEANTEA*", str_replace_all(m14_critical$DBA, fixed(" "), "") ) , ]
dim(THECOFFEE.BEAN.TEA.LEAF)
THECOFFEE.BEAN.TEA.LEAF_mean14<- round(mean(THECOFFEE.BEAN.TEA.LEAF$SCORE),2)
###################
GREGORYS.COFFEE<- m14_critical[ grep("GREGORY'S COFFEE*", m14_critical$DBA) , ]
dim(GREGORYS.COFFEE)
GREGORYS.COFFEE_mean14<- round(mean(GREGORYS.COFFEE$SCORE),2)
###################

####  2013 ###

Manhattan_2013<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2013.csv",head=TRUE,sep=",")
m13<- Manhattan_2013


# clean data
m13 <- m13[complete.cases(m13),]
m13$CUISINE.DESCRIPTION<- gsub("[^0-9A-Za-z///' ]", "",m13$CUISINE.DESCRIPTION)
m13$DBA<- gsub("[^0-9A-Za-z///' ]", "",m13$DBA)

# Finding and removing Outliers
library(outliers)
outlier_tf = outlier(m13$SCORE,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
data_new = m13[-find_outlier,]
#summary(m13$SCORE)


#finding Duplicate rows 
m13<- unique(m13)

# summary data

# dis-regarding rows from initial inspections as grades and scores are not counted
m13<- filter(m13, INSPECTION.TYPE!='Cycle Inspection / Initial Inspection')

################
m13_critical<- filter(m13, CRITICAL.FLAG=='Critical')

dim(m13_critical)

#filter(m13_critical, DBA %in% "^STARBUCKS*")
STARBUCKS <- m13_critical[ grep("STARBUCKS", m13_critical$DBA) , ]
dim(STARBUCKS)
STARBUCKS_mean13<- round(mean(STARBUCKS$SCORE),2)
###############
DUNKIN.DONUTS <- m13_critical[ grep("DUNKIN*", m13_critical$DBA) , ]
dim(DUNKIN.DONUTS)
DUNKIN.DONUTS_mean13<- round(mean(DUNKIN.DONUTS$SCORE),2)
###############
PICCOLO.CAFE <- m13_critical[ grep("PICCOLO*", m13_critical$DBA) , ]
dim(PICCOLO.CAFE)
PICCOLO.CAFE_mean13<- round(mean(PICCOLO.CAFE$SCORE),2)
##################
THECOFFEE.BEAN.TEA.LEAF <- m13_critical[ grep("THECOFFEEBEANTEA*", str_replace_all(m13_critical$DBA, fixed(" "), "") ) , ]
dim(THECOFFEE.BEAN.TEA.LEAF)
THECOFFEE.BEAN.TEA.LEAF_mean13<- round(mean(THECOFFEE.BEAN.TEA.LEAF$SCORE),2)
###################
GREGORYS.COFFEE<- m13_critical[ grep("GREGORY'S COFFEE*", m13_critical$DBA) , ]
dim(GREGORYS.COFFEE)
GREGORYS.COFFEE_mean13<- round(mean(GREGORYS.COFFEE$SCORE),2)
###################


### 2012 ####

Manhattan_2012<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2012.csv",head=TRUE,sep=",")
m12<- Manhattan_2012


# clean data
m12<- m12[complete.cases(m12),]
m12$CUISINE.DESCRIPTION<- gsub("[^0-9A-Za-z///' ]", "",m12$CUISINE.DESCRIPTION)
m12$DBA<- gsub("[^0-9A-Za-z///' ]", "",m12$DBA)

# Finding and removing Outliers
library(outliers)
outlier_tf = outlier(m12$SCORE,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
data_new = m12[-find_outlier,]
#summary(m12$SCORE)


#finding Duplicate rows 
m12<- unique(m12)

# summary data

# dis-regarding rows from initial inspections as grades and scores are not counted
m12<- filter(m12, INSPECTION.TYPE!='Cycle Inspection / Initial Inspection')

################
m12_critical<- filter(m12, CRITICAL.FLAG=='Critical')

dim(m12_critical)

#filter(m12_critical, DBA %in% "STARBUCKS*")
STARBUCKS <- m12_critical[ grep("STARBUCKS", m12_critical$DBA) , ]
dim(STARBUCKS)
STARBUCKS_mean12<- round(mean(STARBUCKS$SCORE),2)
###############
DUNKIN.DONUTS <- m12_critical[ grep("DUNKIN*", m12_critical$DBA) , ]
dim(DUNKIN.DONUTS)
DUNKIN.DONUTS_mean12<- round(mean(DUNKIN.DONUTS$SCORE),2)
###############
PICCOLO.CAFE <- m12_critical[ grep("PICCOLO*", m12_critical$DBA) , ]
dim(PICCOLO.CAFE)
PICCOLO.CAFE_mean12<- round(mean(PICCOLO.CAFE$SCORE),2)
##################
THECOFFEE.BEAN.TEA.LEAF <- m12_critical[ grep("THECOFFEEBEANTEA*", str_replace_all(m12_critical$DBA, fixed(" "), "") ) , ]
dim(THECOFFEE.BEAN.TEA.LEAF)
THECOFFEE.BEAN.TEA.LEAF_mean12<- round(mean(THECOFFEE.BEAN.TEA.LEAF$SCORE),2)
###################
GREGORYS.COFFEE<-m12_critical[ grep("GREGORYS COFFEE", str_replace_all(m12_critical$DBA, fixed(" "), "") ) , ]
dim(GREGORYS.COFFEE)
GREGORYS.COFFEE_mean12<- round(mean(GREGORYS.COFFEE$SCORE),2)
###########################################
## Creating summary table 

STARBUCKS <- c(STARBUCKS_mean12,STARBUCKS_mean13,STARBUCKS_mean14,
               STARBUCKS_mean15, STARBUCKS_mean16)

DUNKIN.DONUTS <- c(DUNKIN.DONUTS_mean12, DUNKIN.DONUTS_mean13, DUNKIN.DONUTS_mean14,
                   DUNKIN.DONUTS_mean15,DUNKIN.DONUTS_mean16)

PICCOLO.CAFE<- c(PICCOLO.CAFE_mean12,PICCOLO.CAFE_mean13,PICCOLO.CAFE_mean14,
                 PICCOLO.CAFE_mean15,PICCOLO.CAFE_mean16)

THECOFFEE.BEAN.TEA.LEAF <- c(THECOFFEE.BEAN.TEA.LEAF_mean12,THECOFFEE.BEAN.TEA.LEAF_mean13,THECOFFEE.BEAN.TEA.LEAF_mean14,
                             THECOFFEE.BEAN.TEA.LEAF_mean15,THECOFFEE.BEAN.TEA.LEAF_mean16)
GREGORYS.COFFEE <- c(GREGORYS.COFFEE_mean12,GREGORYS.COFFEE_mean13,GREGORYS.COFFEE_mean14,
                     GREGORYS.COFFEE_mean15,GREGORYS.COFFEE_mean16)

Year<- c("2012", "2013", "2014","2015", "2016")


coffee_df<- data.frame(Year, STARBUCKS, DUNKIN.DONUTS,PICCOLO.CAFE,THECOFFEE.BEAN.TEA.LEAF,GREGORYS.COFFEE  )
mask <- apply(coffee_df, 2, is.na) 
coffee_df[mask] <-13.76

#coffee_df = do.call(data.frame,rapply(coffee_df, function(x) ifelse(is.na(x),0,x), how="replace"))


avg16<- mean(t(select(coffee_df, STARBUCKS:GREGORYS.COFFEE)[1,]))
avg15<- mean(t(select(coffee_df, STARBUCKS:GREGORYS.COFFEE)[2,]))
avg14<- mean(t(select(coffee_df, STARBUCKS:GREGORYS.COFFEE)[3,]))
avg13<- mean(t(select(coffee_df, STARBUCKS:GREGORYS.COFFEE)[4,]))
avg12<- mean(t(select(coffee_df, STARBUCKS:GREGORYS.COFFEE)[5,]))

##
library(plyr)
coffee_df <- rename( coffee_df,c("Year" = "Year", 
                                 "STARBUCKS" = "STARBUCKS", 
                                 "DUNKIN.DONUTS" = "DUNKIN DONUTS",
                                 "PICCOLO.CAFE"="PICCOLO CAFE", 
                                 "THECOFFEE.BEAN.TEA.LEAF"= "THE COFFEE BEAN TEA LEAF",
                                 "GREGORYS.COFFEE" = "GREGORYS COFFEE"))

coffee_df$Year<-as.character(coffee_df$Year)
coffee_df[, 'Year'] <- as.factor(coffee_df[, 'Year'])


##########


Average<- t(data.frame(avg16,avg15,avg14,avg13, avg12))
coffee_df$Average<- Average

write.csv(coffee_df, file = "C:/CUNY/Courses/IS608/Assignments/Assignment6/extra/MondayMay/coffee_Shops_df.csv",
          row.names = FALSE, quote = TRUE)

str(coffee_df)

######################################################################################################################
#                                                                                                                    #
# 4 -Mapping data preparation for the 50 best and worst restaurants in 2016                                          #
#                                                                                                                    #
######################################################################################################################
Manhattan_2016<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2016.csv",head=TRUE,sep=",")
m16<- Manhattan_2016

zipcodes<- read.csv("C:/CUNY/Courses/IS608/Final_Project/semesterFinal/Final-Wed/zipcodes2.csv",head=TRUE,sep=",")



# clean data
m16 <- m16[complete.cases(m16),]
m16$CUISINE.DESCRIPTION<- gsub("[^0-9A-Za-z///' ]", "",m16$CUISINE.DESCRIPTION)
m16$DBA<- gsub("[^0-9A-Za-z///' ]", "",m16$DBA)

# Finding and removing Outliers
library(outliers)
outlier_tf = outlier(m16$SCORE,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
data_new = m16[-find_outlier,]
#summary(m16$SCORE)


#finding Duplicate rows 
m16<- unique(m16)

library(dplyr)
library(stringr)
# summary data

# dis-regarding rows from initial inspections as grades and scores are not counted
m16<- filter(m16, INSPECTION.TYPE!='Cycle Inspection / Initial Inspection')
str(m16)
################
m16_worst<- filter(m16, CRITICAL.FLAG=='Critical')
m16_best<- filter(m16, CRITICAL.FLAG=='Not Critical')

###############
m16 <- m16_worst 

df16<- select(m16, DBA,BUILDING , STREET, ZIPCODE,CUISINE.DESCRIPTION,SCORE,GRADE)
df16<- rename(df16, RESTAURANT = DBA)
zipcodes<- rename(zipcodes, ZIPCODE =zipcode )
df16<- rename(df16, CUISINE =CUISINE.DESCRIPTION )
df16 <- (merge(zipcodes, df16, by = 'ZIPCODE'))
str(m16)
head(df16)


###############

df16_neighbrood<- group_by(df16, BUILDING , STREET, ZIPCODE, Neighborhood, RESTAURANT, CUISINE,SCORE)

df16_neighbrood_count<- summarise(df16_neighbrood, count = n())

vis<- df16_neighbrood_count[order(desc(df16_neighbrood_count$SCORE)),] 
vis<- arrange(filter(vis, SCORE>0), SCORE)
vis<- vis[order(desc(vis$SCORE)),] 
str(vis)


vis_df<- select(vis, 'address'= BUILDING,
                'street' = STREET,
                'zipcode' = ZIPCODE,
                Neighborhood,
                CUISINE, 
                RESTAURANT, 
                SCORE)

vis_df$state<- 'NY'
vis_df$city<- 'Manhattan'

worst50<- head(vis_df,50)
str(worst50)

worst50_lat_long<- read.csv("C:/CUNY/Courses/IS608/Final_Project/semesterFinal/Final_Frid/worstzips_FINAL.csv",head=TRUE,sep=",")
worst50_lat_long$name <- worst50$RESTAURANT
worst50_lat_long$Marker<- 'red'

write.csv(worst50_lat_long, file = "C:/CUNY/Courses/IS608/Final_Project/semesterFinal/Final_Frid/worst50_lat_long.csv",row.names = FALSE)

###### 

worst50_zips<- select(worst50, address, street,zipcode, city, state)

worst50_zips<- data.frame('address'= worst50$address, 
                          'street'= worst50$street,
                          'postal code'= worst50$zipcode, 
                          'city'= worst50$city, 
                          'state' = worst50$state)

write.csv(worst50_zips, file = "C:/CUNY/Courses/IS608/Final_Project/semesterFinal/Final-Thurs/worst50_zips.csv",row.names = FALSE)

worstzips_final<- read.csv("C:/CUNY/Courses/IS608/Final_Project/semesterFinal/Final-Thurs/Worstzips_withlatt_final.csv",head=TRUE,sep=",")

########
##BEST 

m16<- m16_best

df16<- select(m16, DBA,BUILDING , STREET, ZIPCODE,CUISINE.DESCRIPTION,SCORE,GRADE)
df16<- rename(df16, RESTAURANT = DBA)
#zipcodes<- rename(zipcodes, ZIPCODE =zipcode )
df16<- rename(df16, CUISINE =CUISINE.DESCRIPTION )
df16 <- (merge(zipcodes, df16, by = 'ZIPCODE'))
str(m16)
head(df16)

########

df16_neighbrood<- group_by(df16, BUILDING , STREET, ZIPCODE, Neighborhood, RESTAURANT, CUISINE,SCORE)

df16_neighbrood_count<- summarise(df16_neighbrood, count = n())

vis<- df16_neighbrood_count[order(desc(df16_neighbrood_count$SCORE)),] 
vis<- arrange(filter(vis, SCORE>0), SCORE)
vis<- vis[order(desc(vis$SCORE)),] 
str(vis)


vis_df<- select(vis, 'address'= BUILDING,
                'street' = STREET,
                'zipcode' = ZIPCODE,
                Neighborhood,
                CUISINE, 
                RESTAURANT, 
                SCORE)

vis_df$state<- 'NY'
vis_df$city<- 'Manhattan'

best50<- tail(vis_df,50)


best50_lat_long<- read.csv("C:/CUNY/Courses/IS608/Final_Project/semesterFinal/Final_Frid/best50_lat_long.csv",head=TRUE,sep=",")
best50_lat_long$name <- best50$RESTAURANT
best50_lat_long$Marker<- 'green'

write.csv(best50_lat_long, file = "C:/CUNY/Courses/IS608/Final_Project/semesterFinal/Final_Frid/best50_lat_long2.csv",row.names = FALSE)

####End best 50#######

##Combine best and worst ###
best50_worst50_lat_long<- rbind(best50_lat_long,worst50_lat_long)

write.csv(best50_worst50_lat_long, file = "C:/CUNY/Courses/IS608/Final_Project/semesterFinal/Final_Frid/best50_worst50_lat_long.csv",row.names = FALSE)



best50_zips<- select(best50, address, street,zipcode, city, state)

best50_zips<- data.frame('address'= best50$address, 
                         'street'= best50$street,
                         'postal code'= best50$zipcode, 
                         'city'= best50$city, 
                         'state' = best50$state)

write.csv(best50_zips, file = "C:/CUNY/Courses/IS608/Final_Project/semesterFinal/Final-Thurs/best50_zips.csv",row.names = FALSE)

head(best50,20)

###
## We send the best 50 to get latitude and longititude ##
bestzips2<- read.csv("C:/CUNY/Courses/IS608/Final_Project/semesterFinal/Final-Thurs/bestzips_2.csv",head=TRUE,sep=",")

bestzips2$name<- best50$RESTAURANT
bestzips2$marker<-'green'
bestzips3<- bestzips2


write.csv(bestzips3, file = "C:/CUNY/Courses/IS608/Final_Project/semesterFinal/Final-Thurs/bestzips3.csv",row.names = FALSE)


################################################################################################################
#                                                                                                              #
# 5 -data preparation for data selection by neighborhood and cuisine type, 2016.                               #
#                                                                                                              #
################################################################################################################

####  2016  ###
Manhattan_2016<- read.csv("https://raw.githubusercontent.com/simonnyc/IS608-NYC-data/master/MH_2016.csv",head=TRUE,sep=",")
m16<- Manhattan_2016

zipcodes<- read.csv("C:/CUNY/Courses/IS608/Final_Project/semesterFinal/Final-Wed/zipcodes2.csv",head=TRUE,sep=",")



# clean data
m16 <- m16[complete.cases(m16),]
m16$CUISINE.DESCRIPTION<- gsub("[^0-9A-Za-z///' ]", "",m16$CUISINE.DESCRIPTION)
m16$DBA<- gsub("[^0-9A-Za-z///' ]", "",m16$DBA)

# Finding and removing Outliers
library(outliers)
outlier_tf = outlier(m16$SCORE,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
data_new = m16[-find_outlier,]
#summary(m16$SCORE)


#finding Duplicate rows 
m16<- unique(m16)

library(dplyr)
library(stringr)
# summary data

# dis-regarding rows from initial inspections as grades and scores are not counted
m16<- filter(m16, INSPECTION.TYPE!='Cycle Inspection / Initial Inspection')
str(m16)
################
m16<- filter(m16, CRITICAL.FLAG=='Not Critical')
dim(m16_critical)

df16<- select(m16, DBA,ZIPCODE,CUISINE.DESCRIPTION,SCORE,GRADE)
df16<- rename(df16, RESTAURANT = DBA)
zipcodes<- rename(zipcodes, ZIPCODE =zipcode )
df16<- rename(df16, CUISINE =CUISINE.DESCRIPTION )
df16 <- (merge(zipcodes, df16, by = 'ZIPCODE'))
str(m16)
head(df16)

###################
unique(m16$CRITICAL.FLAG)
df16_neighbrood<- group_by(df16, Neighborhood, RESTAURANT, CUISINE,SCORE)

df16_neighbrood_count<- summarise(df16_neighbrood, count = n())

vis<- df16_neighbrood_count[order(desc(df16_neighbrood_count$SCORE)),] 
vis<- arrange(filter(vis, SCORE>0), SCORE)
vis<- vis[order(desc(vis$SCORE)),] 



vis_df<- select(vis, Neighborhood,CUISINE, RESTAURANT, SCORE)

## testvis<- tail(vis_df,50)

write.csv(vis_df, file = "C:/CUNY/Courses/IS608/Final_Project/semesterFinal/Final-Thurs/vis_df2.csv",row.names = FALSE)





## Assignment Getting and Cleaning Data
##The goal is to prepare tidy data that can be used for later analysis.
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)

##Check if the source is already in the folder
if (!file.exists("HAR_Dataset.zip")) {
    datasource<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(url = datasource, destfile ="HAR_Dataset.zip", mode='wb', cacheOK=FALSE)
    p<-unzip(zipfile="HAR_Dataset.zip")
}
dir<-getwd()
name<-c("test","train")

##Function stat_data to merge the 2 datasets
stat_data <- function(name) {

    stat<-data.frame()
    setwd("UCI HAR Dataset")
    activity<-read.table("activity_labels.txt")  
    names(activity)<-c("ID_a","activity") 
    features<-read.table("features.txt") 
    names(features)<-c("ID_f","feature")
##Access to the folder to merge the stat data and time data of the 2 sets(test and train)
for (j in name){
    setwd(dir)
    setwd("UCI HAR Dataset")    
    wkd<-paste(j,c("/"),sep = "")    
    setwd(wkd)
##Read files and get the labels 

    subject<-read.table(paste("subject_",j,".txt",sep = ""))
    names(subject)<-c("ID_vol")

    set<-read.table(paste("X_",j,".txt",sep = ""))
    names(set)<-features$feature

    lables<-read.table(paste("y_",j,".txt",sep = ""))
    names(lables)<-c("ID_act")

##create the data set
    data<-data.frame()
    data<-rbind(data,set)
    data<-cbind(subject$ID_vol,data)
    names(data)[1]<-c("ID_vol")
    data<-cbind(lables$ID_act,data)
    names(data)[1]<-c("ID_act")
    ID_set<-rep(j,each = dim(data)[1])
    data<-cbind(ID_set,data)
##save in the global data.frame the statistical data
    stat<-rbind(stat,data)
}
stat
}

setwd(dir)
read<-stat_data(name)

##Part 2 Extracts only measurements of mean and standard deviation
onlyms<-read[c("ID_set","ID_act","ID_vol")]
name_search<-names(read)

##loop to get mean() variables
for ( i in 1:length(name_search)){
    c<-strsplit(name_search[i],"-", fixed = FALSE)
    c<-unlist(c)
        if (is.na(pmatch("mean()",c)) == FALSE){
           onlyms<-cbind(onlyms,read[name_search[i]])
        } else {
    }
    }
##loop to get std() variables
for ( i in 1:length(name_search)){
    c<-strsplit(name_search[i],"-", fixed = FALSE)
    c<-unlist(c)
    if (is.na(pmatch("std()",c)) == FALSE){
        onlyms<-cbind(onlyms,read[name_search[i]])
    } else {
    }
}
##Loop to get Mean variables
for ( i in 1:length(name_search)){
    x<-name_search[i]
    x<-gsub('([[:upper:]])', ' \\1', x)
    x<-strsplit(x," ", fixed = FALSE)
    x<-unlist(x)
    x<-strsplit(x,")", fixed = FALSE)
    x<-unlist(x)
    if (is.na(pmatch("Mean",x)) == FALSE){
        onlyms<-cbind(onlyms,read[name_search[i]])
    } else {
    }
}
for ( i in 1:length(name_search)){
    c<-strsplit(name_search[i],"-", fixed = FALSE)
    c<-unlist(c)
    if (is.na(pmatch("meanFreq()",c)) == FALSE){
        onlyms<-cbind(onlyms,read[name_search[i]])
    } else {
    }
}    


##Part3 Name activity with descriptive names
onlyms<-tbl_df(onlyms)
setwd(dir)
activity<-read.table("UCI HAR Dataset/activity_labels.txt")  
activity$V2<-as.character(activity$V2)
number<-onlyms$ID_act
onlyms<-mutate(onlyms,activity_ext=activity[match(ID_act,activity[,1]),2])

##Part4 rename the variables with descriptive names
tobechanged<-select(onlyms,-ID_set,-ID_act,-ID_vol,-activity_ext)

nametbc<-names(tobechanged)
z<-strsplit(nametbc,"-",fixed = FALSE)

for (i in 1:(length(z))){
x<-unlist(z[i])
x<-gsub("X","x",x)
x<-gsub("Y","y",x)
x<-gsub("Z","z",x)
x<-gsub("tGravity","TimeGravity",x)
x<-gsub("tBody","TimeBody",x)
x<-gsub("fBody","FrequencyBody",x)
x<-gsub('([[:upper:]])', " \\1", x) 
x<-strsplit(x," ")
x<-unlist(x)
x<-gsub("Acc","Accelerometer",x)
x<-gsub("Jerk","Jerk_signal",x)
x<-gsub("Gyro","Gyroscope",x)
x<-gsub("std()","Standard_deviation",x)
x<-gsub("Mag","Magnitude",x)
x<-unique(x[x != ""]) 
nametbc[i]<-paste(x,collapse = "_") 
}
names(tobechanged)<-nametbc
id_vector<-select(onlyms,ID_set,ID_vol,ID_act,activity_ext)
tidy<-cbind(id_vector,tobechanged)
tidy<-tbl_df(tidy1)

##Part5 Average of average of each variable for each activity and each subject
averages_tidy<- ddply(tidy, .(ID_vol,activity_ext), function(x) colMeans(x[, 5:90]))
write.table(averages_tidy, "averages_tidy.txt", row.name=FALSE)

#' ---
#' title: "run_analysis"
#' author: "f.soltesz (fruszi)"
#' ---

library(dplyr)
library(data.table)
library(tidyr)

# ***************************************************************
# 0. getting files - NEEDS TO RUN ONLY ONCE
# ***************************************************************
path <- "C:\\Users\\fishie\\Desktop\\coursera\\UCI_HAR"
setwd(path)

fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl,destfile="./Dataset.zip")


unzip(zipfile="Dataset.zip",exdir=path)



# ***************************************************************
# 0. loading data files
# ***************************************************************
# load packages if they haven't been loaded yet
require(dplyr)
require(data.table)
require(tidyr)

# set up path and folders
path <- "C:\\Users\\fishie\\Desktop\\coursera\\UCI_HAR\\UCI HAR Dataset"
trainfolder <- "train"
testfolder <- "test"

# .................. subject files
dataSubjectTrain <- tbl_df(read.table(file.path(path, trainfolder, "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(path, testfolder , "subject_test.txt" )))

# .................. acivity files
dataActivityTrain <- tbl_df(read.table(file.path(path, trainfolder, "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(path, testfolder , "Y_test.txt" )))

# .................. Read data files
dataTrain <- tbl_df(read.table(file.path(path, trainfolder, "X_train.txt")))
dataTest  <- tbl_df(read.table(file.path(path, testfolder , "X_test.txt" )))

# collect garbage after bigger data, free up OS memory
gc()


# ***************************************************************
# 1. Merging training and test data into one set, renaming variables
# ***************************************************************
# merging training and test data within Subject and Activity 
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)

# renaming for datasets above columns
setnames(alldataSubject, "V1", "subject")
setnames(alldataActivity, "V1", "activityNum")

# merging training and test data
dataTable <- rbind(dataTrain, dataTest)

# renaming variables according to features (after reading features from file)
dataFeatures <- tbl_df(read.table(file.path(path, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

# rename activity columns (after reading labels from file)
activityLabels<- tbl_df(read.table(file.path(path, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)


# ***************************************************************
# 2. Extract MEAN and SD measurements from all measurements
# ***************************************************************
# extract mean and std strings from feature names
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE)

# get mean and sd measurements, union with subject and activityNum
dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 


# ***************************************************************
# 3. Naming activities with descriptive labels
# ***************************************************************
# merge names of activities with dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

# aggregate dataTable with variable means and sort by subject and Activity
dataAggr <- aggregate(. ~ subject - activityName, data = dataTable, mean) 
# and sort by subject and Activity
dataTable <- tbl_df(arrange(dataAggr,subject,activityName))


# ***************************************************************
# 4. Appropriately labelling variables in dataset
# ***************************************************************
# now (before renaming)
print("variable names before renaming: ")
head(str(dataTable))

# renaming variable names
names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

# after renaming
print("variable names after renaming: ")
head(str(dataTable))


# ***************************************************************
# 4. Write tidied data into file
# ***************************************************************
# comma separated file
write.table(dataTable, file.path(path,"TidyData.csv"), row.name=FALSE, sep=',')


# create md
rmarkdown::render(file.path(path,"run_analysis.R"))


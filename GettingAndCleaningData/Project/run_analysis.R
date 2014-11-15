# course project for getting and cleaning data

# load packages

packages <- c("data.table", "reshape2")
sapply(packages, require, character.only = TRUE, quietly = TRUE)

# set the working directory to this particular course project.
# if you'd like to reproduce this script, modify the command below to 
# reflect your own working directory.

setwd("C:/Users/Sarah/Documents/Sarah/DataScience_Courses_JHU/GettingAndCleaningData/Project")

# download and unzip files to current working directory

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "dataset.zip", mode = "wb")
unzip("dataset.zip") ## This creats "UCI HAR Dataset" directory

# take a look what is in "UCI HAR Dataset" directory

path <- file.path(getwd(), "UCI HAR Dataset")
list.files(path, recursive = TRUE)

# read the  training data files

subjectTrain <- fread(file.path(path, "train", "subject_train.txt"))
trainingSet <- data.table(read.table(file.path(path, "train", "X_train.txt")))
activityTrain <- fread(file.path(path, "train", "y_train.txt"))

# read the test data files

subjectTest <- fread(file.path(path, "test", "subject_test.txt"))
testSet <- data.table(read.table(file.path(path, "test", "X_test.txt")))
activityTest <- fread(file.path(path, "test", "y_test.txt"))

# merge all training data (subjectTrain, activityTrain and trainingSet)

mergedTrain <- cbind(subjectTrain, activityTrain, trainingSet)
setnames(mergedTrain, 1:2, c("subject", "activity"))

# merge all test data (subjectTest, activityTest, testSet)

mergedTest <- cbind(subjectTest, activityTest, testSet)
setnames(mergedTest, 1:2, c("subject", "activity"))

# merge traing data and test data and create one data set "mergedData"

mergedData <- rbind(mergedTrain, mergedTest)

# extract measurements on the mean and standard deviation 

## read the "features" file and set the column names
features <- fread(file.path(path, "features.txt"))
setnames(features, 1:2, c("featureNum", "featureName"))
## subset features by selecting measurements on the mean and standard deviation 
features <- features[grepl("mean\\(\\)|std\\(\\)", features$featureName)]
features <- features[, featureCode:=paste0("V", featureNum)]

## subset "mergedData" variables with only the measurements on mean and standard deviation
select <- c("subject", "activity", features$featureCode)
mergedData <- mergedData[, select, with = FALSE]

# use descriptive activity names to name the activities

activityLabels <- fread(file.path(path, "activity_labels.txt"))
setnames(activityLabels, 1:2, c("activity", "activityName"))

mergedData <- merge(activityLabels, mergedData, by="activity", all=TRUE)
mergedData <- mergedData[, activity:=subject]
mergedData <- mergedData[, subject := NULL]
setnames(mergedData, 1:2, c("subject", "activity"))

# label the data set (mergeData) with descriptive variable names

## replace the default variable names with the descriptive
## names found in the "feature" file
descriptiveName <- features$featureName
setnames(mergedData, 3:68, descriptiveName) 

# tidy the data
## collapse the 66-element feaure vector (as 66 columns) 
## into one variable, named "feature" 
## this turns the data set from a wide form to a long form
setkey(mergedData, subject, activity)
mergedData <- melt(mergedData, key(mergedData), variable.name = "feature")

## split the "feature" variable into multiple varibles as follows
s <- mergedData$feature
mergedData$domain <- ifelse(grepl("^t", s), "Time", 
                            ifelse(grepl("^f", s), "Freq", NA))
mergedData$instrument <- ifelse(grepl("Acc", s, ignore.case=TRUE), "Accelerometer",
                                ifelse(grepl("Gyro", s, ignore.case=TRUE), "Gyroscope", NA))
mergedData$acceleration <- ifelse(grepl("Body", s, ignore.case=TRUE), "Body",
                                  ifelse(grepl("Gravity", s, ignore.case=TRUE), "Gravity", NA))                        
mergedData$jerk <- ifelse(grepl("Jerk", s, ignore.case=TRUE), "Jerk", NA)
mergedData$magnitude <- ifelse(grepl("Mag", s, ignore.case=TRUE), "Magnitude", NA)                        
mergedData$axis <- ifelse(grepl("X$", s), "X", 
                            ifelse(grepl("Y$", s), "Y", 
                                   ifelse(grepl("Z$", s), "Z", NA)))
mergedData$statistics <- ifelse(grepl("mean", s), "Mean",
                                ifelse(grepl("std", s), "Standard Deviation", NA))
## since the "feature" varible is redundant now, remove it from the data set
mergedData <- mergedData[, feature := NULL]
## move the "value" column to the end
setcolorder(mergedData, c(1, 2, 4:10, 3))

# create a separate independant tidy data set (named "mergedDataTidy" below) 
# with the average of each variable for each activity and each subject

## take each combination of the parameters specified in "setkey"
## and compute the average of the measurements and the number of the measurements
setkey(mergedData, subject, activity, domain, instrument, acceleration, 
       jerk, magnitude, axis, statistics)
mergedDataTidy <- mergedData[, list(number_of_measurement = .N, average = mean(value)), by = key(mergedData)]
## display the final tidy dataset
print(mergedDataTidy)

# write the tidy dataset to a text file

write.table(mergedDataTidy, "tidy_data.txt", row.names = FALSE)

# read "tidy_data.txt" back into R just to check if everything looks fine
# test <- read.table("tidy_data.txt", header = TRUE)
# display(test)
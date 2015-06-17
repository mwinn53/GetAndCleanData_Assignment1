# Getting and Cleaning Data, Programming Assignment 1
#
# This file contains the code that collects, cleans, and processes a data set. 
# The goal is to prepare tidy data that can be used for later analysis. There
# are five (5) requirements for this assignment, which are annotated in the 
# comment headers (##). Note, the requirements were not necessarily completed in 
# the order they were assigned.


rm(list=ls())

## 4. Appropriately labels the data set with descriptive variable names.

    # features.txt contains the column names for the data in x_train.txt 
    # and x_test.txt

    file <- "./UCI HAR Dataset/features.txt"
    varNames <- read.table(file, stringsAsFactors = FALSE)

    # Data is in x_train.txt and x_test.txt. Adding a column to identify
    # the original data set (e.g. test.type = "test" or "train") then combine 
    # the two into a single data frame

    file <- "./UCI HAR Dataset/test/X_test.txt"
    dataSet <- read.table(file, col.names = varNames$V2)

    file <- "./UCI HAR Dataset/test/y_test.txt"
    activities <- read.table(file)

    file <- "./UCI HAR Dataset/test/subject_test.txt"
    subjects <- read.table(file, stringsAsFactors = FALSE)

    dataSet$activity.code <- activities$V1
    dataSet$subject.id <- subjects$V1
    dataSet$test.type <- rep("test", nrow(dataSet))

    file <- "./UCI HAR Dataset/train/X_train.txt"
    dataTrain <- read.table(file,  col.names = varNames$V2)

    file <- "./UCI HAR Dataset/train/y_train.txt"
    activities <- read.table(file)

    file <- "./UCI HAR Dataset/train/subject_train.txt"
    subjects <- read.table(file, stringsAsFactors = FALSE)

    dataTrain$activity.code <- activities$V1
    dataTrain$subject.id <- subjects$V1
    dataTrain$test.type <- rep("train", nrow(dataTrain))

    dataSet <- rbind(dataSet, dataTrain)
    
    # Remove data that is no longer needed to conserve memory
    rm(dataTrain, activities, varNames, subjects)

## 3. Uses descriptive activity names to name the activities in the data set

    # activity.txt contatins the activity names w/index. These indeces
    #  (i.e., keys) correspond to the numbers in y_train.txt and y_test.txt
    
    file <- "./UCI HAR Dataset/activity_labels.txt"
    actNames <- read.table(file, 
                           col.names = c("activity.code", "activity.name"), 
                           stringsAsFactors = FALSE)
    
## 1. Merges the training and the test sets to create one data set.

    dataSet <- merge(dataSet,actNames)
    rm(actNames)

## 2. Extracts only the measurements on the mean and standard deviation 
##    for each measurement.

    # Of the 565 variables, mean measurements are in 
    #  33 variables and std are in 33 variables
    measures <- c(grep("(mean)[^(Freq)]", names(dataSet)), grep("std", names(dataSet)))

    # Add the subject.id, test.type, and activity.name columns
    measures <- c(measures, 563, 564, 565)
    
    finaldataSet <- dataSet[,measures]

## 5. From the [resulting] data set...create a second, independent tidy data 
##    set with the average of each variable for each activity and each subject.
    finaldataSet$meanAllVars <- rowMeans(finaldataSet[, 1:66])

    xt <- xtabs(meanAllVars ~ subject.id + activity.name, data = finaldataSet)

## Dictated by the assignment submission requirements

    write.table(xt, file = "tidydata.txt", row.names = FALSE)

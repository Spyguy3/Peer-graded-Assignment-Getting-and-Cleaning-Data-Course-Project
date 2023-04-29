##  This script is an assignment for the 'Getting & Cleaning Data' week 4 Peer reviewed assignment

##  The script outputs two data frame variables:
##  meansandstdevs -- a tidied extract of mean and standard deviation observations
##  groupedmeansandstdevs --the mean values of these observations for each subject and activity.

## A full description is available at the site where the data was obtained:
## <"http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones"> 
## Here are the data for the project: <"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip">  

## Submission objectives: Create an R script called run_analysis.R that does the following:
## 1. Merge the training and the test sets to create one data set.
## 2. Extract only the measurements on the mean and standard deviation for each measurement. 
## 3. Use descriptive activity names to name the activities in the data set
## 4. Appropriately label the data set with descriptive variable names. 
## 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

## Prepared 29 April 2023 by Bill Lisse

## Load the respective Librarys
library(data.table)
library(dplyr)

## Load the associated Meta Data

FeatureNames <- read.table(file = "features.txt")
ActivityLabels <- read.table(file = "activity_labels.txt",header = FALSE)

## Load the Training Data Set
train_subject <- read.table(file = "train/subject_train.txt",header = FALSE)
train_activity <- read.table(file = "train/y_train.txt",header = FALSE)
train_features <- read.table(file = "train/X_train.txt",header = FALSE)

## Load the Testing Data Set
test_subject <- read.table(file = "test/subject_test.txt",header = FALSE)
test_activity <- read.table(file = "test/y_test.txt",header = FALSE)
test_features <- read.table(file = "test/X_test.txt",header = FALSE)


## Merge the Training and Testing Data Set 
Subject <- rbind(train_subject,test_subject)
Activity <- rbind(train_activity,test_activity)
Features <- rbind(train_features,test_features)

## Name the Columns 
colnames(Features) <- t(FeatureNames[2])
colnames(Activity) <- "Activity"
colnames(Subject) <- "Subject"

## Create a complete Data Set

CompleteData <- cbind(Features,Activity,Subject)

## Extract the Means and Standards Deviation

colMeanStd <- grep(".*Mean.*|.*Std.*",x = names(CompleteData), ignore.case=TRUE)
filteredCols <- c(colMeanStd,562:563)
extractedData <- CompleteData[,filteredCols]

## Assign Descriptive Names to the Activity field

extractedData$Activity <- as.character(extractedData$Activity)
for(i in 1:6) {
  extractedData$Activity[extractedData$Activity == i] <- as.character(ActivityLabels[,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)

## Label the Data Set with descriptive variable names

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

## Extract Tidy Data Set

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

tidydata <- aggregate(. ~Subject+Activity,extractedData,mean) 
tidydata <- tidydata[order(tidydata$Subject,tidydata$Activity),]

## create a new data set

write.table(x = tidydata,file = "tidy.txt",row.names = FALSE)



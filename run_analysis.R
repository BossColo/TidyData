## Load the dplyr library for data manipulations
library(dplyr)

## Set local filename for dataset
filename <- "UCIHARDataset.zip"

## Download and unzip the dataset
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename)
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

## Load the various activity strings
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")

## Load the data 'features' and select only the mean and std observations
features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
selects <- features[grepl(".*mean\\(\\).*|.*std\\(\\).*", features[,2]),]

## Load the training data, and attach which subject each obervation belongs to, and which activity
## they were performing
train <- read.table("UCI HAR Dataset/train/X_train.txt")[selects[[1]]]
trainActivity <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
train <- cbind(trainSubject, trainActivity, train)

## Do the same for the test data
test <- read.table("UCI HAR Dataset/test/X_test.txt")[selects[[1]]]
testActivity <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")
test <- cbind(testSubject, testActivity, test)

## Combine the test and training data, and set the column names to the variable names
dataset <- rbind(train, test) %>%
  `colnames<-`(c("subject", "activity", selects[,2]))

## Link the activity labels to the activity indicator in the dataset as factors
dataset$activity <- factor(dataset$activity, levels = activityLabels[,1], labels = activityLabels[,2])

## As the subject doesn't have labels, just cast the subject number as a factor
dataset$subject <- as.factor(dataset$subject)

## Use dplyr to group by activity and subject, and then take means of the variables
dataset2 <- tbl_df(dataset) %>%
  group_by(activity, subject) %>%
  summarize_all(funs(average=mean))

## Write the grouped and meaned data to a file
write.table(dataset2, "tidy.txt", row.names = FALSE, quote = FALSE)
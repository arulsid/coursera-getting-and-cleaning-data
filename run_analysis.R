##########################################################################################################
# Coursera Getting and Cleaning Data Course Project

# run_analysis.r File Description:
# This script will perform the following steps
# 1. Downloads the zip file into the data folder and unzip it
# 2. Merges the training and the test sets to create one data set.
# 3. Extracts only the measurements on the mean and standard deviation for each measurement.
# 4. Uses descriptive activity names to name the activities in the data set
# 5. Appropriately labels the data set with descriptive variable names.
# 6. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##########################################################################################################

## 1. Download the zip file into the data folder and unzip it 
# Download the zip file
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

# unzip the data file
if (!file.exists("UCI HAR Dataset")) { 
  unzip(zipfile="./data/Dataset.zip",exdir="./data") }

# Get the file names in the unzipped folder
path_df <- file.path("./data", "UCI HAR Dataset")
files <-list.files(path_df, recursive=TRUE)
files

## Read the files required for the project - Activity, Features and Subject
# Features files 
featuresNames <- read.table(file.path(path_df, "features.txt"),header = FALSE)
featuresTrain <- read.table(file.path(path_df, "train", "X_train.txt"),header = FALSE)
featuresTest  <- read.table(file.path(path_df, "test" , "X_test.txt" ),header = FALSE)

# Subject files
subjectTrain <- read.table(file.path(path_df, "train", "subject_train.txt"),header = FALSE)
subjectTest  <- read.table(file.path(path_df, "test" , "subject_test.txt"),header = FALSE)

# Activity files
activityLabels <- read.table(file.path(path_df, "activity_labels.txt"),header = FALSE)
activityTrain <- read.table(file.path(path_df, "train", "Y_train.txt"),header = FALSE)
activityTest  <- read.table(file.path(path_df, "test" , "Y_test.txt" ),header = FALSE)

# Assign column names
names(featuresTrain) <- featuresNames[,2]
names(featuresTest) <- featuresNames[,2]
names(subjectTrain) <- "subjectId"
names(subjectTest) <- "subjectId"
names(activityLabels) <- c("activityId", "activityType")
names(activityTrain) <- "activityId"
names(activityTest) <- "activityId"

## 2. Merge the training and the test sets to create one data set.
# Combine Training Set
trainingData <- cbind(featuresTrain, subjectTrain, activityTrain)
  
# combine Test Set
testData <- cbind(featuresTest, subjectTest, activityTest)

# Merge all the data set
mergedData <- rbind(trainingData, testData)

## 3. Extract only the measurements on the mean and standard deviation for each measurement.
# From features_info.txt => mean(): Mean value & std(): Standard deviation
#  Get colnames with mean() and std() - escape metacharacters ( & ) with double backslash
measureNames <- featuresNames[, 2][grep("mean\\(\\)|std\\(\\)", featuresNames[, 2])]

# Add column names subjectId & activityId
selectedNames <- c(as.character(measureNames), "subjectId", "activityId" )

# Extract the values for measurements on the mean and standard deviation for each measurement
Data <- subset(mergedData,select=selectedNames)

## 4. Use descriptive activity names to name the activities in the data set
# Assign descriptive activity names from "activity_labels.txt"
Data <- merge(Data,activityLabels,by="activityId",all.x=TRUE);

## 5. Appropriately labels the data set with descriptive variable names.
# Descriptive variable names. From features_info.txt => Prefix t is replaced by time, 
# prefix f is replaced by frequency, Acc is replaced by Accelerometer, Gyro is replaced by Gyroscope, 
# Mag is replaced by Magnitude, BodyBody is replaced by Body. Also mean() by Mean, std() by StdDev
names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("-mean\\()", "Mean", names(Data))
names(Data)<-gsub("-std\\()", "StdDev", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))

# 6. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# aggregate on subjectId, activityId & get mean of other columns
tidyData <- aggregate(. ~subjectId + activityId, Data, mean)

# order by SubjectId, activityId
tidyData <- tidyData[order(tidyData$subjectId,  tidyData$activityId), ]

#write the tidyData to a file
write.table(tidyData, file = "./tidydata.txt",row.name=FALSE,sep='\t')

codebook(tidyData)

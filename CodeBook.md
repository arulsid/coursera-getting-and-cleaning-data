# CodeBook for the coursera getting and cleaning data project 

This codebook contains the information about the variables, data and transformations used in this project. The purpose of this project is to demonstrate our ability to collect, work with, and clean a data set.


##Source 
The data for this project was obtained from [UCI Machine Learning Repositoy] (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

[The source data can be found here.] (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

## Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. 

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

## Step 1. Download the zip file into the data folder and unzip it
After setting the source directory, create a folder to  download the file and unzip it.

## Step 2. Merges the training and the test sets to create one data set.
After the files are unzipped, read the following files

- features.txt
- activity_labels.txt
- subject_train.txt
- x_train.txt
- y_train.txt
- subject_test.txt
- x_test.txt
- y_test.txt
Name the columns and merge the files into a single data set

## Step 3. Extracts only the measurements on the mean and standard deviation for each measurement.
The file features_info.txt mentions that mean() is Mean value and std() is Standard deviation. Extract colnames with mean() and std() and escape metacharacters ( and ) with double backslash and then add column names subjectId & activityId to get only those measures.

## Step 4. Uses descriptive activity names to name the activities in the data set
Merge the data with "activity_labels.txt" by activityId to get descriptive activity names.

## Step 5. Appropriately labels the data set with descriptive variable names.
Using gsub() replace the column names with descriptive variable names.

## Step 6. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Create a tidy data file with average of each variable for each activity and subject.





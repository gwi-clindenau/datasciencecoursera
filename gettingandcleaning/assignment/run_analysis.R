# needed packages
install.packages("dplyr")
install.packages("tidyverse")
install.packeges("readr")
library("dplyr")
library("tidyverse")
library("readr")

# check for existing data and download data if not existing
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- "./data/getdata_projectfiles_UCI HAR Dataset.zip"
folder <- "./data/UCI HAR Dataset"
if (!file.exists(destfile)) { 
  download.file(fileURL, destfile = destfile)
}
if (!file.exists(folder)) {
  unzip(destfile, exdir = "./data/")
}
rm(fileURL, destfile, folder)

# read data to environment
subject_test <- read_tsv("./data/UCI HAR Dataset/test/subject_test.txt", col_names = FALSE)
subject_train <- read_tsv("./data/UCI HAR Dataset/train/subject_train.txt", col_names = FALSE)
features <- read_tsv("./data/UCI HAR Dataset/features.txt", col_names = FALSE)
features <- as.vector(features$X1)
X_test <- read_table2("./data/UCI HAR Dataset/test/X_test.txt", col_names = features, na = "NA", )
y_test <- read_tsv("./data/UCI HAR Dataset/test/y_test.txt", col_names = "Activity ID")
X_train <- read_table2("./data/UCI HAR Dataset/train/X_train.txt", col_names = features, na = "NA", )
y_train <- read_tsv("./data/UCI HAR Dataset/train/y_train.txt", col_names = "Activity ID")
activity_labels <- read_table2("./data/UCI HAR Dataset/activity_labels.txt", col_names = FALSE) 

# Use descriptive activity names to name the activities in the data set
colnames(activity_labels) <- c("Activity ID", "Activity")
activitys_test <- inner_join(y_test, activity_labels)
activitys_train <- inner_join(y_train, activity_labels)

# Join Subject, activity names and data
test <- data.frame(subject_test$X1, activitys_test$Activity, X_test)
train <- data.frame(subject_train$X1, activitys_train$Activity, X_train)

# Appropriately label the data set with descriptive variable names.
columns <- c("Subject", "Activity", features)
colnames(test) <- columns
colnames(train) <- columns

# Merge the training and the test sets to create one data set.
all_data <- rbind(test, train)

# Extract only the measurements on the mean and standard deviation for each measurement.
extracted_data <- select(all_data, Subject, Activity, contains("mean()") | contains("std()"))

# create a second, independent tidy data set with the average of each variable for each activity and each subject.
averages <- extracted_data %>% group_by(Subject, Activity) %>% summarise_all(list(mean))

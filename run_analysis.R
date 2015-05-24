## Developer: Brian Dawson
## Date: 05/24/2015
## Course: Getting and Cleaning Data - JH Week 3 Course

execute_analysis <- function(){
  ## load subject_test.txt file from UCI HAR Dataset
  subjectTest = read.table("UCI HAR Dataset/test/subject_test.txt")
  
  ## Read the x and y files
  xTest = read.table("UCI HAR Dataset/test/X_test.txt")
  yTest = read.table("UCI HAR Dataset/test/Y_test.txt")
  
  # load up the training data
  subjectTraining = read.table("UCI HAR Dataset/train/subject_train.txt")
  X_train = read.table("UCI HAR Dataset/train/X_train.txt")
  Y_train = read.table("UCI HAR Dataset/train/Y_train.txt")
  
  # load lookup features
  features <- read.table("UCI HAR Dataset/features.txt", col.names=c("featureId", "featureLabel"))
  
  activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("activityId", "activityLabel"))
  
  activities$activityLabel <- gsub("_", "", as.character(activities$activityLabel))
  
  includedFeatures <- grep("-mean\\(\\)|-std\\(\\)", features$featureLabel)
  
  # merge test and training data and then name them
  mergeSubjectData <- rbind(subjectTest, subjectTraining)
  
  names(mergeSubjectData) <- "subjectId"
  
  X <- rbind(xTest, X_train)
  
  X <- X[, includedFeatures]
  
  names(X) <- gsub("\\(|\\)", "", features$featureLabel[includedFeatures])
  
  Y <- rbind(yTest, Y_train)
  
  names(Y) = "activityId"
  
  activity <- merge(Y, activities, by="activityId")$activityLabel
  
  # merge data frames of different columns to form one data table
  data <- cbind(mergeSubjectData, X, activity)
  
  write.table(data, "tidyData_merged.txt")
  
  # create a dataset grouped by subject and activity after applying standard deviation and average calculations
  library(data.table)
  dataDT <- data.table(data)
  calculatedData<- dataDT[, lapply(.SD, mean), by=c("subjectId", "activity")]
  write.table(calculatedData, "tidyData_calculated.txt")
}

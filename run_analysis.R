#===============================================================================
# Analysis of the data collected from the accelerometers from the Samsung Galaxy S smartphone
#===============================================================================

#This script will process the raw data from:
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

#and realizes the following tasks:
#' 1 Merges the training and the test sets to create one data set.
#' 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
#' 3 Uses descriptive activity names to name the activities in the data set
#' 4 Appropriately labels the data set with descriptive variable names. 
#' 5 From the data set in step 4, creates a second, independent tidy data set 
#' with the average of each variable for each activity and each subject.


#################################################################################
# Preliminary
#################################################################################

#Getting the data
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,"./data/datas.zip")
unzip("./data/datas.zip",exdir="./data")

#-------------------------------------------------------------------------------
#'Reading all the files

#'features.txt': List of all features.
#'activity_labels.txt': Links the class labels with their activity name.

features = read.table("./data/UCI HAR Dataset/features.txt")
activity_labels = read.table("./data/UCI HAR Dataset/activity_labels.txt")

#'subject_test.txt Subject ID
#'test/X_test.txt': Test set.
#'test/y_test.txt': Test labels.

subject_test = read.table("./data/UCI HAR Dataset/test/subject_test.txt")
X_test = read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test = read.table("./data/UCI HAR Dataset/test/y_test.txt")

#'subject_train.txt Subject ID
#'train/X_train.txt': Training set.
#'train/y_train.txt': Training labels.

subject_train = read.table("./data/UCI HAR Dataset/train/subject_train.txt")
X_train = read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train = read.table("./data/UCI HAR Dataset/train/y_train.txt")


#-------------------------------------------------------------------------------
# Assembling the test and train table

test_data <- cbind(subject_test,y_test,X_test)
train_data <- cbind(subject_train,y_train,X_train)

colnames(test_data) <- c("subject","activity_labels",features$V2)
colnames(train_data) <- c("subject","activity_labels",features$V2)

#-------------------------------------------------------------------------------
# Replace activity label

test_data$activity_labels <- sapply(test_data$activity_labels, FUN = function(x) {
        activity_labels$V2[x]
})
train_data$activity_labels <- sapply(train_data$activity_labels, FUN = function(x) {
        activity_labels$V2[x]
})

#-------------------------------------------------------------------------------
# Add a column specifying the original dataset

test_data$original_dataset <- "Test"
train_data$original_dataset <- "Train"

#################################################################################
#' 1 Merges the training and the test sets to create one data set.
#################################################################################

merged_data <- rbind(test_data,train_data)

#################################################################################
#' 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
#################################################################################

library(dplyr)
meancol <- grepl("mean\\(\\)",names(merged_data))
stdcol <- grepl("std\\(\\)",names(merged_data))
#Add the 2 index col
stdcol[c(1,2)] <- T

filtered_data <- merged_data[,meancol|stdcol]

#################################################################################
#' 3 Uses descriptive activity names to name the activities in the data set
#################################################################################

#See above: # Replace activity label

#################################################################################
#' 4 Appropriately labels the data set with descriptive variable names. 
#################################################################################

#'Descriptive --> no shortcut (ex no Dx for Diagnosis)

current_names <- names(filtered_data)

#rename mean() into meanvalue
new_names <- gsub("mean\\(\\)","meanvalue",current_names)
#rename std() into standarddeviation
new_names <- gsub("std\\(\\)","standarddeviation",new_names)
#replace names beginning with t by time
new_names <- gsub("^t","time",new_names)
#replace names beginning with f by frequency
new_names <- gsub("^f","frequency",new_names)
#remove-
new_names <- gsub("-","",new_names)
#lowercase
new_names <- tolower(new_names)
#acc to accelleration
new_names <- gsub("acc","accelleration",new_names)
#end by x replaced to axisx
new_names <- gsub("x$","axisx",new_names)
#end by y replaced to axisy
new_names <- gsub("y$","axisy",new_names)
#end by z replaced to axisz
new_names <- gsub("z$","axisz",new_names)

#affect the names
names(filtered_data) <- new_names

#################################################################################
#' 5 From the data set in step 4, creates a second, independent tidy data set 
#' with the average of each variable for each activity and each subject.
#################################################################################

library(dplyr)
#aggregate according to two factors
tidyData <- aggregate(filtered_data,
                      by=list(filtered_data$subject,filtered_data$activity_labels),
                      FUN=mean)

#remove useless columns
tidyData<-select(tidyData,c(1:2,timebodyaccellerationmeanvalueaxisx:frequencybodybodygyrojerkmagstandarddeviation))

#reuse correct columns labels
names(tidyData) <- new_names

#write to a csv file
data.table::fwrite(x = tidyData, file = "tidyData.csv", quote = FALSE)

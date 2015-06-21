# download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")
# unzip the data inside of a "/R/Cleaning_data/" directory

# set working directory:
setwd("~/R/Cleaning_data/UCI HAR Dataset/")

# clean out the workspace:
rm(list = ls())

# Read feature (variable) and activity names :
activity_names <-read.table("activity_labels.txt")
features <- read.table("features.txt")

# Read data - training:
subject_tr <- read.table("train/subject_train.txt")
x_tr <- read.table("train/X_train.txt")
y_tr <- read.table("train/y_train.txt")

# Read data - test:
subject_test <- read.table("test/subject_test.txt")
x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")

# Replace number values for their activity names:
activity_length <- dim(activity_names)[1]
for (i in 1:activity_length){
  y_tr$V1 <- gsub(i,activity_names$V2[i], y_tr$V1)
  y_test$V1 <- gsub(i,activity_names$V2[i], y_test$V1)
}

# Edit feature (variable) names to make them look nicer
features$V2 <- gsub("\\(\\)-","_", features$V2)
features$V2 <- gsub("-","_", features$V2)

# Edit column names in X_train data
for (i in 1:dim(x_tr)[2]){
  names(x_tr)[i] <- features$V2[i]
}
# Edit column names in X_test data
for (i in 1:dim(x_test)[2]){
  names(x_test)[i] <- features$V2[i]
}

# Collect all "training" data to one table
x_tr_mean_or_stdev <- x_tr[grep("mean", names(x_tr))]
names(subject_tr) <- "Subject"
names(y_tr) <- "Activity"
mearged_table_tr <- cbind(subject_tr,y_tr,x_tr_mean_or_stdev)

# Collect all "test" data to one table
x_test_mean_or_stdev <- x_test[grep("mean", names(x_test))]
names(subject_test) <- "Subject"
names(y_test) <- "Activity"
mearged_table_test <- cbind(subject_test,y_test,x_test_mean_or_stdev)

# Mearge "training" and "test" data
long_data_set <- merge(mearged_table_tr, mearged_table_test, by = intersect(names(mearged_table_tr), names(mearged_table_test)), all = TRUE)

# use dplyr package for easy selecting and filtering data
library(dplyr)
# convert to tbl_df in order to use select and filter
temp_DF <- tbl_df(long_data_set)
names_long_data = names(long_data_set);

# data_means will be a 1D vector of new data
data_means <- numeric(0);
for (i in 1:30) { # there are 30 subjects
  for (j in 1:6) { # there are 6 activities
    data_means <- cbind(data_means, (i));
    data_means <- cbind(data_means, (j));
    for (k in 3:length(long_data_set)[1]) { # loop throught number of variables
      temp2<-select(filter(temp_DF,Subject == i, Activity==activity_names[j,2]),  k)
      data_means <- cbind(data_means, mean(temp2[[1]]))
    }
  }
}
      
# reshape data_means into a matrix 
new_data <- matrix(data_means,ncol=length(long_data_set)[1],byrow=TRUE)
colnames(new_data) <- names_long_data
# convert matrix to a table
new_data <- as.table(new_data)

write.table(new_data, file = "Tidy_data_means.txt",row.name=FALSE)



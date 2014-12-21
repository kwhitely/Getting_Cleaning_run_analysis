library(dplyr)
library(tidyr)
library(knitr)
#read in the 7 files that are used to create 1 tidy dataset
subject_test <- read.table("~/Coursera/subject_test.txt")
subject_train <- read.table("~/Coursera/subject_train.txt")
X_test <- read.table("~/Coursera/X_test.txt")
y_test <- read.table("~/Coursera/y_test.txt")
X_train <- read.table("~/Coursera/X_train.txt")
y_train <- read.table("~/Coursera/y_train.txt")
features <- read.table("~/Coursera/features.txt")
#combine the training and the test sets of data
x <- rbind(X_train, X_test)
#assign the variable names as given in features.txt
colnames(x) <- features[,2]
#combine the two datasets containing the activities performed
y <- rbind(y_train, y_test)
#replace the activity number with the associated descriptive name
y[ , 1] <- gsub(1, "WALKING", y[ , 1])
y[ , 1] <- gsub(2, "WALKING_UPSTAIRS", y[ , 1])
y[ , 1] <- gsub(3, "WALKING_DOWNSTAIRS", y[ , 1])
y[ , 1] <- gsub(4, "SITTING", y[ , 1])
y[ , 1] <- gsub(5, "STANDING", y[ , 1])
y[ , 1] <- gsub(6, "LAYING", y[ , 1])
#combine the two datasets containting the subject IDs
subject<- rbind(subject_train, subject_test)
#combine the subject ID and the activity datatests
subject_activity <- cbind(subject, y)
#assign descriptive names to subject_activity
colnames(subject_activity) <- c("subject_id", "activity")
#combine all datasets into 1
full_data <- cbind(subject_activity, x)
variables <- colnames(full_data)
#find variable names that contain "activity", "subject", "mean",
#or "std" and assign to a logical vector
keep <- grepl("activity", variables) | grepl("subject", variables) |
        grepl("mean", variables) | grepl("std", variables)
#keep any data that makes the logical vector TRUE to have a dataset
#consisting of the the subject, activity, and data pertaining to
#mean and standard deviation
mean_std_data <- full_data[, keep == TRUE]
#group the data by subject ID and activity and calculate the mean
#of each measurement
tidydata <- mean_std_data %>%
        group_by(subject_id, activity) %>%
                summarise_each(funs(mean))
write.table(tidydata, "run_analysis.txt" ,row.name = FALSE)
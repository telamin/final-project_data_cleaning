##############################################
## Located our datasets:
## having the dataset from:
##http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
##The data used for the current  project downloaded and unzipped in our working directory:
##
###############################################
## Libraries needed
library(utils)
## reading the training dataset part
train.data <- read.table("C://Users//abdu//Desktop//final_project//ucidataset//train//X_train.txt")
dim(train.data); head(train.data, 1)

train.label <- read.table("C://Users//abdu//Desktop//final_project//ucidataset//train//y_train.txt")
table(train.label)

train.subject <- read.table("C://Users//abdu//Desktop//final_project//ucidataset//train//subject_train.txt")
dim(train.subject)
## reading the test dataset part
test.data <- read.table("C://Users//abdu//Desktop//final_project//ucidataset//test//X_test.txt")
dim(test.data)

test.label <- read.table("C://Users//abdu//Desktop//final_project//ucidataset//test//y_test.txt")
table(test.label)

test.subject <- read.table("C://Users//abdu//Desktop//final_project//ucidataset//test//subject_test.txt")
dim(test.subject)

## Merging both  the traioning and test datasets
join.data <- rbind(train.data, test.data)
dim(join.data)
join.label <- rbind(train.label, test.label)
dim(join.label)
join.subject <- rbind(train.subject, test.subject)
dim(join.subject)
## in this stage below of our data cleaning and preparation,we will be
## having only the measurements on the average and standard deviation.
data.features <- read.table("C://Users//abdu//Desktop//final_project//ucidataset//features.txt")
dim(data.features)#561 x 2

mean.stdev <- grep("mean\\(\\)|std\\(\\)", data.features[,2])
length(mean.stdev) ## 66

join.data <- join.data[, mean.stdev]
dim(join.data) #10299 x 66

##Cleaning the data:
#cleaning
#we need to remove the "()"
names(join.data) <- gsub("\\(\\)", "", data.features[mean.stdev, 2]) 
#Capitalize S
names(join.data) <- gsub("std", "Std", names(join.data))
#Capitalize M
names(join.data) <- gsub("mean", "Mean", names(join.data))
## then remove the "-" in column names
names(join.data) <- gsub("-", "", names(join.data))



## in this stage of our on-going process of data cleaning
## we  use descriptive activity names to name activities in our dataset
activities <- read.table("C://Users//abdu//Desktop//final_project//ucidataset//activity_labels.txt")
## change to lower case to prevent typos
activities[, 2] <- tolower(gsub("_", "", activities[, 2]))
substr(activities[2, 2], 8, 8) <- toupper(substr(activities[2,2], 8, 8))
substr(activities[3, 2], 8, 8) <- toupper(substr(activities[3,2], 8, 8))
activities.label <- activities[join.label[,1], 2]
join.label[, 1] <- activities.label
names(join.label) <- "activities"

## selecting names
## name the dataset with descriptive column names
names(join.subject) <- "subjects"
clean.data <- cbind(join.subject, join.label, join.data)
dim(clean.data) ## 10299 x 68
write.table(clean.data, "merged_data.txt") ##writing the first dataset
##in this next stage we:
## build a second independent tidy data set that contains the average of each
## variable for each activity and each subject
subject.len <- length(table(join.subject))
activities.len <- dim(activities)[1] ## 6
column.len <- dim(clean.data)[2] ##68
results <- matrix(NA, nrow=subject.len*activities.len, ncol=column.len)
colnames(results) <- colnames(clean.data)
row <- 1
for (i in 1:subject.len){
  for (j in 1:activities.len){
    results[row, 1] <- sort(unique(join.subject)[,1])[i]
    results[row, 2] <- activities[j,2]
    bools1 <- i == clean.data$subject
    bools2 <- activities[j, 2] == clean.data$activities
    results[row, 3:column.len] <- colMeans(clean.data[bools1&bools2, 3:column.len])
    row <- row + 1
  }
}

head(results, 1)
write.table(results, "data_means.txt") #writing the second dataset
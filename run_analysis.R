#Getting and Cleaning Data Course Project

#1.Merges the training and the test sets to create one data set.

# read the test data (3 files) and merge into one test data frame
subject_test <- read.table('subject_test.txt')
x_test <- read.table('X_test.txt')
y_test <- read.table('y_test.txt')
test <- cbind(subject_test,y_test,x_test)

# read the train data (3 files) and merge into one train data frame
subject_train <- read.table('subject_train.txt')
x_train <- read.table('X_train.txt')
y_train <- read.table('y_train.txt')
train <- cbind(subject_train,y_train,x_train)

# merge test and train data frame to create one data set
dat <- rbind(test, train)

#name dat set 'dat' each column
feature <- read.table('features.txt')
colnames(dat)[1] <- 'SubjectId'
colnames(dat)[2] <- 'Activity'
colnames(dat)[3:563] <- as.character(feature$V2)


#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
dat_short <- dat[grepl('Id', colnames(dat))|grepl('Activity', colnames(dat))|grepl('mean\\()', colnames(dat))|grepl('std\\()', colnames(dat))]


#3.Uses descriptive activity names to name the activities in the data set
activity_label <- read.table('activity_labels.txt')
dat_short$Activity <- activity_label$V2[dat_short$Activity]

#4.Appropriately labels the data set with descriptive variable names.
names(dat_short) <- sub('^t', 'Time.', sub('^f', 'Freq.',gsub('\\()', '', gsub('-', '.',names(dat_short)))))

#5.From the data set in step 4, creates a second, independent tidy data set 
#       with the average of each variable for each activity and each subject.
library(dplyr)
activity_clean <- dat_short %>% group_by(SubjectId,Activity) %>% summarise_each(funs(mean))

#save the tidy data into file 'activity_clean.txt'
write.table(activity_clean, file='activity_clean.txt', row.name=FALSE)

library(dplyr)

#download the data set

filename <- "Coursera_DS3_Final.zip"

# Checking if file exists.
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileURL, filename , method="curl")
}  

# Checking if folder exists and unzip 
if (!file.exists("UCI HAR Dataset")) { 
        unzip(filename) 
}
#1.Merges the training and the test sets to create one data set.
 
#Read the data into test and train sets 
Xtest <- read.table("/Users/nuradhika/Documents/Coursera/Cleaningdata/Week4/Dataset/test/X_test.txt",header = FALSE )
Ytest <- read.table("/Users/nuradhika/Documents/Coursera/Cleaningdata/Week4/Dataset/test/Y_test.txt",header = FALSE)
Xtrain <- read.table("/Users/nuradhika/Documents/Coursera/Cleaningdata/Week4/Dataset/train/X_train.txt",header = FALSE)
Ytrain <- read.table("/Users/nuradhika/Documents/Coursera/Cleaningdata/Week4/Dataset/train/Y_train.txt",header = FALSE)

#read the subject text files in test and train sets
testsubject <- read.table("/Users/nuradhika/Documents/Coursera/Cleaningdata/Week4/Dataset/test/subject_test.txt",header = FALSE)
trainsubject <- read.table("/Users/nuradhika/Documents/Coursera/Cleaningdata/Week4/Dataset/train/subject_train.txt",header = FALSE)

#read the feature file to get all features of the data set
features <- read.table("/Users/nuradhika/Documents/Coursera/Cleaningdata/Week4/Dataset/features.txt",head=FALSE)


# set names the columns 
colnames(Xtest) <- features$V2
colnames(Ytest) <- "activity"
colnames(testsubject) <- "subject"

colnames(Xtrain) <- features$V2
colnames(Ytrain) <- "activity"
colnames(trainsubject) <- "subject"


#bind the rows for each data set to merge the data set 
feature_data <- rbind(Xtest, Xtrain)
activity_data <- rbind(Ytest, Ytrain)
subject_data <- rbind(testsubject, trainsubject)
merged_data <- cbind(subject_data,feature_data, activity_data )


#2. Extracts only the measurements on the mean and standard deviation for each measurement. .

# select the names of features with mean and std 
data_summary<-features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)]
#subset the data set 
requiredNames<-c(as.character(data_summary), "activity", "subject" )
data <- subset(merged_data, select=requiredNames)


#3. Uses descriptive activity names to name the activities in the data set
#read activity to get descriptive activity names
activities <- read.table("/Users/nuradhika/Documents/Coursera/Cleaningdata/Week4/Dataset/activity_labels.txt")
#Factorize Variable activity in the data set 
data$activity<-factor(data$activity,labels=activities[,2])


#4. Appropriately labels the data set with descriptive variable names
names(data)<-gsub("Acc", "Accelerometer", names(data))
names(data)<-gsub("Gyro", "Gyroscope", names(data))
names(data)<-gsub("BodyBody", "Body", names(data))
names(data)<-gsub("Mag", "Magnitude", names(data))
names(data)<-gsub("^t", "Time", names(data))
names(data)<-gsub("^f", "Frequency", names(data))

#test data set 
names(data)

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
TidyData<-aggregate(. ~subject + activity, data, mean)
TidyData<-TidyData[order(TidyData$subject,TidyData$activity),]

# Writing second tidy data set in txt file
write.table(data, file = "tidydata.txt",row.name=FALSE,quote = FALSE, sep = '\t')


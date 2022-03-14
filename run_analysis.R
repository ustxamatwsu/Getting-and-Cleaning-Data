library(dplyr)

# Clean up memory
rm(list=ls())

# Set working directory 
setwd('C:/Rtemp/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/');

# Load the data in 

features <- read.table("features.txt", col.names = c("n","Parameter"))
activities <- read.table("activity_labels.txt", col.names = c("ActivityID", "activity"))

subject_test <- read.table("test/subject_test.txt", col.names = "SubjectID")
activity_test <- read.table("test/y_test.txt", col.names = "ActivityID")
value_test <- read.table("test/X_test.txt", col.names = features$Parameter)

subject_train <- read.table("train/subject_train.txt", col.names = "SubjectID")
activity_train <- read.table("train/y_train.txt", col.names = "ActivityID")
value_train <- read.table("train/X_train.txt", col.names = features$Parameter)

# I trim data columns early on so we mess with smaller datasets later
value_train <- value_train %>% select(contains("mean"), contains("std"))
value_test <- value_test %>% select(contains("mean"), contains("std"))

# Check if colum name still match. if not, I will use my own loop to trim
# but it seems that the above "select ()" produced consistent column sequence on the two sets. cool.

train_cols = colnames( value_train )
test_cols = colnames( value_test )
if ( length(train_cols) != length(train_cols) ){
	stop("Go use your own code")
}else{	
	for (i in 1:length(train_cols)){
		if (train_cols[i] !=  test_cols[i] ){
			stop("Go use your own code")
		}
	}
}

# Step 1: Merges the training and the test sets to create one data set.

Subjects 	<- rbind(subject_train, subject_test)
Acivitities <- rbind(activity_train, activity_test)
Values 	<- rbind(value_train, value_test)

FinalData 	<- cbind(Subjects, Acivitities, Values)

# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
# I already did this


# Step 3: Uses descriptive activity names to name the activities in the data set.
# I am keeping the activityId column here and will add the column Activity AFTER the dataset being summerized 
# for a) Group by ActivityID (int) works faster than by Activity (str)
#     b) much less data rows then ( 7352+4947 = 10299 rows now vs just 30x6 = 180 rows then)

nrow(FinalData)

# Step 4: Appropriately labels the data set with descriptive variable names.

colNames  = colnames(FinalData); 

# Check column names, not a very big list, 
colNames  

for (i in 3:length(colNames)) 
{
  colNames[i]<-gsub("mean", "Mean", colNames[i], ignore.case = TRUE)
  colNames[i]<-gsub("std", "Std", colNames[i], ignore.case = TRUE)
  colNames[i]<-gsub("\\.", "", colNames[i])
  colNames[i]<-gsub("Acc", "Accelerometer", colNames[i])
  colNames[i]<-gsub("Gyro", "Gyroscope", colNames[i])
  colNames[i]<-gsub("BodyBody", "Body", colNames[i])
  colNames[i]<-gsub("Mag", "Magnitude", colNames[i])
  colNames[i]<-gsub("^t", "Time", colNames[i])
  colNames[i]<-gsub("^f", "Frequency", colNames[i])
  colNames[i]<-gsub("tBody", "TimeBody", colNames[i])
  colNames[i]<-gsub("-freq()", "Frequency", colNames[i], ignore.case = TRUE)
  colNames[i]<-gsub("angle", "Angle", colNames[i])
  colNames[i]<-gsub("gravity", "Gravity", colNames[i])
};


# Check column names after the change. they look good
colNames;

colnames(FinalData) = colNames;


# Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Note I used the ActivityID here, so the result is order by subject id, and activity id (not activity)

FinalData <- FinalData %>% group_by(SubjectID, ActivityID) %>% summarise_all(list(mean))

# I now do step 3, replace AcivityId with Activity 
FinalData$ActivityID <- activities[FinalData$ActivityID, 2]
names(FinalData)[2] = "Activity"

# Check column names yet another time
colnames(FinalData)
   
# Check number of rows and colums
ncol(FinalData)
nrow(FinalData)
   
write.table(FinalData, "../../FinalData.txt", row.name=FALSE)



library(data.table)
library(dplyr)

# Read datasets

getwd()
# TEST DATASET
subject_test <- read.table(paste0(getwd(),"/UCIHARDataset/test/subject_test.txt"))
X_test <- read.table(paste0(getwd(),"/UCIHARDataset/test/X_test.txt"))                           
Y_test <- read.table(paste0(getwd(),"/UCIHARDataset/test/Y_test.txt"))

# TRAIN DATASET
subject_train <- read.table(paste0(getwd(),"/UCIHARDataset/train/subject_train.txt"))
X_train <- read.table(paste0(getwd(),"/UCIHARDataset/train/X_train.txt"))                           
Y_train <- read.table(paste0(getwd(),"/UCIHARDataset/train/Y_train.txt"))

features_Names <- read.table(paste0(getwd(),"/UCIHARDataset/features.txt"), col.names=c("feature_ID", "feature_Name"))
activity_labels <- read.table(paste0(getwd(), "/UCIHARDataset/activity_labels.txt"))

# Merge the datasets by subject, feature and activity
subject <- rbind(subject_test, subject_train)
features <- rbind(X_test, X_train)
activity <- rbind(Y_test, Y_train)

# Give name to columns. 
colnames(subject) <- "Subject"
colnames(features) <- t(features_Names$feature_Name)
colnames(activity) <- "Activity"

# Merge subject, features and activity. 
cleandata <- cbind(subject, features, activity)

# Create a new dataframe by extracting columns where columnname contains "mean" or "Std". 
mydata <- grep(".*Mean.*|.*Std.*", names(cleandata), ignore.case=TRUE)
meanSD_col <- c(mydata, 1, 563)
cleandata_meanSD <- cleandata[,meanSD_col]

# Use descriptive activity names to name the activities in the data set
# Replace the old name by the new name. 
# Factor the activities
cleandata_meanSD$Activity <- as.character(cleandata_meanSD$Activity)
cleandata_meanSD$Activity[cleandata_meanSD$Activity == 1] <- "WALKING"
cleandata_meanSD$Activity[cleandata_meanSD$Activity == 2] <- "WALKING_UPSTAIRS"
cleandata_meanSD$Activity[cleandata_meanSD$Activity == 3] <- "WALKING_DOWNSTAIRS"
cleandata_meanSD$Activity[cleandata_meanSD$Activity == 4] <- "SITTING"
cleandata_meanSD$Activity[cleandata_meanSD$Activity == 5] <- "STANDING"
cleandata_meanSD$Activity[cleandata_meanSD$Activity == 6] <- "LAYING"
cleandata_meanSD$Activity <- as.factor(cleandata_meanSD$Activity)

# Appropriately labels the data set with descriptive variable names. 

names(cleandata_meanSD)<-gsub("^t", "Time", names(cleandata_meanSD))
names(cleandata_meanSD)<-gsub("^f", "Frequency", names(cleandata_meanSD))
names(cleandata_meanSD)<-gsub("tBody", "TimeBody", names(cleandata_meanSD))
names(cleandata_meanSD)<-gsub("-mean()", "Mean", names(cleandata_meanSD))
names(cleandata_meanSD)<-gsub("-std()", "StandDev", names(cleandata_meanSD))
names(cleandata_meanSD)<-gsub("-freq()", "Frequency", names(cleandata_meanSD))
names(cleandata_meanSD)<-gsub("angle", "Angle", names(cleandata_meanSD))
names(cleandata_meanSD)<-gsub("Acc", "Accelerometer", names(cleandata_meanSD))
names(cleandata_meanSD)<-gsub("Gyro", "AngularSpeed", names(cleandata_meanSD))
names(cleandata_meanSD)<-gsub("BodyBody", "Body", names(cleandata_meanSD))
names(cleandata_meanSD)<-gsub("Mag", "Magnitude", names(cleandata_meanSD))
names(cleandata_meanSD)<-gsub("gravity", "Gravity", names(cleandata_meanSD))

# From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable 
# for each activity and each subject.

cleandata_meanSD <- data.table(cleandata_meanSD)
TidyData <- aggregate(. ~Subject + Activity, cleandata_meanSD, mean)
TidyData <- TidyData[order(TidyData$Subject,TidyData$Activity),]
write.table(TidyData, file = "TidyData.txt", row.names = FALSE)
